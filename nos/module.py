from __future__ import absolute_import
import ctypes
import os
import shutil
import tempfile

from . import llvm


class Module(object):

    def __init__(self, name):
        import uuid

        self.name = name
        self.module = llvm.ModuleCreateWithName(self.name)
        self.builder = llvm.CreateBuilder()
        self.build_dir = os.path.join(tempfile.gettempdir(), "nos", str(uuid.uuid4()))
        self.funcs = []

    def __del__(self):
        llvm.DisposeBuilder(self.builder)
        llvm.DisposeModule(self.module)
        self.clean()

    def function(self, result=None, **kwargs):

        def wrapper(func):
            import inspect

            # Annotate function and remember it for later translation.
            func.__nos_restype__ = result
            func.__nos_argtypes__ = kwargs

            # Capture available symbols at the point of function definition.
            func.__nos_globals__ = {}

            parent_frame = inspect.currentframe().f_back
            func.__nos_globals__.update(parent_frame.f_globals)
            func.__nos_globals__.update(parent_frame.f_locals)
            del parent_frame

            self.funcs.append(func)
            return func

        return wrapper

    def dumps(self):
        return llvm.DumpModuleToString(self.module).value

    def dump(self):
        return llvm.DumpModule(self.module)

    def build(self):
        from .llvm.__config__ import LLC, CLANG
        import tempfile
        import os
        import inspect
        import types

        from subprocess import call

        os.makedirs(self.build_dir)

        # Translate all registered functions
        for func in self.funcs:
            func.__nos_func__ = self._translate(func)

        # Path to output shared library.
        so_path = format(os.path.join(self.build_dir, self.name))

        with tempfile.NamedTemporaryFile(suffix=".bc") as tmp_bc:
            with tempfile.NamedTemporaryFile(suffix=".s") as tmp_s:
                llvm.WriteBitcodeToFile(self.module, tmp_bc.name)

                if call((LLC, "-relocation-model=pic", "-o={0}".format(tmp_s.name), tmp_bc.name)):
                    raise RuntimeError("Could not assemble IR")

                if call((CLANG, "-shared", "-o", so_path, tmp_s.name)):
                    raise RuntimeError("Could not build target extension")

        # Compilation successful; build ctypes interface to new module.
        out_module = type(self.name, (types.ModuleType,), {})
        out_module.__nos_shlib__ = ctypes.cdll.LoadLibrary(so_path)

        for func in self.funcs:
            cfunc = getattr(out_module.__nos_shlib__, self._qualify(func.func_name))

            argtypes = []
            for i, arg in enumerate(inspect.getargspec(func).args):
                argtypes.append(func.__nos_argtypes__[arg].c_type)

            cfunc.argtypes = argtypes
            cfunc.restype = (func.__nos_restype__.c_type
                             if func.__nos_restype__ is not None
                             else None)

            setattr(out_module, func.func_name, cfunc)

        return out_module

    def clean(self):
        if os.path.isdir(self.build_dir):
            shutil.rmtree(self.build_dir)

    def _qualify(self, symbol):
        """Qualifies symbol with parent module name."""
        return "__".join((self.name, symbol))

    def _translate(self, func):
        from .visitor import Visitor, FlattenAttributes, entry_alloca, emit_constant
        from .exceptions import TranslationError, AnnotationError
        from .util import remove_indent
        from .lib import range_

        import ast
        import inspect

        # Function parameters, return type and other glue
        spec = inspect.getargspec(func)
        if spec.varargs or spec.keywords:
            raise AnnotationError("Variable and/or keyword arguments are not allowed")

        argtypes = (llvm.TypeRef * len(spec.args))()
        if set(spec.args) != set(func.__nos_argtypes__):
            raise AnnotationError("Argument type annotations don't match function arguments.")

        for i, arg in enumerate(spec.args):
            argtypes[i] = func.__nos_argtypes__[arg].llvm_type

        restype = (func.__nos_restype__.llvm_type
                   if func.__nos_restype__ is not None
                   else llvm.VoidType())

        functype = llvm.FunctionType(restype, argtypes, len(argtypes), 0)
        nos_func = llvm.AddFunction(self.module, self._qualify(func.func_name), functype)
        llvm.SetLinkage(nos_func, llvm.ExternalLinkage)

        body = llvm.AppendBasicBlock(nos_func, "body")
        llvm.PositionBuilderAtEnd(self.builder, body)

        # Global symbols that were available at the point of function definition.
        global_vars = func.__nos_globals__.copy()
        global_vars["range"] = range_

        # Resolve constants
        for k, v in global_vars.items():
            try:
                global_vars[k] = emit_constant(v)
            except TypeError:
                # Not a constant, something else will handle this.
                continue

        # - Local variables are parameters and anything declared
        #   inside the function itself which resides on stack and
        #   can be written to.
        local_vars = {}
        for i, name in enumerate(spec.args):
            p = llvm.GetParam(nos_func, i)
            local_vars[name] = entry_alloca(nos_func, llvm.TypeOf(p), name + "_ptr")
            llvm.BuildStore(self.builder, p, local_vars[name])

        func_source = remove_indent(inspect.getsourcelines(func))
        t = ast.parse(func_source)
        func_body = list(t.body[0].body)

        v = FlattenAttributes(self.builder)
        for i, node in enumerate(func_body):
            func_body[i] = v.visit(node)

        # Debugging
        # from.util import dump_ast
        # for tt in t.body[0].body:
        #     print dump_ast(tt)

        v = Visitor(self.module, self.builder, global_vars, local_vars)

        try:
            for node in t.body[0].body:
                v.visit(node)
        except TranslationError, e:
            raise _unpack_translation_error(func.func_name, func_source, e)

        last_block = llvm.GetInsertBlock(self.builder)
        if not llvm.IsATerminatorInst(llvm.GetLastInstruction(last_block)):
            # Last return out of a void function can be implicit.
            if llvm.GetTypeKind(restype) == llvm.VoidTypeKind:
                llvm.BuildRetVoid(self.builder)
            else:
                raise TypeError("Function {0}() must return a {1}"
                                .format(func.func_name, func.__nos_restype__))

        if llvm.VerifyFunction(nos_func, llvm.PrintMessageAction):
            print self.dumps()
            raise RuntimeError("Could not produce a valid function for " + func.func_name)

        return nos_func


def _unpack_translation_error(func_name, func_lines, e, before=2, after=5):
    """Unpacks TranslationError instance *e* and reconstructs the contained exception.

    Translation errors are used to attach source localtion (line number / column offset)
    and shuttle them out of AST traversal where they can be formatted and rethrown.

    :param before: number of lines to show before the offending one.
    :param after: number of lines to show after the offending one.

    """
    from itertools import chain

    def draw_arrow(snippet, line_index):
        for i, line in enumerate(snippet):
            prefix = "  >>> " if i == line_index else "      "
            yield prefix + line

    error_type, line_number, message = e.args
    line_i = line_number - 1

    snippet = func_lines.split("\n")[min(0, line_i - before): line_i + after]
    tb = draw_arrow(snippet, line_i)

    return error_type("\n".join(chain((message, "  Traceback:"), tb)))
