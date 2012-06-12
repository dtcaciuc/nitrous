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

    def function(self, result, **kwargs):
        from .visitor import Visitor, FlattenAttributes, entry_alloca, emit_constant
        import ast

        def wrapper(func):
            from .exceptions import CompilationError
            from .util import remove_indent
            from .lib import range_
            import inspect

            func.__nos_restype__ = result
            func.__nos_argtypes__ = kwargs

            # Function parameters, return type and other glue
            spec = inspect.getargspec(func)
            if spec.varargs or spec.keywords:
                raise CompilationError("Variable and/or keyword arguments are not allowed")

            argtypes = (llvm.TypeRef * len(spec.args))()
            if set(spec.args) != set(func.__nos_argtypes__):
                raise CompilationError("Argument type annotations don't "
                                       "match function arguments.")

            for i, arg in enumerate(spec.args):
                argtypes[i] = func.__nos_argtypes__[arg].llvm_type

            restype = func.__nos_restype__.llvm_type

            functype = llvm.FunctionType(restype, argtypes, len(argtypes), 0)
            nos_func = llvm.AddFunction(self.module, self._qualify(func.func_name), functype)
            llvm.SetLinkage(nos_func, llvm.ExternalLinkage)

            body = llvm.AppendBasicBlock(nos_func, "body")
            llvm.PositionBuilderAtEnd(self.builder, body)

            # Collecting available symbols
            # - Global variables are symbols declared outside the function.
            global_vars = {}
            global_vars["range"] = range_

            parent_frame = inspect.currentframe().f_back
            global_vars.update(parent_frame.f_globals)
            global_vars.update(parent_frame.f_locals)
            del parent_frame

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

            t = ast.parse(remove_indent(inspect.getsourcelines(func)))
            func_body = list(t.body[0].body)

            v = FlattenAttributes(self.builder)
            for i, node in enumerate(func_body):
                func_body[i] = v.visit(node)

            # Debugging
            # from.util import dump_ast
            # for tt in t.body[0].body:
            #     print dump_ast(tt)

            v = Visitor(self.module, self.builder, global_vars, local_vars)
            for node in t.body[0].body:
                v.visit(node)

            # TODO if stack is not empty, return last value

            if llvm.VerifyFunction(nos_func, llvm.PrintMessageAction):
                print self.dumps()
                raise RuntimeError("Could not produce a valid function for " + func.func_name)

            # Remember original function object
            func.__nos_func__ = nos_func
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

        with tempfile.NamedTemporaryFile(suffix=".bc") as tmp_bc:
            with tempfile.NamedTemporaryFile(suffix=".s") as tmp_s:
                llvm.WriteBitcodeToFile(self.module, tmp_bc.name)

                if call((LLC, "-relocation-model=pic", "-o={0}".format(tmp_s.name), tmp_bc.name)):
                    raise RuntimeError("Could not assemble IR")

                so_path = format(os.path.join(self.build_dir, self.name))
                if call((CLANG, "-shared", "-o", so_path, tmp_s.name)):
                    raise RuntimeError("Could not build target extension")

                out_module = type(self.name, (types.ModuleType,), {})
                out_module.__nos_shlib__ = ctypes.cdll.LoadLibrary(so_path)

                for func in self.funcs:
                    cfunc = getattr(out_module.__nos_shlib__, self._qualify(func.func_name))

                    argtypes = []
                    for i, arg in enumerate(inspect.getargspec(func).args):
                        argtypes.append(func.__nos_argtypes__[arg].c_type)

                    cfunc.argtypes = argtypes
                    cfunc.restype = func.__nos_restype__.c_type

                    setattr(out_module, func.func_name, cfunc)

                return out_module

    def clean(self):
        if os.path.isdir(self.build_dir):
            shutil.rmtree(self.build_dir)

    def _qualify(self, symbol):
        """Qualifies symbol with parent module name."""
        return "__".join((self.name, symbol))
