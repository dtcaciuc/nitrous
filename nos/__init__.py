from __future__ import absolute_import
from .visitor import Visitor
from .types import Double
from . import llvm

import ast
import os
import tempfile


__all__ = ["Double", "result", "args", "compiled"]


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

    def args(self, **kwargs):
        def wrapper(func):
            func.__nos_argtypes__ = kwargs
            return func
        return wrapper

    def result(self, type_):
        def wrapper(func):
            func.__nos_restype__ = type_
            return func
        return wrapper

    def compiled(self):
        def wrapper(func):
            import inspect

            # Sanity checks
            if not hasattr(func, "__nos_argtypes__"):
                raise ValueError("Forgot to annotate {0} arguments".format(func.func_name))
            if not hasattr(func, "__nos_restype__"):
                raise ValueError("Forgot to annotate {0} result type".format(func.func_name))

            # Function parameters, return type and other glue

            # TODO assert len(argtypes) == len(args)
            # TODO assert only positional args are allowed
            argnames = inspect.getargspec(func).args
            argtypes = (llvm.TypeRef * len(argnames))()
            for i, arg in enumerate(argnames):
                argtypes[i] = func.__nos_argtypes__[arg].generate()

            restype = func.__nos_restype__.generate()

            functype = llvm.FunctionType(restype, argtypes, len(argtypes), 0)
            func_ = llvm.AddFunction(self.module, self._qualify(func.func_name), functype)
            llvm.SetLinkage(func_, llvm.ExternalLinkage)

            body = llvm.AppendBasicBlock(func_, "body")
            llvm.PositionBuilderAtEnd(self.builder, body)

            # Collecting available symbols; start with function parameters
            vars = {}

            for i, name in enumerate(argnames):
                p = llvm.GetParam(func_, i)
                llvm.SetValueName(p, name)
                vars[name] = p

            t = ast.parse(_remove_indent(inspect.getsourcelines(func)))

            # Debugging
            # for tt in t.body[0].body:
            #     print dump_ast(tt)

            v = Visitor(self.builder, vars)
            for node in t.body[0].body:
                v.visit(node)

            if llvm.VerifyFunction(func_, llvm.AbortProcessAction):
                raise RuntimeError("Could not produce a valid function for " + func.func_name)

            # Remember original function object
            self.funcs.append(func)

            return func

        return wrapper

    def dump(self):
        llvm.DumpModule(self.module)

    def compile(self):
        import tempfile
        import ctypes
        import os
        import inspect
        import types

        from subprocess import call

        os.makedirs(self.build_dir)

        with tempfile.NamedTemporaryFile(suffix=".bc") as tmp_bc:
            with tempfile.NamedTemporaryFile(suffix=".s") as tmp_s:
                llvm.WriteBitcodeToFile(self.module, tmp_bc.name)

                if call(("llc", "-o={0}".format(tmp_s.name), tmp_bc.name)):
                    raise RuntimeError("Could not assemble IR")

                so_path = format(os.path.join(self.build_dir, self.name))
                if call(("clang", "-shared", "-o", so_path, tmp_s.name)):
                    raise RuntimeError("Could not compile target extension")

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
        import shutil
        if os.path.isdir(self.build_dir):
            shutil.rmtree(self.build_dir)

    def _qualify(self, symbol):
        """Qualifies symbol with parent module name."""
        return "__".join((self.name, symbol))


def _remove_indent(source_lines):
    """Removes base indent for a set of source lines."""
    lines, _ = source_lines
    line_0 = lines[0].lstrip()
    indent = len(lines[0]) - len(line_0)
    return "".join(line[indent:] for line in lines)
