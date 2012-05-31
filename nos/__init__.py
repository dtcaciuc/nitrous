from __future__ import absolute_import
from . import llvm

import ctypes
import os
import shutil
import tempfile


LLC = os.environ.get("NOS_LLC", "llc")
CLANG = os.environ.get("NOS_CLANG", "clang")

__all__ = ["Module", "ValueEmitter", "value_emitter", "cast"]


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
        from .visitor import Visitor
        import ast

        def wrapper(func):
            from .exceptions import CompilationError
            from .util import remove_indent
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
            func_ = llvm.AddFunction(self.module, self._qualify(func.func_name), functype)
            llvm.SetLinkage(func_, llvm.ExternalLinkage)

            body = llvm.AppendBasicBlock(func_, "body")
            llvm.PositionBuilderAtEnd(self.builder, body)

            # Collecting available symbols; start with function parameters
            vars = {}
            vars["range"] = range_

            parent_frame = inspect.currentframe().f_back
            vars.update(parent_frame.f_globals)
            vars.update(parent_frame.f_locals)
            del parent_frame

            for i, name in enumerate(spec.args):
                p = llvm.GetParam(func_, i)
                llvm.SetValueName(p, name)
                vars[name] = p

            t = ast.parse(remove_indent(inspect.getsourcelines(func)))
            func_body = list(t.body[0].body)

            from .visitor import FlattenAttributes
            v = FlattenAttributes(self.builder, vars)
            for i, node in enumerate(func_body):
                func_body[i] = v.visit(node)

            # Debugging
            # from.util import dump_ast
            # for tt in t.body[0].body:
            #     print dump_ast(tt)

            v = Visitor(self.module, self.builder, vars)
            for node in t.body[0].body:
                v.visit(node)

            # TODO if stack is not empty, return last value

            if llvm.VerifyFunction(func_, llvm.PrintMessageAction):
                print self.dumps()
                raise RuntimeError("Could not produce a valid function for " + func.func_name)

            # Remember original function object
            self.funcs.append(func)

            return func

        return wrapper

    def dumps(self):
        return llvm.DumpModuleToString(self.module).value

    def dump(self):
        return llvm.DumpModule(self.module)

    def compile(self):
        import tempfile
        import os
        import inspect
        import types

        from subprocess import call

        os.makedirs(self.build_dir)

        with tempfile.NamedTemporaryFile(suffix=".bc") as tmp_bc:
            with tempfile.NamedTemporaryFile(suffix=".s") as tmp_s:
                llvm.WriteBitcodeToFile(self.module, tmp_bc.name)

                if call((LLC, "-o={0}".format(tmp_s.name), tmp_bc.name)):
                    raise RuntimeError("Could not assemble IR")

                so_path = format(os.path.join(self.build_dir, self.name))
                if call((CLANG, "-shared", "-o", so_path, tmp_s.name)):
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
        if os.path.isdir(self.build_dir):
            shutil.rmtree(self.build_dir)

    def _qualify(self, symbol):
        """Qualifies symbol with parent module name."""
        return "__".join((self.name, symbol))


class ValueEmitter(object):

    __nos_emitter__ = True

    def __init__(self, func, args, kwargs):
        self.func = func
        self.args = args
        self.kwargs = kwargs

    def emit(self, module, builder):
        return self.func(module, builder, *self.args, **self.kwargs)


def value_emitter(func):
    def wrapper(*args, **kwargs):
        return ValueEmitter(func, args, kwargs)
    return wrapper


_CASTS = {
    (llvm.DoubleTypeKind, llvm.IntegerTypeKind): llvm.FPToSI,
    (llvm.IntegerTypeKind, llvm.DoubleTypeKind): llvm.SIToFP,
}


@value_emitter
def cast(_, builder, value, target_type):
    """Casts expression to specified type."""
    from .exceptions import CompilationError

    value_kind = llvm.GetTypeKind(llvm.TypeOf(value))
    target_kind = llvm.GetTypeKind(target_type.llvm_type)

    # TODO support
    # * unsigned integers
    # * floats and integers of different width

    if value_kind == target_kind:
        return value

    try:
        op = _CASTS[(value_kind, target_kind)]
    except KeyError:
        raise CompilationError("Cannot cast {0} to {1}".format(value, target_type))

    return llvm.BuildCast(builder, op, value, target_type.llvm_type, "tmp")


@value_emitter
def range_(_, builder, *args):
    """range() intrinsic implementation.

    Returns (start, stop, step) LLVM value tuple.

    """
    from .types import Long

    # TODO add checks
    #  start > stop & step > 0;
    #  start < stop & step < 0;
    #  step < stop - start

    data = [llvm.ConstInt(Long.llvm_type, 0, True),
            llvm.ConstInt(Long.llvm_type, 0, True),
            llvm.ConstInt(Long.llvm_type, 1, True)]

    if len(args) == 0:
        raise TypeError("Range accepts at least 1 argument")
    elif len(args) > 3:
        raise TypeError("Range accepts at most 3 arguments")
    elif len(args) == 1:
        data[1] = args[0]
    else:
        data[0] = args[0]
        data[1] = args[1]
        if len(args) == 3:
            data[2] = args[2]

    return data


@value_emitter
def alloca(_, builder, element_type, n=1):
    """Reserves stack memory for *n* elements of *element_type*.

    Returns pointer to newly reserved block.

    """
    from .types import Long

    if n <= 0:
        raise ValueError("Number of elements must be positive")

    if n == 1:
        return llvm.BuildAlloca(builder, element_type.llvm_type, "v")
    else:
        return llvm.BuildArrayAlloca(builder, element_type.llvm_type,
                                     llvm.ConstInt(Long.llvm_type, n, True))


@value_emitter
def sqrt(module, builder, value):
    func = llvm.GetIntrinsicDeclaration(module,
                                        llvm.INTRINSICS["llvm.sqrt"],
                                        ctypes.byref(llvm.TypeOf(value)), 1)
    return llvm.BuildCall(builder, func, ctypes.byref(value), 1, "sqrt")
