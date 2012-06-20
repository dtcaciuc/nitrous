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
        self.funcs = []
        self.libs = []
        self.libdirs = []

        self.module = llvm.ModuleCreateWithName(self.name)
        self.builder = llvm.CreateBuilder()
        if llvm.InitializeNativeTarget__():
            raise SystemError("Cannot initialize LLVM target")

        triple = llvm.GetDefaultTargetTriple__()

        # At this point, multiple targets can be initialized
        # (eg x86 and x86_64), but only one is functional.
        message = ctypes.c_char_p()
        self.target = llvm.LookupTarget__(triple, ctypes.byref(message))
        if not self.target:
            err = RuntimeError("Could not find suitable target: {0}".format(message.value))
            llvm.DisposeMessage(message)
            raise err

        self.machine = llvm.CreateTargetMachine(self.target,
                                                triple, "", "",
                                                llvm.CodeGenLevelDefault,
                                                llvm.RelocPIC,
                                                llvm.CodeModelDefault)

        self.build_dir = os.path.join(tempfile.gettempdir(), "nos", str(uuid.uuid4()))

    def __del__(self):
        llvm.DisposeTargetMachine(self.machine)
        llvm.DisposeBuilder(self.builder)
        llvm.DisposeModule(self.module)
        self.clean()

    def include_function(self, name, restype, argtypes, lib=None, libdir=None):
        """Includes externally defined function for use with the module.

        :param name: function name
        :param restype: function return value type
        :param argtypes: sequence of function argument types
        :param lib: library name where function is defined and we'll link with
        :param libdir: directory where library is located

        """
        from .visitor import ExternalFunction

        func = _create_function(self.module, name, restype, argtypes)

        self.libs.append(lib)
        self.libdirs.append(libdir)

        return ExternalFunction(name, func, restype, argtypes)

    def function(self, result=None, **kwargs):

        def resolve_constants(symbols):
            """Converts eligible values in a dictionary to LLVM constant objects."""
            from .visitor import emit_constant

            for k, v in symbols.iteritems():
                try:
                    yield k, emit_constant(v)
                except TypeError:
                    # Not a constant, something else will handle this.
                    yield k, v

        def wrapper(func):
            from .exceptions import AnnotationError
            from .lib import range_
            import inspect

            # Annotate function and remember it for later translation.
            func.__nos_restype__ = result
            func.__nos_argtypes__ = kwargs

            # - Ordered argument name sequence
            spec = inspect.getargspec(func)
            if spec.varargs or spec.keywords:
                raise AnnotationError("Variable and/or keyword arguments are not allowed")
            if set(spec.args) != set(func.__nos_argtypes__):
                raise AnnotationError("Argument type annotations don't match function arguments.")

            # TODO Replace this with __nos_argtypes__ ordered dictionary?
            # For consistency, same in ExternalFunction as well.
            func.__nos_args__ = spec.args

            # Immutable global symbols.
            func.__nos_globals__ = {}
            # - Built-ins
            func.__nos_globals__["range"] = range_
            # - Other symbols available at the point of function
            #   definition; try to resolve as many constants as possible.
            parent_frame = inspect.currentframe().f_back
            func.__nos_globals__.update(resolve_constants(parent_frame.f_globals))
            func.__nos_globals__.update(resolve_constants(parent_frame.f_locals))
            del parent_frame

            self.funcs.append(func)
            return func

        return wrapper

    def dumps(self):
        return llvm.DumpModuleToString(self.module).value

    def dump(self):
        return llvm.DumpModule(self.module)

    def build(self):
        import os
        import tempfile
        import types

        from subprocess import call
        from .visitor import emit_body

        os.makedirs(self.build_dir)

        # Translate all registered functions
        for func in self.funcs:
            argtypes = [func.__nos_argtypes__[name] for name in func.__nos_args__]
            func.__nos_func__ = _create_function(self.module,
                                                 self._qualify(func.func_name),
                                                 func.__nos_restype__,
                                                 argtypes)

        # Once all functions are declared, emit their contents
        for func in self.funcs:
            emit_body(self.module, self.builder, func)
            if llvm.VerifyFunction(func.__nos_func__, llvm.PrintMessageAction):
                raise RuntimeError("Could not compile {0}()".format(func.func_name))


        # Path to output shared library.
        so_path = format(os.path.join(self.build_dir, self.name))
        libs = tuple("-l{0}".format(lib) for lib in self.libs)
        libdirs = tuple("-L{0}".format(d) for d in self.libdirs)

        with tempfile.NamedTemporaryFile(suffix=".s") as tmp_s:
            message = ctypes.c_char_p()
            status = llvm.TargetMachineEmitToFile(self.machine, self.module,
                                                  tmp_s.name, llvm.AssemblyFile,
                                                  ctypes.byref(message))
            if status != 0:
                error = RuntimeError("Could not assemble IR: {0}".format(message.value))
                llvm.DisposeMessage(message)
                raise error

            if call(("clang", "-shared", "-o", so_path, tmp_s.name) + libs + libdirs):
                raise RuntimeError("Could not build target extension")

        # Compilation successful; build ctypes interface to new module.
        out_module = type(self.name, (types.ModuleType,), {})
        out_module.__nos_shlib__ = ctypes.cdll.LoadLibrary(so_path)

        for func in self.funcs:
            cfunc = getattr(out_module.__nos_shlib__, self._qualify(func.func_name))

            argtypes = []
            for i, arg in enumerate(func.__nos_args__):
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


def _create_function(module, name, restype, argtypes):
    """Creates an empty LLVM function."""

    # Result type
    restype_ = restype.llvm_type if restype is not None else llvm.VoidType()
    argtypes_ = (llvm.TypeRef * len(argtypes))()
    for i, ty in enumerate(argtypes):
        argtypes_[i] = ty.llvm_type

    func_type = llvm.FunctionType(restype_, argtypes_, len(argtypes_), 0)
    func = llvm.AddFunction(module, name, func_type)
    llvm.SetLinkage(func, llvm.ExternalLinkage)

    return func
