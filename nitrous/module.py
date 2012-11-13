from __future__ import absolute_import
import ctypes
import os
import shutil
import tempfile

from . import llvm


class Module(object):
    """A unit of compilation and a container for optimized functions."""

    def __init__(self, name, backend=None):
        """Create a new module with a *name*."""
        self.name = name
        self.funcs = []
        self.libs = []
        self.libdirs = []

        # Upon .build(), the module ownership is transferred to the
        # resulting wrapper object and below reference is set to None.
        self.module = llvm.ModuleCreateWithName(self.name)

        self.backend = backend or build_so

    def include_function(self, name, restype, argtypes, lib=None, libdir=None):
        """Includes externally defined function for use with the module.

        Typically this is a function in an external shared or static library. C and
        Fortran functions should be good to interface with; unfortunately C++ should
        be kept at a distance from for the time being.

        :param name: function name as it's listed in library symbols.
        :param restype: function return value type
        :param argtypes: sequence of function argument types
        :param lib: library name where function is defined and we'll link with
        :param libdir: directory where library is located

        """
        from .function import ExternalFunction

        func = _create_function(self.module, name, restype, argtypes)

        if lib is not None:
            self.libs.append(lib)

        if libdir is not None:
            self.libdirs.append(libdir)

        return ExternalFunction(name, func, restype, argtypes)

    def function(self, restype=None, **kwargs):
        """Decorate an existing function with signature type annotations.

        The function gets included and will be built with the module.

        *restype* is the function return type. *kwargs* key/value pairs map argument
        names to their respective types.

        """
        def wrapper(func):
            from .exceptions import AnnotationError
            from .lib import _range
            import inspect

            # Annotate function and remember it for later translation.
            func.__n2o_restype__ = restype
            # Types can provide a susbtitution if they're used directly
            # as an argument type (eg. Structure needs to be implicitly
            # passed as Reference() to said structure.
            func.__n2o_argtypes__ = dict(
                (k, t.argtype if hasattr(t, "argtype") else t)
                for k, t in kwargs.items()
            )

            # - Ordered argument name sequence
            spec = inspect.getargspec(func)
            if spec.varargs or spec.keywords:
                raise AnnotationError("Variable and/or keyword arguments are not allowed")
            if set(spec.args) != set(func.__n2o_argtypes__):
                raise AnnotationError("Argument type annotations don't match function arguments.")

            # TODO Replace this with __n2o_argtypes__ ordered dictionary?
            # For consistency, same in ExternalFunction as well.
            func.__n2o_args__ = spec.args

            # Immutable global symbols.
            func.__n2o_globals__ = {}
            # - Built-ins
            func.__n2o_globals__["range"] = _range
            # - Other symbols available at the point of function
            #   definition; try to resolve as many constants as possible.
            parent_frame = inspect.currentframe().f_back
            func.__n2o_globals__.update(parent_frame.f_globals)
            func.__n2o_globals__.update(parent_frame.f_locals)
            del parent_frame

            # Options
            func.__n2o_options__ = dict(cdiv=False, inline=False)

            self.funcs.append(func)
            return func

        return wrapper

    def options(self, cdiv=False, inline=False):
        """Set behavioural options which affect the generated code.

        :param cdiv: Set ``True`` to match C behaviour when performing integer division.
        :param inline: Set ``True`` to always inline the function.

        """
        def wrapper(func):
            func.__n2o_options__.update(cdiv=cdiv, inline=inline)
            return func
        return wrapper

    def build(self):
        """Build the module and return a handle to the resulting container.

        The returned object is a Python module itself and contains wrappers under
        the same attribute names as the original functions being wrapped.

        """
        from .function import emit_body

        if self.module is None:
            raise RuntimeError("Module was already built")

        # Translate all registered functions
        for func in self.funcs:
            argtypes = [func.__n2o_argtypes__[name] for name in func.__n2o_args__]
            func.__n2o_func__ = _create_function(self.module,
                                                 self._qualify(func.func_name),
                                                 func.__n2o_restype__,
                                                 argtypes)
            if func.__n2o_options__["inline"]:
                llvm.AddFunctionAttr(func.__n2o_func__, llvm.AlwaysInlineAttribute)

        ir_builder = llvm.CreateBuilder()

        # Once all functions are declared, emit their contents
        for func in self.funcs:
            # Update globals with local function objects, including
            # ourselves; this allows for declaration order-independent
            # visibility and recursive calls.
            for other_func in self.funcs:
                func.__n2o_globals__[other_func.func_name] = other_func

            emit_body(self.module, ir_builder, func)
            if llvm.VerifyFunction(func.__n2o_func__, llvm.PrintMessageAction):
                raise RuntimeError("Could not compile {0}()".format(func.func_name))

        llvm.DisposeBuilder(ir_builder)

        # Invalidate module reference and return the wrapper
        out = self.backend(self)
        self.module = None

        return out

    def _qualify(self, symbol):
        """Qualifies symbol with parent module name."""
        return "__".join((self.name, symbol))


class ModuleOutput(object):
    """Returned as output of Module.build() call.

    Provides access to compiled functions and calls user defined
    dispose function to free up any associated resources on deallocation.

    """

    def __init__(self, module, dispose):
        self.__n2o_module__ = module
        self.__n2o_dispose__ = dispose

    def __del__(self):
        self.__n2o_dispose__()


def build_so(module):
    """Output and return module wrapper backed by shared object file."""

    from .function import Function

    from subprocess import call
    from uuid import uuid4
    from imp import new_module

    import atexit

    if llvm.InitializeNativeTarget__():
        raise SystemError("Cannot initialize LLVM target")

    # At this point, multiple targets can be initialized
    # (eg x86 and x86_64), but only one is functional.
    message = ctypes.c_char_p()
    triple = llvm.GetDefaultTargetTriple__()
    target = llvm.LookupTarget__(triple, ctypes.byref(message))
    if not target:
        err = RuntimeError("Could not find suitable target: {0}".format(message.value))
        llvm.DisposeMessage(message)
        raise err

    machine = llvm.CreateTargetMachine(target,
                                       triple, "", "",
                                       llvm.CodeGenLevelDefault,
                                       llvm.RelocPIC,
                                       llvm.CodeModelDefault)

    build_dir = os.path.join(tempfile.gettempdir(), "n2o", str(uuid4()))
    os.makedirs(build_dir)

    # Path to output shared library.
    so_path = format(os.path.join(build_dir, module.name))
    libs = tuple("-l{0}".format(lib) for lib in module.libs)
    libdirs = tuple("-L{0}".format(d) for d in module.libdirs)

    # TODO get target data from TargetMachine?
    _optimize(module.module, None)

    with tempfile.NamedTemporaryFile(suffix=".s") as tmp_s:
        message = ctypes.c_char_p()
        status = llvm.TargetMachineEmitToFile(machine, module.module,
                                              tmp_s.name, llvm.AssemblyFile,
                                              ctypes.byref(message))
        if status != 0:
            error = RuntimeError("Could not assemble IR: {0}".format(message.value))
            llvm.DisposeMessage(message)
            raise error

        if call(("clang", "-shared", "-o", so_path, tmp_s.name) + libs + libdirs):
            raise RuntimeError("Could not build target extension")

        # Debug
        # if call(("otool", "-tv", so_path)):
        #     raise RuntimeError("Could not disassemble target extension")
        # if call(("objdump", "-S", so_path)):
        #     raise RuntimeError("Could not disassemble target extension")

    llvm.DisposeTargetMachine(machine)

    def dispose():
        llvm.DisposeModule(module.module)
        shutil.rmtree(build_dir)

    # Compilation successful; build ctypes interface to new module.
    so = ctypes.cdll.LoadLibrary(so_path)
    out_module = ModuleOutput(module.module, dispose)
    out_module.__n2o_so__ = so

    for func in module.funcs:
        cfunc = getattr(so, module._qualify(func.func_name))
        setattr(out_module, func.func_name, Function.wrap(func, cfunc))

    return out_module


def build_jit(module):
    """Output module wrapper backed by JIT execution engine."""
    from .function import Function

    if llvm.InitializeNativeTarget__():
        raise SystemError("Cannot initialize LLVM target")

    engine = llvm.ExecutionEngineRef()
    opt_level = 3

    message = ctypes.c_char_p()
    if llvm.CreateJITCompilerForModule(ctypes.byref(engine), module.module, opt_level, ctypes.byref(message)):
        err = RuntimeError("Could not create execution engine: {0}".format(message.value))
        llvm.DisposeMessage(message)
        raise err

    _optimize(module.module, llvm.GetExecutionEngineTargetData(engine))

    def dispose():
        llvm.DisposeExecutionEngine(engine)
        # The engine takes its ownership of module; no need to dispose separately.

    out_module = ModuleOutput(module.module, dispose)
    out_module.__n2o_engine__ = engine

    for func in module.funcs:
        result_c_type = (func.__n2o_restype__.c_type
                         if func.__n2o_restype__ is not None
                         else None)
        arg_c_types = (func.__n2o_argtypes__[name].c_type
                       for name in func.__n2o_args__)
        proto = ctypes.CFUNCTYPE(result_c_type, *arg_c_types)
        c_func = proto(llvm.GetPointerToGlobal(engine, func.__n2o_func__))
        setattr(out_module, func.func_name, Function.wrap(func, c_func))

    return out_module


def dump(output):
    """Return a string with module output's LLVM IR."""
    return llvm.DumpModuleToString(output.__n2o_module__).value


def _optimize(module, target_data):
    """Runs optimization passes on given module."""

    # IR optimizations; currently at default opt level.
    pm = llvm.CreatePassManager()
    if target_data is not None:
        llvm.AddTargetData(target_data, pm)

    pm_builder = llvm.PassManagerBuilderCreate()
    llvm.PassManagerBuilderUseInlinerWithThreshold(pm_builder, 275)
    llvm.PassManagerBuilderPopulateModulePassManager(pm_builder, pm)

    if not llvm.RunPassManager(pm, module):
        raise RuntimeError("Could not run IR optimization passes")

    llvm.PassManagerBuilderDispose(pm_builder)
    llvm.DisposePassManager(pm)


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
