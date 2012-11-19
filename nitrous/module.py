import ctypes
import os
import shutil
import tempfile

from . import llvm


class Module(object):
    """A unit of compilation and a container for optimized functions."""

    def __init__(self, module, cleanup):
        # List of callables to run to free up resources associated with build.
        self.__n2o_module__ = module
        self.__cleanup = cleanup

    def __del__(self):
        for f in self.__cleanup:
            f()


def so_module(decls, libs=[], libdirs=[], name=None):
    """Build a module backed by shared object file."""

    from functools import partial
    from subprocess import call
    from uuid import uuid4

    module, funcs = _create_module(decls, name)

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
    # Getting module name again since _create_module may have used a default.
    so_path = format(os.path.join(build_dir, llvm.GetModuleName(module)))
    libs = tuple("-l{0}".format(lib) for lib in libs)
    libdirs = tuple("-L{0}".format(d) for d in libdirs)

    # TODO get target data from TargetMachine?
    _optimize(module, None)

    with tempfile.NamedTemporaryFile(suffix=".s") as tmp_s:
        message = ctypes.c_char_p()
        status = llvm.TargetMachineEmitToFile(machine, module,
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

    # Compilation successful; build ctypes interface to new module.
    so = ctypes.cdll.LoadLibrary(so_path)

    cleanup = [partial(llvm.DisposeModule, module), partial(shutil.rmtree, build_dir)]

    out = Module(module, cleanup)
    out.__n2o_so__ = so

    for func in funcs:
        func.wrap_so(so)
        setattr(out, func.__name__, func)

    return out


def jit_module(decls, name=None):
    """Build a module backed by JIT execution engine."""

    from functools import partial

    module, funcs = _create_module(decls, name)

    if llvm.InitializeNativeTarget__():
        raise SystemError("Cannot initialize LLVM target")

    engine = llvm.ExecutionEngineRef()
    opt_level = 3

    message = ctypes.c_char_p()
    if llvm.CreateJITCompilerForModule(ctypes.byref(engine), module, opt_level, ctypes.byref(message)):
        err = RuntimeError("Could not create execution engine: {0}".format(message.value))
        llvm.DisposeMessage(message)
        raise err

    _optimize(module, llvm.GetExecutionEngineTargetData(engine))

    cleanup = [partial(llvm.DisposeExecutionEngine, engine)]

    out = Module(module, cleanup)
    out.__n2o_engine__ = engine

    for func in funcs:
        func.wrap_engine(engine)
        setattr(out, func.__name__, func)

    return out


def dump(module):
    """Return a string with module output's LLVM IR."""
    return llvm.DumpModuleToString(module.__n2o_module__).value


#: Default module builder.
module = so_module


def _create_module(decls, name):
    from .function import emit_body, Function
    from uuid import uuid4

    if not name:
        name = "n2o_" + str(uuid4())

    module = llvm.ModuleCreateWithName(name)
    funcs = []

    # Translate all registered functions
    for decl in decls:
        func = Function(decl)
        argtypes = [func.__n2o_argtypes__[arg] for arg in decl.__n2o_args__]
        func.__n2o_func__ = _create_function(module, _qualify(module, decl.__name__),
                                             decl.__n2o_restype__,
                                             argtypes)
        if func.__n2o_options__["inline"]:
            llvm.AddFunctionAttr(func.__n2o_func__, llvm.AlwaysInlineAttribute)
        funcs.append(func)

    ir_builder = llvm.CreateBuilder()

    # Once all known functions are declared, emit their contents
    # Other functions can be added as they're discovered during emission.
    i = 0
    while i < len(funcs):
        func = funcs[i]

        # Update globals with local function objects, including
        # ourselves; this allows for declaration order-independent
        # visibility and recursive calls.
        # TODO this is broken for the new module() arrangement; needs rewrite.
        for other_func in funcs:
            func.__n2o_globals__[other_func.__name__] = other_func

        # Add functions used but not prevously declared.
        new_funcs = emit_body(ir_builder, func)
        funcs.extend(new_funcs)

        if llvm.VerifyFunction(func.__n2o_func__, llvm.PrintMessageAction):
            raise RuntimeError("Could not compile {0}()".format(func.__name__))

        i += 1

    llvm.DisposeBuilder(ir_builder)

    return module, funcs


def _qualify(module, symbol):
    """Qualifies symbol with parent module name."""
    return "__".join((llvm.GetModuleName(module), symbol))


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
