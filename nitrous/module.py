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
    from tempfile import mkdtemp

    module, funcs = _create_module(decls, name)

    for lib in libs:
        if not isinstance(lib, basestring):
            llvm.link_modules(module, lib.create_module())

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

    build_dir = mkdtemp(prefix="n2o-")
    cleanup = [partial(llvm.DisposeModule, module), partial(shutil.rmtree, build_dir)]

    out = Module(module, cleanup)

    # Path to output shared library.
    # Getting module name again since _create_module may have used a default.
    so_path = format(os.path.join(build_dir, llvm.GetModuleName(module)))
    libs = tuple("-l{0}".format(lib) for lib in libs if isinstance(lib, basestring))
    libdirs = tuple("-L{0}".format(d) for d in libdirs)

    _optimize(module, llvm.GetTargetMachineData(machine))

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
        # if call(("llvm-objdump", "-disassemble", so_path)):
        #     raise RuntimeError("Could not disassemble target extension")

    llvm.DisposeTargetMachine(machine)

    # Compilation successful; build ctypes interface to new module.
    so = ctypes.cdll.LoadLibrary(so_path)
    out.__n2o_so__ = so

    for func in funcs:
        func.wrap_so(so)
        setattr(out, func.__name__, func)

    return out


def jit_module(decls, libs=[], name=None):
    """Build a module backed by JIT execution engine."""

    from functools import partial

    module, funcs = _create_module(decls, name)

    for lib in libs:
        llvm.link_modules(module, lib.create_module())

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


class CppLibrary(object):
    """Creates a library from C/C++ sources."""

    def __init__(self, sources, compile_args=[]):
        self.sources = sources
        self.compile_args = tuple(compile_args)

    def create_module(self):
        if not self.sources:
            raise ValueError("No source files")

        module = self._compile_source(self.sources[0])
        for source in self.sources[1:]:
            llvm.link_modules(module, self._compile_source(source))

        return module

    def _compile_source(self, source):
        from subprocess import call

        buffer = llvm.MemoryBufferRef()
        message = ctypes.c_char_p()

        with tempfile.NamedTemporaryFile(suffix=".bc") as tmp_bc:
            if call(("clang", "-c", "-emit-llvm", "-o", tmp_bc.name, source) + self.compile_args):
                raise RuntimeError("Could not compile {0}".format(source))

            status = llvm.CreateMemoryBufferWithContentsOfFile(
                tmp_bc.name, ctypes.byref(buffer),
                ctypes.byref(message)
            )
            if status != 0:
                error = RuntimeError("Could not load module: {0}".format(message.value))
                llvm.DisposeMessage(message)
                raise error

        lib_module = llvm.ModuleRef()

        status = llvm.ParseBitcode(buffer, ctypes.byref(lib_module), message)
        llvm.DisposeMemoryBuffer(buffer)
        if status != 0:
            error = RuntimeError("Could not parse bitcode: {0}".format(message.value))
            llvm.DisposeMessage(message)
            raise error

        return lib_module


def _create_module(decls, name):
    from .function import emit_body, _get_or_create_function, Function

    if not name:
        name = "n2o_" + _random_hex()

    module = llvm.ModuleCreateWithName(name)
    funcs = []

    # Translate all registered functions
    for decl in decls:
        func, exists = _get_or_create_function(module, decl)
        if exists:
            raise RuntimeError("Duplicate function name: {0}" .format(decl.__name__))
        funcs.append(Function(decl, func))

    ir_builder = llvm.CreateBuilder()

    # Once all known functions are declared, emit their contents
    # Other functions can be added as they're discovered during emission.
    i = 0
    while i < len(funcs):
        func = funcs[i]

        # Making current function declaration available to itself for recursive calls.
        func.globals[func.__name__] = func.decl

        # Emit new defined functions.
        if func.decl.pyfunc is not None:
            new_funcs = emit_body(ir_builder, func)
            funcs.extend(new_funcs)

            if func.decl not in decls:
                # Any compiled function that does not appear in module arguments can be hidden.
                llvm.SetLinkage(func.llvm_func, llvm.PrivateLinkage)

            if llvm.VerifyFunction(func.llvm_func, llvm.PrintMessageAction):
                raise RuntimeError("Could not compile {0}()".format(func.__name__))

        i += 1

    llvm.DisposeBuilder(ir_builder)

    # Return only the functions from explicitly listed declarations.
    return module, funcs[:len(decls)]


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


def _random_hex(digits=8):
    """Generates a random hexadecimal string *n* characters long"""
    from string import hexdigits
    from random import choice

    return "".join(choice(hexdigits) for _ in range(digits))
