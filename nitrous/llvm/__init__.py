import ctypes
import ctypes.util
import os

try:
    # If possible, get nicer tracebacks from low-level signals.
    import faulthandler
    faulthandler.enable()
except:
    pass

from .v31 import *

class _Mock(object):
    def __init__(self, *args, **kwargs):
        pass

try:
    _llvm = ctypes.cdll.LoadLibrary(os.path.join(os.path.dirname(__file__), "_llvm.so"))
    _libc = ctypes.cdll.LoadLibrary(ctypes.util.find_library("c"))

    def _func(func_name, restype, argtypes=[]):
        """Creates ctypes wrapper for an LLVM API function.

        LLVM{name} -> llvm.{name}

        """
        g = globals()

        g[func_name] = getattr(_llvm, "LLVM" + func_name)
        g[func_name].restype = restype
        g[func_name].argtypes = argtypes

except OSError:
    # Allows us to complete the module import when LLMV library
    # is not available; currently used for documentation building.
    def _func(func_name, restype, argtypes=[]):
        globals()[func_name] = _Mock


class owned_c_char_p(ctypes.c_char_p):
    """Char pointer which collects the memory of the return value."""

    def __del__(self):
        _libc.free(self)


Bool = ctypes.c_int
FALSE = 0
TRUE = 1


# Context
class OpaqueContext(ctypes.Structure):
    pass

ContextRef = ctypes.POINTER(OpaqueContext)

_func("GetGlobalContext", ContextRef)


# Module
class OpaqueModule(ctypes.Structure):
    pass

ModuleRef = ctypes.POINTER(OpaqueModule)

_func("ModuleCreateWithName", ModuleRef)
_func("GetModuleName", ctypes.c_char_p, [ModuleRef])
_func("DumpModule", None, [ModuleRef])
_func("DumpModuleToString", owned_c_char_p, [ModuleRef])
_func("DisposeModule", None, [ModuleRef])

_func("DisposeMessage", None, [ctypes.c_char_p])


# Linker
(LinkerDestroySource, LinkerPreserveSource) = range(2)

_func("LinkModules__", Bool,
      [ModuleRef, ModuleRef, ctypes.c_int, ctypes.POINTER(ctypes.c_char_p)])


# Type
class OpaqueType(ctypes.Structure):
    pass

TypeRef = ctypes.POINTER(OpaqueType)

TypeKind = ctypes.c_int

_func("VoidType", TypeRef)
_func("FloatType", TypeRef)
_func("DoubleType", TypeRef)
_func("IntType", TypeRef, [ctypes.c_uint])

_func("GetTypeKind", TypeKind, [TypeRef])
_func("GetIntTypeWidth", ctypes.c_uint, [TypeRef])


# Sequential types
_func("PointerType", TypeRef, [TypeRef, ctypes.c_uint])
_func("VectorType", TypeRef, [TypeRef, ctypes.c_uint])
_func("ArrayType", TypeRef, [TypeRef, ctypes.c_uint])

_func("GetElementType", TypeRef, [TypeRef])


# Value
class OpaqueValue(ctypes.Structure):
    pass

ValueRef = ctypes.POINTER(OpaqueValue)

_func("TypeOf", TypeRef, [ValueRef])
_func("SetValueName", None, [ValueRef, ctypes.c_char_p])
_func("GetValueName", ctypes.c_char_p, [ValueRef])
_func("DumpValue", None, [ValueRef])

# Operations on scalar constants
_func("ConstNull", ValueRef, [TypeRef])
_func("ConstInt", ValueRef, [TypeRef, ctypes.c_ulonglong, Bool])
_func("ConstReal", ValueRef, [TypeRef, ctypes.c_double])

_func("ConstString", ValueRef, [ctypes.c_char_p, ctypes.c_uint, Bool])
_func("ConstArray", ValueRef, [TypeRef, ctypes.POINTER(ValueRef), ctypes.c_uint])

_func("IsATerminatorInst", ValueRef, [ValueRef])


Opcode = ctypes.c_int


# Globals
_func("AddGlobal", ValueRef, [ModuleRef, TypeRef, ctypes.c_char_p])
_func("GetNamedGlobal", ValueRef, [ModuleRef, ctypes.c_char_p])
_func("SetInitializer", None, [ValueRef, ValueRef])
_func("SetGlobalConstant", None, [ValueRef, Bool])


# Functions
_func("FunctionType", TypeRef, [TypeRef, ctypes.POINTER(TypeRef), ctypes.c_uint, ctypes.c_int])
_func("AddFunction", ValueRef, [ModuleRef, ctypes.c_char_p, TypeRef])
_func("GetNamedFunction", ValueRef, [ModuleRef, ctypes.c_char_p])
_func("SetLinkage", None, [ValueRef, ctypes.c_int])
_func("GetParam", ValueRef, [ValueRef, ctypes.c_uint])
_func("GetReturnType", TypeRef, [TypeRef])

_func("AddAttribute", None, [ValueRef, ctypes.c_int]);

_func("AddFunctionAttr", None, [ValueRef, ctypes.c_int])

NoAliasAttribute = 1 << 6
AlwaysInlineAttribute = 1 << 12


def function_return_type(func):
    """Gets the return type directly from a function object."""
    return GetReturnType(GetElementType(TypeOf(func)))

_func("GetIntrinsicDeclaration", ValueRef,
      [ModuleRef, ctypes.c_uint, ctypes.POINTER(TypeRef), ctypes.c_uint])

_func("GetIntrinsicCount__", ctypes.c_uint, [])
_func("GetIntrinsicName__", owned_c_char_p, [ctypes.c_uint])


# This is False if we're generating docs didn't build the LLVM interface
if GetIntrinsicCount__ is not _Mock:
    INTRINSICS = dict((GetIntrinsicName__(i).value, i) for i in range(GetIntrinsicCount__()))
    """Intrinsic map; from name to intrinsic ID to use with GetIntrinsicDeclaration."""


ExternalLinkage = 0
PrivateLinkage = 8

# Structure Types
_func("StructType", TypeRef, [ctypes.POINTER(TypeRef), ctypes.c_uint, ctypes.c_bool])
_func("StructCreateNamed", TypeRef, [ContextRef, ctypes.c_char_p])
_func("StructSetBody", None, [TypeRef,
                              ctypes.POINTER(TypeRef),
                              ctypes.c_uint, ctypes.c_bool])

_func("GetStructName", ctypes.c_char_p, [TypeRef])


# Blocks
class OpaqueBasicBlock(ctypes.Structure):
    pass

BasicBlockRef = ctypes.POINTER(OpaqueBasicBlock)

_func("AppendBasicBlock", BasicBlockRef, [ValueRef, ctypes.c_char_p])
_func("MoveBasicBlockBefore", None, [BasicBlockRef, BasicBlockRef])
_func("MoveBasicBlockAfter", None, [BasicBlockRef, BasicBlockRef])
_func("GetBasicBlockParent", ValueRef, [BasicBlockRef])
_func("DeleteBasicBlock", None, [BasicBlockRef])

_func("GetEntryBasicBlock", BasicBlockRef, [ValueRef])
_func("GetFirstInstruction", ValueRef, [BasicBlockRef])
_func("GetLastInstruction", ValueRef, [BasicBlockRef])

# Phi expressions
_func("AddIncoming", None, [ValueRef, ctypes.POINTER(ValueRef),
                            ctypes.POINTER(BasicBlockRef),
                            ctypes.c_uint])


# Builder
class OpaqueBuilder(ctypes.Structure):
    pass

BuilderRef = ctypes.POINTER(OpaqueBuilder)

_func("CreateBuilder", BuilderRef)
_func("DisposeBuilder", None, [BuilderRef])
_func("PositionBuilderAtEnd", None, [BuilderRef, BasicBlockRef])
_func("PositionBuilder", None, [BuilderRef, BasicBlockRef, ValueRef])
_func("GetInsertBlock", BasicBlockRef, [BuilderRef])

_func("GetParentModule__", ModuleRef, [BuilderRef])


# Terminators
_func("BuildRetVoid", ValueRef, [BuilderRef])
_func("BuildRet", ValueRef, [BuilderRef, ValueRef])
_func("BuildBr", ValueRef, [BuilderRef, BasicBlockRef])
_func("BuildCondBr", ValueRef, [BuilderRef, ValueRef,
                                BasicBlockRef, BasicBlockRef])


# Float Expressions
for name in ("BuildFAdd", "BuildFSub", "BuildFMul", "BuildFDiv", "BuildFRem"):
    _func(name, ValueRef, [BuilderRef, ValueRef, ValueRef, ctypes.c_char_p])

_func("BuildFNeg", ValueRef, [BuilderRef, ValueRef, ctypes.c_char_p])
_func("BuildFCmp", ValueRef, [BuilderRef, ctypes.c_int, ValueRef, ValueRef, ctypes.c_char_p])


# Int expressions
for name in ("BuildAdd", "BuildSub", "BuildMul", "BuildUDiv", "BuildSDiv", "BuildSRem", "BuildLShr"):
    _func(name, ValueRef, [BuilderRef, ValueRef, ValueRef, ctypes.c_char_p])

_func("BuildNeg", ValueRef, [BuilderRef, ValueRef, ctypes.c_char_p])
_func("BuildICmp", ValueRef, [BuilderRef, ctypes.c_int, ValueRef, ValueRef, ctypes.c_char_p])

_func("BuildNot", ValueRef, [BuilderRef, ValueRef, ctypes.c_char_p])
for name in ("BuildAnd", "BuildOr", "BuildXor"):
    _func(name, ValueRef, [BuilderRef, ValueRef, ValueRef, ctypes.c_char_p])


# Memory

_func("BuildLoad", ValueRef, [BuilderRef, ValueRef, ctypes.c_char_p])
_func("BuildStore", ValueRef, [BuilderRef, ValueRef, ValueRef])
_func("BuildGEP", ValueRef, [BuilderRef, ValueRef,
                             ctypes.POINTER(ValueRef), ctypes.c_uint,
                             ctypes.c_char_p])
_func("BuildStructGEP", ValueRef, [BuilderRef, ValueRef, ctypes.c_uint, ctypes.c_char_p])

_func("BuildAlloca", ValueRef, [BuilderRef, TypeRef, ctypes.c_char_p])
_func("BuildArrayAlloca", ValueRef, [BuilderRef, TypeRef, ValueRef, ctypes.c_char_p])

_func("BuildMalloc", ValueRef, [BuilderRef, TypeRef, ctypes.c_char_p])
_func("BuildArrayMalloc", ValueRef, [BuilderRef, TypeRef, ValueRef, ctypes.c_char_p])


# Casting
Trunc = 30
ZExt = 31
FPToSI = 34
SIToFP = 36
FPTrunc = 37
FPExt = 38
BitCast = 41


_func("BuildCast", ValueRef, [BuilderRef, Opcode, ValueRef, TypeRef, ctypes.c_char_p])
_func("BuildPointerCast", ValueRef, [BuilderRef, ValueRef, TypeRef, ctypes.c_char_p])

# Misc
_func("BuildPhi", ValueRef, [BuilderRef, TypeRef, ctypes.c_char_p])
_func("BuildCall", ValueRef, [BuilderRef, ValueRef,
                              ctypes.POINTER(ValueRef), ctypes.c_uint,
                              ctypes.c_char_p])
_func("BuildSelect", ValueRef, [BuilderRef, ValueRef, ValueRef, ValueRef, ctypes.c_char_p])

_func("BuildExtractElement", ValueRef, [BuilderRef, ValueRef, ValueRef, ctypes.c_char_p])
_func("BuildInsertElement", ValueRef, [BuilderRef, ValueRef, ValueRef, ValueRef, ctypes.c_char_p])


# Analysis
VerifierFailureAction = ctypes.c_int
(AbortProcessAction, PrintMessageAction, ReturnStatusAction) = range(3)

_func("VerifyFunction", Bool, [ValueRef, VerifierFailureAction])


# Target
class OpaqueTarget(ctypes.Structure):
    pass

class OpaqueTargetData(ctypes.Structure):
    pass

class OpaqueTargetMachine(ctypes.Structure):
    pass

TargetRef = ctypes.POINTER(OpaqueTarget)
TargetDataRef = ctypes.POINTER(OpaqueTargetData)
TargetMachineRef = ctypes.POINTER(OpaqueTargetMachine)

_func("InitializeNativeTarget__", Bool, [])
_func("GetDefaultTargetTriple__", owned_c_char_p, [])
_func("LookupTarget__", TargetRef, [ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)])

_func("GetFirstTarget", TargetRef, [])
_func("GetNextTarget", TargetRef, [TargetRef])
_func("GetTargetDescription", ctypes.c_char_p, [TargetRef])

_func("CreateTargetMachine", TargetMachineRef,
      [TargetRef, ctypes.c_char_p, ctypes.c_char_p,
       ctypes.c_char_p, ctypes.c_int,
       ctypes.c_int, ctypes.c_int])

_func("DisposeTargetMachine", None, [TargetMachineRef])
_func("GetTargetMachineData", TargetDataRef, [TargetMachineRef])
_func("TargetMachineEmitToFile", Bool, [TargetMachineRef, ModuleRef,
                                        ctypes.c_char_p, ctypes.c_int,
                                        ctypes.POINTER(ctypes.c_char_p)])

# Pass Managers
class OpaquePassManager(ctypes.Structure):
    pass

class OpaquePassManagerBuilder(ctypes.Structure):
    pass

PassManagerRef = ctypes.POINTER(OpaquePassManager)
PassManagerBuilderRef = ctypes.POINTER(OpaquePassManagerBuilder)

_func("CreatePassManager", PassManagerRef, [])
_func("DisposePassManager", None, [PassManagerRef])
_func("RunPassManager", Bool, [PassManagerRef, ModuleRef])

_func("PassManagerBuilderCreate", PassManagerBuilderRef, [])
_func("PassManagerBuilderDispose", None, [PassManagerBuilderRef])

_func("PassManagerBuilderSetOptLevel", None, [PassManagerBuilderRef, ctypes.c_uint])
_func("PassManagerBuilderUseInlinerWithThreshold", None, [PassManagerBuilderRef, ctypes.c_uint])
_func("PassManagerBuilderPopulateModulePassManager", None,
      [PassManagerBuilderRef, PassManagerRef])

_func("AddTargetData", None, [TargetDataRef, PassManagerRef])


# Execution engine
class OpaqueExecutionEngine(ctypes.Structure):
    pass

ExecutionEngineRef = ctypes.POINTER(OpaqueExecutionEngine)

_func("CreateJITCompilerForModule", Bool,
      [ctypes.POINTER(ExecutionEngineRef), ModuleRef,
       ctypes.c_uint, ctypes.POINTER(ctypes.c_char_p)])

_func("GetExecutionEngineTargetData", TargetDataRef, [ExecutionEngineRef])
_func("GetPointerToGlobal", ctypes.c_void_p, [ExecutionEngineRef, ValueRef])

_func("DisposeExecutionEngine", None, [ExecutionEngineRef])


# Memory Buffers
class OpaqueMemoryBuffer(ctypes.Structure):
    pass

MemoryBufferRef = ctypes.POINTER(OpaqueMemoryBuffer)

_func("CreateMemoryBufferWithContentsOfFile", Bool,
      [ctypes.c_char_p, ctypes.POINTER(MemoryBufferRef), ctypes.POINTER(ctypes.c_char_p)])
_func("DisposeMemoryBuffer", None, [MemoryBufferRef])


# Bitcode Readers
_func("ParseBitcode", Bool,
      [MemoryBufferRef, ctypes.POINTER(ModuleRef), ctypes.POINTER(ctypes.c_char_p)])


# Command line options
_func("ParseEnvironmentOptions", None, [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p])
if os.environ.get("NITROUS_LLVM_OPTS"):
    ParseEnvironmentOptions("nitrous", "NITROUS_LLVM_OPTS", None)


def address_of(r):
    """Returns LLVM reference (eg. ValueRef) pointer value."""
    return ctypes.cast(r, ctypes.c_void_p).value


def types_equal(tx, ty):
    """Returns True if *tx* is the same LLVMTypeRef as *ty*.

    To check equality, retrieve and compare raw pointer values.

    """
    # In LLVM, getting the same type (eg. IntType(32)) yields
    # same unique pointer value each time its invoked.
    return address_of(tx) == address_of(ty)


def get_intrinsic(builder, name, spec):
    """Return intrinsic declaration for name and specialization types."""
    module = GetParentModule__(builder)
    i = INTRINSICS["llvm.{0}".format(name)]
    return GetIntrinsicDeclaration(module, i, spec, len(spec))


def build_py_idiv(builder, a, b, name):
    """Build expression for floor integer division.

    As seen in Cython:

        long q = a / b;
        long r = a - q*b;
        q -= ((r != 0) & ((r ^ b) < 0));
        return q;

    """

    q = BuildSDiv(builder, a, b, name + "_q")
    r = BuildSub(builder, a, BuildMul(builder, q, b, name + "_r"), name + "_sub")

    # TODO Assumes signed integers
    zero = ConstNull(TypeOf(r))
    q_sub = BuildAnd(builder,
                     BuildICmp(builder, IntNE, r, zero, name + "_cmp_1"),
                     BuildICmp(builder, IntSLT,
                               BuildXor(builder, r, b, name + "_xor"),
                               zero, name + "_cmp_2"),
                     name + "_q_and")

    return BuildSub(builder, q,
                    BuildCast(builder, ZExt, q_sub, TypeOf(a), name + "_cast"),
                    name)


def build_pow(builder, a, b, name):
    """Builds an expression for a ** b."""
    pow = get_intrinsic(builder, "pow", (TypeRef * 1)(TypeOf(a)))
    v = BuildCall(builder, pow, (ValueRef * 2)(a, b), 2, "call")
    return v


# Modulo (%) implementation for integers and floats
#
#   r = a (s|f)rem b
#   if (a >> (sizeof(a) * 8)) ^ (b >> (sizeof(b) * 8)) == 1:
#       return -r
#   else
#       return r


def _mod_scale(builder, int_a, int_b):
    """When building `mod`, returns True if the result of `rem` should
    be scaled by -1, False otherwise.

    Assume *int_a* and *int_b* are integers of equal size.

    """
    ty = TypeOf(int_a)
    size = GetIntTypeWidth(ty)

    one = ConstInt(ty, 1, True)
    sign_shift = ConstInt(ty, size - 1, True)

    sign_a = BuildLShr(builder, int_a, sign_shift, "")
    cond_a = BuildICmp(builder, IntEQ, sign_a, one, "")

    sign_b = BuildLShr(builder, int_b, sign_shift, "")
    cond_b = BuildICmp(builder, IntEQ, sign_b, one, "")

    return BuildXor(builder, cond_a, cond_b, "")


def build_smod(builder, a, b, name):
    """Builds expression for signed int modulo."""

    rem = BuildSRem(builder, a, b, "")
    neg_rem = BuildMul(builder, rem, ConstInt(TypeOf(rem), -1, True), "")

    return BuildSelect(builder, _mod_scale(builder, a, b), neg_rem, rem, name)


def build_fmod(builder, a, b, name):
    """Builds expression for floating point modulo."""

    kind = GetTypeKind(TypeOf(a))
    if kind == FloatTypeKind:
        size = 32
    elif kind == DoubleTypeKind:
        size = 64
    else:
        raise TypeError("Cannot build %: unknown float type kind")

    int_ty = IntType(size)
    int_a = BuildCast(builder, BitCast, a, int_ty, "")
    int_b = BuildCast(builder, BitCast, b, int_ty, "")

    rem = BuildFRem(builder, a, b, "")
    neg_rem = BuildFMul(builder, rem, ConstReal(TypeOf(rem), -1), "")

    return BuildSelect(builder, _mod_scale(builder, int_a, int_b), neg_rem, rem, name)


def link_modules(dst, src):
    """Link source module into destination one.

    Source module is destroyed.

    """
    message = ctypes.c_char_p()
    status = LinkModules__(dst, src, LinkerDestroySource, ctypes.byref(message))
    if status != 0:
        error = RuntimeError("Could not link modules: {0}".format(message.value))
        llvm.DisposeMessage(message)
        raise error
