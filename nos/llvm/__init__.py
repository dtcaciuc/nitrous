import ctypes
import platform
import os


def _load_llvm():
    # TODO is there a standard way to get the extension?
    ext = dict(Darwin="dylib", Linux="so", Windows="dll")[platform.system()]
    name = "libLLVM-{0}.{1}".format(os.environ["NOS_LLVM_VERSION"], ext)
    return ctypes.cdll.LoadLibrary(name)


_llvm = _load_llvm()


def _func(func_name, restype, argtypes=[]):
    """Creates ctypes wrapper for an LLVM API function.

    LLVM{name} -> llvm.{name}

    """
    g = globals()

    g[func_name] = getattr(_llvm, "LLVM" + func_name)
    g[func_name].restype = restype
    g[func_name].argtypes = argtypes


Bool = ctypes.c_int


# Module
class OpaqueModule(ctypes.Structure):
    pass

ModuleRef = ctypes.POINTER(OpaqueModule)

_func("ModuleCreateWithName", ModuleRef)
_func("DumpModule", None, [ModuleRef])
_func("WriteBitcodeToFile", Bool, [ModuleRef, ctypes.c_char_p])
_func("DisposeModule", None, [ModuleRef])

_func("DisposeMessage", None, [ctypes.c_char_p])


# Type
class OpaqueType(ctypes.Structure):
    pass

TypeRef = ctypes.POINTER(OpaqueType)

(
    VoidTypeKind,
    HalfTypeKind,
    FloatTypeKind,
    DoubleTypeKind,
    X86_FP80TypeKind,
    FP128TypeKind,
    PPC_FP128TypeKind,
    LabelTypeKind,
    IntegerTypeKind,
    FunctionTypeKind,
    StructTypeKind,
    ArrayTypeKind,
    PointerTypeKind,
    VectorTypeKind,
    MetadataTypeKind,
    X86_MMXTypeKind

) = range(16)

TypeKind = ctypes.c_int

_func("FloatType", TypeRef)
_func("DoubleType", TypeRef)
_func("IntType", TypeRef, [ctypes.c_uint])

_func("GetTypeKind", TypeKind, [TypeRef])


# Value
class OpaqueValue(ctypes.Structure):
    pass

ValueRef = ctypes.POINTER(OpaqueValue)

_func("TypeOf", TypeRef, [ValueRef])
_func("SetValueName", None, [ValueRef, ctypes.c_char_p])

# Operations on scalar constants
_func("ConstInt", ValueRef, [TypeRef, ctypes.c_ulonglong, Bool])
_func("ConstReal", ValueRef, [TypeRef, ctypes.c_double])

Opcode = ctypes.c_int

# Functions
_func("FunctionType", TypeRef, [TypeRef, ctypes.POINTER(TypeRef), ctypes.c_uint, ctypes.c_int])
_func("AddFunction", ValueRef, [ModuleRef, ctypes.c_char_p, TypeRef])
_func("SetLinkage", None, [ValueRef, ctypes.c_int])
_func("GetParam", ValueRef, [ValueRef, ctypes.c_uint])

(ExternalLinkage,) = range(1)


# Blocks
class OpaqueBasicBlock(ctypes.Structure):
    pass

BasicBlockRef = ctypes.POINTER(OpaqueBasicBlock)

_func("AppendBasicBlock", BasicBlockRef, [ValueRef, ctypes.c_char_p])


# Execution engine
class OpaqueExecutionEngine(ctypes.Structure):
    pass

ExecutionEngineRef = ctypes.POINTER(OpaqueExecutionEngine)

_func("CreateExecutionEngineForModule", ExecutionEngineRef)


# Builder
class OpaqueBuilder(ctypes.Structure):
    pass

BuilderRef = ctypes.POINTER(OpaqueBuilder)

_func("CreateBuilder", BuilderRef)
_func("DisposeBuilder", None, [BuilderRef])
_func("PositionBuilderAtEnd", None, [BuilderRef, BasicBlockRef])
_func("BuildRet", ValueRef, [BuilderRef, ValueRef])


# Float Expressions
for name in ("BuildFAdd", "BuildFSub", "BuildFMul", "BuildFDiv"):
    _func(name, ValueRef, [BuilderRef, ValueRef, ValueRef, ctypes.c_char_p])


# Int expressions
for name in ("BuildAdd", "BuildSub", "BuildMul", "BuildUDiv", "BuildSDiv"):
    _func(name, ValueRef, [BuilderRef, ValueRef, ValueRef, ctypes.c_char_p])

# Casting
# TODO add the rest of LLVMOpcode
FPToSI = 34
SIToFP = 36

_func("BuildCast", ValueRef, [BuilderRef, Opcode, ValueRef, TypeRef, ctypes.c_char_p])

# Misc
_func("BuildCall", ValueRef, [BuilderRef, ValueRef,
                              ctypes.POINTER(ValueRef), ctypes.c_uint,
                              ctypes.c_char_p])

# Analysis
VerifierFailureAction = ctypes.c_int
(AbortProcessAction, PrintMessageAction, ReturnStatusAction) = range(3)

_func("VerifyFunction", Bool, [ValueRef, VerifierFailureAction])