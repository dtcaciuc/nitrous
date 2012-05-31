import ctypes
import ctypes.util
import platform
import os


VERSION = os.environ["NOS_LLVM_VERSION"]

if VERSION == "2.9":
    from .v29 import *
elif VERSION == "3.1":
    from .v31 import *
else:
    raise RuntimeError("Incompatible LLVM version {0}".format(VERSION))


def _load_llvm():
    # TODO is there a standard way to get the extension?
    ext = dict(Darwin="dylib", Linux="so", Windows="dll")[platform.system()]
    name = "libLLVM-{0}.{1}".format(os.environ["NOS_LLVM_VERSION"], ext)
    return ctypes.cdll.LoadLibrary(name)


_llvm = _load_llvm()
_llvm_addons = ctypes.cdll.LoadLibrary(os.path.join(os.path.dirname(__file__), "addons.so"))
_libc = ctypes.cdll.LoadLibrary(ctypes.util.find_library("c"))


def _func(func_name, restype, argtypes=[], shlib=None):
    """Creates ctypes wrapper for an LLVM API function.

    LLVM{name} -> llvm.{name}

    """
    g = globals()

    g[func_name] = getattr((shlib or _llvm), "LLVM" + func_name)
    g[func_name].restype = restype
    g[func_name].argtypes = argtypes


class owned_c_char_p(ctypes.c_char_p):
    """Char pointer which collects the memory of the return value."""

    def __del__(self):
        _libc.free(self)


Bool = ctypes.c_int


# Module
class OpaqueModule(ctypes.Structure):
    pass

ModuleRef = ctypes.POINTER(OpaqueModule)

_func("ModuleCreateWithName", ModuleRef)
_func("DumpModule", None, [ModuleRef])
_func("DumpModuleToString", owned_c_char_p, [ModuleRef], _llvm_addons)
_func("WriteBitcodeToFile", Bool, [ModuleRef, ctypes.c_char_p])
_func("DisposeModule", None, [ModuleRef])

_func("DisposeMessage", None, [ctypes.c_char_p])


# Type
class OpaqueType(ctypes.Structure):
    pass

TypeRef = ctypes.POINTER(OpaqueType)

TypeKind = ctypes.c_int

_func("FloatType", TypeRef)
_func("DoubleType", TypeRef)
_func("IntType", TypeRef, [ctypes.c_uint])

_func("GetTypeKind", TypeKind, [TypeRef])
_func("GetIntTypeWidth", ctypes.c_uint, [TypeRef])


# Sequential types
_func("PointerType", TypeRef, [TypeRef, ctypes.c_uint])


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

_func("GetIntrinsicDeclaration", ValueRef,
      [ModuleRef, ctypes.c_uint, ctypes.POINTER(TypeRef), ctypes.c_uint],
      _llvm_addons)

_func("GetIntrinsicCount__", ctypes.c_uint, [], _llvm_addons)
_func("GetIntrinsicName__", owned_c_char_p, [ctypes.c_uint], _llvm_addons)


INTRINSICS = dict((GetIntrinsicName__(i).value, i) for i in range(GetIntrinsicCount__()))
"""Intrinsic map; from name to intrinsic ID to use with GetIntrinsicDeclaration."""


(ExternalLinkage,) = range(1)


# Blocks
class OpaqueBasicBlock(ctypes.Structure):
    pass

BasicBlockRef = ctypes.POINTER(OpaqueBasicBlock)

_func("AppendBasicBlock", BasicBlockRef, [ValueRef, ctypes.c_char_p])
_func("MoveBasicBlockAfter", None, [BasicBlockRef, BasicBlockRef])
_func("GetBasicBlockParent", ValueRef, [BasicBlockRef])


# Phi expressions
_func("AddIncoming", None, [ValueRef, ctypes.POINTER(ValueRef),
                            ctypes.POINTER(BasicBlockRef),
                            ctypes.c_uint])


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
_func("GetInsertBlock", BasicBlockRef, [BuilderRef])

_func("BuildRet", ValueRef, [BuilderRef, ValueRef])
_func("BuildBr", ValueRef, [BuilderRef, BasicBlockRef])
_func("BuildCondBr", ValueRef, [BuilderRef, ValueRef,
                                BasicBlockRef, BasicBlockRef])


# Float Expressions
for name in ("BuildFAdd", "BuildFSub", "BuildFMul", "BuildFDiv"):
    _func(name, ValueRef, [BuilderRef, ValueRef, ValueRef, ctypes.c_char_p])

_func("BuildFCmp", ValueRef, [BuilderRef, ctypes.c_int, ValueRef, ValueRef, ctypes.c_char_p])


# Int expressions
for name in ("BuildAdd", "BuildSub", "BuildMul", "BuildUDiv", "BuildSDiv"):
    _func(name, ValueRef, [BuilderRef, ValueRef, ValueRef, ctypes.c_char_p])

_func("BuildICmp", ValueRef, [BuilderRef, ctypes.c_int, ValueRef, ValueRef, ctypes.c_char_p])


# Boolean expressions
for name in ("BuildAnd", "BuildOr"):
    _func(name, ValueRef, [BuilderRef, ValueRef, ValueRef, ctypes.c_char_p])


# Memory

_func("BuildLoad", ValueRef, [BuilderRef, ValueRef, ctypes.c_char_p])
_func("BuildStore", ValueRef, [BuilderRef, ValueRef, ValueRef])
_func("BuildGEP", ValueRef, [BuilderRef, ValueRef,
                             ctypes.POINTER(ValueRef), ctypes.c_uint,
                             ctypes.c_char_p])
_func("BuildAlloca", ValueRef, [BuilderRef, TypeRef, ctypes.c_char_p])
_func("BuildArrayAlloca", ValueRef, [BuilderRef, TypeRef, ValueRef, ctypes.c_char_p])

# Casting
ZExt = 31
FPToSI = 34
SIToFP = 36

_func("BuildCast", ValueRef, [BuilderRef, Opcode, ValueRef, TypeRef, ctypes.c_char_p])

# Misc
_func("BuildPhi", ValueRef, [BuilderRef, TypeRef, ctypes.c_char_p])
_func("BuildCall", ValueRef, [BuilderRef, ValueRef,
                              ctypes.POINTER(ValueRef), ctypes.c_uint,
                              ctypes.c_char_p])


# Analysis
VerifierFailureAction = ctypes.c_int
(AbortProcessAction, PrintMessageAction, ReturnStatusAction) = range(3)

_func("VerifyFunction", Bool, [ValueRef, VerifierFailureAction])
