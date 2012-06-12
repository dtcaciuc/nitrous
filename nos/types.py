import ctypes
import ast

from collections import namedtuple
from . import llvm


def type_key(ty):
    """Returns unique key for type *ty*.

    In LLVM, getting the same type (eg. IntType(32)) yields
    same unique pointer value each time its invoked.

    """
    return ctypes.cast(ty, ctypes.c_void_p).value


def types_equal(tx, ty):
    """Returns True if *tx* is the same LLVMTypeRef as *ty*.

    To check equality, retrieve and compare raw pointer values.

    """
    return type_key(tx) == type_key(ty)


ScalarType = namedtuple("ScalarType", ("c_type", "llvm_type"))
"""Base for all scalar data types."""


Double = ScalarType(ctypes.c_double, llvm.DoubleType())

Long = ScalarType(ctypes.c_long, llvm.IntType(ctypes.sizeof(ctypes.c_long) * 8))

Bool = ScalarType(ctypes.c_bool, llvm.IntType(8))


BINARY_INST = {
    type_key(Double.llvm_type): {
        ast.Add: llvm.BuildFAdd,
        ast.Sub: llvm.BuildFSub,
        ast.Mult: llvm.BuildFMul,
        ast.Div: llvm.BuildFDiv,
    },
    type_key(Long.llvm_type): {
        ast.Add: llvm.BuildAdd,
        ast.Sub: llvm.BuildSub,
        ast.Mult: llvm.BuildMul,
        # Python uses floor integer division
        ast.Div: llvm.build_py_idiv,
    }
}


COMPARE_INST = {
    type_key(Double.llvm_type): (
        llvm.BuildFCmp, {
            ast.Eq: llvm.RealUEQ,
            ast.Gt: llvm.RealUGT,
            ast.GtE: llvm.RealUGE,
            ast.Lt: llvm.RealULT,
            ast.LtE: llvm.RealULE,
            ast.NotEq: llvm.RealUNE,
        }
    ),
    type_key(Long.llvm_type): (
        llvm.BuildICmp,  {
            ast.Eq: llvm.IntEQ,
            ast.Gt: llvm.IntSGT,
            ast.GtE: llvm.IntSGE,
            ast.Lt: llvm.IntSLT,
            ast.LtE: llvm.IntSLE,
            ast.NotEq: llvm.IntNE
        }
    )
}


class Pointer(object):
    """Pointer to memory block, each element of type `element_type`.

    Does not provide any dimensionality information; user must
    take care of tracking access index bounds.

    """

    def __init__(self, element_type):
        self.element_type = element_type

    @property
    def llvm_type(self):
        return llvm.PointerType(self.element_type.llvm_type, 0)

    @property
    def c_type(self):
        # Ctypes requires an object with from_param() method.
        return self

    def from_param(self, p):
        import array

        pointer_type = ctypes.POINTER(self.element_type.c_type)

        try:
            import numpy as np
            if isinstance(p, np.ndarray):
                return p.ctypes.data_as(pointer_type)
        except ImportError:
            pass

        if isinstance(p, array.array):
            addr, count = p.buffer_info()
            return ctypes.cast(addr, pointer_type)

        return p
