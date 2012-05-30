import ctypes
from collections import namedtuple
from . import llvm


Type = namedtuple("Type", "c_type, llvm_type")
"""Base for NOS data types."""


Double = Type(ctypes.c_double, llvm.DoubleType())
Long = Type(ctypes.c_long, llvm.IntType(ctypes.sizeof(ctypes.c_long) * 8))
Bool = Type(ctypes.c_bool, llvm.IntType(8))


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
        return ctypes.POINTER(self.element_type.c_type)
