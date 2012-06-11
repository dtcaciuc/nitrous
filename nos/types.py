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
