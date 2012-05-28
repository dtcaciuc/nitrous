import ctypes
from collections import namedtuple
from . import llvm


Type = namedtuple("Type", "c_type, llvm_type")
"""Base for NOS data types."""


Double = Type(ctypes.c_double, llvm.DoubleType())
Long = Type(ctypes.c_long, llvm.IntType(ctypes.sizeof(ctypes.c_long) * 8))
