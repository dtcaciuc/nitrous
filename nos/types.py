import ctypes
from collections import namedtuple
from . import llvm


Type = namedtuple("Type", "c_type, llvm_type")
"""Base for NOS data types."""


Double = Type(ctypes.c_double, llvm.DoubleType())
Int64 = Type(ctypes.c_long, llvm.IntType(64))
