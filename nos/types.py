import ctypes
from collections import namedtuple
from . import llvm


Type = namedtuple("Type", "c_type, llvm_type")
"""Base for NOS data types."""


Double = Type(ctypes.c_double, llvm.DoubleType())
