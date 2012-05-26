import ctypes


class Double(object):

    c_type = ctypes.c_double

    def generate(self):
        from .llvm import DoubleType
        return DoubleType()
