import unittest2 as unittest
import ctypes

from nitrous.module import module
from nitrous.function import function
from nitrous.types import Double, Pointer

try:
    import numpy as np
except ImportError:
    np = None


class InitTests(unittest.TestCase):

    def test_repr(self):
        self.assertEqual(repr(Pointer(Double)), "<Pointer [? x Double]>")
        self.assertEqual(repr(Pointer(Double, shape=(2, 3))), "<Pointer [2 x [3 x Double]]>")


class ComparisonTests(unittest.TestCase):

    def test(self):
        from nitrous.types import Bool

        DoubleN = Pointer(Double)

        @function(Bool, x=DoubleN)
        def is_null(x):
            return x == DoubleN.null

        m = module([is_null])

        self.assertFalse(m.is_null((Double.c_type * 2)()))
        self.assertTrue(m.is_null(None))


class ConverterTests(unittest.TestCase):

    def setUp(self):

        @function(Double, x=Pointer(Double))
        def f(x):
            return x[0]

        self.m = module([f])

    def test_array(self):
        """Pointer() accepts array.array objects."""
        from array import array
        x = array('d', (3,))
        self.assertEqual(self.m.f(x), 3)

    @unittest.skipIf(not np, "NumPy integration feature")
    def test_numpy_array(self):
        """Pointer() accepts NumPy ndarray objects."""
        x = np.double((3,))
        self.assertEqual(self.m.f(x), 3)

    def test_invalid_param(self):
        """Raise error if an incompatible data structure is supplied"""
        with self.assertRaises(ctypes.ArgumentError):
            self.m.f([1])


class CTypesPointerTests(unittest.TestCase):

    def test_pointer(self):
        from nitrous.types import Long, Pointer

        @function(Long, d=Pointer(Long), i=Long)
        def f(d, i):
            return d[i]

        m = module([f])

        d1 = (((Long.c_type * 2) * 2) * 2)(
            ((0, 1), (2, 3)), ((4, 5), (6, 7))
        )

        for i in range(8):
            self.assertEqual(m.f(d1[0][0], i), i)

        for i in range(2):
            for j in range(2):
                for k in range(2):
                    self.assertEqual(m.f(d1[i][j], k), i * 2 * 2 + j * 2 + k)

    def test_argtype_check(self):
        from nitrous.types import Long, Pointer

        @function(Long, d=Pointer(Long))
        def f(d):
            return d[0]

        m = module([f])

        # Different variations of valid pointers
        self.assertEqual(m.f(ctypes.byref(ctypes.c_long(5))), 5)
        self.assertEqual(m.f(ctypes.pointer(ctypes.c_long(5))), 5)
        self.assertEqual(m.f((ctypes.c_long * 1)(5)), 5)

        # ctypes automatically takes pointer of an object if target type is a pointer.
        self.assertEqual(m.f(ctypes.c_long(5)), 5)

        # One pointer too many.
        with self.assertRaises(ctypes.ArgumentError):
            m.f(ctypes.byref(ctypes.pointer(ctypes.c_long(5))))

        # Integral type, but different width
        with self.assertRaises(ctypes.ArgumentError):
            m.f(ctypes.byref(ctypes.c_byte(5)))

        # Different type altogether
        with self.assertRaises(ctypes.ArgumentError):
            m.f(ctypes.byref(ctypes.c_float(5)))
        with self.assertRaises(ctypes.ArgumentError):
            m.f(ctypes.c_float(5))
