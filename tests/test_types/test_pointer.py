import unittest
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
        self.assertEqual(repr(Pointer(Double)), "<Pointer to Double>")


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

        @function(Pointer(Double), x=Pointer(Double))
        def f(x):
            return x

        self.m = module([f])
        self.addCleanup(delattr, self, "m")

    def test_array(self):
        """Pointer() accepts array.array objects."""
        from array import array
        x = array('d', (3,))
        self.assertEqual(self.m.f(x).contents.value, 3)

    @unittest.skipIf(not np, "NumPy integration feature")
    def test_numpy_array(self):
        """Pointer() accepts NumPy ndarray objects."""
        x = np.double((3,))
        self.assertEqual(self.m.f(x).contents.value, 3)

    def test_invalid_param(self):
        """Raise error if an incompatible data structure is supplied"""
        with self.assertRaises(ctypes.ArgumentError):
            self.m.f([1])
