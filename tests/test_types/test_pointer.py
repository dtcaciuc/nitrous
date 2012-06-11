import unittest2 as unittest

from nos.types import Double, Pointer
from nos.util import ModuleTest

try:
    import numpy as np
except ImportError:
    np = None


class PointerTests(ModuleTest, unittest.TestCase):

    def setUp(self):
        super(PointerTests, self).setUp()

        @self.m.function(Double, x=Pointer(Double))
        def f(x):
            return x[0]

        self.f = f

    def test_ctypes_array(self):
        """Pointer() accepts regular ctypes arrays"""
        out = self.m.compile()
        x = (Double.c_type * 1)(3)
        self.assertEqual(out.f(x), 3)

    def test_array(self):
        """Pointer() accepts array.array objects."""
        from array import array

        out = self.m.compile()
        x = array('d', (3,))
        self.assertEqual(out.f(x), 3)

    @unittest.skipIf(not np, "NumPy integration feature")
    def test_numpy_array(self):
        """Pointer() accepts NumPy ndarray objects."""
        out = self.m.compile()
        x = np.double((3,))
        self.assertEqual(out.f(x), 3)

    def test_invalid_param(self):
        """Raise error if an incompatible data structure is supplied"""
        import ctypes

        out = self.m.compile()
        with self.assertRaises(ctypes.ArgumentError):
            out.f([1])
