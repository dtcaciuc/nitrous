import unittest2 as unittest
import ctypes

from nitrous.types import Array, Long
from nitrous.util import ModuleTest

try:
    import numpy as np
except ImportError:
    np = None


class ArrayTests(ModuleTest, unittest.TestCase):

    def setUp(self):
        super(ArrayTests, self).setUp()

        X, Y, Z = range(3)

        @self.m.function(Long, a=Array(Long, 3), b=Array(Long))
        def f(a, b):
            m = 0
            for i in range(a.shape[X]):
                for j in range(a.shape[Y]):
                    for k in range(a.shape[Z]):
                        b[m] = a[i, j, k]
                        m += 1
            return m

    def test_array(self):
        out = self.m.build()

        A = (((ctypes.c_long * 2) * 3) * 2)
        a = A(((1, 2), (3, 4), (5, 6)),
              ((7, 8), (9, 10), (11, 12)))

        B = ctypes.c_long * 12
        b = B()

        m = out.f(a, b)

        self.assertEqual(m, 12)
        self.assertEqual(list(b), range(1, 13))

    @unittest.skipIf(not np, "NumPy integration feature")
    def test_ndarray(self):
        dtype = np.dtype("i{0}".format(ctypes.sizeof(ctypes.c_long)))
        a = np.array([
            ((1, 2), (3, 4), (5, 6)),
            ((7, 8), (9, 10), (11, 12))
        ], dtype=dtype)

        b = np.empty(12, dtype=dtype)

        out = self.m.build()
        m = out.f(a, b)

        self.assertEqual(m, 12)
        self.assertEqual(list(b), range(1, 13))
