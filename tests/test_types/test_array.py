import unittest2 as unittest
import ctypes

from nitrous.types import DynamicArray, StaticArray, Long, Dynamic
from nitrous.util import ModuleTest

try:
    import numpy as np
except ImportError:
    np = None


class ArrayTests(object):

    def setUp(self):
        super(ArrayTests, self).setUp()

        X, Y, Z = range(3)

        @self.m.function(Long, a=self.A, b=self.B)
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


class DynamicArrayTests(ArrayTests, ModuleTest, unittest.TestCase):

    A = DynamicArray(Long, (Dynamic,) * 3)
    B = DynamicArray(Long)


class StaticArrayTests(ArrayTests, ModuleTest, unittest.TestCase):

    A = StaticArray(Long, (2, 3, 2))
    B = StaticArray(Long, (12,))


class StatiicAllocTests(ModuleTest, unittest.TestCase):

    def test_alloc(self):
        """Stack allocation of a fixed size array by calling its type"""
        from nitrous.types import Double, StaticArray

        Mat2d = StaticArray(Double, shape=(2, 2))

        @self.m.function(Double)
        def f():

            m = Mat2d()
            a = 0.0

            m[0, 0] = 2.0
            m[0, 1] = 11.0
            m[1, 0] = 13.0
            m[1, 1] = 17.0

            for i in range(2):
                for j in range(2):
                    a += m[i, j]

            return a

        out = self.m.build()

        x = (Double.c_type * 4)()
        self.assertEqual(out.f(x), 43.0)



class IndexTests(ModuleTest, unittest.TestCase):

    def setUp(self):
        super(IndexTests, self).setUp()
        self.data = (((Long.c_type * 3) * 3) * 3)(
            ((0, 1, 2), (3, 4, 5), (6, 7, 8)),
            ((18, 19, 20), (21, 22, 23), (24, 25, 26)),
            ((9, 10, 11), (12, 13, 14), (15, 16, 17)),
        )

    def test_static_dimension(self):
        """Replace access to known dimensions with direct constants"""

        D = DynamicArray(Long, shape=(Dynamic, 3, 3))
        X, Y, Z = range(3)

        @self.m.function(Long, a=D)
        def f(a):
            return a[2, 1, 2]

        out = self.m.build()
        # All indices should be resolved at run-time, so there should be no multiplications.
        self.assertNotRegexpMatches(self.m.dumps(), "mul")
        self.assertEqual(out.f(self.data), 14)

    def test_all_dynamic_dimension(self):
        """All dimensions are dynamic, no indices can be resolved at runtime"""
        D = DynamicArray(Long, shape=(Dynamic, Dynamic, Dynamic))
        X, Y, Z = range(3)

        @self.m.function(Long, a=D)
        def f(a):
            return a[2, 1, 2]

        out = self.m.build()
        # Should have run-time multiplications during index flattening.
        self.assertRegexpMatches(self.m.dumps(), "mul")
        self.assertEqual(out.f(self.data), 14)

    def test_mixed_dynamic_dimension(self):
        """Some dimensions are dynamic, other than major one"""

        D = DynamicArray(Long, shape=(Dynamic, 3, Dynamic))
        X, Y, Z = range(3)

        @self.m.function(Long, a=D)
        def f(a):
            return a[2, 1, 2]

        out = self.m.build()
        # Should have run-time multiplications during index flattening.
        self.assertRegexpMatches(self.m.dumps(), "mul")
        self.assertEqual(out.f(self.data), 14)
