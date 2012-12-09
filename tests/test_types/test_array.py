import unittest2 as unittest
import ctypes

from nitrous.module import module
from nitrous.function import function
from nitrous.types import Long
from nitrous.types.array import Array, Slice, Any

try:
    import numpy as np
except ImportError:
    np = None


class ArrayTests(object):

    def setUp(self):

        X, Y, Z = range(3)

        @function(Long, a=self.A, b=self.B)
        def f(a, b):
            m = 0
            for i in range(a.shape[X]):
                for j in range(a.shape[Y]):
                    for k in range(a.shape[Z]):
                        b[m] = a[i, j, k]
                        m += 1
            return m

        self.m = module([f])

    def test_array(self):

        A = (((ctypes.c_long * 2) * 3) * 2)
        a = A(((1, 2), (3, 4), (5, 6)),
              ((7, 8), (9, 10), (11, 12)))

        B = ctypes.c_long * 12
        b = B()

        m = self.m.f(a, b)

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

        m = self.m.f(a, b)

        self.assertEqual(m, 12)
        self.assertEqual(list(b), range(1, 13))


class SliceTests(ArrayTests, unittest.TestCase):

    A = Slice(Long, (Any,) * 3)
    B = Slice(Long)

    def test_repr(self):
        self.assertEqual(repr(self.A), "<Slice [? x [? x [? x Long]]]>")
        self.assertEqual(repr(self.B), "<Slice [? x Long]>")


class ArrayTests(ArrayTests, unittest.TestCase):

    A = Array(Long, (2, 3, 2))
    B = Array(Long, (12,))

    def test_repr(self):
        self.assertEqual(repr(self.A), "<Array [2 x [3 x [2 x Long]]]>")
        self.assertEqual(repr(self.B), "<Array [12 x Long]>")


class AllocTests(unittest.TestCase):

    def test_alloc(self):
        """Stack allocation of a fixed size array by calling its type"""
        from nitrous.types import Double

        Mat2d = Array(Double, shape=(2, 2))

        @function(Double)
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

        m = module([f])

        x = (Double.c_type * 4)()
        self.assertEqual(m.f(x), 43.0)


class IndexTests(unittest.TestCase):

    def setUp(self):
        self.data = (((Long.c_type * 3) * 3) * 3)(
            ((0, 1, 2), (3, 4, 5), (6, 7, 8)),
            ((18, 19, 20), (21, 22, 23), (24, 25, 26)),
            ((9, 10, 11), (12, 13, 14), (15, 16, 17)),
        )

    def test_static_dimension(self):
        """Replace access to known dimensions with direct constants"""
        from nitrous.module import dump

        D = Slice(Long, shape=(Any, 3, 3))
        X, Y, Z = range(3)

        @function(Long, a=D)
        def f(a):
            return a[2, 1, 2]

        m = module([f])
        # All indices should be resolved at run-time, so there should be no multiplications.
        self.assertNotRegexpMatches(dump(m), "mul")
        self.assertEqual(m.f(self.data), 14)

    def test_all_dynamic_dimension(self):
        """All dimensions are dynamic, no indices can be resolved at runtime"""
        from nitrous.module import dump

        D = Slice(Long, shape=(Any, Any, Any))
        X, Y, Z = range(3)

        @function(Long, a=D)
        def f(a):
            return a[2, 1, 2]

        m = module([f])
        # Should have run-time multiplications during index flattening.
        self.assertRegexpMatches(dump(m), "mul")
        self.assertEqual(m.f(self.data), 14)

    def test_mixed_dynamic_dimension(self):
        """Some dimensions are dynamic, other than major one"""
        from nitrous.module import dump

        D = Slice(Long, shape=(Any, 3, Any))
        X, Y, Z = range(3)

        @function(Long, a=D)
        def f(a):
            return a[2, 1, 2]

        m = module([f])
        # Should have run-time multiplications during index flattening.
        self.assertRegexpMatches(dump(m), "mul")
        self.assertEqual(m.f(self.data), 14)
