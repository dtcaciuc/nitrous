import unittest
import ctypes

from nitrous.module import module
from nitrous.function import function
from nitrous.types import Long
from nitrous.types.array import Array, FastSlice, Slice, Any

try:
    import numpy as np
except ImportError:
    np = None


class ArrayTestsBase(object):

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
        self.addCleanup(delattr, self, "m")

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


class SliceTests(ArrayTestsBase, unittest.TestCase):

    A = Slice(Long, (Any,) * 3)
    B = Slice(Long)

    def test_repr(self):
        self.assertEqual(repr(self.A), "Slice(Long, shape=(Any, Any, Any))")
        self.assertEqual(repr(self.B), "Slice(Long, shape=(Any,))")

    def test_str(self):
        self.assertEqual(str(self.A), "<Slice [? x [? x [? x Long]]]>")
        self.assertEqual(str(self.B), "<Slice [? x Long]>")


class FastSliceTests(ArrayTestsBase, unittest.TestCase):

    A = FastSlice(Long, (2, 3, 2))
    B = FastSlice(Long, (12,))

    def test_repr(self):
        self.assertEqual(repr(self.A), "FastSlice(Long, shape=(2, 3, 2))")
        self.assertEqual(repr(self.B), "FastSlice(Long, shape=(12,))")

    def test_str(self):
        self.assertEqual(str(self.A), "<FastSlice [2 x [3 x [2 x Long]]]>")
        self.assertEqual(str(self.B), "<FastSlice [12 x Long]>")


class ArrayTests(ArrayTestsBase, unittest.TestCase):

    A = Array(Long, (2, 3, 2))
    B = Array(Long, (12,))

    def test_repr(self):
        self.assertEqual(repr(self.A), "Array(Long, shape=(2, 3, 2))")
        self.assertEqual(repr(self.B), "Array(Long, shape=(12,))")

    def test_str(self):
        self.assertEqual(str(self.A), "<Array [2 x [3 x [2 x Long]]]>")
        self.assertEqual(str(self.B), "<Array [12 x Long]>")


class ArrayAllocTests(unittest.TestCase):

    def test_alloc_return(self):
        """Allocate array and pass back through return value."""
        from nitrous.types import Double

        Coord = Array(Double, (3,))

        @function(Coord, x=Double, y=Double, z=Double)
        def make_coord(x, y, z):
            return Coord((x, y, z))

        @function(Coord, x=Double, y=Double, z=Double)
        def make_coord_2(x, y, z):
            return make_coord(x, y, z)

        m = module([make_coord, make_coord_2])
        c = m.make_coord_2(1.0, 2.0, 3.0)

        self.assertEqual(tuple(c), (1.0, 2.0, 3.0))

    def test_init_2d(self):
        """Multi-dimensional array initialization."""
        from nitrous.types import Double

        Double2x2 = Array(Double, (2, 2))

        @function(Double2x2, x=Double, y=Double, z=Double, w=Double)
        def make_2x2(x, y, z, w):
            return Double2x2(((x, y), (z, w)))

        m = module([make_2x2])
        c = m.make_2x2(1.0, 2.0, 3.0, 4.0)

        self.assertEqual(c[0][0], 1.0)
        self.assertEqual(c[0][1], 2.0)
        self.assertEqual(c[1][0], 3.0)
        self.assertEqual(c[1][1], 4.0)


class SliceReferenceTests(unittest.TestCase):

    def test_reference_arg(self):
        """Slice is treated as reference type."""
        # Since slices don't directly inherit structs,
        # make sure that they are returned by reference
        # in array item access.

        # FIXME Rewrite this, since this doesn't work as
        # advertised at all. Whether x0 is a reference or
        # a copy, it will contain the same information either
        # way and the test will pass.
        self.assertTrue(False)

        @function(x=Array(Slice(Long), (1,)), i=Long, v=Long)
        def set_i(x, i, v):
            x0 = x[0]
            x0[i] = v

        m = module([set_i])

        x = (Long.c_type * 3)(3, 11, 13)
        px = (ctypes.POINTER(Long.c_type) * 1)(x)

        m.set_i(px, 1, 12)

        self.assertEqual(list(x), [3, 12, 13])


class IndexTests(unittest.TestCase):

    def setUp(self):
        self.data = (((Long.c_type * 3) * 3) * 3)(
            ((0, 1, 2), (3, 4, 5), (6, 7, 8)),
            ((18, 19, 20), (21, 22, 23), (24, 25, 26)),
            ((9, 10, 11), (12, 13, 14), (15, 16, 17)),
        )
        self.addCleanup(delattr, self, "data")

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


class SubsliceTests(unittest.TestCase):

    def setUp(self):
        self.DataSlice = Slice(Long, (5, 2, 3))
        self.data = (((Long.c_type * 3) * 2) * 5)(
            ((0, 1, 2), (3, 4, 5)),
            ((6, 7, 8), (18, 19, 20)),
            ((21, 22, 23), (24, 25, 26)),
            ((9, 10, 11), (12, 13, 14)),
            ((15, 16, 17), (33, 34, 35)),
        )
        self.addCleanup(delattr, self, "DataSlice")
        self.addCleanup(delattr, self, "data")

    def test_subslice_shape_i(self):
        """Subslice shape reduced by one dimension (two remain)"""

        ND, S0, S1 = range(3)

        @function(x=self.DataSlice, i=Long, v=Slice(Long))
        def get_i(x, i, v):
            s = x[i]
            v[ND] = s.ndim
            v[S0] = s.shape[0]
            v[S1] = s.shape[1]

        m = module([get_i])
        v = (Long.c_type * 3)()

        # Shape and dimensions should not depend on indices.
        for i in range(5):
            m.get_i(self.data, i, v)
            self.assertEqual(v[ND], 2)
            self.assertEqual(v[S0], 2)
            self.assertEqual(v[S1], 3)

    def test_subslice_shape_ij(self):
        """Subslice shape reduced by two dimensions (one remains)"""

        ND, S0 = range(2)

        @function(x=self.DataSlice, i=Long, j=Long, v=Slice(Long))
        def get_ij(x, i, j, v):
            s = x[i, j]
            v[ND] = s.ndim
            v[S0] = s.shape[0]

        m = module([get_ij])
        v = (Long.c_type * 2)()

        # Shape and dimensions should not depend on indices.
        for i in range(5):
            for j in range(2):
                m.get_ij(self.data, i, j, v)
                self.assertEqual(v[ND], 1)
                self.assertEqual(v[S0], 3)

    def test_subslice_data_i(self):
        """Subslice data reduced by one dimension (two remain)"""

        @function(x=self.DataSlice, i=Long, v=Slice(Long, (2, 3)))
        def get_i(x, i, v):
            s = x[i]
            for j in range(2):
                for k in range(3):
                    v[j, k] = s[j, k]

        m = module([get_i])
        v = ((Long.c_type * 3) * 2)()

        for i in range(5):
            m.get_i(self.data, i, v)
            ref_v = list(list(row) for row in self.data[i])
            self.assertEqual(list(list(row) for row in v), ref_v)

    def test_subslice_data_ij(self):
        """Subslice data reduced by one dimension (two remain)"""

        @function(x=self.DataSlice, i=Long, j=Long, v=Slice(Long, (3,)))
        def get_ij(x, i, j, v):
            s = x[i, j]
            for k in range(3):
                v[k] = s[k]

        m = module([get_ij])
        v = (Long.c_type * 3)()

        for i in range(5):
            for j in range(2):
                m.get_ij(self.data, i, j, v)
                self.assertEqual(list(v), list(self.data[i][j]))
