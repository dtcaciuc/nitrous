
import unittest2 as unittest

from nitrous.module import module
from nitrous.function import function
from nitrous.types import Double, Long, Structure, Pointer


Coord = Structure("Coord", ("x", Double), ("y", Double), ("z", Double))


class StructureTests(unittest.TestCase):

    def test_repr(self):
        self.assertEqual(repr(Coord), "<Structure 'Coord', 3 fields>")

    def test_load_fields(self):
        import ctypes

        # Attribute load from subscript
        @function(Double, a=Pointer(Coord), i=Long)
        def sum_1(a, i):
            return a[i].x + a[i].y + a[i].z

        # Subscript, reference assignment, then attribute load
        @function(Double, a=Pointer(Coord), i=Long)
        def sum_2(a, i):
            ai = a[i]
            return ai.x + ai.y + ai.z

        m = module([sum_1, sum_2])

        a1 = Coord.c_type(1, 2, 3)
        a2 = (Coord.c_type * 2)(
            (1, 2, 3),
            (4, 5, 6)
        )

        for f in (m.sum_1, m.sum_2):
            self.assertAlmostEqual(f(ctypes.byref(a1), 0), 6)
            self.assertAlmostEqual(f(a2, 0), 6)
            self.assertAlmostEqual(f(ctypes.byref(a2[0]), 0), 6)
            self.assertAlmostEqual(f(a2, 1), 15)
            self.assertAlmostEqual(f(ctypes.byref(a2[1]), 0), 15)

    def test_store_fields(self):

        # Attribute store to subscript
        @function(None, a=Pointer(Coord), i=Long, x=Double, y=Double, z=Double)
        def store_1(a, i, x, y, z):
            a[i].x = x
            a[i].y = y
            a[i].z = z

        # Subscript, reference assignment, then attribute store
        @function(None, a=Pointer(Coord), i=Long, x=Double, y=Double, z=Double)
        def store_2(a, i, x, y, z):
            ai = a[i]
            ai.x = x
            ai.y = y
            ai.z = z

        m = module([store_1, store_2])

        for f in (m.store_1, m.store_2):

            a = (Coord.c_type * 2)(
                (0, 0, 0),
                (0, 0, 0)
            )

            # Store second coordinate, make sure first one is intact
            f(a, 1, 4, 5, 6)

            self.assertAlmostEqual(a[0].x, 0)
            self.assertAlmostEqual(a[0].y, 0)
            self.assertAlmostEqual(a[0].z, 0)

            self.assertAlmostEqual(a[1].x, 4)
            self.assertAlmostEqual(a[1].y, 5)
            self.assertAlmostEqual(a[1].z, 6)

            # Store first coordinate, make sure second one is intact
            f(a, 0, 1, 2, 3)

            self.assertAlmostEqual(a[0].x, 1)
            self.assertAlmostEqual(a[0].y, 2)
            self.assertAlmostEqual(a[0].z, 3)

            self.assertAlmostEqual(a[1].x, 4)
            self.assertAlmostEqual(a[1].y, 5)
            self.assertAlmostEqual(a[1].z, 6)

    def test_aug_assign(self):
        """Augmented attribute assignment"""

        @function(None, a=Pointer(Coord), i=Long, j=Long)
        def add_i(a, i, j):
            a[i].x += a[j].x
            a[i].y += a[j].y
            a[i].z += a[j].z

        a = (Coord.c_type * 2)(
            (1, 2, 3),
            (4, 5, 6)
        )

        m = module([add_i])
        m.add_i(a, 0, 1)

        self.assertAlmostEqual(a[0].x, 5)
        self.assertAlmostEqual(a[0].y, 7)
        self.assertAlmostEqual(a[0].z, 9)

        self.assertAlmostEqual(a[1].x, 4)
        self.assertAlmostEqual(a[1].y, 5)
        self.assertAlmostEqual(a[1].z, 6)

    def test_value_arg(self):
        """Passing structure argument by value"""
        from nitrous.lib.math import sqrt

        @function(Double, a=Coord)
        def _norm(a):
            return sqrt(Double)(a.x * a.x + a.y * a.y + a.z * a.z)

        @function(Double, a=Coord)
        def norm(a):
            # Test passing by-value to inner compiled function.
            return _norm(a)

        m = module([norm])

        c = Coord.c_type(1, 2, 3)
        self.assertAlmostEqual(m.norm(c), 3.741657386)
