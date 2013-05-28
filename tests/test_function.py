from nitrous.module import module, dump
from nitrous.function import function
import unittest


class NameTest(unittest.TestCase):

    def test_encode_args(self):
        """Function name is uniqued according to its argument types."""
        from nitrous.types.array import Slice
        from nitrous.types import Float, Double

        def get0(T):

            @function(T.element_type, v=T)
            def get0(v):
                return v[0]

            return get0

        get0f = get0(Slice(Float))
        get0d = get0(Slice(Double))

        m = module([get0f, get0d])
        ir = dump(m)

        self.assertIn("get0_RBdAnyf4", ir)
        self.assertIn("get0_RBdAnyf8", ir)


class AggregateReturnTests(unittest.TestCase):

    def test_struct(self):
        from nitrous.types import Structure
        from nitrous.types import Double

        Coord = Structure("Coord", ("x", Double), ("y", Double), ("z", Double))

        @function(Coord, x=Double, y=Double, z=Double)
        def make_coord(x, y, z):
            return Coord(x, y, z)

        @function(Coord, x=Double, y=Double, z=Double)
        def make_coord_2(x, y, z):
            return make_coord(x, y, z)

        m = module([make_coord, make_coord_2])

        c = m.make_coord_2(1.0, 2.0, 3.0)

        self.assertEqual(c.x, 1.0)
        self.assertEqual(c.y, 2.0)
        self.assertEqual(c.z, 3.0)
