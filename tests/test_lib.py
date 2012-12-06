from nitrous.types import Double, Long, ScalarType
from nitrous.module import module
from nitrous.function import function
from nitrous import llvm
import nitrous.lib.math
import ctypes
import math

import unittest2 as unittest


class MathTests(unittest.TestCase):

    def test_pow(self):

        @function(Double, x=Double, y=Double)
        def pow(x, y):
            return nitrous.lib.math.pow(Double)(x, y)

        m = module([pow])
        self.assertAlmostEqual(math.pow(3.0, 5.0), m.pow(3.0, 5.0))

    def test_sqrt(self):

        @function(Double, x=Double)
        def sqrt(x):
            return nitrous.lib.math.sqrt(Double)(x)

        m = module([sqrt])
        self.assertAlmostEqual(math.sqrt(10.0), m.sqrt(10.0))

    def test_exp(self):

        @function(Double, x=Double)
        def exp(x):
            return nitrous.lib.math.exp(Double)(x)

        m = module([exp])
        self.assertAlmostEqual(math.exp(10.0), m.exp(10.0))

    def test_log(self):

        @function(Double, x=Double)
        def log(x):
            return nitrous.lib.math.log(Double)(x)

        m = module([log])
        self.assertAlmostEqual(math.log(10.0), m.log(10.0))
        self.assertAlmostEqual(math.log(1.0), m.log(1.0))


class CastTests(unittest.TestCase):

    Byte = ScalarType(ctypes.c_byte, llvm.IntType(8))

    def test_cast(self):
        from nitrous.lib import cast

        @function(Long, a=Long, b=Double)
        def div_long(a, b):
            return a / cast(b, Long)

        @function(Double, a=Long, b=Double)
        def div_double(a, b):
            return cast(a, Double) / b

        m = module([div_long, div_double])

        self.assertEqual(m.div_long(3, 2), 1)
        self.assertEqual(m.div_double(3, 2), 1.5)

    def test_cast_noop(self):
        from nitrous.lib import cast

        @function(Double, a=Double, b=Double)
        def div_double(a, b):
            return cast(a, Double) / b

        m = module([div_double])
        self.assertEqual(m.div_double(3, 2), 1.5)

    def test_cast_byte_to_int(self):
        """Cast between narrower and wider integer"""
        from nitrous.lib import cast
        from nitrous.types import Int
        from nitrous.module import dump

        @function(Int, a=self.Byte)
        def int_to_long(a):
            return cast(a, Int)

        m = module([int_to_long])
        self.assertEqual(m.int_to_long(3), 3)
        self.assertRegexpMatches(dump(m), "zext")

    def test_cast_long_to_int(self):
        """Cast between wider and narrower integer"""
        from nitrous.lib import cast
        from nitrous.types import Int
        from nitrous.module import dump

        @function(self.Byte, a=Int)
        def int_to_long(a):
            return cast(a, self.Byte)

        m = module([int_to_long])
        self.assertEqual(m.int_to_long(3), 3)
        self.assertRegexpMatches(dump(m), "trunc")

    def test_cast_double_to_float(self):
        """Cast between wider and narrower float"""
        from nitrous.lib import cast
        from nitrous.types import Float
        from nitrous.module import dump

        @function(Float, a=Double)
        def double_to_float(a):
            return cast(a, Float)

        m = module([double_to_float])
        self.assertEqual(m.double_to_float(1.0), 1.0)
        self.assertRegexpMatches(dump(m), "trunc")

    def test_cast_float_to_double(self):
        """Cast between narrower and wider float"""
        from nitrous.lib import cast
        from nitrous.types import Float
        from nitrous.module import dump

        @function(Double, a=Float)
        def float_to_double(a):
            return cast(a, Double)

        m = module([float_to_double])
        self.assertEqual(m.float_to_double(1.0), 1.0)
        self.assertRegexpMatches(dump(m), "ext")

    def test_invalid_cast(self):
        from nitrous.lib import cast
        from nitrous.types import Structure

        S = Structure("S", ("x", Long))

        @function(Long, a=S)
        def int_to_long(a):
            return cast(a, Long)

        with self.assertRaisesRegexp(TypeError, "Cannot cast"):
            module([int_to_long])


class PrintTests(unittest.TestCase):

    def test_various(self):
        from nitrous.types import Float, Int, Byte
        from tempfile import NamedTemporaryFile

        with NamedTemporaryFile() as fout:

            @function()
            def print_test():
                print >>fout.file, "string0",
                print >>fout.file, 1, Int(1), Byte(1),
                print >>fout.file, 2.0, Float(2.0)
                print >>fout.file, "end"

            m = module([print_test])
            m.print_test()

            fout.seek(0)
            self.assertEqual(fout.read(), "string01 1 12.000000 2.000000\nend\n")
