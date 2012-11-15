from nitrous.util import ModuleTest
from nitrous.types import Double, Long, ScalarType
from nitrous import llvm
import nitrous.lib.math
import ctypes
import math

import unittest2 as unittest


class MathTests(ModuleTest, unittest.TestCase):

    def test_pow(self):
        from nitrous.module import dump

        @self.m.function(Double, x=Double, y=Double)
        def pow(x, y):
            return nitrous.lib.math.pow(x, y)

        out = self.m.build()
        self.assertAlmostEqual(math.pow(3.0, 5.0), out.pow(3.0, 5.0))

    def test_sqrt(self):

        @self.m.function(Double, x=Double)
        def sqrt(x):
            return nitrous.lib.math.sqrt(x)

        out = self.m.build()
        self.assertAlmostEqual(math.sqrt(10.0), out.sqrt(10.0))

    def test_exp(self):

        @self.m.function(Double, x=Double)
        def exp(x):
            return nitrous.lib.math.exp(x)

        out = self.m.build()
        self.assertAlmostEqual(math.exp(10.0), out.exp(10.0))

    def test_log(self):

        @self.m.function(Double, x=Double)
        def log(x):
            return nitrous.lib.math.log(x)

        out = self.m.build()
        self.assertAlmostEqual(math.log(10.0), out.log(10.0))
        self.assertAlmostEqual(math.log(1.0), out.log(1.0))

    def test_base_log(self):

        @self.m.function(Double, x=Double, b=Double)
        def log(x, b):
            return nitrous.lib.math.log(x, b)

        out = self.m.build()
        self.assertAlmostEqual(math.log(10.0, 4.0), out.log(10.0, 4.0))
        self.assertAlmostEqual(math.log(1.0, 4.0), out.log(1.0, 4.0))


class CastTests(ModuleTest, unittest.TestCase):

    Byte = ScalarType(ctypes.c_byte, llvm.IntType(8))

    def test_cast(self):
        from nitrous.lib import cast

        @self.m.function(Long, a=Long, b=Double)
        def div_long(a, b):
            return a / cast(b, Long)

        @self.m.function(Double, a=Long, b=Double)
        def div_double(a, b):
            return cast(a, Double) / b

        out = self.m.build()

        self.assertEqual(out.div_long(3, 2), 1)
        self.assertEqual(out.div_double(3, 2), 1.5)

    def test_cast_noop(self):
        from nitrous.lib import cast

        @self.m.function(Double, a=Double, b=Double)
        def div_double(a, b):
            return cast(a, Double) / b

        out = self.m.build()
        self.assertEqual(out.div_double(3, 2), 1.5)

    def test_cast_byte_to_int(self):
        """Cast between narrower and wider integer"""
        from nitrous.lib import cast
        from nitrous.types import Int
        from nitrous.module import dump

        @self.m.function(Int, a=self.Byte)
        def int_to_long(a):
            return cast(a, Int)

        out = self.m.build()
        self.assertEqual(out.int_to_long(3), 3)
        self.assertRegexpMatches(dump(out), "zext")

    def test_cast_long_to_int(self):
        """Cast between wider and narrower integer"""
        from nitrous.lib import cast
        from nitrous.types import Int
        from nitrous.module import dump

        @self.m.function(self.Byte, a=Int)
        def int_to_long(a):
            return cast(a, self.Byte)

        out = self.m.build()
        self.assertEqual(out.int_to_long(3), 3)
        self.assertRegexpMatches(dump(out), "trunc")

    def test_cast_double_to_float(self):
        """Cast between wider and narrower float"""
        from nitrous.lib import cast
        from nitrous.types import Float
        from nitrous.module import dump

        @self.m.function(Float, a=Double)
        def double_to_float(a):
            return cast(a, Float)

        out = self.m.build()
        self.assertEqual(out.double_to_float(1.0), 1.0)
        self.assertRegexpMatches(dump(out), "trunc")

    def test_cast_float_to_double(self):
        """Cast between narrower and wider float"""
        from nitrous.lib import cast
        from nitrous.types import Float
        from nitrous.module import dump

        @self.m.function(Double, a=Float)
        def float_to_double(a):
            return cast(a, Double)

        out = self.m.build()
        self.assertEqual(out.float_to_double(1.0), 1.0)
        self.assertRegexpMatches(dump(out), "ext")

    def test_invalid_cast(self):
        from nitrous.lib import cast
        from nitrous.types import Structure

        S = Structure("S", ("x", Long))

        @self.m.function(Long, a=S)
        def int_to_long(a):
            return cast(a, Long)

        with self.assertRaisesRegexp(TypeError, "Cannot cast"):
            self.m.build()
