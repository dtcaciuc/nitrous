from nitrous.util import ModuleTest
from nitrous.types import Double, Long, ScalarType
from nitrous import llvm
import nitrous.lib.math
import ctypes
import math

import unittest2 as unittest


class MathTests(ModuleTest, unittest.TestCase):

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

        @self.m.function(Int, a=self.Byte)
        def int_to_long(a):
            return cast(a, Int)

        out = self.m.build()
        self.assertEqual(out.int_to_long(3), 3)
        self.assertRegexpMatches(self.m.dumps(), "zext")

    def test_cast_long_to_int(self):
        """Cast between wider and narrower integer"""
        from nitrous.lib import cast
        from nitrous.types import Int

        @self.m.function(self.Byte, a=Int)
        def int_to_long(a):
            return cast(a, self.Byte)

        out = self.m.build()
        self.assertEqual(out.int_to_long(3), 3)
        self.assertRegexpMatches(self.m.dumps(), "trunc")

    def test_invalid_cast(self):
        from nitrous.lib import cast
        from nitrous.types import Structure

        S = Structure("S", ("x", Long))

        @self.m.function(Long, a=S)
        def int_to_long(a):
            return cast(a, Long)

        with self.assertRaisesRegexp(TypeError, "Cannot cast"):
            self.m.build()
