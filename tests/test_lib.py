from nitrous.util import ModuleTest
from nitrous.types import Double
import nitrous.lib.math
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
