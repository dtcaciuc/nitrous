import unittest2 as unittest

from nitrous.types import Bool, Long, Double
from nitrous.util import ModuleTest


class BoolTests(ModuleTest, unittest.TestCase):

    def test_and(self):

        @self.m.function(Bool, a=Bool, b=Bool)
        def and_(a, b):
            return a and b

        out = self.m.build()

        self.assertEqual(out.and_(True, True), True)
        self.assertEqual(out.and_(True, False), False)
        self.assertEqual(out.and_(False, True), False)
        self.assertEqual(out.and_(False, False), False)

    def test_or(self):

        @self.m.function(Bool, a=Bool, b=Bool)
        def or_(a, b):
            return a or b

        out = self.m.build()

        self.assertEqual(out.or_(True, True), True)
        self.assertEqual(out.or_(True, False), True)
        self.assertEqual(out.or_(False, True), True)
        self.assertEqual(out.or_(False, False), False)


class LongTests(ModuleTest, unittest.TestCase):

    def test_const(self):
        """Constant declaration."""

        @self.m.function(Double)
        def x():
            a = 5.0
            return a

        out = self.m.build()
        self.assertEqual(out.x(), 5.0)

    def test_binary(self):

        annotate = self.m.function(Long, a=Long, b=Long)
        funcs = [annotate(f) for f in binary_funcs()]
        out = self.m.build()

        values = [(0, 1), (3, 2), (2, 3), (-3, 2), (3, -2)]

        for f in funcs:
            cf = getattr(out, f.func_name)
            for a, b in values:
                self.assertEqual(cf(a, b), f(a, b),
                                 "{0}({1}, {2})".format(f.func_name, a, b))

    def test_cmp(self):

        annotate = self.m.function(Bool, a=Long, b=Long)
        funcs = [annotate(f) for f in cmp_funcs()]
        out = self.m.build()

        for f in funcs:
            cf = getattr(out, f.func_name)
            self.assertEqual(cf(6, 5), f(6, 5))
            self.assertEqual(cf(5, 5), f(5, 5))
            self.assertEqual(cf(5, 6), f(5, 6))


class DoubleTests(ModuleTest, unittest.TestCase):

    def test_const(self):
        """Constant declaration."""

        @self.m.function(Long)
        def x():
            a = 5
            return a

    def test_binary(self):

        annotate = self.m.function(Double, a=Double, b=Double)
        funcs = [annotate(f) for f in binary_funcs()]
        out = self.m.build()

        values = [(0.0, 1.0), (3.0, 2.0), (2.0, 3.0), (-3.0, 2.0), (3.0, -2.0)]

        for f in funcs:
            cf = getattr(out, f.func_name)
            for a, b in values:
                self.assertAlmostEqual(cf(a, b), f(a, b), 9,
                                       "{0}({1}, {2})".format(f.func_name, a, b))

    def test_cmp(self):

        annotate = self.m.function(Bool, a=Double, b=Double)
        funcs = [annotate(f) for f in cmp_funcs()]
        out = self.m.build()

        for f in funcs:
            cf = getattr(out, f.func_name)
            self.assertEqual(cf(6.0, 5.0), f(6.0, 5.0))
            self.assertEqual(cf(5.0, 5.0), f(5.0, 5.0))
            self.assertEqual(cf(5.0, 6.0), f(5.0, 6.0))


def binary_funcs():

    def add(a, b):
        return a + b

    def sub(a, b):
        return a - b

    def mul(a, b):
        return a * b

    def div(a, b):
        return a / b

    return [add, sub, mul, div]


def cmp_funcs():

    def lte(a, b):
        return a <= b

    def lt(a, b):
        return a < b

    def eq(a, b):
        return a == b

    def neq(a, b):
        return a != b

    def gt(a, b):
        return a > b

    def gte(a, b):
        return a >= b

    return [lte, lt, eq, neq, gt, gte]
