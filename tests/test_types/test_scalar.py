import unittest2 as unittest

from nitrous.types import Bool, Long, Float, Double
from nitrous.util import ModuleTest


class CastTests(ModuleTest, unittest.TestCase):

    def test(self):
        from nitrous.module import dump

        @self.m.function(Float, a=Double)
        def cast_(a):
            return Float(a)

        out = self.m.build()
        self.assertEqual(out.cast_(1.0), 1.0)
        self.assertRegexpMatches(dump(out), "trunc")


class BoolTests(ModuleTest, unittest.TestCase):

    def test_not(self):

        @self.m.function(Bool, a=Bool)
        def not_(a):
            return not a

        out = self.m.build()

        self.assertTrue(out.not_(False))
        self.assertFalse(out.not_(True))

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

    def test_complex_and(self):
        """Multi-value `and` expression."""

        @self.m.function(Bool, a=Long, b=Long, c=Long)
        def and_(a, b, c):
            return a < -2 and b == 0 and c > 2

        out = self.m.build()

        self.assertFalse(out.and_(1, 1, 1))

        self.assertFalse(out.and_(-3, 1, 1))
        self.assertFalse(out.and_(-3, 0, 1))
        self.assertFalse(out.and_(-3, 1, 3))

        self.assertFalse(out.and_(1, 0, 1))
        self.assertFalse(out.and_(1, 0, 3))
        self.assertFalse(out.and_(3, 0, 1))

        self.assertFalse(out.and_(1, 1, 3))
        self.assertFalse(out.and_(1, 0, 3))
        self.assertFalse(out.and_(-3, 1, 3))

        self.assertTrue(out.and_(-3, 0, 3))

    def test_complex_or(self):
        """Multi-value `or` expression."""

        @self.m.function(Bool, a=Long, b=Long, c=Long)
        def or_(a, b, c):
            return a < -2 or b == 0 or c > 2

        out = self.m.build()

        self.assertFalse(out.or_(1, 1, 1))

        self.assertTrue(out.or_(-3, 1, 1))
        self.assertTrue(out.or_(1, 0, 1))
        self.assertTrue(out.or_(1, 0, 3))


class LongTests(ModuleTest, unittest.TestCase):

    def test_const(self):
        """Constant declaration."""

        @self.m.function(Long)
        def x():
            a = 5
            return a

        out = self.m.build()
        self.assertEqual(out.x(), 5)

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

    def test_unary(self):

        annotate = self.m.function(Long, a=Long)
        funcs = [annotate(f) for f in unary_funcs()]
        out = self.m.build()

        values = [-5, 0, 5]

        for f in funcs:
            cf = getattr(out, f.func_name)
            for a in values:
                self.assertEqual(cf(a), f(a), "{0}({1})".format(f.func_name, a))

    def test_cmp(self):

        annotate = self.m.function(Bool, a=Long, b=Long)
        funcs = [annotate(f) for f in cmp_funcs()]
        out = self.m.build()

        for f in funcs:
            cf = getattr(out, f.func_name)
            self.assertEqual(cf(6, 5), f(6, 5))
            self.assertEqual(cf(5, 5), f(5, 5))
            self.assertEqual(cf(5, 6), f(5, 6))

    def test_cdiv(self):
        """Switch to enable C-style integer division"""

        @self.m.function(Long, a=Long, b=Long)
        def pydiv(a, b):
            return a / b

        @self.m.options(cdiv=True)
        @self.m.function(Long, a=Long, b=Long)
        def cdiv(a, b):
            return a / b

        out = self.m.build()

        self.assertEqual(-5 / 2, -3)
        self.assertEqual(out.pydiv(-5, 2), -3)
        self.assertEqual(out.cdiv(-5, 2), -2)


class FloatingTests(ModuleTest):

    def test_unary(self):

        annotate = self.m.function(self.Type, a=self.Type)
        funcs = [annotate(f) for f in unary_funcs()]
        out = self.m.build()

        values = [-5.0, 0.0, 5.0]

        for f in funcs:
            cf = getattr(out, f.func_name)
            for a in values:
                self.assertEqual(cf(a), f(a), "{0}({1})".format(f.func_name, a))

    def test_binary(self):

        annotate = self.m.function(self.Type, a=self.Type, b=self.Type)
        funcs = [annotate(f) for f in binary_funcs() + floating_binary_funcs()]
        out = self.m.build()

        values = [(0.0, 1.0), (3.0, 2.0), (2.0, 3.0), (-3.0, 2.0), (3.0, -2.0)]

        for f in funcs:
            cf = getattr(out, f.func_name)
            for a, b in values:
                self.assertAlmostEqual(cf(a, b), f(a, b), self.digits,
                                       "{0}({1}, {2})".format(f.func_name, a, b))

    def test_cmp(self):

        annotate = self.m.function(Bool, a=self.Type, b=self.Type)
        funcs = [annotate(f) for f in cmp_funcs()]
        out = self.m.build()

        for f in funcs:
            cf = getattr(out, f.func_name)
            self.assertEqual(cf(6.0, 5.0), f(6.0, 5.0))
            self.assertEqual(cf(5.0, 5.0), f(5.0, 5.0))
            self.assertEqual(cf(5.0, 6.0), f(5.0, 6.0))


class FloatTests(FloatingTests, unittest.TestCase):

    Type = Float
    digits = 7

    # TODO test_const when we can do `x = Float(5.0)`


class DoubleTests(FloatingTests, unittest.TestCase):

    Type = Double
    digits = 9

    def test_const(self):
        """Constant declaration."""

        @self.m.function(self.Type)
        def x():
            a = 5.0
            return a

        out = self.m.build()
        self.assertEqual(out.x(), 5.0)


def unary_funcs():

    def negate(a):
        return -a

    return [negate]


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


def floating_binary_funcs():

    def pow(a, b):
        return a ** b

    return [pow]


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
