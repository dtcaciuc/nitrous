import unittest

from nitrous.module import module
from nitrous.function import function
from nitrous.types import Bool, Long, Float, Double


class CastTests(unittest.TestCase):

    def test(self):
        from nitrous.module import dump

        @function(Float, a=Double)
        def cast_(a):
            return Float(a)

        m = module([cast_])
        self.assertEqual(m.cast_(1.0), 1.0)
        self.assertRegexpMatches(dump(m), "trunc")


class BoolTests(unittest.TestCase):

    def test_not(self):

        @function(Bool, a=Bool)
        def not_(a):
            return not a

        m = module([not_])

        self.assertTrue(m.not_(False))
        self.assertFalse(m.not_(True))

    def test_and(self):

        @function(Bool, a=Bool, b=Bool)
        def and_(a, b):
            return a and b

        m = module([and_])

        self.assertEqual(m.and_(True, True), True)
        self.assertEqual(m.and_(True, False), False)
        self.assertEqual(m.and_(False, True), False)
        self.assertEqual(m.and_(False, False), False)

    def test_or(self):

        @function(Bool, a=Bool, b=Bool)
        def or_(a, b):
            return a or b

        m = module([or_])

        self.assertEqual(m.or_(True, True), True)
        self.assertEqual(m.or_(True, False), True)
        self.assertEqual(m.or_(False, True), True)
        self.assertEqual(m.or_(False, False), False)

    def test_complex_and(self):
        """Multi-value `and` expression."""

        @function(Bool, a=Long, b=Long, c=Long)
        def and_(a, b, c):
            return a < -2 and b == 0 and c > 2

        m = module([and_])

        self.assertFalse(m.and_(1, 1, 1))

        self.assertFalse(m.and_(-3, 1, 1))
        self.assertFalse(m.and_(-3, 0, 1))
        self.assertFalse(m.and_(-3, 1, 3))

        self.assertFalse(m.and_(1, 0, 1))
        self.assertFalse(m.and_(1, 0, 3))
        self.assertFalse(m.and_(3, 0, 1))

        self.assertFalse(m.and_(1, 1, 3))
        self.assertFalse(m.and_(1, 0, 3))
        self.assertFalse(m.and_(-3, 1, 3))

        self.assertTrue(m.and_(-3, 0, 3))

    def test_complex_or(self):
        """Multi-value `or` expression."""

        @function(Bool, a=Long, b=Long, c=Long)
        def or_(a, b, c):
            return a < -2 or b == 0 or c > 2

        m = module([or_])

        self.assertFalse(m.or_(1, 1, 1))

        self.assertTrue(m.or_(-3, 1, 1))
        self.assertTrue(m.or_(1, 0, 1))
        self.assertTrue(m.or_(1, 0, 3))


class LongTests(unittest.TestCase):

    def test_const(self):
        """Constant declaration."""

        @function(Long)
        def x():
            a = 5
            return a

        m = module([x])
        self.assertEqual(m.x(), 5)

    def test_binary(self):

        annotate = function(Long, a=Long, b=Long)
        funcs = binary_funcs() + integer_binary_funcs()
        m = module(map(annotate, funcs))

        values = [(0, 1), (3, 2), (2, 3), (-3, 2), (3, -2)]

        for f in funcs:
            cf = getattr(m, f.__name__)
            for a, b in values:
                self.assertEqual(cf(a, b), f(a, b),
                                 "{0}({1}, {2})".format(f.__name__, a, b))

    def test_unary(self):

        annotate = function(Long, a=Long)
        funcs = unary_funcs() + integer_unary_funcs()
        m = module(map(annotate, funcs))

        values = [-5, 0, 5]

        for f in funcs:
            cf = getattr(m, f.__name__)
            for a in values:
                self.assertEqual(cf(a), f(a), "{0}({1})".format(f.__name__, a))

    def test_cmp(self):

        annotate = function(Bool, a=Long, b=Long)
        funcs = cmp_funcs()
        m = module(map(annotate, funcs))

        for f in funcs:
            cf = getattr(m, f.__name__)
            self.assertEqual(cf(6, 5), f(6, 5))
            self.assertEqual(cf(5, 5), f(5, 5))
            self.assertEqual(cf(5, 6), f(5, 6))

    def test_cdiv(self):
        """Switch to enable C-style integer division"""
        from nitrous.function import options

        @function(Long, a=Long, b=Long)
        def pydiv(a, b):
            return a / b

        @options(cdiv=True)
        @function(Long, a=Long, b=Long)
        def cdiv(a, b):
            return a / b

        m = module([pydiv, cdiv])

        self.assertEqual(-5 / 2, -3)
        self.assertEqual(m.pydiv(-5, 2), -3)
        self.assertEqual(m.cdiv(-5, 2), -2)


class FloatingTests(object):

    def test_unary(self):

        annotate = function(self.Type, a=self.Type)
        funcs = unary_funcs()
        m = module(map(annotate, funcs))

        values = [-5.0, 0.0, 5.0]

        for f in funcs:
            cf = getattr(m, f.__name__)
            for a in values:
                self.assertEqual(cf(a), f(a), "{0}({1})".format(f.__name__, a))

    def test_binary(self):

        annotate = function(self.Type, a=self.Type, b=self.Type)
        funcs = binary_funcs() + floating_binary_funcs()
        m = module(map(annotate, funcs))

        values = [(0.0, 1.0), (3.0, 2.0), (2.0, 3.0), (-3.0, 2.0), (3.0, -2.0)]

        for f in funcs:
            cf = getattr(m, f.__name__)
            for a, b in values:
                self.assertAlmostEqual(cf(a, b), f(a, b), self.digits,
                                       "{0}({1}, {2})".format(f.__name__, a, b))

    def test_cmp(self):

        annotate = function(Bool, a=self.Type, b=self.Type)
        funcs = cmp_funcs()
        m = module(map(annotate, funcs))

        for f in funcs:
            cf = getattr(m, f.__name__)
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

        @function(self.Type)
        def x():
            a = 5.0
            return a

        m = module([x])
        self.assertEqual(m.x(), 5.0)


def unary_funcs():

    def negate(a):
        return -a

    return [negate]


def integer_unary_funcs():

    def invert(a):
        return ~a

    return [invert]


def binary_funcs():

    def add(a, b):
        return a + b

    def sub(a, b):
        return a - b

    def mul(a, b):
        return a * b

    def div(a, b):
        return a / b

    def mod(a, b):
        return a % b

    return [add, sub, mul, div, mod]


def integer_binary_funcs():

    def bor(a, b):
        return a | b

    def band(a, b):
        return a | b

    def bxor(a, b):
        return a ^ b

    return [bor, band, bxor]


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
