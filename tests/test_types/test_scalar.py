import unittest2 as unittest
import nos

from nos.util import ModuleTest


class BoolTests(ModuleTest, unittest.TestCase):

    def test_and(self):
        from nos.types import Bool

        @self.m.function(Bool, a=Bool, b=Bool)
        def and_(a, b):
            return a and b

        out = self.m.compile()

        self.assertEqual(out.and_(True, True), True)
        self.assertEqual(out.and_(True, False), False)
        self.assertEqual(out.and_(False, True), False)
        self.assertEqual(out.and_(False, False), False)

    def test_or(self):
        from nos.types import Bool

        @self.m.function(Bool, a=Bool, b=Bool)
        def or_(a, b):
            return a or b

        out = self.m.compile()

        self.assertEqual(out.or_(True, True), True)
        self.assertEqual(out.or_(True, False), True)
        self.assertEqual(out.or_(False, True), True)
        self.assertEqual(out.or_(False, False), False)


class LongTests(ModuleTest, unittest.TestCase):

    def test_const(self):
        """Constant declaration."""
        from nos.types import Double

        @self.m.function(Double)
        def x():
            a = 5.0
            return a

        out = self.m.compile()
        self.assertEqual(out.x(), 5.0)

    def test_add(self):
        from nos.types import Double

        @self.m.function(Double, a=Double, b=Double)
        def add(a, b):
            return a + b

        out = self.m.compile()
        self.assertEqual(out.add(3.0, 2.0), 5.0)

    def test_sub(self):
        from nos.types import Double

        @self.m.function(Double, a=Double, b=Double)
        def sub(a, b):
            return a - b

        out = self.m.compile()
        self.assertEqual(out.sub(3.0, 2.0), 1.0)

    def test_div(self):
        from nos.types import Double

        @self.m.function(Double, a=Double, b=Double)
        def div(a, b):
            return a / b

        out = self.m.compile()
        self.assertEqual(out.div(3.0, 2.0), 1.5)

    def test_cmp(self):
        from nos.types import Bool, Long

        annotate = self.m.function(Bool, a=Long, b=Long)
        funcs = [annotate(f) for f in create_compare_funcs()]
        out = self.m.compile()

        for f in funcs:
            cf = getattr(out, f.func_name)
            self.assertEqual(cf(6, 5), f(6, 5))
            self.assertEqual(cf(5, 5), f(5, 5))
            self.assertEqual(cf(5, 6), f(5, 6))


class DoubleTests(ModuleTest, unittest.TestCase):

    def test_const(self):
        """Constant declaration."""

        @self.m.function(nos.types.Long)
        def x():
            a = 5
            return a

        out = self.m.compile()
        self.assertEqual(out.x(), 5)

    def test_add(self):
        from nos.types import Long

        @self.m.function(Long, a=Long, b=Long)
        def add(a, b):
            return a + b

        out = self.m.compile()
        self.assertEqual(out.add(3, 2), 5)

    def test_sub(self):
        from nos.types import Long

        @self.m.function(Long, a=Long, b=Long)
        def sub(a, b):
            return a - b

        out = self.m.compile()
        self.assertEqual(out.sub(3, 2), 1)
        self.assertEqual(out.sub(3, 5), -2)

    def test_div(self):
        from nos.types import Long

        @self.m.function(Long, a=Long, b=Long)
        def div(a, b):
            return a / b

        out = self.m.compile()
        self.assertEqual(out.div(3, 2), 1)
        self.assertEqual(out.div(3, -2), -1)

    def test_cmp(self):
        from nos.types import Bool, Double
        annotate = self.m.function(Bool, a=Double, b=Double)
        funcs = [annotate(f) for f in create_compare_funcs()]
        out = self.m.compile()

        for f in funcs:
            cf = getattr(out, f.func_name)
            self.assertEqual(cf(6.0, 5.0), f(6.0, 5.0))
            self.assertEqual(cf(5.0, 5.0), f(5.0, 5.0))
            self.assertEqual(cf(5.0, 6.0), f(5.0, 6.0))



def create_compare_funcs():

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
