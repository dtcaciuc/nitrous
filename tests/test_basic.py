import unittest2 as unittest
import nos


class ModuleTest(object):

    def setUp(self):
        self.m = nos.Module(__name__)
        self.addCleanup(self.m.clean)


class AnnotationTests(ModuleTest, unittest.TestCase):

    def setUp(self):
        self.m = nos.Module(__name__)
        self.addCleanup(self.m.clean)

    def test_args_mismatch(self):

        def x(y):
            pass

        error = "Argument type annotations don't match function arguments"
        with self.assertRaisesRegexp(nos.CompilationError, error):
            self.m.function(nos.Double, z=nos.Double)(x)


class EmitterTests(ModuleTest, unittest.TestCase):

    def test_emitter(self):
        """Simple function call."""
        import nos.types

        @self.m.function(nos.types.Double, y=nos.types.Long)
        def x(y):
            return nos.cast(y, nos.types.Double)

        out = self.m.compile()
        rv = out.x(int(5))

        self.assertEqual(rv, 5.0)
        self.assertEqual(type(rv), float)

    def test_emitter_locals(self):
        """Simple function call; check if symbols are imported in outer scope."""
        from nos.types import Double, Long
        from nos import cast

        @self.m.function(Double, y=Long)
        def x(y):
            return cast(y, Double)

        out = self.m.compile()
        rv = out.x(int(5))

        self.assertEqual(rv, 5.0)
        self.assertEqual(type(rv), float)


class LongTests(ModuleTest, unittest.TestCase):

    def test_const(self):
        """Constant declaration."""

        @self.m.function(nos.types.Double)
        def x():
            a = 5.0
            return a

        out = self.m.compile()
        self.assertEqual(out.x(), 5.0)

    def test_add(self):

        @self.m.function(nos.Double, a=nos.Double, b=nos.Double)
        def add(a, b):
            return a + b

        out = self.m.compile()
        self.assertEqual(out.add(3.0, 2.0), 5.0)

    def test_sub(self):

        @self.m.function(nos.Double, a=nos.Double, b=nos.Double)
        def sub(a, b):
            return a - b

        out = self.m.compile()
        self.assertEqual(out.sub(3.0, 2.0), 1.0)

    def test_div(self):

        @self.m.function(nos.Double, a=nos.Double, b=nos.Double)
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


class CastTests(ModuleTest, unittest.TestCase):

    def test_cast(self):
        from nos.types import Long, Double
        from nos import cast

        @self.m.function(Long, a=Long, b=Double)
        def div_long(a, b):
            return a / cast(b, Long)

        @self.m.function(Double, a=Long, b=Double)
        def div_double(a, b):
            return cast(a, Double) / b

        out = self.m.compile()

        self.assertEqual(out.div_long(3, 2), 1)
        self.assertEqual(out.div_double(3, 2), 1.5)

    def test_cast_noop(self):
        from nos.types import Double
        from nos import cast

        @self.m.function(Double, a=Double, b=Double)
        def div_double(a, b):
            return cast(a, Double) / b

        out = self.m.compile()
        self.assertEqual(out.div_double(3, 2), 1.5)


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


class IfTests(ModuleTest, unittest.TestCase):

    def test_if_expr(self):
        from nos.types import Long

        # Simple expression
        @self.m.function(Long, a=Long, b=Long)
        def max2(a, b):
            return a if a > b else b

        # Nested expressions
        @self.m.function(Long, a=Long, b=Long, c=Long)
        def max3(a, b, c):
            return (a if (a > b and a > c) else
                    (b if (b > a and b > c) else
                     (c)))

        out = self.m.compile()

        self.assertEqual(out.max2(2, 3), 3)
        self.assertEqual(out.max2(4, 1), 4)

        self.assertEqual(out.max3(2, 3, 1), 3)
        self.assertEqual(out.max3(4, 1, 5), 5)


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
