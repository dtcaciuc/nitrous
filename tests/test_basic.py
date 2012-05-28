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


class TestA(ModuleTest, unittest.TestCase):

    def test_axpy(self):

        @self.m.function(nos.Double, a=nos.Double, x=nos.Double, y=nos.Double)
        def axpy(a, x, y):
            return (a * x) + y

        out = self.m.compile()
        self.assertEqual(out.axpy(2.0, 3.0, 5.0), 11.0)

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


class LongTest(ModuleTest, unittest.TestCase):

    def test_const(self):
        """Constant declaration."""

        @self.m.function(nos.types.Long)
        def x():
            a = 5
            return a

        out = self.m.compile()
        self.assertEqual(out.x(), 5)

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


class DoubleTest(ModuleTest, unittest.TestCase):

    def test_const(self):
        """Constant declaration."""

        @self.m.function(nos.types.Double)
        def x():
            a = 5.0
            return a

        out = self.m.compile()
        self.assertEqual(out.x(), 5.0)

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


class OpTest(ModuleTest, unittest.TestCase):
    pass

    # TODO def test_mismatch_operand_type(self):
