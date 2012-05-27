import unittest2 as unittest
import nos


class AnnotationTests(unittest.TestCase):

    def setUp(self):
        self.m = nos.Module(__name__)
        self.addCleanup(self.m.clean)

    def test_args_mismatch(self):

        def x(y):
            pass

        error = "Argument type annotations don't match function arguments"
        with self.assertRaisesRegexp(nos.CompilationError, error):
            self.m.function(nos.Double, z=nos.Double)(x)


class TestA(unittest.TestCase):

    def setUp(self):
        self.m = nos.Module(__name__)
        self.addCleanup(self.m.clean)

    def test_add_sub(self):
        """Basic add/subtract; two functions per module."""

        @self.m.function(nos.Double, a=nos.Double, b=nos.Double)
        def add(a, b):
            return a + b

        @self.m.function(nos.Double, a=nos.Double, b=nos.Double)
        def sub(a, b):
            return a - b

        out = self.m.compile()
        self.assertEqual(out.add(3.0, 2.0), 5.0)
        self.assertEqual(out.sub(3.0, 2.0), 1.0)

    def test_axpy(self):

        @self.m.function(nos.Double, a=nos.Double, x=nos.Double, y=nos.Double)
        def axpy(a, x, y):
            return (a * x) + y

        out = self.m.compile()
        self.assertEqual(out.axpy(2.0, 3.0, 5.0), 11.0)

    def test_emitter(self):
        """Simple function call."""
        import nos.types

        @self.m.function(nos.types.Double, y=nos.types.Int64)
        def x(y):
            return nos.cast(y, nos.types.Double)

        out = self.m.compile()
        rv = out.x(int(5))

        self.assertEqual(rv, 5.0)
        self.assertEqual(type(rv), float)

    def test_emitter_locals(self):
        """Simple function call; check if symbols are imported in outer scope."""
        from nos.types import Double, Int64
        from nos import cast

        @self.m.function(Double, y=Int64)
        def x(y):
            return cast(y, Double)

        out = self.m.compile()
        rv = out.x(int(5))

        self.assertEqual(rv, 5.0)
        self.assertEqual(type(rv), float)
