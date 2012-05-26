import unittest2 as unittest
import nos


class AnnotationTests(unittest.TestCase):

    def setUp(self):
        self.m = nos.Module(__name__)
        self.addCleanup(self.m.clean)

    def test_missing_result(self):
        @self.m.args(y=nos.Double())
        def x(y):
            pass

        error = "Forgot to annotate x result type"
        with self.assertRaisesRegexp(nos.CompilationError, error):
            self.m.compiled()(x)

    def test_missing_args(self):
        @self.m.result(nos.Double())
        def x(y):
            pass

        error = "Forgot to annotate x arguments"
        with self.assertRaisesRegexp(nos.CompilationError, error):
            self.m.compiled()(x)

    def test_args_mismatch(self):

        @self.m.args(z=nos.Double())
        @self.m.result(nos.Double())
        def x(y):
            pass

        error = "Argument type annotations don't match function arguments"
        with self.assertRaisesRegexp(nos.CompilationError, error):
            self.m.compiled()(x)


class TestA(unittest.TestCase):

    def setUp(self):
        self.m = nos.Module(__name__)
        self.addCleanup(self.m.clean)

    def test_add_sub(self):
        """Basic add/subtract; two functions per module."""

        @self.m.compiled()
        @self.m.args(a=nos.Double(), b=nos.Double())
        @self.m.result(nos.Double())
        def add(a, b):
            return a + b

        @self.m.compiled()
        @self.m.args(a=nos.Double(), b=nos.Double())
        @self.m.result(nos.Double())
        def sub(a, b):
            return a - b

        out = self.m.compile()
        self.assertEqual(out.add(3.0, 2.0), 5.0)
        self.assertEqual(out.sub(3.0, 2.0), 1.0)

    def test_axpy(self):

        @self.m.compiled()
        @self.m.args(a=nos.Double(), x=nos.Double(), y=nos.Double())
        @self.m.result(nos.Double())
        def axpy(a, x, y):
            return (a * x) + y

        out = self.m.compile()
        self.assertEqual(out.axpy(2.0, 3.0, 5.0), 11.0)
