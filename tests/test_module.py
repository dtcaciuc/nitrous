from nitrous.module import module
from nitrous.function import function
from nitrous.types import Long

import unittest


MODULE_1 = """

float multiply(float a, float p);

float axpy(float a, float p, float y) {
    return multiply(a, p) + y;
}

"""

MODULE_2 = """

float multiply(float a, float b) {
    return a * b;
}

"""


class ModuleTests(unittest.TestCase):

    def test_exposed(self):
        """Interface only functions from decls given to module constructor."""

        @function(Long, a=Long)
        def add1(a):
            return a + 1

        @function(Long, a=Long)
        def add2(a):
            return add1(add1(a))

        @function(Long, a=Long)
        def add3(a):
            return add1(add2(a))

        m = module([add1, add3])

        self.assertEqual(m.add1(5), 6)
        self.assertEqual(m.add3(11), 14)

        self.assertFalse(hasattr(m, "add2"))

    def test_duplicate_function(self):

        def get_foo():

            @function(Long, a=Long)
            def foo(a):
                return 1

            return foo

        message = "Duplicate function name: foo"
        with self.assertRaisesRegexp(RuntimeError, message):
            module([get_foo(), get_foo()])


class JITTests(unittest.TestCase):

    def test(self):
        from nitrous.module import jit_module

        @function(Long, a=Long, b=Long)
        def add(a, b):
            return a + b

        m = jit_module([add])

        self.assertEqual(m.add(3, 7), 10)


class CppLibraryTests(unittest.TestCase):

    def setUp(self):
        from nitrous.module import CppLibrary
        from tempfile import NamedTemporaryFile

        self.module_1 = NamedTemporaryFile(suffix=".c")
        self.module_1.write(MODULE_1)
        self.module_1.flush()
        self.addCleanup(self.module_1.close)

        self.module_2 = NamedTemporaryFile(suffix=".c")
        self.module_2.write(MODULE_2)
        self.module_2.flush()
        self.addCleanup(self.module_2.close)

        sources = [self.module_1.name, self.module_2.name]
        self.lib = CppLibrary(sources=sources, compile_args=["-O2"])

    def test_compile(self):
        from nitrous.llvm import GetNamedFunction

        m = self.lib.create_module()

        self.assertTrue(GetNamedFunction(m, "axpy"))
        self.assertTrue(GetNamedFunction(m, "multiply"))

    def test_run(self):
        from nitrous.module import jit_module, so_module
        from nitrous.function import function, c_function
        from nitrous.types import Float

        axpy = c_function("axpy", Float, [Float, Float, Float])
        multiply = c_function("multiply", Float, [Float, Float])

        @function(Float, a=Float, b=Float, c=Float)
        def foo(a, b, c):
            return axpy(a, b, c) + multiply(a, c)

        a, b, c = 11, 23, 7
        ref = (a * b + c) + a * c

        jit_m = jit_module([foo], libs=[self.lib])
        self.assertEqual(jit_m.foo(a, b, c), ref)

        so_m = so_module([foo], libs=[self.lib])
        self.assertEqual(so_m.foo(a, b, c), ref)
