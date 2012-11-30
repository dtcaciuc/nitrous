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

    def test_duplicate_function(self):
        from nitrous.module import module
        from nitrous.function import function
        from nitrous.types import Long

        def get_foo():

            @function(Long, a=Long)
            def foo(a):
                return 1

            return foo

        message = "Duplicate function name: foo"
        with self.assertRaisesRegexp(RuntimeError, message):
            module([get_foo(), get_foo()])


class CppLibraryTests(unittest.TestCase):

    def setUp(self):
        import tempfile

        self.module_1 = tempfile.NamedTemporaryFile(suffix=".c")
        self.module_1.write(MODULE_1)
        self.module_1.flush()
        self.addCleanup(self.module_1.close)

        self.module_2 = tempfile.NamedTemporaryFile(suffix=".c")
        self.module_2.write(MODULE_2)
        self.module_2.flush()
        self.addCleanup(self.module_2.close)

    def test_compile(self):
        from nitrous.module import CppLibrary
        from nitrous.llvm import GetNamedFunction

        sources = [self.module_1.name, self.module_2.name]
        lib = CppLibrary(sources=sources, compile_args=["-O2"])
        m = lib.create_module()

        self.assertTrue(GetNamedFunction(m, "axpy"))
        self.assertTrue(GetNamedFunction(m, "multiply"))
