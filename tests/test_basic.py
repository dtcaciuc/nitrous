import unittest2 as unittest

from nitrous.exceptions import AnnotationError
from nitrous.types import Bool, Long, Double, Pointer
from nitrous.util import ModuleTest


class AnnotationTests(ModuleTest, unittest.TestCase):

    def test_args_mismatch(self):

        def x(y):
            pass

        error = "Argument type annotations don't match function arguments"
        with self.assertRaisesRegexp(AnnotationError, error):
            self.m.function(Double, z=Double)(x)


class SymbolTests(ModuleTest, unittest.TestCase):

    def test_unsupported_context(self):
        """Raise error on unsupported context (eg. `del x`)."""

        @self.m.function(Long)
        def x():
            y = 1
            del y
            return 0

        message = ">>>     del y"
        with self.assertRaisesRegexp(NotImplementedError, message):
            self.m.build()

    def test_emitter(self):
        """Simple function call."""
        import nitrous.types
        import nitrous.lib

        @self.m.function(nitrous.types.Double, y=nitrous.types.Long)
        def x(y):
            return nitrous.lib.cast(y, nitrous.types.Double)

        out = self.m.build()
        rv = out.x(int(5))

        self.assertEqual(rv, 5.0)
        self.assertEqual(type(rv), float)

    def test_emitter_locals(self):
        """Simple function call; check if symbols are imported in outer scope."""
        from nitrous.lib import cast

        @self.m.function(Double, y=Long)
        def x(y):
            return cast(y, Double)

        out = self.m.build()
        rv = out.x(int(5))

        self.assertEqual(rv, 5.0)
        self.assertEqual(type(rv), float)


class LoadTests(ModuleTest, unittest.TestCase):

    def test_missing_symbol(self):
        """Raise error if cannot resolve a symbol."""

        @self.m.function(Double, y=Long)
        def x(y):
            return z

        error = ">>>     return z"
        with self.assertRaisesRegexp(NameError, error):
            self.m.build()

    def test_symbol_out_of_scope(self):
        """Raise error if symbol is available but not in the current scope."""

        @self.m.function(Double, y=Long)
        def x(y):
            for i in range(y):
                z = i
            return z

        error = ">>>     return z"
        with self.assertRaisesRegexp(NameError, error):
            self.m.build()


class AssignTests(ModuleTest, unittest.TestCase):

    def test_unsupported_chain(self):
        """Raise error on chain assignment."""

        @self.m.function(Long)
        def f():
            a = b = 1
            return 0

        message = ">>>     a = b = 1"
        with self.assertRaisesRegexp(NotImplementedError, message):
            self.m.build()

    def test_unsupported_target(self):
        """Check for unsupported assignments."""

        @self.m.function(Long, a=Long, b=Long)
        def f(a, b):
            a, b = 1
            return 0

        message = ">>>     a, b = 1"
        with self.assertRaisesRegexp(NotImplementedError, message):
            self.m.build()

    def test_aug(self):
        """Augmented assignment."""

        @self.m.function(Long, a=Long, b=Pointer(Long))
        def f(a, b):
            a += 5
            b[0] += 7
            return a

        out = self.m.build()

        b = (Long.c_type * 1)(5)
        self.assertEqual(f(6, b), 11)
        self.assertEqual(b[0], 12)

        b = (Long.c_type * 1)(5)
        self.assertEqual(out.f(6, b), 11)
        self.assertEqual(b[0], 12)

    def test_reassign(self):

        @self.m.function(Long, a=Long)
        def f(a):
            x = a
            x = x + 5
            return x

        out = self.m.build()

        self.assertEqual(f(10), 15)
        self.assertEqual(out.f(10), 15)

    def test_assign_global_const(self):
        """Externally declared values are resolved at compile."""

        y = 5

        @self.m.function(Long, a=Long)
        def f(a):
            x = a + y
            return x

        out = self.m.build()

        self.assertEqual(f(10), 15)
        self.assertEqual(out.f(10), 15)


class SubscriptTests(ModuleTest, unittest.TestCase):

    def test_unsupported_slice(self):
        """Raise error on unsupported context (eg. `del x`)."""

        @self.m.function(Long, y=Long)
        def x(y):
            y[:]
            return 0

        message = ">>>     y\[:\]"
        with self.assertRaisesRegexp(NotImplementedError, message):
            self.m.build()


class IndexTests(ModuleTest, unittest.TestCase):

    def test_nd_index(self):
        import ctypes

        @self.m.function(Long, y=Pointer(Long, shape=(2, 3, 2)), i=Long, j=Long, k=Long)
        def x(y, i, j, k):
            return y[i, j, k]

        out = self.m.build()

        dtype = (((ctypes.c_long * 2) * 3) * 2)
        data = dtype(((1, 2), (3, 4), (5, 6)), ((7, 8), (9, 10), (11, 12)))
        c_data = ctypes.cast(data, Pointer(Long).c_type)

        for i in range(2):
            for j in range(3):
                for k in range(2):
                    self.assertEqual(out.x(c_data, i, j, k), data[i][j][k])


class ReturnTests(ModuleTest, unittest.TestCase):

    def test_if(self):
        """Return from if/else block"""

        @self.m.function(Long, a=Long, b=Long)
        def max2(a, b):
            if a > b:
                return a
            else:
                return b

        out = self.m.build()

        self.assertEqual(out.max2(2, 3), 3)
        self.assertEqual(out.max2(4, 1), 4)

    def test_return_comparison(self):
        """Returning comparison (1-bit integer) casts it to Bool type."""

        @self.m.function(Bool, a=Long, b=Long)
        def max(a, b):
            return a > b

        out = self.m.build()

        self.assertEqual(out.max(3, 2), True)
        self.assertEqual(out.max(2, 3), False)

    def test_return_void(self):

        @self.m.function()
        def f():
            return

        out = self.m.build()
        self.assertIsNone(out.f())

    def test_return_implicit_void(self):

        @self.m.function()
        def f():
            pass

        out = self.m.build()
        self.assertIsNone(out.f())

    def test_missing_return(self):
        """Raise error if no return in function with non-void return type."""

        @self.m.function(Double)
        def f():
            pass

        message = ">>>     pass"
        with self.assertRaisesRegexp(TypeError, message):
            self.m.build()

    def test_return_non_void(self):
        """Raise error if void function returns non-void value"""

        @self.m.function()
        def f():
            return 5

        message = ">>>     return 5"
        with self.assertRaisesRegexp(ValueError, message):
            self.m.build()

    def test_unexpected_type(self):
        """Raise error if returning unexpected value type."""

        @self.m.function(Double, x=Double)
        def f(x):
            return 5

        message = ">>>     return 5"
        with self.assertRaisesRegexp(TypeError, message):
            self.m.build()


class ConditionalTests(ModuleTest, unittest.TestCase):

    def test_type_mismatch(self):

        @self.m.function(Bool, x=Long)
        def f1(x):
            return x < 1.0

        message = ">>>     return x < 1.0"
        with self.assertRaisesRegexp(TypeError, message):
            self.m.build()

    def test_compound_test(self):
        """Support compound conditionals such as 1 < x < 2."""

        @self.m.function(Bool, x=Long)
        def f1(x):
            return 1 < x < 2

        message = ">>>     return 1 < x < 2"
        with self.assertRaisesRegexp(NotImplementedError, message):
            self.m.build()


class IfTests(ModuleTest, unittest.TestCase):

    def test_if(self):

        # if clause only
        @self.m.function(Long, a=Long, b=Long)
        def max2_1(a, b):
            v = b
            if a > b:
                v = a
            return v

        # if/else clause
        @self.m.function(Long, a=Long, b=Long)
        def max2_2(a, b):
            v = 0
            if a > b:
                v = a
            else:
                v = b
            return v

        out = self.m.build()

        for f in [out.max2_1, out.max2_2]:
            self.assertEqual(f(2, 3), 3)
            self.assertEqual(f(4, 1), 4)

    def test_elif_else(self):

        @self.m.function(Long, a=Long, b=Long)
        def f(a, b):
            c = 0
            if a > b:
                c = a
            elif a < b:
                c = b
            else:
                c = 0
            return c

        out = self.m.build()

        self.assertEqual(out.f(2, 3), 3)
        self.assertEqual(out.f(3, 2), 3)
        self.assertEqual(out.f(2, 2), 0)

    def test_elif(self):

        @self.m.function(Long, a=Long, b=Long)
        def f(a, b):
            c = 0
            if a > b:
                c = a
            elif a < b:
                c = b
            return c

        out = self.m.build()

        self.assertEqual(out.f(2, 3), 3)
        self.assertEqual(out.f(3, 2), 3)
        self.assertEqual(out.f(2, 2), 0)

    def test_if_expr(self):

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

        out = self.m.build()

        self.assertEqual(out.max2(2, 3), 3)
        self.assertEqual(out.max2(4, 1), 4)

        self.assertEqual(out.max3(2, 3, 1), 3)
        self.assertEqual(out.max3(4, 1, 5), 5)

    def test_if_expr_type_mismatch(self):
        """Raise error when `if` expression clause types don't match."""

        # Simple expression
        @self.m.function(Long, a=Long, b=Long)
        def max2(a, b):
            return 1.0 if a > b else 0

        message = ">>>     return 1.0 if a > b else 0"
        with self.assertRaisesRegexp(TypeError, message):
            self.m.build()


class MemoryTests(ModuleTest, unittest.TestCase):

    def test_load_element(self):
        import ctypes

        @self.m.function(Long, data=Pointer(Long), i=Long)
        def get_i(data, i):
            e = data[i]
            return e

        out = self.m.build()

        dtype = (ctypes.c_long * 5)
        data = dtype(0, 10, 20, 30, 40)

        for i in range(5):
            self.assertEqual(out.get_i(data, i), data[i])

    def test_store_element(self):
        import ctypes

        @self.m.function(Long, data=Pointer(Long), i=Long, e=Long)
        def set_i(data, i, e):
            data[i] = e
            return 0

        out = self.m.build()

        dtype = (ctypes.c_long * 5)
        data = dtype(0, 0, 0, 0, 0)

        out.set_i(data, 1, 2)
        out.set_i(data, 2, 5)
        out.set_i(data, 4, 10)

        expected = (0, 2, 5, 0, 10)

        for i in range(5):
            self.assertEqual(data[i], expected[i])


class LoopTests(ModuleTest, unittest.TestCase):

    def test_for(self):
        """Simple for loop given range stop value."""
        import ctypes

        @self.m.function(Long, data=Pointer(Long), n=Long)
        def loop_1(data, n):
            for i in range(n):
                data[i] = (i if i < 3 else 99)

            return 0

        out = self.m.build()
        # 5 + 1 elements to check stop correctness
        data = (ctypes.c_long * 6)()

        out.loop_1(data, 5)
        self.assertEqual(list(data), [0, 1, 2, 99, 99, 0])

    def test_for_else(self):
        """for/else clause is not supported."""

        @self.m.function(Long, n=Long)
        def loop_1(n):
            for i in range(n):
                pass
            else:
                pass

            return 0

        message = ">>>     for i in range\(n\):"
        with self.assertRaisesRegexp(NotImplementedError, message):
            self.m.build()

    def test_for_range(self):
        """More advanced loop ranges."""
        import ctypes

        @self.m.function(Long, data=Pointer(Long), start=Long, end=Long)
        def loop_1(data, start, end):
            for i in range(start, end):
                data[i] = i

            return 0

        @self.m.function(Long, data=Pointer(Long), start=Long, end=Long, step=Long)
        def loop_2(data, start, end, step):
            for i in range(start, end, step):
                data[i] = i

            return 0

        out = self.m.build()

        data = (ctypes.c_long * 8)()
        out.loop_1(data, 2, 7)
        self.assertEqual(list(data), [0, 0, 2, 3, 4, 5, 6, 0])

        data = (ctypes.c_long * 8)()
        out.loop_2(data, 2, 7, 2)
        self.assertEqual(list(data), [0, 0, 2, 0, 4, 0, 6, 0])

    def test_for_break_continue(self):
        """Test for loop with break/continue."""

        @self.m.function(Long, n=Long)
        def loop_1(n):
            a = 0
            for i in range(n):
                if i < 3:
                    continue
                elif i == 7:
                    break
                else:
                    a = a + 1

            return a

        out = self.m.build()
        self.assertEqual(out.loop_1(10), 4)

    def test_double_for(self):
        import ctypes

        @self.m.function(Long, data=Pointer(Long), n=Long)
        def loop_1(data, n):
            j = 0
            for i in range(n):
                for j in range(n):
                    data[i * n + j] = i + j

            # Check availability and correctness of
            # loop variables outside the loop.
            return i * j

        out = self.m.build()
        data = (ctypes.c_long * 9)()
        expected = [0, 1, 2,
                    1, 2, 3,
                    2, 3, 4]

        self.assertEqual(loop_1(data, 3), 4)
        self.assertEqual(list(data), expected)

        self.assertEqual(out.loop_1(data, 3), 4)
        self.assertEqual(list(data), expected)

    def test_while(self):
        import ctypes
        from nitrous.module import dump

        @self.m.function(Long, data=Pointer(Long), n=Long)
        def loop_1(data, n):
            i = 0
            while i < n:
                data[i] = (i if i < 3 else 99)
                i += 1
            return 0

        out = self.m.build()

        # 5 + 1 elements to check stop correctness
        data = (ctypes.c_long * 6)()

        out.loop_1(data, 5)
        self.assertEqual(list(data), [0, 1, 2, 99, 99, 0])

    def test_while_break_continue(self):
        """Test while loop with break/continue."""

        @self.m.function(Long, n=Long)
        def loop_1(n):
            a = 0
            i = 0
            while i < n:
                if i < 3:
                    a += i * 3
                    i += 1
                    continue
                elif i == 7:
                    break
                else:
                    a = a + 1
                i += 1

            return a

        out = self.m.build()
        self.assertEqual(out.loop_1(10), 13)

    def test_double_while(self):
        import ctypes

        @self.m.function(data=Pointer(Long), n=Long)
        def loop_1(data, n):
            i = 0
            while i < n:
                j = 0
                while j < n:
                    data[i * n + j] = i + j
                    j += 1
                i += 1

        out = self.m.build()
        data = (ctypes.c_long * 9)()
        expected = [0, 1, 2,
                    1, 2, 3,
                    2, 3, 4]

        loop_1(data, 3)
        self.assertEqual(list(data), expected)

        out.loop_1(data, 3)
        self.assertEqual(list(data), expected)


class CallTests(ModuleTest, unittest.TestCase):

    def test_call(self):
        """Calling one compiled function from another."""

        @self.m.function(Long, x=Long)
        def f1(x):
            return x * x + 1

        @self.m.function(Long, x=Long)
        def f2(x):
            return x + f1(x)

        out = self.m.build()

        self.assertEqual(f2(2), 7)
        self.assertEqual(out.f2(2), 7)

    def test_call_wrong_arg_count(self):

        @self.m.function(Long, x=Long)
        def f1(x):
            return x

        @self.m.function(Long, x=Long)
        def f2(x):
            return f1(x, 1)

        message = "f1\(\) takes exactly 1 argument\(s\) \(2 given\)"
        with self.assertRaisesRegexp(TypeError, message):
            self.m.build()

    def test_call_wrong_arg_type(self):

        @self.m.function(Long, x=Long)
        def f1(x):
            return x

        @self.m.function(Long, x=Long)
        def f2(x):
            return f1(1.0)

        message = "f1\(\) called with wrong argument type\(s\) for x"
        with self.assertRaisesRegexp(TypeError, message):
            self.m.build()


class ExternalCallTests(ModuleTest, unittest.TestCase):

    def setUp(self):
        from distutils.ccompiler import new_compiler
        import tempfile
        import shutil

        super(ExternalCallTests, self).setUp()

        self.libdir = tempfile.mkdtemp()
        compiler = new_compiler()

        with tempfile.NamedTemporaryFile(suffix=".c", dir=self.libdir, delete=False) as src:
            src.write("#include <math.h>\ndouble my_pow(double x, double y) { return pow(x, y); }\n")

        obj = compiler.compile([src.name], extra_preargs=["-fPIC"], output_dir=self.libdir)
        compiler.create_static_lib(obj, "foo", output_dir=self.libdir)

        self.addCleanup(shutil.rmtree, self.libdir, ignore_errors=True)

    def test_shlib(self):
        """Calling functions from arbitrary shared libraries."""

        # Test call to functions in LLVM library itself
        lib_args = dict(lib="foo", libdir=self.libdir)
        my_pow = self.m.include_function("my_pow", Double, [Double, Double], **lib_args)

        @self.m.function(Double, x=Double, y=Double)
        def wrapper(x, y):
            return my_pow(x, y)

        out = self.m.build()
        self.assertEqual(out.wrapper(3, 5), 3 ** 5)


class ScopedVarsTests(unittest.TestCase):

    def test_scope(self):
        from nitrous.function import ScopedVars

        # Topmost local scope.
        v = ScopedVars()
        v["a"] = 1

        self.assertEqual(v["a"], 1)
        with self.assertRaises(KeyError):
            v["b"]

        # Adding nested scopes
        with v.scope():
            v["a"] = 2
            v["b"] = 3

            with v.scope():
                v["c"] = 4
                self.assertEqual(v["c"], 4)

                # Should try most nested scope first
                self.assertEqual(v["a"], 2)
                self.assertEqual(v["b"], 3)

            # Scope ended; variable c should disappear
            with self.assertRaises(KeyError):
                v["c"]

        # Scope ended; a should return to its old value
        self.assertEqual(v["a"], 1)
        with self.assertRaises(KeyError):
            v["b"]


class OptimizationTests(ModuleTest, unittest.TestCase):

    def test_branch_elimination(self):
        from nitrous.module import dump

        add_5 = False
        add_any = True

        @self.m.function(Long, a=Long, b=Bool)
        def f1(a, b):
            if add_any and b:
                a += 5
            return a

        @self.m.function(Long, a=Long)
        def f2(a):
            if add_any and add_5:
                a += 5
            return a

        out = self.m.build()
        ir = " ".join(dump(out).split("\n"))

        # In first function, conditional depends on a parameter
        self.assertRegexpMatches(ir, "f1.+\{.+icmp.+\}")

        # In second, entire conditional is resolved at
        # compile time and optimized away
        self.assertNotRegexpMatches(ir, "f2.+\{.+icmp.+\}")


class UnpackTests(ModuleTest, unittest.TestCase):

    def test_unpack(self):

        @self.m.function(Long, a=Long, b=Long)
        def foo(a, b):
            b, a = a, b
            return b * 10 + a * 100

        out = self.m.build()
        self.assertEqual(out.foo(5, 6), 650)

    def test_shape_mismatch(self):
        """Raise error if packed/unpacked tuple lengths differ"""

        @self.m.function(Long, a=Long, b=Long)
        def foo(a, b):
            b, = a, b

        message = "Cannot unpack 2 values into 1"
        with self.assertRaisesRegexp(ValueError, message):
            self.m.build()


class InlineTests(ModuleTest, unittest.TestCase):

    def test(self):
        from nitrous.module import dump

        @self.m.options(inline=True)
        @self.m.function(Long, a=Long, b=Long)
        def foo(a, b):
            return a + b

        out = self.m.build()
        self.assertRegexpMatches(dump(out), "alwaysinline")


class JITTests(unittest.TestCase):

    def test(self):
        from nitrous.module import Module, build_jit

        m = Module("X", backend=build_jit)

        @m.function(Long, a=Long, b=Long)
        def add(a, b):
            return a + b

        out = m.build()
        self.assertEqual(out.add(3, 7), 10)
