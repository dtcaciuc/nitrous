import unittest2 as unittest

from nitrous.exceptions import AnnotationError
from nitrous.types import Bool, Long, Double, Pointer

from nitrous.module import module
from nitrous.function import function


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


class AnnotationTests(unittest.TestCase):

    def test_args_mismatch(self):

        def x(y):
            pass

        error = "Argument type annotations don't match function arguments"
        with self.assertRaisesRegexp(AnnotationError, error):
            function(Double, z=Double)(x)


class SymbolTests(unittest.TestCase):

    def test_unsupported_context(self):
        """Raise error on unsupported context (eg. `del x`)."""

        @function(Long)
        def x():
            y = 1
            del y
            return 0

        message = ">>>     del y"
        with self.assertRaisesRegexp(NotImplementedError, message):
            module([x])

    def test_emitter(self):
        """Simple function call."""
        import nitrous.types
        import nitrous.lib

        @function(nitrous.types.Double, y=nitrous.types.Long)
        def x(y):
            return nitrous.lib.cast(y, nitrous.types.Double)

        m = module([x])
        rv = m.x(int(5))

        self.assertEqual(rv, 5.0)
        self.assertEqual(type(rv), float)

    def test_emitter_locals(self):
        """Simple function call; check if symbols are imported in outer scope."""
        from nitrous.lib import cast

        @function(Double, y=Long)
        def x(y):
            return cast(y, Double)

        m = module([x])
        rv = m.x(int(5))

        self.assertEqual(rv, 5.0)
        self.assertEqual(type(rv), float)


class LoadTests(unittest.TestCase):

    def test_missing_symbol(self):
        """Raise error if cannot resolve a symbol."""

        @function(Double, y=Long)
        def x(y):
            return z

        error = ">>>     return z"
        with self.assertRaisesRegexp(NameError, error):
            module([x])

    def test_symbol_out_of_scope(self):
        """Raise error if symbol is available but not in the current scope."""

        @function(Double, y=Long)
        def x(y):
            for i in range(y):
                z = i
            return z

        error = ">>>     return z"
        with self.assertRaisesRegexp(NameError, error):
            module([x])


class AssignTests(unittest.TestCase):

    def test_unsupported_chain(self):
        """Raise error on chain assignment."""

        @function(Long)
        def f():
            a = b = 1
            return 0

        message = ">>>     a = b = 1"
        with self.assertRaisesRegexp(NotImplementedError, message):
            module([f])

    def test_unsupported_target(self):
        """Check for unsupported assignments."""

        @function(Long, a=Long, b=Long)
        def f(a, b):
            a, b = 1
            return 0

        message = ">>>     a, b = 1"
        with self.assertRaisesRegexp(NotImplementedError, message):
            module([f])

    def test_aug(self):
        """Augmented assignment."""

        @function(Long, a=Long, b=Pointer(Long))
        def f(a, b):
            a += 5
            b[0] += 7
            return a

        m = module([f])

        b = (Long.c_type * 1)(5)
        self.assertEqual(m.f(6, b), 11)
        self.assertEqual(b[0], 12)

    def test_reassign(self):

        @function(Long, a=Long)
        def f(a):
            x = a
            x = x + 5
            return x

        m = module([f])
        self.assertEqual(m.f(10), 15)

    def test_assign_global_const(self):
        """Externally declared values are resolved at compile."""

        y = 5

        @function(Long, a=Long)
        def f(a):
            x = a + y
            return x

        m = module([f])
        self.assertEqual(m.f(10), 15)


class SubscriptTests(unittest.TestCase):

    def test_unsupported_slice(self):
        """Raise error on unsupported context (eg. `del x`)."""

        @function(Long, y=Long)
        def x(y):
            y[:]
            return 0

        message = ">>>     y\[:\]"
        with self.assertRaisesRegexp(NotImplementedError, message):
            module([x])


class IndexTests(unittest.TestCase):

    def test_nd_index(self):
        import ctypes

        @function(Long, y=Pointer(Long, shape=(2, 3, 2)), i=Long, j=Long, k=Long)
        def x(y, i, j, k):
            return y[i, j, k]

        m = module([x])

        dtype = (((ctypes.c_long * 2) * 3) * 2)
        data = dtype(((1, 2), (3, 4), (5, 6)), ((7, 8), (9, 10), (11, 12)))
        c_data = ctypes.cast(data, Pointer(Long).c_type)

        for i in range(2):
            for j in range(3):
                for k in range(2):
                    self.assertEqual(m.x(c_data, i, j, k), data[i][j][k])


class ReturnTests(unittest.TestCase):

    def test_if(self):
        """Return from if/else block"""

        @function(Long, a=Long, b=Long)
        def max2(a, b):
            if a > b:
                return a
            else:
                return b

        m = module([max2])

        self.assertEqual(m.max2(2, 3), 3)
        self.assertEqual(m.max2(4, 1), 4)

    def test_return_comparison(self):
        """Returning comparison (1-bit integer) casts it to Bool type."""

        @function(Bool, a=Long, b=Long)
        def max(a, b):
            return a > b

        m = module([max])

        self.assertEqual(m.max(3, 2), True)
        self.assertEqual(m.max(2, 3), False)

    def test_return_void(self):

        @function()
        def f():
            return

        m = module([f])
        self.assertIsNone(m.f())

    def test_return_implicit_void(self):

        @function()
        def f():
            pass

        m = module([f])
        self.assertIsNone(m.f())

    def test_missing_return(self):
        """Raise error if no return in function with non-void return type."""

        @function(Double)
        def f():
            pass

        message = ">>>     pass"
        with self.assertRaisesRegexp(TypeError, message):
            module([f])

    def test_return_non_void(self):
        """Raise error if void function returns non-void value"""

        @function()
        def f():
            return 5

        message = ">>>     return 5"
        with self.assertRaisesRegexp(ValueError, message):
            module([f])

    def test_unexpected_type(self):
        """Raise error if returning unexpected value type."""

        @function(Double, x=Double)
        def f(x):
            return 5

        message = ">>>     return 5"
        with self.assertRaisesRegexp(TypeError, message):
            module([f])


class ConditionalTests(unittest.TestCase):

    def test_type_mismatch(self):

        @function(Bool, x=Long)
        def f1(x):
            return x < 1.0

        message = ">>>     return x < 1.0"
        with self.assertRaisesRegexp(TypeError, message):
            module([f1])

    def test_compound_test(self):
        """Support compound conditionals such as 1 < x < 2."""

        @function(Bool, x=Long)
        def f1(x):
            return 1 < x < 2

        message = ">>>     return 1 < x < 2"
        with self.assertRaisesRegexp(NotImplementedError, message):
            module([f1])


class IfTests(unittest.TestCase):

    def test_if(self):

        # if clause only
        @function(Long, a=Long, b=Long)
        def max2_1(a, b):
            v = b
            if a > b:
                v = a
            return v

        # if/else clause
        @function(Long, a=Long, b=Long)
        def max2_2(a, b):
            v = 0
            if a > b:
                v = a
            else:
                v = b
            return v

        m = module([max2_1, max2_2])

        for f in [m.max2_1, m.max2_2]:
            self.assertEqual(f(2, 3), 3)
            self.assertEqual(f(4, 1), 4)

    def test_elif_else(self):

        @function(Long, a=Long, b=Long)
        def f(a, b):
            c = 0
            if a > b:
                c = a
            elif a < b:
                c = b
            else:
                c = 0
            return c

        m = module([f])

        self.assertEqual(m.f(2, 3), 3)
        self.assertEqual(m.f(3, 2), 3)
        self.assertEqual(m.f(2, 2), 0)

    def test_elif(self):

        @function(Long, a=Long, b=Long)
        def f(a, b):
            c = 0
            if a > b:
                c = a
            elif a < b:
                c = b
            return c

        m = module([f])

        self.assertEqual(m.f(2, 3), 3)
        self.assertEqual(m.f(3, 2), 3)
        self.assertEqual(m.f(2, 2), 0)

    def test_if_expr(self):

        # Simple expression
        @function(Long, a=Long, b=Long)
        def max2(a, b):
            return a if a > b else b

        # Nested expressions
        @function(Long, a=Long, b=Long, c=Long)
        def max3(a, b, c):
            return (a if (a > b and a > c) else
                    (b if (b > a and b > c) else
                     (c)))

        m = module([max2, max3])

        self.assertEqual(m.max2(2, 3), 3)
        self.assertEqual(m.max2(4, 1), 4)

        self.assertEqual(m.max3(2, 3, 1), 3)
        self.assertEqual(m.max3(4, 1, 5), 5)

    def test_if_expr_type_mismatch(self):
        """Raise error when `if` expression clause types don't match."""

        # Simple expression
        @function(Long, a=Long, b=Long)
        def max2(a, b):
            return 1.0 if a > b else 0

        message = ">>>     return 1.0 if a > b else 0"
        with self.assertRaisesRegexp(TypeError, message):
            module([max2])


class MemoryTests(unittest.TestCase):

    def test_load_element(self):
        import ctypes

        @function(Long, data=Pointer(Long), i=Long)
        def get_i(data, i):
            e = data[i]
            return e

        m = module([get_i])

        dtype = (ctypes.c_long * 5)
        data = dtype(0, 10, 20, 30, 40)

        for i in range(5):
            self.assertEqual(m.get_i(data, i), data[i])

    def test_store_element(self):
        import ctypes

        @function(Long, data=Pointer(Long), i=Long, e=Long)
        def set_i(data, i, e):
            data[i] = e
            return 0

        m = module([set_i])

        dtype = (ctypes.c_long * 5)
        data = dtype(0, 0, 0, 0, 0)

        m.set_i(data, 1, 2)
        m.set_i(data, 2, 5)
        m.set_i(data, 4, 10)

        expected = (0, 2, 5, 0, 10)

        for i in range(5):
            self.assertEqual(data[i], expected[i])


class LoopTests(unittest.TestCase):

    def test_for(self):
        """Simple for loop given range stop value."""
        import ctypes

        @function(Long, data=Pointer(Long), n=Long)
        def loop_1(data, n):
            for i in range(n):
                data[i] = (i if i < 3 else 99)

            return 0

        m = module([loop_1])
        # 5 + 1 elements to check stop correctness
        data = (ctypes.c_long * 6)()

        m.loop_1(data, 5)
        self.assertEqual(list(data), [0, 1, 2, 99, 99, 0])

    def test_for_else(self):
        """for/else clause is not supported."""

        @function(Long, n=Long)
        def loop_1(n):
            for i in range(n):
                pass
            else:
                pass

            return 0

        message = ">>>     for i in range\(n\):"
        with self.assertRaisesRegexp(NotImplementedError, message):
            module([loop_1])

    def test_for_range(self):
        """More advanced loop ranges."""
        import ctypes

        @function(Long, data=Pointer(Long), start=Long, end=Long)
        def loop_1(data, start, end):
            for i in range(start, end):
                data[i] = i

            return 0

        @function(Long, data=Pointer(Long), start=Long, end=Long, step=Long)
        def loop_2(data, start, end, step):
            for i in range(start, end, step):
                data[i] = i

            return 0

        m = module([loop_1, loop_2])

        data = (ctypes.c_long * 8)()
        m.loop_1(data, 2, 7)
        self.assertEqual(list(data), [0, 0, 2, 3, 4, 5, 6, 0])

        data = (ctypes.c_long * 8)()
        m.loop_2(data, 2, 7, 2)
        self.assertEqual(list(data), [0, 0, 2, 0, 4, 0, 6, 0])

    def test_for_break_continue(self):
        """Test for loop with break/continue."""

        @function(Long, n=Long)
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

        m = module([loop_1])
        self.assertEqual(m.loop_1(10), 4)

    def test_double_for(self):
        import ctypes

        @function(Long, data=Pointer(Long), n=Long)
        def loop_1(data, n):
            j = 0
            for i in range(n):
                for j in range(n):
                    data[i * n + j] = i + j

            # Check availability and correctness of
            # loop variables outside the loop.
            return i * j

        m = module([loop_1])
        data = (ctypes.c_long * 9)()
        expected = [0, 1, 2,
                    1, 2, 3,
                    2, 3, 4]

        self.assertEqual(m.loop_1(data, 3), 4)
        self.assertEqual(list(data), expected)

    def test_while(self):
        import ctypes

        @function(Long, data=Pointer(Long), n=Long)
        def loop_1(data, n):
            i = 0
            while i < n:
                data[i] = (i if i < 3 else 99)
                i += 1
            return 0

        m = module([loop_1])

        # 5 + 1 elements to check stop correctness
        data = (ctypes.c_long * 6)()

        m.loop_1(data, 5)
        self.assertEqual(list(data), [0, 1, 2, 99, 99, 0])

    def test_while_break_continue(self):
        """Test while loop with break/continue."""

        @function(Long, n=Long)
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

        m = module([loop_1])
        self.assertEqual(m.loop_1(10), 13)

    def test_double_while(self):
        import ctypes

        @function(data=Pointer(Long), n=Long)
        def loop_1(data, n):
            i = 0
            while i < n:
                j = 0
                while j < n:
                    data[i * n + j] = i + j
                    j += 1
                i += 1

        m = module([loop_1])
        data = (ctypes.c_long * 9)()
        expected = [0, 1, 2,
                    1, 2, 3,
                    2, 3, 4]

        m.loop_1(data, 3)
        self.assertEqual(list(data), expected)


class CallTests(unittest.TestCase):

    def test_call(self):
        """Calling one compiled function from another."""

        @function(Long, x=Long)
        def f1(x):
            return x * x + 1

        @function(Long, x=Long)
        def f2(x):
            return x + f1(x)

        m = module([f2])
        self.assertEqual(m.f2(2), 7)

    def test_call_wrong_arg_count(self):

        @function(Long, x=Long)
        def f1(x):
            return x

        @function(Long, x=Long)
        def f2(x):
            return f1(x, 1)

        message = "f1\(\) takes exactly 1 argument\(s\) \(2 given\)"
        with self.assertRaisesRegexp(TypeError, message):
            module([f2])

    def test_call_wrong_arg_type(self):

        @function(Long, x=Long)
        def f1(x):
            return x

        @function(Long, x=Long)
        def f2(x):
            return f1(1.0)

        message = "f1\(\) called with wrong argument type\(s\) for x"
        with self.assertRaisesRegexp(TypeError, message):
            module([f2])


# FIXME restore
# class ExternalCallTests(unittest.TestCase):
# 
#     def setUp(self):
#         from distutils.ccompiler import new_compiler
#         import tempfile
#         import shutil
# 
#         self.libdir = tempfile.mkdtemp()
#         compiler = new_compiler()
# 
#         with tempfile.NamedTemporaryFile(suffix=".c", dir=self.libdir, delete=False) as src:
#             src.write("#include <math.h>\ndouble my_pow(double x, double y) { return pow(x, y); }\n")
# 
#         obj = compiler.compile([src.name], extra_preargs=["-fPIC"], output_dir=self.libdir)
#         compiler.create_static_lib(obj, "foo", output_dir=self.libdir)
# 
#         self.addCleanup(shutil.rmtree, self.libdir, ignore_errors=True)
# 
#     def test_shlib(self):
#         """Calling functions from arbitrary shared libraries."""
# 
#         # Test call to functions in LLVM library itself
#         lib_args = dict(lib="foo", libdir=self.libdir)
#         my_pow = self.m.include_function("my_pow", Double, [Double, Double], **lib_args)
# 
#         @self.m.function(Double, x=Double, y=Double)
#         def wrapper(x, y):
#             return my_pow(x, y)
# 
#         self.m.build()
#         self.assertEqual(wrapper(3, 5), 3 ** 5)


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


class OptimizationTests(unittest.TestCase):

    def test_branch_elimination(self):
        from nitrous.module import dump

        add_5 = False
        add_any = True

        @function(Long, a=Long, b=Bool)
        def f1(a, b):
            if add_any and b:
                a += 5
            return a

        @function(Long, a=Long)
        def f2(a):
            if add_any and add_5:
                a += 5
            return a

        m1 = module([f1])
        ir = " ".join(dump(m1).split("\n"))
        # In first function, conditional depends on a parameter
        self.assertRegexpMatches(ir, "icmp")

        m2 = module([f2])
        ir = " ".join(dump(m2).split("\n"))
        # In second, entire conditional is resolved at
        # compile time and optimized away
        self.assertNotRegexpMatches(ir, "icmp")


class UnpackTests(unittest.TestCase):

    def test_unpack(self):

        @function(Long, a=Long, b=Long)
        def foo(a, b):
            b, a = a, b
            return b * 10 + a * 100

        m = module([foo])
        self.assertEqual(m.foo(5, 6), 650)

    def test_shape_mismatch(self):
        """Raise error if packed/unpacked tuple lengths differ"""

        @function(Long, a=Long, b=Long)
        def foo(a, b):
            b, = a, b

        message = "Cannot unpack 2 values into 1"
        with self.assertRaisesRegexp(ValueError, message):
            module([foo])


class InlineTests(unittest.TestCase):

    def test(self):
        from nitrous.module import dump
        from nitrous.function import options

        @options(inline=True)
        @function(Long, a=Long, b=Long)
        def foo(a, b):
            return a + b

        m = module([foo])
        self.assertRegexpMatches(dump(m), "alwaysinline")


class JITTests(unittest.TestCase):

    def test(self):
        from nitrous.module import jit_module
        from nitrous.function import function

        @function(Long, a=Long, b=Long)
        def add(a, b):
            return a + b

        m = jit_module([add])

        self.assertEqual(m.add(3, 7), 10)
