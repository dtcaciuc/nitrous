import unittest2 as unittest
import nos

from nos.util import ModuleTest


class AnnotationTests(ModuleTest, unittest.TestCase):

    def setUp(self):
        self.m = nos.Module(__name__)
        self.addCleanup(self.m.clean)

    def test_args_mismatch(self):
        from nos.exceptions import CompilationError
        from nos.types import Double

        def x(y):
            pass

        error = "Argument type annotations don't match function arguments"
        with self.assertRaisesRegexp(CompilationError, error):
            self.m.function(Double, z=Double)(x)


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


class MemoryTests(ModuleTest, unittest.TestCase):

    def test_load_pointer(self):
        from nos.types import Pointer, Long
        import ctypes

        @self.m.function(Long, data=Pointer(Long), i=Long)
        def get_i(data, i):
            e = data[i]
            return e

        out = self.m.compile()

        dtype = (ctypes.c_long * 5)
        data = dtype(0, 10, 20, 30, 40)

        for i in range(5):
            self.assertEqual(out.get_i(data, i), data[i])

    def test_store(self):
        from nos.types import Pointer, Long
        import ctypes

        @self.m.function(Long, data=Pointer(Long), i=Long, e=Long)
        def set_i(data, i, e):
            data[i] = e
            return 0

        out = self.m.compile()

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
        from nos.types import Pointer, Long
        import ctypes

        @self.m.function(Long, data=Pointer(Long), n=Long)
        def loop_1(data, n):
            for i in range(n):
                data[i] = (i if i < 3 else 99)

            return 0

        out = self.m.compile()
        # 5 + 1 elements to check stop correctness
        data = (ctypes.c_long * 6)()

        out.loop_1(data, 5)
        self.assertEqual(list(data), [0, 1, 2, 99, 99, 0])

    def test_for_range(self):
        """More advanced loop ranges."""
        from nos.types import Pointer, Long
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

        out = self.m.compile()

        data = (ctypes.c_long * 8)()
        out.loop_1(data, 2, 7)
        self.assertEqual(list(data), [0, 0, 2, 3, 4, 5, 6, 0])

        data = (ctypes.c_long * 8)()
        out.loop_2(data, 2, 7, 2)
        self.assertEqual(list(data), [0, 0, 2, 0, 4, 0, 6, 0])

    def test_double_for(self):
        from nos.types import Pointer, Long
        import ctypes

        @self.m.function(Long, data=Pointer(Long), n=Long)
        def loop_1(data, n):
            for i in range(n):
                for j in range(n):
                    data[i * n + j] = i + j

            return 0

        out = self.m.compile()
        data = (ctypes.c_long * 9)()
        expected = [0, 1, 2,
                    1, 2, 3,
                    2, 3, 4]

        loop_1(data, 3)
        self.assertEqual(list(data), expected)

        out.loop_1(data, 3)
        self.assertEqual(list(data), expected)


class IntrinsicTests(ModuleTest, unittest.TestCase):

    def test_sqrt(self):
        from nos.types import Double
        import math

        @self.m.function(Double, x=Double)
        def sqrt(x):
            return nos.sqrt(x)

        out = self.m.compile()
        self.assertAlmostEqual(math.sqrt(10.0), out.sqrt(10.0))

        ir = self.m.dumps()
        self.assertRegexpMatches(ir, "%sqrt = call double @llvm.sqrt.f64\(double %x\)")
