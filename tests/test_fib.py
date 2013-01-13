import unittest


class FibTest(unittest.TestCase):

    def test(self):
        from nitrous.module import module
        from nitrous.function import function
        from nitrous.types import Long

        @function(Long, n=Long)
        def fib(n):
            if n == 0 or n == 1:
                return n
            else:
                return fib(n - 1) + fib(n - 2)

        m = module([fib])
        self.assertEqual(m.fib(28), 317811)
