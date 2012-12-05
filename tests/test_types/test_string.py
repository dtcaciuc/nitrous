import unittest

from nitrous.module import module
from nitrous.function import function
from nitrous.types import String


class StringTests(unittest.TestCase):

    def test_string_literal(self):

        @function(String)
        def return_const():
            return "hello world"

        m = module([return_const])
        self.assertEqual(m.return_const(), "hello world")

    def test_string_const(self):
        greeting = "hello world"

        @function(String)
        def return_const():
            return greeting

        m = module([return_const])
        self.assertEqual(m.return_const(), greeting)
