from nitrous.module import module, dump
from nitrous.function import function
import unittest


class NameTest(unittest.TestCase):

    def test_encode_args(self):
        """Function name is uniqued according to its argument types."""
        from nitrous.types.array import Slice
        from nitrous.types import Float, Double

        def get0(T):

            @function(T.element_type, v=T)
            def get0(v):
                return v[0]

            return get0

        get0f = get0(Slice(Float))
        get0d = get0(Slice(Double))

        m = module([get0f, get0d])
        ir = dump(m)

        self.assertIn("get0_RBdAnyf4", ir)
        self.assertIn("get0_RBdAnyf8", ir)
