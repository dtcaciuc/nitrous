import unittest

try:
    import numpy as np
except ImportError:
    np = None


@unittest.skipIf(not np, "Uses NumPy arrays")
class SpectralNormTests(unittest.TestCase):

    def test(self):
        from nitrous.module import module
        from nitrous.function import function, options
        from nitrous.types import Long, Double
        from nitrous.types.array import Slice, Any, Array
        from nitrous.lib.math import sqrt

        DoubleN = Slice(Double)

        @options(inline=True)
        @function(Double, i=Double, j=Double)
        def eval_A(i, j):
            ij = i + j
            return 1. / (ij * (ij + 1.) / 2. + i + 1.)

        @function(u=DoubleN, out=DoubleN)
        def eval_A_times_u(u, out):
            for i in range(u.shape[0]):
                out[i] = 0.
                for j in range(u.shape[0]):
                    out[i] += eval_A(Double(i), Double(j)) * u[j]

        @function(u=DoubleN, out=DoubleN)
        def eval_At_times_u(u, out):
            for i in range(u.shape[0]):
                out[i] = 0.
                for j in range(u.shape[0]):
                    out[i] += eval_A(Double(j), Double(i)) * u[j]

        @function(u=DoubleN, out=DoubleN, tmp=DoubleN)
        def eval_AtA_times_u(u, out, tmp):
            eval_A_times_u(u, tmp)
            eval_At_times_u(tmp, out)

        from sys import argv
        import numpy as np
        import math

        N = 5500
        u = np.ones(N)
        v = np.ones(N)
        tmp = np.zeros(N)

        m = module([eval_AtA_times_u])

        for _ in range(10):
            m.eval_AtA_times_u(u, v, tmp)
            m.eval_AtA_times_u(v, u, tmp)

        vBv = vv = 0
        for ue, ve in zip(u, v):
            vBv += ue * ve
            vv += ve * ve

        self.assertAlmostEqual(math.sqrt(vBv/vv), 1.274224153, 9)
