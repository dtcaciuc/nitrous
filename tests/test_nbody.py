import unittest

try:
    import numpy as np
except ImportError:
    np = None


@unittest.skipIf(not np, "Uses NumPy arrays")
class NBodyTests(unittest.TestCase):

    def test(self):
        """N-body benchmark adaptation from http://shootout.alioth.debian.org"""
        from nitrous.module import module
        from nitrous.function import function
        from nitrous.types import Long, Double
        from nitrous.types.array import Slice, Any
        from nitrous.lib.math import sqrt

        X, Y, Z = range(3)

        PI = 3.14159265358979323
        SOLAR_MASS = 4 * PI * PI
        DAYS_PER_YEAR = 365.24

        # In the following sequence
        # - Sun
        # - Jupiter
        # - Saturn
        # - Uranus
        # - Neptune

        DoubleNx3 = Slice(Double, shape=(Any, 3))
        DoubleN = Slice(Double)

        common_args = {
            "xyz": DoubleNx3,
            "vxyz": DoubleNx3,
            "mass": DoubleN,
            "n_bodies": Long
        }

        @function(vxyz=DoubleNx3, mass=DoubleN, n_bodies=Long)
        def offset_momentum(vxyz, mass, n_bodies):
            px = 0.0
            py = 0.0
            pz = 0.0

            for i in range(n_bodies):
                px -= vxyz[i, X] * mass[i]
                py -= vxyz[i, Y] * mass[i]
                pz -= vxyz[i, Z] * mass[i]

            vxyz[0, X] = px / SOLAR_MASS
            vxyz[0, Y] = py / SOLAR_MASS
            vxyz[0, Z] = pz / SOLAR_MASS

        @function(Double, **common_args)
        def energy(xyz, vxyz, mass, n_bodies):

            e = 0.0

            for i in range(n_bodies):
                vx = vxyz[i, X]
                vy = vxyz[i, Y]
                vz = vxyz[i, Z]

                e += 0.5 * mass[i] * (vx * vx + vy * vy + vz * vz)

                for j in range(i + 1, n_bodies):
                    dx = xyz[i, X] - xyz[j, X]
                    dy = xyz[i, Y] - xyz[j, Y]
                    dz = xyz[i, Z] - xyz[j, Z]

                    d2 = dx * dx + dy * dy + dz * dz
                    e -= mass[i] * mass[j] / sqrt(Double)(d2)

            return e

        @function(dt=Double, **common_args)
        def advance(xyz, vxyz, mass, n_bodies, dt):
            for i in range(n_bodies):
                for j in range(i + 1, n_bodies):
                    dx = xyz[i, X] - xyz[j, X]
                    dy = xyz[i, Y] - xyz[j, Y]
                    dz = xyz[i, Z] - xyz[j, Z]

                    d2 = dx * dx + dy * dy + dz * dz
                    mag = dt / (d2 * sqrt(Double)(d2))

                    vxyz[i, X] -= dx * mass[j] * mag
                    vxyz[i, Y] -= dy * mass[j] * mag
                    vxyz[i, Z] -= dz * mass[j] * mag

                    vxyz[j, X] += dx * mass[i] * mag
                    vxyz[j, Y] += dy * mass[i] * mag
                    vxyz[j, Z] += dz * mass[i] * mag

            for i in range(n_bodies):
                xyz[i, X] += dt * vxyz[i, X]
                xyz[i, Y] += dt * vxyz[i, Y]
                xyz[i, Z] += dt * vxyz[i, Z]

        @function(n_steps=Long, **common_args)
        def loop(xyz, vxyz, mass, n_bodies, n_steps):
            for i in range(n_steps):
                advance(xyz, vxyz, mass, n_bodies, 0.01)

        m = module([offset_momentum, energy, loop])

        xyz = np.genfromtxt("tests/data/nbody-position")
        vxyz = np.genfromtxt("tests/data/nbody-velocity") * DAYS_PER_YEAR
        mass = np.genfromtxt("tests/data/nbody-mass") * SOLAR_MASS

        m.offset_momentum(vxyz, mass, 5)

        # from time import time
        # t0 = time()

        e0 = m.energy(xyz, vxyz, mass, 5)
        self.assertAlmostEqual(e0, -0.169075164)
        # print " e=", e0, "Elapsed", time() - t0

        m.loop(xyz, vxyz, mass, 5, 50000000)

        e1 = m.energy(xyz, vxyz, mass, 5)
        self.assertAlmostEqual(e1, -0.169059907)
        # print " e=", e1, "Elapsed", time() - t0
