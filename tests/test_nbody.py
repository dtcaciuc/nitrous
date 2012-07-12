import unittest2 as unittest
from nitrous.util import ModuleTest

try:
    import numpy as np
except ImportError:
    np = None


@unittest.skipIf(not np, "Uses NumPy arrays")
class NBodyTests(ModuleTest, unittest.TestCase):

    def test(self):
        """N-body benchmark adaptation from http://shootout.alioth.debian.org"""
        from nitrous.types import Long, Double, Pointer
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

        common_args = {
            "xyz": Pointer(Double),
            "vxyz": Pointer(Double),
            "mass": Pointer(Double),
            "n_bodies": Long
        }

        @self.m.function(vxyz=Pointer(Double), mass=Pointer(Double), n_bodies=Long)
        def offset_momentum(vxyz, mass, n_bodies):
            px = 0.0
            py = 0.0
            pz = 0.0

            for i in range(n_bodies):
                px -= vxyz[i * 3 + X] * mass[i]
                py -= vxyz[i * 3 + Y] * mass[i]
                pz -= vxyz[i * 3 + Z] * mass[i]

            vxyz[X] = px / SOLAR_MASS
            vxyz[Y] = py / SOLAR_MASS
            vxyz[Z] = pz / SOLAR_MASS

        @self.m.function(Double, **common_args)
        def energy(xyz, vxyz, mass, n_bodies):

            e = 0.0

            for i in range(n_bodies):
                vx = vxyz[i * 3 + X]
                vy = vxyz[i * 3 + Y]
                vz = vxyz[i * 3 + Z]

                e += 0.5 * mass[i] * (vx * vx + vy * vy + vz * vz)

                for j in range(i + 1, n_bodies):
                    dx = xyz[i * 3 + X] - xyz[j * 3 + X]
                    dy = xyz[i * 3 + Y] - xyz[j * 3 + Y]
                    dz = xyz[i * 3 + Z] - xyz[j * 3 + Z]

                    d2 = dx * dx + dy * dy + dz * dz
                    e -= mass[i] * mass[j] / sqrt(d2)

            return e

        @self.m.function(dt=Double, **common_args)
        def advance(xyz, vxyz, mass, n_bodies, dt):
            for i in range(n_bodies):
                for j in range(i + 1, n_bodies):
                    dx = xyz[i * 3 + X] - xyz[j * 3 + X]
                    dy = xyz[i * 3 + Y] - xyz[j * 3 + Y]
                    dz = xyz[i * 3 + Z] - xyz[j * 3 + Z]

                    d2 = dx * dx + dy * dy + dz * dz
                    mag = dt / (d2 * sqrt(d2))

                    vxyz[i * 3 + X] -= dx * mass[j] * mag
                    vxyz[i * 3 + Y] -= dy * mass[j] * mag
                    vxyz[i * 3 + Z] -= dz * mass[j] * mag

                    vxyz[j * 3 + X] += dx * mass[i] * mag
                    vxyz[j * 3 + Y] += dy * mass[i] * mag
                    vxyz[j * 3 + Z] += dz * mass[i] * mag

            for i in range(n_bodies):
                xyz[i * 3 + X] += dt * vxyz[i * 3 + X]
                xyz[i * 3 + Y] += dt * vxyz[i * 3 + Y]
                xyz[i * 3 + Z] += dt * vxyz[i * 3 + Z]

        @self.m.function(n_steps=Long, **common_args)
        def loop(xyz, vxyz, mass, n_bodies, n_steps):
            for i in range(n_steps):
                advance(xyz, vxyz, mass, n_bodies, 0.01)

        out = self.m.build()

        xyz = np.genfromtxt("tests/data/nbody-position")
        vxyz = np.genfromtxt("tests/data/nbody-velocity") * DAYS_PER_YEAR
        mass = np.genfromtxt("tests/data/nbody-mass") * SOLAR_MASS

        out.offset_momentum(vxyz, mass, 5)

        # from time import time
        # t0 = time()

        e0 = out.energy(xyz, vxyz, mass, 5)
        self.assertAlmostEqual(e0, -0.169075164)
        # print " e=", e0, "Elapsed", time() - t0

        out.loop(xyz, vxyz, mass, 5, 50000000)

        e1 = out.energy(xyz, vxyz, mass, 5)
        self.assertAlmostEqual(e1, -0.169059907)
        # print " e=", e1, "Elapsed", time() - t0
