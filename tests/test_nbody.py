import unittest2 as unittest
from nos.util import ModuleTest

try:
    import numpy as np
except ImportError:
    np = None


@unittest.skipIf(not np, "Uses NumPy arrays")
class NBodyTests(ModuleTest, unittest.TestCase):

    def test(self):
        """N-body benchmark adaptation from http://shootout.alioth.debian.org"""
        from nos.types import Long, Double, Pointer
        from nos.lib import sqrt

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

        xyz = np.array([
            [0.0, 0.0, 0.0],
            [4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01],
            [8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01],
            [1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01],
            [1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01]
        ], dtype=np.float64)

        vxyz = np.array([
            [0.0, 0.0, 0.0],
            [1.66007664274403694e-03, 7.69901118419740425e-03, -6.90460016972063023e-05],
            [-2.76742510726862411e-03, 4.99852801234917238e-03, 2.30417297573763929e-05],
            [2.96460137564761618e-03, 2.37847173959480950e-03, -2.96589568540237556e-05],
            [2.68067772490389322e-03, 1.62824170038242295e-03, -9.51592254519715870e-05],
        ], dtype=np.float64) * DAYS_PER_YEAR

        mass = np.array([
            1.0,
            9.54791938424326609e-04,
            2.85885980666130812e-04,
            4.36624404335156298e-05,
            5.15138902046611451e-05,
        ], dtype=np.float64) * SOLAR_MASS

        common_args = {
            "xyz": Pointer(Double),
            "vxyz": Pointer(Double),
            "mass": Pointer(Double),
            "n_bodies": Long
        }

        @self.m.function(Double, **common_args)
        def offset_momentum(xyz, vxyz, mass, n_bodies):
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

            return 0.0

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

        @self.m.function(Double, dt=Double, **common_args)
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

            return 0.0

        @self.m.function(Double, n_steps=Long, **common_args)
        def loop(xyz, vxyz, mass, n_bodies, n_steps):
            for i in range(n_steps):
                advance(xyz, vxyz, mass, n_bodies, 0.01)
            return 0.0

        out = self.m.build()

        out.offset_momentum(xyz, vxyz, mass, 5)

        # from time import time
        # t0 = time()

        e0 = out.energy(xyz, vxyz, mass, 5)
        self.assertAlmostEqual(e0, -0.169075164)
        # print " e=", e0, "Elapsed", time() - t0

        out.loop(xyz, vxyz, mass, 5, 50000000)

        e1 = out.energy(xyz, vxyz, mass, 5)
        self.assertAlmostEqual(e1, -0.169059907)
        # print " e=", e1, "Elapsed", time() - t0
