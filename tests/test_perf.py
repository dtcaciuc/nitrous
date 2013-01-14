import unittest
import numpy as np

from nitrous.function import function
from nitrous.types import Double, Index
from nitrous.types.array import Array, Slice, Any


DoubleNx3 = Slice(Double, shape=(Any, 3))
DoubleN = Slice(Double)
DoubleNArray = Array(Double)

Double3 = Array(Double, (3,))

X, Y, Z = range(3)

@function(d=DoubleNx3, out=DoubleNArray, n=Index, m=Index)
def sum_1(d, out, n, m):

    for k in range(m):
        for i in range(n):
            for j in range(i + 1, n):
                out[X] += d[i, X] * d[j, X]
                out[Y] += d[i, Y] * d[j, Y]
                out[Z] += d[i, Z] * d[j, Z]

@function(d=DoubleNx3, out=DoubleN, n=Index, m=Index)
def sum_2(d, out, n, m):

    for k in range(m):
        for i in range(n):
            for j in range(i + 1, n):
                out[X] += d[i, X] * d[j, X]
                out[Y] += d[i, Y] * d[j, Y]
                out[Z] += d[i, Z] * d[j, Z]


@function(d=DoubleNx3, out=DoubleN, n=Index, m=Index)
def sum_3(d, out, n, m):

    # XXX Reading from slices is about the same as arrays,
    # but writing to slice in sum_2 is about 30% slower.
    # Writing to temporary local array regains the
    # performance; figure out why.

    __out = Double3()
    __out[X] = 0.0
    __out[Y] = 0.0
    __out[Z] = 0.0

    for k in range(m):
        for i in range(n):
            for j in range(i + 1, n):
                __out[X] += d[i, X] * d[j, X]
                __out[Y] += d[i, Y] * d[j, Y]
                __out[Z] += d[i, Z] * d[j, Z]

    for i in range(3):
        out[i] = __out[i]


class SliceDoubleLoop(unittest.TestCase):

    def test(self):
        from nitrous.module import module
        from time import time

        m = module([sum_1, sum_2, sum_3])
        xyz = np.random.rand(10000, 3)
        N = 10

        s = np.zeros(len(xyz))
        t0 = time()
        m.sum_1(xyz, s, len(xyz), N)
        print "sum_1, Elapsed", (time() - t0) / N

        s = np.zeros(len(xyz))
        t0 = time()
        m.sum_2(xyz, s, len(xyz), N)
        print "sum_2, Elapsed", (time() - t0) / N

        s = np.zeros(len(xyz))
        t0 = time()
        m.sum_3(xyz, s, len(xyz), N)
        print "sum_3, Elapsed", (time() - t0) / N
