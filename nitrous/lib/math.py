from __future__ import absolute_import
import math as _pymath

from . import IntrinsicEmitter

pi = _pymath.pi
e = _pymath.e


def exp(T):
    """``exp(T)(x) -> y``

    Return e raised to the power of x.

    """
    def exp_(x):
        return IntrinsicEmitter("exp", (x,), (T,))
    return exp_


def pow(T):
    """``pow(T)(x, y) -> z``

    Return *x* raised to the power of *y*.

    """
    def pow_(x, y):
        return IntrinsicEmitter("pow", (x, y), (T,))
    return pow_


def log(T):
    """``log(T)(x) -> y``

    Return the natural logarithm of *x*.

    """
    def log_(x):
        return IntrinsicEmitter("log", (x,), (T,))
    return log_


def sqrt(T):
    """``sqrt(T)(x) -> y``

    Return the square root of *x*.

    """
    def sqrt_(x):
        return IntrinsicEmitter("sqrt", (x,), (T,))
    return sqrt_
