from __future__ import absolute_import
import math as _pymath

from .. import llvm
from . import IntrinsicEmitter, value_emitter

pi = _pymath.pi
e = _pymath.e


def _pymath_doc(f):
    """Copies docstring from Python stdlib `math` function of the same name."""
    f.__doc__ = getattr(_pymath, f.__name__).__doc__
    return f


@_pymath_doc
def exp(x):
    return IntrinsicEmitter("exp", (x,))


@_pymath_doc
def pow(x, y):
    return IntrinsicEmitter("pow", (x, y), spec=(x,))


@_pymath_doc
def log(x, base=None):

    @value_emitter
    def emit(builder):
        n, n_ty = IntrinsicEmitter("log", (x,))(builder)
        if base is not None:
            d, _ = IntrinsicEmitter("log", (base,))(builder)
            n = llvm.BuildFDiv(builder, n, d, "")
        return n, n_ty

    return emit


@_pymath_doc
def sqrt(x):
    return IntrinsicEmitter("sqrt", (x,))
