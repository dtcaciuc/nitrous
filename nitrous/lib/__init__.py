from __future__ import absolute_import
from .. import llvm

import ctypes


_CASTS = {
    (llvm.FloatTypeKind, llvm.IntegerTypeKind): llvm.FPToSI,
    (llvm.DoubleTypeKind, llvm.IntegerTypeKind): llvm.FPToSI,
    (llvm.IntegerTypeKind, llvm.DoubleTypeKind): llvm.SIToFP,
    (llvm.IntegerTypeKind, llvm.FloatTypeKind): llvm.SIToFP,
}


def value_emitter(func):
    """Marks a function as one which emits LLVM value as its result."""
    func.__n2o_emitter__ = True
    return func


@value_emitter
class IntrinsicEmitter(object):
    """Convenicnce emitter for wrapping ``llvm.{name}`` intrinsic functions."""

    def __init__(self, name, *args):
        self.name = name
        self.args = args

    def __call__(self, module, builder):
        n_args = len(self.args)

        i = llvm.INTRINSICS["llvm.{0}".format(self.name)]
        argtypes = (llvm.TypeRef * n_args)(*map(llvm.TypeOf, self.args))
        func = llvm.GetIntrinsicDeclaration(module, i, argtypes, n_args)
        args = (llvm.ValueRef * n_args)(*self.args)

        return llvm.BuildCall(builder, func, args, n_args, ""), None


def cast(value, target_type):
    """Casts *value* to a specified *target_type*."""

    @value_emitter
    def emit(module, builder):
        from ..types import type_key

        # TODO support
        # * unsigned integers

        # No-op if type is the same
        value_type = llvm.TypeOf(value)
        if type_key(target_type.llvm_type) == type_key(value_type):
            return value, None

        value_kind = llvm.GetTypeKind(value_type)
        target_kind = llvm.GetTypeKind(target_type.llvm_type)

        def build_cast(op):
            return llvm.BuildCast(builder, op, value, target_type.llvm_type, "cast"), None

        if len(set((value_kind, target_kind))) == 2:
            # Casting between two different kinds of types
            try:
                return build_cast(_CASTS[(value_kind, target_kind)])
            except KeyError:
                raise TypeError("Cannot cast {0} to {1}".format(value, target_type))

        elif target_kind == llvm.IntegerTypeKind:
            # Same kind, but different(?) integer width
            value_width = llvm.GetIntTypeWidth(value_type)
            target_width = llvm.GetIntTypeWidth(target_type.llvm_type)

            if target_width > value_width:
                return build_cast(llvm.ZExt)
            elif target_width < value_width:
                return build_cast(llvm.Trunc)
            else:
                return value, None

        else:
            raise TypeError("Cannot cast {0} to {1}".format(value, target_type))

    return emit


def _range(*args):
    """range() built-in implementation.

    Returns (start, stop, step) LLVM value tuple which is then can be used
    by the compiler to emit the ``for`` loop code.

    """

    @value_emitter
    def emit(module, builder):
        from ..types import const_index

        # TODO add checks
        #  start > stop & step > 0;
        #  start < stop & step < 0;
        #  step < stop - start

        # [start, stop, step]
        data = [const_index(0), const_index(0), const_index(1)]

        if len(args) == 0:
            raise TypeError("Range accepts at least 1 argument")
        elif len(args) > 3:
            raise TypeError("Range accepts at most 3 arguments")
        elif len(args) == 1:
            data[1] = args[0]
        else:
            data[0] = args[0]
            data[1] = args[1]
            if len(args) == 3:
                data[2] = args[2]

        return data, None

    return emit
