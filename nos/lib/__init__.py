from .. import llvm
import ctypes


class ValueEmitter(object):

    __nos_emitter__ = True

    def __init__(self, func, args, kwargs):
        self.func = func
        self.args = args
        self.kwargs = kwargs

    def emit(self, module, builder):
        return self.func(module, builder, *self.args, **self.kwargs)


def value_emitter(func):
    def wrapper(*args, **kwargs):
        return ValueEmitter(func, args, kwargs)
    return wrapper


_CASTS = {
    (llvm.DoubleTypeKind, llvm.IntegerTypeKind): llvm.FPToSI,
    (llvm.IntegerTypeKind, llvm.DoubleTypeKind): llvm.SIToFP,
}


@value_emitter
def cast(_, builder, value, target_type):
    """Casts expression to specified type."""
    from ..exceptions import CompilationError

    value_kind = llvm.GetTypeKind(llvm.TypeOf(value))
    target_kind = llvm.GetTypeKind(target_type.llvm_type)

    # TODO support
    # * unsigned integers
    # * floats and integers of different width

    if value_kind == target_kind:
        return value

    try:
        op = _CASTS[(value_kind, target_kind)]
    except KeyError:
        raise CompilationError("Cannot cast {0} to {1}".format(value, target_type))

    return llvm.BuildCast(builder, op, value, target_type.llvm_type, "tmp")


@value_emitter
def range_(_, builder, *args):
    """range() intrinsic implementation.

    Returns (start, stop, step) LLVM value tuple.

    """
    from ..types import Long

    # TODO add checks
    #  start > stop & step > 0;
    #  start < stop & step < 0;
    #  step < stop - start

    data = [llvm.ConstInt(Long.llvm_type, 0, True),
            llvm.ConstInt(Long.llvm_type, 0, True),
            llvm.ConstInt(Long.llvm_type, 1, True)]

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

    return data


@value_emitter
def sqrt(module, builder, value):
    func = llvm.GetIntrinsicDeclaration(module,
                                        llvm.INTRINSICS["llvm.sqrt"],
                                        ctypes.byref(llvm.TypeOf(value)), 1)
    return llvm.BuildCall(builder, func, ctypes.byref(value), 1, "sqrt")
