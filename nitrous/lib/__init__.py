from __future__ import absolute_import
from .. import llvm


_KIND_CASTS = {

    # Integer -- Floating
    (llvm.FloatTypeKind, llvm.IntegerTypeKind): llvm.FPToSI,
    (llvm.DoubleTypeKind, llvm.IntegerTypeKind): llvm.FPToSI,
    (llvm.IntegerTypeKind, llvm.DoubleTypeKind): llvm.SIToFP,
    (llvm.IntegerTypeKind, llvm.FloatTypeKind): llvm.SIToFP,

    # Float -- Double
    (llvm.FloatTypeKind, llvm.DoubleTypeKind): llvm.FPExt,
    (llvm.DoubleTypeKind, llvm.FloatTypeKind): llvm.FPTrunc,

}


def value_emitter(func):
    """Marks a function as one which emits LLVM value as its result."""
    func.__n2o_emitter__ = True
    return func


@value_emitter
class IntrinsicEmitter(object):
    """Convenicnce emitter for wrapping ``llvm.{name}`` intrinsic functions.

    The *spec* lists types that are used to select particular
    overloaded variant.

    """

    def __init__(self, name, args, spec):
        self.name = name
        self.args = (llvm.ValueRef * len(args))(*args)
        self.spec = (llvm.TypeRef * len(spec))(*(t.llvm_type for t in spec))

    def __call__(self, builder):
        func = llvm.get_intrinsic(builder, self.name, self.spec)
        return llvm.BuildCall(builder, func, self.args, len(self.args), "call"), None


def cast(value, target_type):
    """Casts *value* to a specified *target_type*."""

    @value_emitter
    def emit(builder):

        # No-op if LLVM type is the same
        value_type = llvm.TypeOf(value)
        if llvm.types_equal(target_type.llvm_type, value_type):
            return value, target_type

        value_kind = llvm.GetTypeKind(value_type)
        target_kind = llvm.GetTypeKind(target_type.llvm_type)

        def build_cast(op):
            return llvm.BuildCast(builder, op, value, target_type.llvm_type, "cast"), target_type

        if len(set((value_kind, target_kind))) == 2:
            try:
                # Casting between two different kinds of types
                return build_cast(_KIND_CASTS[(value_kind, target_kind)])
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
                return value, target_type

        else:
            raise TypeError("Cannot cast {0} to {1}".format(value, target_type))

    return emit


def range_(*args):
    """``range([start,] stop[, step])``

    Returns (start, stop, step) LLVM value tuple which is then can be used
    by the compiler to emit the ``for`` loop code.

    """

    @value_emitter
    def emit(builder):
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


def print_(*args, **kwargs):
    """``print_(value, ..., end='\\n', file=sys.stdout)``

    Also available through ``print`` built-in.

    Currently supports only several basic types, namely strings, integers of
    different widths, floats and doubles. *file* argument accepts only file
    objects with ``fileno()`` method. *sep* argument is currently unsupported.

    """
    from nitrous.function import c_function, _get_or_create_function, emit_constant_string
    from nitrous.types import Int, Long, Double, String
    from sys import stdout

    # TODO add support for .sep parameter; this will require
    # to implement string joining or have one printf for every arg.

    # Move this somewhere, maybe `nitrous.lib.c`?
    dprintf = c_function("dprintf", Int, [Int, String])

    @value_emitter
    def emit(builder):

        module = llvm.GetParentModule__(builder)
        llvm_printf, _ = _get_or_create_function(module, dprintf, vargs=True)

        cast_args = []
        formats = []

        # Getting file descriptor
        file_ = kwargs['file'] or stdout
        fileno = llvm.ConstInt(Int.llvm_type, file_.fileno(), True)

        # Performing certain casts for simplicity.
        for a in args:
            ty = llvm.TypeOf(a)
            if llvm.types_equal(ty, String.llvm_type):
                formats.append("%s")
            elif llvm.GetTypeKind(ty) == llvm.IntegerTypeKind:
                formats.append("%ld")
                a, _ = cast(a, Long)(builder)
            elif llvm.GetTypeKind(ty) == llvm.DoubleTypeKind:
                formats.append("%lf")
            elif llvm.GetTypeKind(ty) == llvm.FloatTypeKind:
                formats.append("%lf")
                # XXX for some reason floats are not printing. Looking
                # at Clang disassembly of printf("%f", (float)2.0), the
                # resulting constant type is double... Why?
                a, _ = cast(a, Double)(builder)
            else:
                raise TypeError("Unknown argument type")

            cast_args.append(a)

        # Appending end terminator, if any, else a newline.
        end = kwargs['end']
        if end is None:
            end = emit_constant_string(builder, "\n")

        cast_args.append(end)

        # Creating final format string, args and making the call.
        format_ = emit_constant_string(builder, " ".join(formats) + "%s")
        args_ = (llvm.ValueRef * (len(cast_args) + 2))(fileno, format_, *cast_args)

        llvm.BuildCall(builder, llvm_printf, args_, len(args_), "print")

        return None, None

    return emit
