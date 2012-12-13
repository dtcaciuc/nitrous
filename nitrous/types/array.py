from . import Pointer, Structure, Reference, Index, const_index, type_key, is_aggregate
from .. import llvm
import ctypes


__all__ = ["Any", "Array", "Slice"]


class _AnyClass(object):

    def __repr__(self):
        return "Any"


#: Used in in slice or array specification to indicate variable shape dimension.
Any = _AnyClass()


class _ItemAccessor(object):
    """Mixin for common Array/Slice item accessing routines."""

    def emit_getitem(self, builder, v, i):
        gep = self._item_gep(builder, v, i)
        if is_aggregate(self.element_type):
            return gep, Reference(self.element_type)
        else:
            return llvm.BuildLoad(builder, gep, "getitem"), self.element_type

    def emit_setitem(self, builder, v, i, e):
        gep = self._item_gep(builder, v, i)
        llvm.BuildStore(builder, e, gep)


class Array(_ItemAccessor):

    def __init__(self, element_type, shape=(Any,)):
        self.element_type = element_type
        self.shape = shape
        self.ndim = len(shape)

    def __repr__(self):
        return "Array({0}, shape={1})".format(self.element_type, repr(self.shape))

    def __str__(self):
        return "<Array {0}>".format(shape_str(self.element_type, self.shape))

    def __call__(self):
        from nitrous.lib import value_emitter
        from nitrous.function import entry_array_alloca
        from operator import mul

        @value_emitter
        def emit(builder):
            func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(builder))
            # Total number of elements across all dimensions.
            n = const_index(reduce(mul, self.shape, 1))
            a = entry_array_alloca(func, self.element_type.llvm_type, n, "v")
            return a, self

        return emit

    @property
    def llvm_type(self):
        return llvm.PointerType(self.element_type.llvm_type, 0)

    @property
    def c_type(self):
        return ctypes.POINTER(self.element_type.c_type)

    def convert(self, p):
        pointer_type = ctypes.POINTER(self.element_type.c_type)
        # FIXME conversions are unsafe, since they force-cast
        # anything to pointer to element_type.

        try:
            import numpy as np
            if isinstance(p, np.ndarray):
                return p.ctypes.data_as(pointer_type)
        except ImportError:
            pass

        return ctypes.cast(p, pointer_type)

    def emit_getattr(self, builder, ref, attr):
        if attr == "ndim":
            return const_index(self.ndim), None

        elif attr == "shape":
            # First time, initialize a global constant array
            # and then use it on every access.
            module = llvm.GetParentModule__(builder)
            shape_name = "StaticArray{0}".format(id(self))
            shape = llvm.GetNamedGlobal(module, shape_name)

            if not shape:
                dims = (llvm.ValueRef * self.ndim)(*(const_index(d) for d in self.shape))
                shape_init = llvm.ConstArray(Index.llvm_type, dims, self.ndim)

                shape = llvm.AddGlobal(module, llvm.TypeOf(shape_init), shape_name)
                llvm.SetInitializer(shape, shape_init)
                llvm.SetGlobalConstant(shape, llvm.TRUE)

            cast_shape = llvm.BuildPointerCast(builder, shape, Pointer(Index).llvm_type, "")
            return (llvm.ensure_name(builder, cast_shape, Pointer(Index), "shape"),
                    Array(Index, (self.ndim,)))

        else:
            raise AttributeError(attr)

    def _item_gep(self, builder, v, i):
        if len(i) != len(self.shape):
            raise TypeError("Index and pointer shapes don't match ({0} != {1})"
                            .format(len(i), len(self.shape)))

        # TODO check const shape dimension values?

        # Build conversion from ND-index to flat memory offset
        # FIXME currently assumes row-major memory alignment, first dimension can vary
        const_shape = [const_index(d) for d in self.shape[1:]]
        ii = flatten_index(builder, i, const_shape)
        return llvm.BuildGEP(builder, v, ctypes.byref(ii), 1, "addr")


_slice_types = {}


class Slice(_ItemAccessor):
    # Wraps incoming np.array or ctypes array into a structure
    # with standard shape/number-of-dimensions attributes that can be
    # used from compiled function.
    #
    # The resulting structure supports getitem/setitem so that there's
    # no need to address it's `data` attribute.

    def __init__(self, element_type, shape=(Any,)):
        self.element_type = element_type
        self.shape = shape
        self.ndim = len(shape)

        # Prevent distinct slice LLVM types being allocated every single
        # time one declares them. This is a problem in places like
        # templates where only the data types being passed in and slice
        # type gets derived from it. Key types on their data type and shape.
        k = (type_key(element_type.llvm_type), shape)
        try:
            self._struct = _slice_types[k]
        except KeyError:
            self._struct = _slice_types.setdefault(
                k, Structure("Slice",
                             ("data", Pointer(element_type)),
                             ("shape", Array(Index, (len(shape),))),
                             ("ndim", Index))
            )

    def __repr__(self):
        return "Slice({0}, shape={1})".format(self.element_type, repr(self.shape))

    def __str__(self):
        return "<Slice {0}>".format(shape_str(self.element_type, self.shape))

    @property
    def llvm_type(self):
        return self._struct.llvm_type

    @property
    def c_type(self):
        return self._struct.c_type

    def convert(self, p):
        pointer_type = ctypes.POINTER(self.element_type.c_type)
        # FIXME conversions are unsafe, since they force-cast
        # anything to pointer to element_type.

        try:
            import numpy as np
            if isinstance(p, np.ndarray):
                return self._struct.c_type(p.ctypes.data_as(pointer_type),
                                           (Index.c_type * len(p.shape))(*p.shape),
                                           p.ndim)

        except ImportError:
            pass

        shape = ctypes_shape(p)
        conv_p = ctypes.cast(p, pointer_type)
        return self._struct.c_type(conv_p, (Index.c_type * len(shape))(*shape), self.ndim)

    def emit_getattr(self, builder, ref, attr):
        return self._struct.emit_getattr(builder, ref, attr)

    def emit_setattr(self, builder, ref, attr, v):
        raise TypeError("Slice is immutable")

    def _item_gep(self, builder, v, i):
        # Get array shape from struct value
        shape_value, shape_type = self.emit_getattr(builder, v, "shape")
        data_value, data_type = self.emit_getattr(builder, v, "data")

        def emit_dimension(i):
            # Use direct constants, if possible; otherwise load from actual shape array.
            if self.shape[i] == Any:
                dim, _ = shape_type.emit_getitem(builder, shape_value, (const_index(i),))
            else:
                dim = const_index(self.shape[i])
            return dim

        # Build conversion from ND-index to flat memory offset
        # FIXME currently assumes row-major memory alignment, first dimension can vary
        const_shape = [emit_dimension(d) for d in range(1, self.ndim)]
        ii = flatten_index(builder, i, const_shape)
        return llvm.BuildGEP(builder, data_value, ctypes.byref(ii), 1, "addr")


def flatten_index(builder, index, const_shape):
    """Converts N-dimensional index into 1-dimensional one.

    index is of a form ``(i0, i1, ... iN)``, where *i* is ValueRefs
    holding individual dimension indices.

    First dimension is considered to be variable. Given array shape
    ``(d0, d1, ... dN)``, *const_shape* contains ``(d1, d2, ... dN)``.

    If array is 1-dimensional, *const_shape* is an empty tuple.

    """
    mul_ = lambda x, y: llvm.BuildMul(builder, x, y, "v")

    # out = 0
    out = const_index(0)

    for i in range(0, len(const_shape)):
        # out += index[i-1] * reduce(mul, const_shape[i:], 1)
        tmp = reduce(mul_, const_shape[i:], const_index(1))
        rhs = llvm.BuildMul(builder, index[i], tmp, "v")
        out = llvm.BuildAdd(builder, out, rhs, "v")

    # return out + index[-1]
    return llvm.BuildAdd(builder, out, index[-1], "v")


def ctypes_shape(x):
    """Infer shape of a ctypes array."""
    try:
        dim = x._length_
        return (dim,) + ctypes_shape(x[0])
    except AttributeError:
        return ()


def shape_str(element_type, shape):
    """Return human-friendly description of array shape."""
    dim_0 = "?" if shape[0] in (Any, None) else shape[0]
    sub_shape = element_type if len(shape) == 1 else shape_str(element_type, shape[1:])
    return "[{0} x {1}]".format(dim_0, sub_shape)
