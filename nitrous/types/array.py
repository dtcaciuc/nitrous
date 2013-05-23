from . import Pointer, Structure, Reference, Index, const_index, is_aggregate
from .. import llvm
import ctypes

try:
    import numpy as np
except ImportError:
    np = None


__all__ = ["Any", "Array", "Slice"]


class _AnyClass(object):

    def __repr__(self):
        return "Any"


#: Used in in slice or array specification to indicate variable shape dimension.
Any = _AnyClass()


class _ItemAccessor(object):
    """Mixin for common Array/Slice item accessing routines."""

    def emit_getitem(self, builder, v, i):
        if len(i) < len(self.shape):
            return self._emit_subslice(builder, v, i)
        else:
            gep = self._item_gep(builder, v, i)
            if is_aggregate(self.element_type):
                return gep, Reference(self.element_type)
            else:
                return llvm.BuildLoad(builder, gep, "getitem"), self.element_type

    def emit_setitem(self, builder, v, i, e):
        if not llvm.types_equal(self.element_type.llvm_type, llvm.TypeOf(e)):
            # FIXME because we don't have e's nitrous type, for now just state
            # what the type *should* be for assignment to succeed.
            raise TypeError("Element value must be a(n) {0}".format(self.element_type))
        gep = self._item_gep(builder, v, i)
        llvm.BuildStore(builder, e, gep)

    def _emit_subslice(self, builder, v, i):
        """Emits a sub-slice based on partial index *i*"""
        from ..function import entry_alloca, entry_array_alloca

        SSTy = Slice(self.element_type, self.shape[len(i):])
        ss = entry_alloca(builder, SSTy.llvm_type, "subslice")

        # Setting ndim
        n_subdims = const_index(len(self.shape) - len(i))
        SSTy._struct.emit_setattr(builder, ss, "ndim", n_subdims)

        # Setting shape dimensions
        subshape, subshape_ty = SSTy._struct.emit_getattr(builder, ss, "shape")

        # shape is a reference
        shape, shape_ty = self.emit_getattr(builder, v, "shape")
        for j in range(len(self.shape) - len(i)):
            dim, _ = shape_ty.value_type.emit_getitem(builder, shape, (const_index(j + len(i)),))
            subshape_ty.value_type.emit_setitem(builder, subshape, (const_index(j),), dim)

        # Setting pointer to data sub-block.
        data_idx = i + (const_index(0),) * (len(self.shape) - len(i))
        SSTy._struct.emit_setattr(builder, ss, "data", self._item_gep(builder, v, data_idx))

        return ss, SSTy


class Array2(_ItemAccessor):
    """Array backed by llvm.ArrayType rather than pointer to memory.

    This enables us to declare it as an aggregate type which can be returned by value.

    TODO describe constructor initialization etc.

    """

    def __init__(self, element_type, shape):
        self.element_type = element_type
        self.shape = shape
        self.ndim = len(shape)

    def __repr__(self):
        return "Array({0}, shape={1})".format(self.element_type, repr(self.shape))

    def __str__(self):
        return "<Array {0}>".format(shape_str(self.element_type, self.shape))

    def __call__(self, values=None):
        from nitrous.lib import ValueEmitter
        from nitrous.function import entry_alloca
        from itertools import product

        def emit(builder):
            v = entry_alloca(builder, self.llvm_type, "v.array")
            if values is not None:
                for i in product(*(range(d) for d in self.shape)):
                    ii = tuple(const_index(j) for j in i)
                    vi = values
                    for k in i:
                        vi = vi[k]
                    self.emit_setitem(builder, v, ii, vi)

            return v, Reference(self)

        return ValueEmitter(emit)

    @property
    def llvm_type(self):
        from operator import mul
        n = reduce(mul, self.shape, 1)
        return llvm.ArrayType(self.element_type.llvm_type, n)

    @property
    def c_type(self):
        from operator import mul
        return reduce(mul, self.shape[::-1], self.element_type.c_type)

    @property
    def tag(self):
        shape_tag = "".join("d{0}".format(d) for d in self.shape)
        return "AX{0}{1}".format(shape_tag, self.element_type.tag)

    def convert(self, p):
        if np and isinstance(p, np.ndarray):
            p = np.ctypeslib.as_ctypes(p)
        return p

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

            return shape, Array2(Index, (self.ndim,))

        else:
            raise AttributeError(attr)

    def _item_gep(self, builder, v, i):
        if len(i) != len(self.shape):
            raise TypeError("Index and array shapes don't match ({0} != {1})"
                            .format(len(i), len(self.shape)))

        # TODO check const shape dimension values?

        # Build conversion from ND-index to flat memory offset
        # FIXME currently assumes row-major memory alignment, first dimension can vary
        const_shape = map(const_index, self.shape[1:])
        ii = flatten_index(builder, i, const_shape)
        # Cast so that we can get GEP to a particular element.
        p_type = llvm.PointerType(self.element_type.llvm_type, 0)
        p = llvm.BuildPointerCast(builder, v, p_type, "array.ptr")
        return llvm.BuildGEP(builder, p, ctypes.byref(ii), 1, "addr")


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
        from nitrous.lib import ValueEmitter
        from nitrous.function import entry_array_alloca
        from operator import mul

        def emit(builder):
            # Total number of elements across all dimensions.
            n = const_index(reduce(mul, self.shape, 1))
            a = entry_array_alloca(builder, self.element_type.llvm_type, n, "v")
            return a, self

        return ValueEmitter(emit)

    @property
    def llvm_type(self):
        return llvm.PointerType(self.element_type.llvm_type, 0)

    @property
    def c_type(self):
        return ctypes.POINTER(self.element_type.c_type)

    @property
    def tag(self):
        shape_tag = "".join("d{0}".format(d) for d in self.shape)
        return "A{0}{1}".format(shape_tag, self.element_type.tag)

    def convert(self, p):
        pointer_type = ctypes.POINTER(self.element_type.c_type)
        # FIXME conversions are unsafe, since they force-cast
        # anything to pointer to element_type.

        if np and isinstance(p, np.ndarray):
            return p.ctypes.data_as(pointer_type)

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

            return shape, Array2(Index, (self.ndim,))

        else:
            raise AttributeError(attr)

    def _item_gep(self, builder, v, i):
        if len(i) != len(self.shape):
            raise TypeError("Index and array shapes don't match ({0} != {1})"
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
        k = (llvm.address_of(element_type.llvm_type), shape)
        try:
            self._struct = _slice_types[k]
        except KeyError:
            self._struct = _slice_types.setdefault(
                k, Structure("Slice",
                             ("data", Pointer(element_type)),
                             ("shape", Array2(Index, (len(shape),))),
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

    @property
    def tag(self):
        shape_tag = "".join("d{0}".format(d) for d in self.shape)
        return "B{0}{1}".format(shape_tag, self.element_type.tag)

    def convert(self, p):
        pointer_type = ctypes.POINTER(self.element_type.c_type)
        # FIXME conversions are unsafe, since they force-cast
        # anything to pointer to element_type.

        if np and isinstance(p, np.ndarray):
            return self._struct.c_type(p.ctypes.data_as(pointer_type),
                                       (Index.c_type * len(p.shape))(*p.shape),
                                       p.ndim)

        shape = ctypes_shape(p)
        conv_p = ctypes.cast(p, pointer_type)
        return self._struct.c_type(conv_p, (Index.c_type * len(shape))(*shape), self.ndim)

    def emit_getattr(self, builder, ref, attr):
        return self._struct.emit_getattr(builder, ref, attr)

    def emit_setattr(self, builder, ref, attr, v):
        raise TypeError("Slice is immutable")

    def _item_gep(self, builder, v, i):
        if len(i) != len(self.shape):
            raise TypeError("Index and slice shapes don't match ({0} != {1})"
                            .format(len(i), len(self.shape)))

        # Get array shape from struct value
        shape_value, shape_type = self.emit_getattr(builder, v, "shape")
        data_value, data_type = self.emit_getattr(builder, v, "data")

        def emit_dimension(i):
            # Use direct constants, if possible; otherwise load from actual shape array.
            if self.shape[i] == Any:
                # Shape type is a reference to array, use the actual type
                dim, _ = shape_type.value_type.emit_getitem(builder, shape_value, (const_index(i),))
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
