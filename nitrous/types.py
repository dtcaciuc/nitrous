import ctypes
import ast

from collections import namedtuple
from . import llvm


def type_key(ty):
    """Returns unique key for type *ty*.

    In LLVM, getting the same type (eg. IntType(32)) yields
    same unique pointer value each time its invoked.

    """
    return ctypes.cast(ty, ctypes.c_void_p).value


def types_equal(tx, ty):
    """Returns True if *tx* is the same LLVMTypeRef as *ty*.

    To check equality, retrieve and compare raw pointer values.

    """
    return type_key(tx) == type_key(ty)


class ScalarType(object):
    """Base for all scalar data types."""

    __slots__ = ("c_type", "llvm_type")

    def __init__(self, c_type, llvm_type):
        self.c_type = c_type
        self.llvm_type = llvm_type

    def __call__(self, v):
        """Nicer equivalent to ``cast(v, Type)``"""
        from .lib import cast
        return cast(v, self)


Double = ScalarType(ctypes.c_double, llvm.DoubleType())

Float = ScalarType(ctypes.c_float, llvm.FloatType())

Long = ScalarType(ctypes.c_long, llvm.IntType(ctypes.sizeof(ctypes.c_long) * 8))

Int = ScalarType(ctypes.c_int, llvm.IntType(ctypes.sizeof(ctypes.c_int) * 8))

Bool = ScalarType(ctypes.c_bool, llvm.IntType(8))

Byte = ScalarType(ctypes.c_byte, llvm.IntType(8))


# Akin to size_t in C, this is used for all memory accessing operations.
# TODO switch this to Int by default?
Index = Long


def const_index(v):
    """Creates a new constant index value."""
    return llvm.ConstInt(Index.llvm_type, v, True)


_FLOATING_BINARY_INST = {
    ast.Add: llvm.BuildFAdd,
    ast.Sub: llvm.BuildFSub,
    ast.Mult: llvm.BuildFMul,
    ast.Div: llvm.BuildFDiv,
}

_INTEGRAL_BINARY_INST = {
    ast.Add: llvm.BuildAdd,
    ast.Sub: llvm.BuildSub,
    ast.Mult: llvm.BuildMul,
    # Integer division is consciously left out and
    # handled in function.py/emit_binary_op
}

BINARY_INST = {
    type_key(Double.llvm_type): _FLOATING_BINARY_INST,
    type_key(Float.llvm_type): _FLOATING_BINARY_INST,
    type_key(Long.llvm_type): _INTEGRAL_BINARY_INST,
    type_key(Byte.llvm_type): _INTEGRAL_BINARY_INST,
}


_FLOATING_COMPARE_INST = (
    llvm.BuildFCmp, {
        ast.Eq: llvm.RealUEQ,
        ast.Gt: llvm.RealUGT,
        ast.GtE: llvm.RealUGE,
        ast.Lt: llvm.RealULT,
        ast.LtE: llvm.RealULE,
        ast.NotEq: llvm.RealUNE,
    }
)


_INTEGRAL_COMPARE_INST = (
    llvm.BuildICmp, {
        ast.Eq: llvm.IntEQ,
        ast.Gt: llvm.IntSGT,
        ast.GtE: llvm.IntSGE,
        ast.Lt: llvm.IntSLT,
        ast.LtE: llvm.IntSLE,
        ast.NotEq: llvm.IntNE
    }
)


COMPARE_INST = {
    type_key(Double.llvm_type): _FLOATING_COMPARE_INST,
    type_key(Float.llvm_type): _FLOATING_COMPARE_INST,
    type_key(Long.llvm_type): _INTEGRAL_COMPARE_INST,
    type_key(Int.llvm_type): _INTEGRAL_COMPARE_INST,
    type_key(Byte.llvm_type): _INTEGRAL_COMPARE_INST
}


Dynamic = object()


class Pointer(object):
    """Pointer to memory block, each element of type `element_type`.

    Fixed-size blocks can be allocated by calling the type object inside
    a compiled function::

        from nitrous import Float, Pointer

        Vec3f = Pointer(Float, shape=(3,))

        @m.function(e0=Float)
        def func(e0):

            v = Vec3f()
            v[0] = e0

    """

    def __init__(self, element_type, shape=(None,)):
        self.element_type = element_type
        self.shape = shape

    @property
    def llvm_type(self):
        return llvm.PointerType(self.element_type.llvm_type, 0)

    @property
    def c_type(self):
        return ctypes.POINTER(self.element_type.c_type)

    def convert(self, p):
        import array

        # TODO it would be nice to just see if the object
        # supports buffer access interface, however it seems that
        # neither array.array nor ctypes byref() results support that.

        pointer_type = ctypes.POINTER(self.element_type.c_type)

        try:
            import numpy as np
            if isinstance(p, np.ndarray):
                return p.ctypes.data_as(pointer_type)
        except ImportError:
            pass

        if isinstance(p, array.array):
            addr, count = p.buffer_info()
            return ctypes.cast(addr, pointer_type)

        return p

    def emit_getitem(self, builder, v, i):
        gep = self._item_gep(builder, v, i)
        if isinstance(self.element_type, Structure):
            return gep, Reference(self.element_type)
        else:
            return llvm.BuildLoad(builder, gep, "v"), self.element_type

    def emit_setitem(self, builder, v, i):
        return self._item_gep(builder, v, i), Reference(self.element_type)

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


class Structure(object):

    FIELD_NAME, FIELD_TYPE = range(2)

    # TODO add packing flag

    def __init__(self, name, *fields):
        self.name = name
        self.fields = fields
        self.c_type = type(name + "_CType",
                           (ctypes.Structure,),
                           {"_fields_": [(f, t.c_type) for f, t in fields]})

        # TODO check if name exists or does it unique it automatically?
        self.llvm_type = llvm.StructCreateNamed(llvm.GetGlobalContext(), name)
        llvm_fields = (llvm.TypeRef * len(fields))(*(t.llvm_type for f, t in fields))
        llvm.StructSetBody(self.llvm_type, llvm_fields, len(fields), False)

    @property
    def argtype(self):
        # Pass by reference if directly used as argument type.
        return Reference(self)

    def emit_getattr(self, module, builder, ref, attr):
        """IR: Emits attribute value load from structure reference."""
        gep, t = self._field_gep(builder, ref, attr)
        return llvm.BuildLoad(builder, gep, "v"), t

    def emit_setattr(self, module, builder, ref, attr):
        """IR: Emits GEP used to set the attribute value."""
        gep, t = self._field_gep(builder, ref, attr)
        return gep, Reference(t)

    def _field_gep(self, builder, p, field):
        """Returns GEP and type for a *field*"""
        for i, (f, t) in enumerate(self.fields):
            if f == field:
                gep = llvm.BuildStructGEP(builder, p, i, "gep")
                return gep, self.fields[i][self.FIELD_TYPE]

        raise KeyError(field)


def array(element_type, shape, *args, **kwargs):
    t = DynamicArray if Dynamic in shape else StaticArray
    return t(shape, *args, **kwargs)


class StaticArray(Pointer):
    # TODO add a guard against None/Dynamic dimensions
    # TODO abstract element access interface into a mixin?

    def emit_getattr(self, module, builder, ref, attr):
        from .function import entry_alloca

        if attr == "ndim":
            return const_index(len(self.shape)), None

        elif attr == "shape":
            # First time, initialize a global constant array
            # and then use it on every access.
            shape_name = "StaticArray{0}".format(id(self))
            shape = llvm.GetNamedGlobal(module, shape_name)

            if not shape:
                n_dims = len(self.shape)
                dims = (llvm.ValueRef * n_dims)(*(const_index(d) for d in self.shape))
                shape_init = llvm.ConstArray(Index.llvm_type, dims, n_dims)

                shape = llvm.AddGlobal(module, llvm.TypeOf(shape_init), shape_name)
                llvm.SetInitializer(shape, shape_init)
                llvm.SetGlobalConstant(shape, llvm.TRUE)

            # XXX even though BuildPointerCast has a name, the resulting
            # value doensn't? Returning a temporary instead fixes that.
            func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(builder))
            cast = llvm.BuildPointerCast(builder, shape, Pointer(Index).llvm_type, "")
            cast_shape = entry_alloca(func, Pointer(Index).llvm_type, "")
            llvm.BuildStore(builder, cast, cast_shape)

            return llvm.BuildLoad(builder, cast_shape, "shape"), Pointer(Index)

        else:
            raise AttributeError(attr)

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

    def __call__(self):
        from .lib import value_emitter
        from .function import entry_array_alloca
        from operator import mul

        @value_emitter
        def emit(module, builder):
            func = llvm.GetBasicBlockParent(llvm.GetInsertBlock(builder))
            # Total number of elements across all dimensions.
            n = const_index(reduce(mul, self.shape, 1))
            a = entry_array_alloca(func, self.element_type.llvm_type, n, "v")
            return a, self

        return emit


class DynamicArray(Structure):
    # Wraps incoming np.array or ctypes array into a structure
    # with standard shape/number-of-dimensions attributes that can be
    # used from compiled function.
    #
    # The resulting structure supports getitem/setitem so that there's
    # no need to address it's `data` attribute.

    def __init__(self, element_type, shape=(Dynamic,)):
        self.element_type = element_type
        self.shape = shape
        # TODO make use of static dimensions to speed up indexing
        self.ndim = len(shape)

        super(DynamicArray, self).__init__(
            # TODO better way to generate structure name
            "Array" + str(id(self)),
            ("data", Pointer(element_type)),
            ("shape", Pointer(Index)),
            ("ndim", Index)
        )

    def convert(self, p):
        pointer_type = ctypes.POINTER(self.element_type.c_type)
        # FIXME conversions are unsafe, since they force-cast
        # anything to pointer to element_type.

        try:
            import numpy as np
            if isinstance(p, np.ndarray):
                return self.c_type(p.ctypes.data_as(pointer_type),
                                   (Index.c_type * len(p.shape))(*p.shape),
                                   p.ndim)
        except ImportError:
            pass

        shape = ctypes_shape(p)
        conv_p = ctypes.cast(p, pointer_type)
        return self.c_type(conv_p, (Index.c_type * len(shape))(*shape), self.ndim)

    def emit_getitem(self, builder, v, i):
        gep = self._item_gep(builder, v, i)
        if isinstance(self.element_type, Structure):
            return gep, Reference(self.element_type)
        else:
            return llvm.BuildLoad(builder, gep, "v"), self.element_type

    def emit_setitem(self, builder, v, i):
        return self._item_gep(builder, v, i), Reference(self.element_type)

    def _item_gep(self, builder, v, i):
        # Get array shape from struct value
        # TODO first arg is module instance
        shape_value, shape_type = self.emit_getattr(None, builder, v, "shape")
        data_value, data_type = self.emit_getattr(None, builder, v, "data")

        def emit_dimension(i):
            # Use direct constants, if possible; otherwise load from actual shape array.
            if self.shape[i] == Dynamic:
                dim, _ = shape_type.emit_getitem(builder, shape_value, (const_index(i),))
            else:
                dim = const_index(self.shape[i])
            return dim

        # Build conversion from ND-index to flat memory offset
        # FIXME currently assumes row-major memory alignment, first dimension can vary
        const_shape = [emit_dimension(d) for d in range(1, self.ndim)]
        ii = flatten_index(builder, i, const_shape)
        return llvm.BuildGEP(builder, data_value, ctypes.byref(ii), 1, "addr")


class Reference(object):
    """Special type to denote reference to an aggregate value / vector."""

    def __init__(self, value_type):
        self.value_type = value_type

    @property
    def c_type(self):
        return ctypes.POINTER(self.value_type.c_type)

    @property
    def llvm_type(self):
        return llvm.PointerType(self.value_type.llvm_type, 0)

    def convert(self, v):
        return ctypes.byref(self.value_type.convert(v)
                            if hasattr(self.value_type, "convert")
                            else v)


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
