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


ScalarType = namedtuple("ScalarType", ("c_type", "llvm_type"))
"""Base for all scalar data types."""


Double = ScalarType(ctypes.c_double, llvm.DoubleType())

Long = ScalarType(ctypes.c_long, llvm.IntType(ctypes.sizeof(ctypes.c_long) * 8))

Int = ScalarType(ctypes.c_int, llvm.IntType(ctypes.sizeof(ctypes.c_int) * 8))

Bool = ScalarType(ctypes.c_bool, llvm.IntType(8))


BINARY_INST = {
    type_key(Double.llvm_type): {
        ast.Add: llvm.BuildFAdd,
        ast.Sub: llvm.BuildFSub,
        ast.Mult: llvm.BuildFMul,
        ast.Div: llvm.BuildFDiv,
    },
    type_key(Long.llvm_type): {
        ast.Add: llvm.BuildAdd,
        ast.Sub: llvm.BuildSub,
        ast.Mult: llvm.BuildMul,
        # Python uses floor integer division
        ast.Div: llvm.build_py_idiv,
    }
}


COMPARE_INST = {
    type_key(Double.llvm_type): (
        llvm.BuildFCmp, {
            ast.Eq: llvm.RealUEQ,
            ast.Gt: llvm.RealUGT,
            ast.GtE: llvm.RealUGE,
            ast.Lt: llvm.RealULT,
            ast.LtE: llvm.RealULE,
            ast.NotEq: llvm.RealUNE,
        }
    ),
    type_key(Long.llvm_type): (
        llvm.BuildICmp,  {
            ast.Eq: llvm.IntEQ,
            ast.Gt: llvm.IntSGT,
            ast.GtE: llvm.IntSGE,
            ast.Lt: llvm.IntSLT,
            ast.LtE: llvm.IntSLE,
            ast.NotEq: llvm.IntNE
        }
    )
}


class Pointer(object):
    """Pointer to memory block, each element of type `element_type`."""

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
        const_shape = [llvm.ConstInt(Long.llvm_type, d, True) for d in self.shape[1:]]
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

    def emit_getattr(self, builder, ref, attr):
        """IR: Emits attribute value load from structure reference."""
        gep, t = self._field_gep(builder, ref, attr)
        return llvm.BuildLoad(builder, gep, "v"), t

    def emit_setattr(self, builder, ref, attr):
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


def flatten_index(builder, index, const_shape):
    """Converts N-dimensional index into 1-dimensional one.

    index is of a form ``(i0, i1, ... iN)``, where *i* is ValueRefs
    holding individual dimension indices.

    First dimension is considered to be variable. Given array shape
    ``(d0, d1, ... dN)``, *const_shape* contains ``(d1, d2, ... dN)``.

    If array is 1-dimensional, *const_shape* is an empty tuple.

    """
    from .types import Long

    int_ = lambda x: llvm.ConstInt(Long.llvm_type, x, True)
    mul_ = lambda x, y: llvm.BuildMul(builder, x, y, "v")

    # out = 0
    out = int_(0)

    for i in range(0, len(const_shape)):
        # out += index[i-1] * reduce(mul, const_shape[i:], 1)
        tmp = reduce(mul_, const_shape[i:], int_(1))
        rhs = llvm.BuildMul(builder, index[i], tmp, "v")
        out = llvm.BuildAdd(builder, out, rhs, "v")

    # return out + index[-1]
    return llvm.BuildAdd(builder, out, index[-1], "v")
