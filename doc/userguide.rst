
Installation
============

Nitrous requires LLVM and clang to be installed (the current officially
supported version is 3.1).

The easiest way is to download precompiled binaries from
http://llvm.org/releases. The exact install location is not important, however
make sure that the ``bin/`` directory is in your system's path.

First Steps
===========

Let's write a customary semi-useful function which may take an appreciable
amount of time to run::

    from time import time

    def fib(n):
        if n == 0 or n == 1:
            return n
        else:
            return fib(n - 1) + fib(n - 2)


    t0 = time()
    print fib(35), time() - t0
    # 9227465 6.34074497223


The absolute timing number will vary between machines and is not really
important. While this is clearly not the best formulation of the problem, in
our case, it's actually useful to showcase the speed gains. To achieve them, we
need to augment the code as following::

    from time import time
    from nitrous.module import module      # (1)
    from nitrous.function import function
    from nitrous.types import Long

    @function(Long, n=Long)                # (2)
    def fib(n):
        if n == 0 or n == 1:
            return n
        else:
            return fib(n - 1) + fib(n - 2)

    out = module([fib])                    # (3)

    t1 = time()
    print out.fib(35), time() - t1         # (4)
    # 9227465 0.0859870910645

1. The library symbols reside in ``nitrous`` package.

2. Annotate the function with result and argument types. Here, we'll use
   :class:`~nitrous.types.Long` type, which is equivalent to C ``long int`` or
   Python ``int``. Nitrous follows ``ctypes`` convention and names its types
   after their matching C counterparts of the same width/precision.

3. Build the module. The supplied list of annotated functions will be exposed
   through the return value and will be callable from Python.

4. The build result is a new :class:`~nitrous.module.Module` instance which
   contains the optimized version of our functions under the same attribute
   name.

The second timed interval is clearly shorter and yields the same result, which
is what we want. The :meth:`~nitrous.function.function` decorator
instructed the library to parse ``fib()`` into a syntax tree, then translate it
LLVM IR. That, in turn,  got sent through range of available LLVM optimizers,
translated to native machine code and loaded back into Python as a shared
library using ``ctypes``.


Language Constraints
====================

Unsupported Features
--------------------

The following Python features are not yet implemented or do not fit into the
general pattern of the framework:

* Classes, methods and inheritance
* List, set, and dictionary comprehensions
* Generator functions
* Closures

Constant Types
--------------

Numeric constants are associated with a default type. The current mapping
follows the underlying C datatypes used internally by CPython implementation.

+-------------+---------+---------------+
| Python type | Example | Nitrous Type  |
+=============+=========+===============+
| IntType     | 3       | Long          |
+-------------+---------+---------------+
| FloatType   | 1.0     | Double        |
+-------------+---------+---------------+
| StringType  | "abcd"  | Pointer(Char) |
+-------------+---------+---------------+

Assignments
-----------

Basic assignments work as expected::

    x = 5.0  # OK, initialized variable x to Double(5.0)

Here, ``x`` is assigned the first time with a constant of type ``float``, which
maps to Nitrous ``Double``.

The variable can be reassigned, but only with the value of the same type::

    x = 10.0  # OK, x now stores 10.0
    x = 5     # Error, int constants map to type Long

Casting
-------

The type system necessitates the exact match without any implicit casting.
When necessary, casting can be performed by making a call to the destination
type::

    x = 5.0              # type Double
    y = 1                # type Long

    x = Double(y)        # OK, assignment through explicit cast


Variable Scope
--------------

Variable lifetime and visibility is limited to the innermost enclosing
conditional/loop block rather than the function::

    z = 0.0

    if x > 2.0:
        y = x + 10.0    # First use of y
        z = y + 5.0     # OK, y and z are both in the scope

    x = z               # OK, z is in the current scope
    x = y               # Error, y scope is limited to `if` block

Functions
=========

The :func:`~nitrous.function.function` decorator accepts return type in the
first positional argument; the rest are keyword arguments describing the
compiled function arguments in order independent manner. ``None`` is used to
indicate the absense of return value.

Both can be omitted for brevity. For example,

.. code-block:: python

    @function(Long)
    def const1():
        return 1

accepts no arguments and returns constant and


.. code-block:: python

    @function(x=Slice(Double))
    def normalize(x):
        ...

is a function without a return value.


Calling Other Functions
-----------------------

Functions can call other Nitrous functions by referring to them directly, as
expected::

    @function(Double, a=Double, b=Double)
    def add2(a, b):
        return a + b

    @function(Double, a=Double, b=Double)
    def avg2(a, b):
        return add2(a, b) / 2.0


Calling Python Functions
------------------------

Python functions can also be called. The limitation here is that the call chain
has to end up with a Nitrous function or an *emitter* (more on those later).
This, for example, can be used to implement simple templates. Given

.. code-block:: python

    def element_sum(T):
        """
        ``element_sum(T)(p, n) -> v``

        Sums an array of *n* elements of type *T*.

        """
        @function(T, p=Slice(T), n=Long)
        def sum_(p, n):
            s = T()
            for i in range(n):
                s += p[i]
            return s

        return sum_

we can write

.. code-block:: python

    @function(x=Slice(Float))
    def somefunc(x, n):
        s = element_sum(Float)(x, n)

Types
=====

TODO

Scalars
-------

TODO

Pointers
--------

As opposed to C dialects, Nitrous pointers do not support indexing. They are
mostly used internally and, occasionally, for library inter-operation (eg.
standard library).

Slices
------

Slice is the official way to access a typed block of memory. They are constructed with two pieces of information: element type and shape.

.. code-block:: python

    from nitrous.types.array import Slice, Any
    from nitrous.types import Double

    Coords = Slice(Double, (Any, 3))  # Two dimensional array, any number of rows by 3 columns.

Shape specification is a tuple, where each element is either a numeric constant
or a special object ``Any``. If specified, it means that the length of a
particular dimension(s) will only be known at runtime. Default slice object is
one-dimensional and of arbitrary length.

.. code-block:: python

    Coords = Slice(Double)  # Equivalent to `Slice(Double, (Any,))`


Concrete shape dimensions are preferable from the performance standpoint, since
optimizer is then able to eliminate a lot of additions/multiplications,
especially for high number of dimensions.

Similar to NumPy arrays or Python lists, slice elements can be accessed though item notation::

    x = coords[i, 0]  # x is now of type Double

Furthermore, it is possible to access shape and number of dimensions from the
compiled functions through familiar ``shape`` and ``ndim`` attibutes::

    for i in range(coords.shape[0]):
        x = coords[i, 0]


Memory Aliasing
***************

.. warning:: Nitrous currently requires all arrays and slices to use unaliased
    memory blocks. Ignoring this rule will likely result in undefined behaviour.

Structures
----------

A familiar sight to many other languages, Structures are a way to tie together
serveral pieces of potentially different types. One good example of such would
be a toy implementation of a Slice::

    from nitrous.types import Structure

    DoubleSlice = Structure("TestSlice",
                            ("data", Pointer(Double)),
                            ("shape", Pointer(Index)),
                            ("ndim", Index))


Here we have a slice structure with 3 elements: a pointer to data memory, a
pointer to shape information and the number of slice dimensions.  Inside
Nitrous functions, these can be accessed with regular attribute notation. We
already saw that in the previous section where ``shape`` attribute was
accessed.  Similarly,

.. code-block:: python

    @function(x=DoubleSlice)
    def f(x):
        n = x.ndim
        ...

Vectors
-------

Vectors are mainly used to perform math operations on several scalar values in
one go. Although they can be of arbitrary length, typically, modern computer
hardware is optimized to handle multiples of 4 the best.

.. code-block:: python

    from nitrous.types.vector import Vector
    from nitrous.types import Float

    Float16 = Vector(Float, 16)

    @function(x=Slice(Float16), i=Index, j=Index)
    def f(x, i, j):
        diff = x[i] - x[j]  # 16 elements are subtracted at the same time.


Vector Operations
*****************

Vectors are a bit different from other data structures because, for one, they do not
support regular indexing. To get/set an element, special functions are used::

    from nitrous.types.vector import get_element

    e7 = get_element(Float16)(v, 7)

Vectors are not generally meant for frequent element access. Once loaded from
memory, the idea is to perform as many operations as possible before storing
the result back as a whole without resorting to element fiddling, which is what
square bracket accessors are good for.

Another peculiarity with vectors is that they are *immutable*. For example,
setting and element does not modify the existing vector, but returns a new
one::

    from nitrous.types.vector import set_element

    v = Float16()  # Declare new vector.
    for i in range(16):
        v = set_element(Float16)(v, i, i + Float(1))

    # Vector v is (1, 2, 3, ..., 16)

Vector Math
***********

Most functions from standard math library can be used directly::

    from nitrous.lib.math import sqrt

    ...

    w = sqrt(Float16)(v)


Note that some operations, like ``sqrt``, are translated into optimized
hardware instructions and are very fast. Some, on the other hand, like ``log``
do not have such mappings and are translated into equivalent number of scalar
opeartions on individual vector elements.


Interfacing C Libraries
=======================

The :func:`~nitrous.function.c_function` can be used to call functions defined
in static or shared libraries::


    _atol = c_function("atol", Long, [Pointer(Char)])

    @function(Long, s=Pointer(Char))
    def atol(s):
        return _atol(s)

    m = module([atol], libs=["c"])
    assert m.atol("42") == 42


The optional ``libs`` argument is similar to ``-l`` argument to GCC or Clang
and instructs Nitrous to look for symbols in specified libraries.

.. note::

    Using already built libraries is currently limited to shared object-backed modules.

Building From Source
--------------------

If you're working with source files rather than already built libraries, the
``libs`` argument can accept an instance of
:class:`~nitrous.module.CppLibrary`. Source files are transparently compiled
into objects and are linked together with the target module. Extending the
previous example:

.. code-block:: cpp

    // write_long.c

    #include <stdio.h>

    void write_long(long x) {
        printf(" value of x: %li\n", x);
    }

.. code-block:: python

    # print_atol.py

    _atol = c_function("atol", Long, [Pointer(Char)])
    _write_long = c_function("write_long", None, [Long])

    @function(s=Pointer(Char))
    def print_atol(s):
        _write_long(_atol(s))

    m = module([print_atol], libs=[CppLibrary(["write_long.c"]), "c"])

    # Prints out `value of x: 42`
    m.print_atol("42")


Extending the Framework
=======================

More on Types
-------------

TODO

Metafunctions and Emitters
--------------------------

Another type of callable that usually appears in a Nitrous code is a
*metafunction*. These are native Python routines that get executed at module
compile time and emit code which goes into the compiled binary.

The :func:`~nitrous.lib.cast` is one example of such functions::

    x = cast(y, Double)    # equivalent to x = Double(y)

The challenge here is that the cast (an the majority of other metafunctions)
needs access to the function builder object to actually produce the IR, which
is unavailable in the function body and thus cannot be referenced directly.

For that reason, metafunctions return *emitters* as their result. Emitters are
callables which accept the :class:`~nitrous.llvm.BuilderRef` instance. Because
not every call results in an emitter, Nitrous recognizes them by reading the
`__n2o_emitter__` magic attribute on the result object. If so, the compiler
silently inserts another call which actually results in final IR::

    def cast(value, target_type):
        """Casts *value* to a specified *target_type*."""

        @value_emitter                                                         # 1
        def emit(builder):
            target_type_ = target_type.llvm_type
            cast_op = _get_cast(llvm.TypeOf(value), target_type_)              # 2
            return llvm.BuildCast(builder, cast_op, value, target_type_, "")

        return emit

1. :func:`~nitrous.lib.value_emitter` decorates a function with the magic
   emitter attribute.

2. Emitter is a closure that captures the metafunction arguments and uses them
   when it is called by Nitrous compiler.
