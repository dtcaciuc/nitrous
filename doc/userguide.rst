
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
    from nitrous.types import Long         #

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

TODO

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

The type system necessitates the exact match without any implicit casting. The
:func:`nitrous.lib.cast` can be used to perform the conversions when
necessary::

    from nitrous.lib import cast
    ...

    x = 5.0              # type Double
    y = 1                # type Long

    x = cast(y, Double)  # OK, assignment through explicit cast

For simple types, such as scalars, casting can be performed more eloquently by
making a call to the destination type::

    x = Double(y)        # OK, assignment through explicit cast

Variable Scope
--------------

Variable lifetime and visibility is limited to the innermost enclosing conditional/loop block rather than the function::

    z = 0.0

    if x > 2.0:
        y = x + 10.0    # First use of y
        z = y + 5.0     # OK, y and z are both in the scope

    x = z               # OK, z is in the current scope
    x = y               # Error, y scope is limited to `if` block

Types
=====

TODO

Functions
=========

TODO

.. External Functions
.. ==================
.. 
.. The :meth:`~nitrous.module.Module.include_function` method can be used to call functions defined in external static or shared libraries alongside the natively defined ones::
.. 
..     _atol = m.include_function("atol", Long, [Pointer(Char)], lib="c")
.. 
..     @m.function(Long, s=Pointer(Char))
..     def atol(s):
..         return _atol(s)
.. 
..     assert out.atol(ctypes.c_char_p("42")) == 42
.. 
.. Note that, currently, included functions can be called from functions in Nitrous mode, however they don't automatically get a Python interface and thus cannot be called by themselves without a wrapper like the one above.

Extending the Framework
=======================

More on Types
-------------

TODO

Metafunctions and Emitters
--------------------------

Another type of callable that usually appears in a Nitrous code is a *metafunction*. These are native Python routines that get executed at module compile time and emit code which goes into the compiled binary.

The :func:`~nitrous.lib.cast` is one example of such functions::

    x = cast(y, Double)

The challenge here is that the cast (an the majority of other metafunctions) needs access to the function builder object to actually produce the IR. However, these sort of objects should be invisible to whoever's writing the code and it would be incredibly tacky to pass them around everywhere.

For that reason, metafunctions return *emitters* as their result. Emitters are callables which accept two arguments: LLVM :class:`~nitrous.llvm.ModuleRef` and :class:`~nitrous.llvm.BuilderRef` instances. Because not every call results in an emitter, Nitrous recognizes them by reading the `__n2o_emitter__` magic attribute on the result object. If so, the compiler silently inserts another call which actually results in final IR::

    def cast(value, target_type):
        """Casts *value* to a specified *target_type*."""

        @value_emitter                                                         # 1
        def emit(module, builder):
            target_type_ = target_type.llvm_type
            cast_op = _get_cast(llvm.TypeOf(value), target_type_)              # 2
            return llvm.BuildCast(builder, cast_op, value, target_type_, "")

        return emit

1. :func:`~nitrous.lib.value_emitter` decorates a function with the magic emitter attribute.
2. Emitter is a closure that captures the metafunction arguments and uses them when it is called by Nitrous compiler.
