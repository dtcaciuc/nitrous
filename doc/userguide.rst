
First Steps
===========

Some code::

    from nitrous.module import Module

    # Declare a module. This will contain one or more related functions.
    m = Module(__name__)

    # Annotate a function with result and argument types
    @m.function(Double, x=Double, y=Double)
    def max2(x, y):
        return x if x < y else y

    # Build the module
    out = m.build()

    # This calls the original function
    assert max2(5, 6) == 6

    # This calls the optimized one
    assert out.max2(5, 6) == 6

