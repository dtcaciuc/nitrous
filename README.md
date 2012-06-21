NOS
===

[![Build Status](https://secure.travis-ci.org/dtcaciuc/nitrous.png)](http://travis-ci.org/dtcaciuc/nitrous)

Nitrous provides a run-time LLVM-based compiler for CPython functions.

The project is already quite functional, however is still in very early stages of development. [Read the docs](http://nitrous.readthedocs.org/en/latest/) to get started.

Requirements
------------

* Python 2.7
* LLVM + clang 3.1 (can be acquired from the official [download page](http://llvm.org/releases/download.html))
* `nose`, `unittest2` and `coverage` for running tests
* `faulthandler` if you want to get meaningful tracebacks from low-level crashes.