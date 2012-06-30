from setuptools import setup, find_packages, Extension
from subprocess import Popen, PIPE
import platform
import os


# Don't build extensions if we're on readthedocs.org
on_rtd = os.environ.get('READTHEDOCS', None) == 'True'


# Determine the name for LLVM config binary
for path in ["llvm-config-3.1", "llvm-config"]:
    proc = Popen(["which", path], stdout=PIPE, stderr=PIPE)
    if not proc.wait():
        LLVM_CONFIG = proc.stdout.read().strip()
        break
else:
    if not on_rtd:
        raise OSError("Could not locate a suitable llvm-config binary")


def llvm_config(*args):
    p = Popen([LLVM_CONFIG] + list(args), stdout=PIPE)
    return p.communicate()[0].strip().split()


def link_args():
    # System-specific link flags for LLVM shared library.
    system = platform.system()
    llvm_libs = llvm_config("--libs", "native", "ipo", "jit")
    args = llvm_config("--ldflags")

    if system == "Linux":
        args += ["-Wl,--whole-archive"] + llvm_libs
        args += ["-Wl,--no-whole-archive", "-Wl,--no-undefined", "-lpthread", "-ldl", "-lc"]
    elif system == "Darwin":
        args += llvm_libs + ["-all_load", "-Wl,-dead_strip", "-Wl,-seg1addr", "-Wl,0xE0000000"]
    else:
        raise OSError("Unsupported system type {0}".format(system))

    return args


ext_modules = []

if not on_rtd:
    llvm_version = llvm_config("--version")[0]
    # HACK this isn't really a Python extension, however it's
    # a valid shared library so we'll use the available facilities.
    ext_modules.append(
        Extension(
            "nitrous.llvm._llvm", ["src/_llvm.cc"],
            include_dirs=llvm_config("--includedir"),
            extra_compile_args=llvm_config("--cxxflags"),
            define_macros=[("NOS_LLVM_VERSION", int(llvm_version.replace(".", "")))],
            library_dirs=llvm_config("--libdir"),
            extra_link_args=link_args()
        )
    )

setup(name="nitrous",
      version="0.0.0",
      description="Run-time LLVM-based compiler for CPython functions",
      author="Dimitri Tcaciuc",
      author_email="dtcaciuc@gmail.com",
      url="https://github.com/dtcaciuc/nitrous",
      license="MIT License",
      classifiers=["Development Status :: 2 - Pre-Alpha",
                   "Intended Audience :: Developers",
                   "Natural Language :: English",
                   "Programming Language :: Python :: 2.7",
                   "Topic :: Software Development :: Libraries"],
      install_requires=["nose", "unittest2"],
      ext_modules=ext_modules,
      packages=find_packages(),
     )
