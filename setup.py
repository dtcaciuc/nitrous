from setuptools import setup, find_packages, Extension
import os

llvm_version = os.environ.setdefault("NOS_LLVM_VERSION", "3.1")
llvm_prefix = os.environ.setdefault("NOS_LLVM_PREFIX", "/usr")


# HACK this isn't really a Python extension, however it's
# a valid shared library so we'll use the available facilities.
llvm_addons = Extension(
    "nos.llvm.addons",
    ["src/llvm-addons.cc"],
    include_dirs=[os.path.join(llvm_prefix, "include")],
    library_dirs=[d for d in [os.path.join(llvm_prefix, "lib"),
                              os.path.join(llvm_prefix, "lib64")]
                  if os.path.isdir(d)],
    libraries=["LLVM-{0}".format(llvm_version)],
    define_macros=[("__STDC_LIMIT_MACROS", 1),
                   ("__STDC_CONSTANT_MACROS", 1)]
)


setup(name="nos",
      version="0.1.0",
      description="Run-time LLVM-based compiler for CPython functions",
      author="Dimitri Tcaciuc",
      author_email="dtcaciuc@gmail.com",
      url="https://github.com/dtcaciuc/nos",
      license="MIT License",
      classifiers=["Development Status :: 3 - Alpha",
                   "Intended Audience :: Developers",
                   "Natural Language :: English",
                   "Programming Language :: Python :: 2.7",
                   "Topic :: Software Development :: Libraries"],
      install_requires=["nose", "unittest2"],
      ext_modules=[llvm_addons],
      packages=find_packages(),
     )
