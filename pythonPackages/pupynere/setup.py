RELEASE = True

from setuptools import setup, find_packages
import sys, os

classifiers = """\
Development Status :: 5 - Production/Stable
Environment :: Console
Intended Audience :: Developers
Intended Audience :: Science/Research
License :: OSI Approved :: MIT License
Operating System :: OS Independent
Programming Language :: Python
Topic :: Scientific/Engineering
Topic :: Software Development :: Libraries :: Python Modules
"""

version = '1.0.15'

setup(
        name='pupynere',
        version=version,
        description="NetCDF file reader and writer.",
        long_description="""\
Pupynere is a Python module for reading and writing NetCDF files,
using the same API as Scientific.IO.NetCDF and pynetcdf. It depends only
on Numpy, so you don't need to have the NetCDF library installed.

Changelog:

1.0.15
    Added fix for empty attributes.

1.0.14
    Added support for Unicode attributes.

1.0.13
    Fixed bug when reading character variables without mmap.

1.0.12
  Fixed bug.

1.0.11
  Fixed bug.

1.0.10
  Fixed bug when packing integer attributes in 64-bit systems.

1.0.9
  Should work with Python 2.3.
  Accepts file objects instead of only filenames.

1.0.8
  Allow writing version 2 files (Large Files).

1.0.7
  Removed reads from asserts to allow PYTHONOPTIMIZE.

1.0.6
  Allows zero-length record variables.

1.0.5
  Added the option to open files without using mmap, since mmap can't
  handle huge files on Windows.

1.0.4
  Fixed packing of dimensions when writing a file. The order was being
  read from a dictionary (essentially unordered), instead of from the list
  with the proper order.

1.0.3
  Fixed bug so that it can write scalar variables.

1.0.2
  Fixed broken 1.0.1, ``var.shape`` was returning the current number
  of records in the first dimension, breaking the detection of record
  variables.

1.0.1
  Changed the code to read the variable shape from the underlying
  data object.

1.0.0
  Initial stable release. Handles record arrays properly (using a single
  mmap for all record variables) and writes files.
""",
        classifiers=filter(None, classifiers.split("\n")),
        keywords='netcdf data array math',
        author='Roberto De Almeida',
        author_email='roberto@dealmeida.net',
        url='http://bitbucket.org/robertodealmeida/pupynere/',
        download_url = "http://cheeseshop.python.org/packages/source/p/pupynere/pupynere-%s.tar.gz" % version,
        license='MIT',
        py_modules=['pupynere'],
        include_package_data=True,
        zip_safe=True,
        test_suite = 'nose.collector',
        install_requires=[
            'numpy',
        ],
        extras_require={
            'test': ['nose'],
        },
)
