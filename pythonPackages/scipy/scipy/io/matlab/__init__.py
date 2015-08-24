<<<<<<< HEAD
# Matlab file read and write utilities
from mio import loadmat, savemat
=======
"""
Utilities for dealing with MATLAB(R) files

Notes
-----
MATLAB(R) is a registered trademark of The MathWorks, Inc., 3 Apple Hill
Drive, Natick, MA 01760-2098, USA.

"""
from __future__ import division, print_function, absolute_import

# Matlab file read and write utilities
from .mio import loadmat, savemat, whosmat
from . import byteordercodes

__all__ = ['loadmat', 'savemat', 'whosmat', 'byteordercodes']
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

from numpy.testing import Tester
test = Tester().test
bench = Tester().bench
