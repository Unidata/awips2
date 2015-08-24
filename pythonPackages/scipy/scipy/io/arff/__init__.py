<<<<<<< HEAD
"""Module to read arff files (weka format).

arff is a simple file format which support numerical, string and data values.
It supports sparse data too.

See http://weka.sourceforge.net/wekadoc/index.php/en:ARFF_(3.4.6) for more
details about arff format and available datasets."""

from arffread import *
import arffread

__all__ = arffread.__all__
=======
"""
Module to read ARFF files, which are the standard data format for WEKA.

ARFF is a text file format which support numerical, string and data values.
The format can also represent missing data and sparse data.

See the `WEKA website
<http://weka.wikispaces.com/ARFF>`_
for more details about arff format and available datasets.
"""
from __future__ import division, print_function, absolute_import

from .arffread import *
from . import arffread

__all__ = arffread.__all__

from numpy.testing import Tester
test = Tester().test
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
