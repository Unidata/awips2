#!/usr/bin/env python
# Created by Pearu Peterson, September 2002
<<<<<<< HEAD
""" Test functions for fftpack.helper module
"""
=======

from __future__ import division, print_function, absolute_import

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
__usage__ = """
Build fftpack:
  python setup_fftpack.py build
Run tests if scipy is installed:
  python -c 'import scipy;scipy.fftpack.test(<level>)'
Run tests if fftpack is not installed:
  python tests/test_helper.py [<level>]
"""

<<<<<<< HEAD
from numpy.testing import *
=======
from numpy.testing import (TestCase, assert_array_almost_equal, rand,
                           run_module_suite)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
from scipy.fftpack import fftshift,ifftshift,fftfreq,rfftfreq

from numpy import pi

<<<<<<< HEAD
def random(size):
    return rand(*size)

=======

def random(size):
    return rand(*size)


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
class TestFFTShift(TestCase):

    def test_definition(self):
        x = [0,1,2,3,4,-4,-3,-2,-1]
        y = [-4,-3,-2,-1,0,1,2,3,4]
        assert_array_almost_equal(fftshift(x),y)
        assert_array_almost_equal(ifftshift(y),x)
        x = [0,1,2,3,4,-5,-4,-3,-2,-1]
        y = [-5,-4,-3,-2,-1,0,1,2,3,4]
        assert_array_almost_equal(fftshift(x),y)
        assert_array_almost_equal(ifftshift(y),x)

    def test_inverse(self):
        for n in [1,4,9,100,211]:
            x = random((n,))
            assert_array_almost_equal(ifftshift(fftshift(x)),x)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
class TestFFTFreq(TestCase):

    def test_definition(self):
        x = [0,1,2,3,4,-4,-3,-2,-1]
        assert_array_almost_equal(9*fftfreq(9),x)
        assert_array_almost_equal(9*pi*fftfreq(9,pi),x)
        x = [0,1,2,3,4,-5,-4,-3,-2,-1]
        assert_array_almost_equal(10*fftfreq(10),x)
        assert_array_almost_equal(10*pi*fftfreq(10,pi),x)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
class TestRFFTFreq(TestCase):

    def test_definition(self):
        x = [0,1,1,2,2,3,3,4,4]
        assert_array_almost_equal(9*rfftfreq(9),x)
        assert_array_almost_equal(9*pi*rfftfreq(9,pi),x)
        x = [0,1,1,2,2,3,3,4,4,5]
        assert_array_almost_equal(10*rfftfreq(10),x)
        assert_array_almost_equal(10*pi*rfftfreq(10,pi),x)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
if __name__ == "__main__":
    run_module_suite()
