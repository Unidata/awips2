<<<<<<< HEAD
from numpy.testing import *
import scipy.ndimage as ndi

import os
=======
from __future__ import division, print_function, absolute_import

from numpy.testing import assert_array_equal, dec, run_module_suite
import scipy.ndimage as ndi

import os
import warnings
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

try:
    from PIL import Image
    pil_missing = False
except ImportError:
    pil_missing = True

<<<<<<< HEAD
@dec.skipif(pil_missing, msg="The Python Image Library could not be found.")
def test_imread():
    lp = os.path.join(os.path.dirname(__file__), 'dots.png')
    img = ndi.imread(lp)
    assert_array_equal(img.shape, (300, 420, 3))

    img = ndi.imread(lp, flatten=True)
    assert_array_equal(img.shape, (300, 420))

=======

@dec.skipif(pil_missing, msg="The Python Image Library could not be found.")
def test_imread():
    lp = os.path.join(os.path.dirname(__file__), 'dots.png')
    with warnings.catch_warnings(record=True):  # Py3k ResourceWarning
        img = ndi.imread(lp, mode="RGB")
    assert_array_equal(img.shape, (300, 420, 3))

    with warnings.catch_warnings(record=True):  # PIL ResourceWarning
        img = ndi.imread(lp, flatten=True)
    assert_array_equal(img.shape, (300, 420))

    with open(lp, 'rb') as fobj:
        img = ndi.imread(fobj, mode="RGB")
        assert_array_equal(img.shape, (300, 420, 3))


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
if __name__ == "__main__":
    run_module_suite()
