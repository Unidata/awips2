<<<<<<< HEAD
""" Testing 

"""

import numpy as np

from nose.tools import assert_true, assert_false, \
     assert_equal, assert_raises

from numpy.testing import assert_array_equal, assert_array_almost_equal
=======
""" Testing

"""

from __future__ import division, print_function, absolute_import

import numpy as np

from numpy.testing import assert_array_equal, assert_array_almost_equal, \
     run_module_suite, assert_
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

from scipy.io.matlab.mio_utils import cproduct, squeeze_element, \
    chars_to_strings


def test_cproduct():
<<<<<<< HEAD
    yield assert_equal, cproduct(()), 1
    yield assert_equal, cproduct((1,)), 1
    yield assert_equal, cproduct((1,3)), 3
    yield assert_equal, cproduct([1,3]), 3
=======
    assert_(cproduct(()) == 1)
    assert_(cproduct((1,)) == 1)
    assert_(cproduct((1,3)) == 3)
    assert_(cproduct([1,3]) == 3)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b


def test_squeeze_element():
    a = np.zeros((1,3))
<<<<<<< HEAD
    yield (assert_array_equal,
           np.squeeze(a),
           squeeze_element(a))
    # 0d output from squeeze gives scalar
    sq_int = squeeze_element(np.zeros((1,1), dtype=np.float))
    yield assert_true, isinstance(sq_int, float)
    # Unless it's a structured array
    sq_sa = squeeze_element(np.zeros((1,1),dtype=[('f1', 'f')]))
    yield assert_true, isinstance(sq_sa, np.ndarray)
                                      
    
def test_chars_strings():
    # chars as strings
    strings = ['learn ', 'python', 'fast  ', 'here  ']
    str_arr = np.array(strings, dtype='U6') # shape (4,)
    chars = [list(s) for s in strings]
    char_arr = np.array(chars, dtype='U1') # shape (4,6)
    yield assert_array_equal, chars_to_strings(char_arr), str_arr
    ca2d = char_arr.reshape((2,2,6))
    sa2d = str_arr.reshape((2,2))
    yield assert_array_equal, chars_to_strings(ca2d), sa2d
    ca3d = char_arr.reshape((1,2,2,6))
    sa3d = str_arr.reshape((1,2,2))
    yield assert_array_equal, chars_to_strings(ca3d), sa3d
    # Fortran ordered arrays
    char_arrf = np.array(chars, dtype='U1', order='F') # shape (4,6)
    yield assert_array_equal, chars_to_strings(char_arrf), str_arr
    # empty array
    arr = np.array([['']], dtype='U1')
    out_arr = np.array([''], dtype='U1')
    yield assert_array_equal, chars_to_strings(arr), out_arr
    
=======
    assert_array_equal(np.squeeze(a), squeeze_element(a))
    # 0d output from squeeze gives scalar
    sq_int = squeeze_element(np.zeros((1,1), dtype=float))
    assert_(isinstance(sq_int, float))
    # Unless it's a structured array
    sq_sa = squeeze_element(np.zeros((1,1),dtype=[('f1', 'f')]))
    assert_(isinstance(sq_sa, np.ndarray))


def test_chars_strings():
    # chars as strings
    strings = ['learn ', 'python', 'fast  ', 'here  ']
    str_arr = np.array(strings, dtype='U6')  # shape (4,)
    chars = [list(s) for s in strings]
    char_arr = np.array(chars, dtype='U1')  # shape (4,6)
    assert_array_equal(chars_to_strings(char_arr), str_arr)
    ca2d = char_arr.reshape((2,2,6))
    sa2d = str_arr.reshape((2,2))
    assert_array_equal(chars_to_strings(ca2d), sa2d)
    ca3d = char_arr.reshape((1,2,2,6))
    sa3d = str_arr.reshape((1,2,2))
    assert_array_equal(chars_to_strings(ca3d), sa3d)
    # Fortran ordered arrays
    char_arrf = np.array(chars, dtype='U1', order='F')  # shape (4,6)
    assert_array_equal(chars_to_strings(char_arrf), str_arr)
    # empty array
    arr = np.array([['']], dtype='U1')
    out_arr = np.array([''], dtype='U1')
    assert_array_equal(chars_to_strings(arr), out_arr)


if __name__ == "__main__":
    run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
