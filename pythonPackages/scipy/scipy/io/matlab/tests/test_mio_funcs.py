#!/usr/bin/env python
''' Jottings to work out format for __function_workspace__ matrix at end
of mat file.

'''
<<<<<<< HEAD
from os.path import join as pjoin, dirname
from cStringIO import StringIO

from numpy.testing import \
     assert_array_equal, \
     assert_array_almost_equal, \
     assert_equal, \
     assert_raises

from nose.tools import assert_true

import numpy as np

from scipy.io.matlab.mio5 import MatlabObject, MatFile5Writer, \
      MatFile5Reader, MatlabFunction

test_data_path = pjoin(dirname(__file__), 'data')
=======
from __future__ import division, print_function, absolute_import

import os.path
import sys
import io

from numpy.testing import run_module_suite
from numpy.compat import asstr

from scipy.io.matlab.mio5 import (MatlabObject, MatFile5Writer,
                                  MatFile5Reader, MatlabFunction)

test_data_path = os.path.join(os.path.dirname(__file__), 'data')

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def read_minimat_vars(rdr):
    rdr.initialize_read()
    mdict = {'__globals__': []}
    i = 0
    while not rdr.end_of_stream():
        hdr, next_position = rdr.read_var_header()
<<<<<<< HEAD
        name = hdr.name
=======
        name = asstr(hdr.name)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        if name == '':
            name = 'var_%d' % i
            i += 1
        res = rdr.read_var_array(hdr, process=False)
        rdr.mat_stream.seek(next_position)
        mdict[name] = res
        if hdr.is_global:
            mdict['__globals__'].append(name)
    return mdict

<<<<<<< HEAD
def read_workspace_vars(fname):
    rdr = MatFile5Reader(file(fname, 'rb'),
                          struct_as_record=True)
    vars = rdr.get_variables()
    fws = vars['__function_workspace__']
    ws_bs = StringIO(fws.tostring())
=======

def read_workspace_vars(fname):
    fp = open(fname, 'rb')
    rdr = MatFile5Reader(fp, struct_as_record=True)
    vars = rdr.get_variables()
    fws = vars['__function_workspace__']
    ws_bs = io.BytesIO(fws.tostring())
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    ws_bs.seek(2)
    rdr.mat_stream = ws_bs
    # Guess byte order.
    mi = rdr.mat_stream.read(2)
<<<<<<< HEAD
    rdr.byte_order = mi == 'IM' and '<' or '>'
    rdr.mat_stream.read(4) # presumably byte padding
    return read_minimat_vars(rdr)
=======
    rdr.byte_order = mi == b'IM' and '<' or '>'
    rdr.mat_stream.read(4)  # presumably byte padding
    mdict = read_minimat_vars(rdr)
    fp.close()
    return mdict
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b


def test_jottings():
    # example
<<<<<<< HEAD
    fname = pjoin(test_data_path, 'parabola.mat')
    ws_vars = read_workspace_vars(fname)
=======
    fname = os.path.join(test_data_path, 'parabola.mat')
    ws_vars = read_workspace_vars(fname)

if __name__ == "__main__":
    run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
