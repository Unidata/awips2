""" Comparison of several different ways of calculating a "ramp"
    function.

    C:\home\ej\wrk\junk\scipy\weave\examples>python ramp.py
    python (seconds*ratio): 128.149998188
    arr[500]: 0.0500050005001
    compiled numeric1 (seconds, speed up): 1.42199993134 90.1195530071
    arr[500]: 0.0500050005001
    compiled numeric2 (seconds, speed up): 0.950999975204 134.752893301
    arr[500]: 0.0500050005001
    compiled list1 (seconds, speed up): 53.100001812 2.41337088164
    arr[500]: 0.0500050005001
    compiled list4 (seconds, speed up): 30.5500030518 4.19476220578
    arr[500]: 0.0500050005001

"""
<<<<<<< HEAD
=======
from __future__ import absolute_import, print_function
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

import time
import scipy.weave as weave
from numpy import *

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def Ramp(result, size, start, end):
    step = (end-start)/(size-1)
    for i in xrange(size):
        result[i] = start + step*i

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def Ramp_numeric1(result,start,end):
    code = """
           const int size = Nresult[0];
           const double step = (end-start)/(size-1);
           double val = start;
           for (int i = 0; i < size; i++)
               *result++ = start + step*i;
           """
    weave.inline(code,['result','start','end'],compiler='gcc')

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def Ramp_numeric2(result,start,end):
    code = """
           const int size = Nresult[0];
           double step = (end-start)/(size-1);
           double val = start;
           for (int i = 0; i < size; i++)
           {
              result[i] = val;
              val += step;
           }
           """
    weave.inline(code,['result','start','end'],compiler='gcc')

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def Ramp_list1(result, start, end):
    code = """
           const int size = result.len();
           const double step = (end-start)/(size-1);
           for (int i = 0; i < size; i++)
               result[i] = start + step*i;
           """
    weave.inline(code, ["result","start", "end"], verbose=2)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def Ramp_list2(result, start, end):
    code = """
           const int size = result.len();
           const double step = (end-start)/(size-1);
           for (int i = 0; i < size; i++)
           {
               PyObject* val = PyFloat_FromDouble( start + step*i );
               PySequence_SetItem(py_result,i, val);
           }
           """
    weave.inline(code, ["result", "start", "end"], verbose=2)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def main():
    N_array = 10000
    N_py = 200
    N_c = 10000

    ratio = float(N_c) / N_py

    arr = [0]*N_array
    t1 = time.time()
    for i in xrange(N_py):
        Ramp(arr, N_array, 0.0, 1.0)
    t2 = time.time()
    py_time = (t2 - t1) * ratio
<<<<<<< HEAD
    print 'python (seconds*ratio):', py_time
    print 'arr[500]:', arr[500]
=======
    print('python (seconds*ratio):', py_time)
    print('arr[500]:', arr[500])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    arr1 = array([0]*N_array,float)
    # First call compiles function or loads from cache.
    # I'm not including this in the timing.
    Ramp_numeric1(arr1, 0.0, 1.0)
    t1 = time.time()
    for i in xrange(N_c):
        Ramp_numeric1(arr1, 0.0, 1.0)
    t2 = time.time()
    c_time = (t2 - t1)
<<<<<<< HEAD
    print 'compiled numeric1 (seconds, speed up):', c_time, py_time/ c_time
    print 'arr[500]:', arr1[500]
=======
    print('compiled numeric1 (seconds, speed up):', c_time, py_time / c_time)
    print('arr[500]:', arr1[500])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    arr2 = array([0]*N_array,float)
    # First call compiles function or loads from cache.
    # I'm not including this in the timing.
    Ramp_numeric2(arr2, 0.0, 1.0)
    t1 = time.time()
    for i in xrange(N_c):
        Ramp_numeric2(arr2, 0.0, 1.0)
    t2 = time.time()
    c_time = (t2 - t1)
<<<<<<< HEAD
    print 'compiled numeric2 (seconds, speed up):', c_time, py_time/ c_time
    print 'arr[500]:', arr2[500]
=======
    print('compiled numeric2 (seconds, speed up):', c_time, py_time / c_time)
    print('arr[500]:', arr2[500])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    arr3 = [0]*N_array
    # First call compiles function or loads from cache.
    # I'm not including this in the timing.
    Ramp_list1(arr3, 0.0, 1.0)
    t1 = time.time()
    for i in xrange(N_py):
        Ramp_list1(arr3, 0.0, 1.0)
    t2 = time.time()
    c_time = (t2 - t1) * ratio
<<<<<<< HEAD
    print 'compiled list1 (seconds, speed up):', c_time, py_time/ c_time
    print 'arr[500]:', arr3[500]
=======
    print('compiled list1 (seconds, speed up):', c_time, py_time / c_time)
    print('arr[500]:', arr3[500])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    arr4 = [0]*N_array
    # First call compiles function or loads from cache.
    # I'm not including this in the timing.
    Ramp_list2(arr4, 0.0, 1.0)
    t1 = time.time()
    for i in xrange(N_py):
        Ramp_list2(arr4, 0.0, 1.0)
    t2 = time.time()
    c_time = (t2 - t1) * ratio
<<<<<<< HEAD
    print 'compiled list4 (seconds, speed up):', c_time, py_time/ c_time
    print 'arr[500]:', arr4[500]
=======
    print('compiled list4 (seconds, speed up):', c_time, py_time / c_time)
    print('arr[500]:', arr4[500])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b


if __name__ == '__main__':
    main()
