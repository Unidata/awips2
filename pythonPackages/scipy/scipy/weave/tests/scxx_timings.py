<<<<<<< HEAD
import weave
import time

force = 0
N = 1000000

=======
from __future__ import absolute_import, print_function

import time

from scipy import weave


force = 0
N = 1000000


>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def list_append_scxx(a,Na):
    code = """
           for(int i = 0; i < Na;i++)
               a.append(i);
           """
    weave.inline(code,['a','Na'],force=force,verbose=2,compiler='gcc')

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def list_append_c(a,Na):
    code = """
           for(int i = 0; i < Na;i++)
           {
               PyObject* oth = PyInt_FromLong(i);
               int res = PyList_Append(py_a,oth);
               Py_DECREF(oth);
               if(res == -1)
               {
                 PyErr_Clear();  //Python sets one
                 throw_error(PyExc_RuntimeError, "append failed");
               }
           }
           """
    weave.inline(code,['a','Na'],force=force,compiler='gcc')

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def list_append_py(a,Na):
    for i in xrange(Na):
        a.append(i)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def time_list_append(Na):
    """ Compare the list append method from scxx to using the Python API
        directly.
    """
<<<<<<< HEAD
    print 'list appending times:',
=======
    print('list appending times:', end=' ')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    a = []
    t1 = time.time()
    list_append_c(a,Na)
    t2 = time.time()
<<<<<<< HEAD
    print 'py api: ', t2 - t1, '<note: first time takes longer -- repeat below>'
=======
    print('py api: ', t2 - t1, '<note: first time takes longer -- repeat below>')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    a = []
    t1 = time.time()
    list_append_c(a,Na)
    t2 = time.time()
<<<<<<< HEAD
    print 'py api: ', t2 - t1
=======
    print('py api: ', t2 - t1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    a = []
    t1 = time.time()
    list_append_scxx(a,Na)
    t2 = time.time()
<<<<<<< HEAD
    print 'scxx:   ', t2 - t1
=======
    print('scxx:   ', t2 - t1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    a = []
    t1 = time.time()
    list_append_c(a,Na)
    t2 = time.time()
<<<<<<< HEAD
    print 'python: ', t2 - t1
=======
    print('python: ', t2 - t1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

#----------------------------------------------------------------------------
#
#----------------------------------------------------------------------------

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def list_copy_scxx(a,b):
    code = """
           for(int i = 0; i < a.length();i++)
               b[i] = a[i];
           """
    weave.inline(code,['a','b'],force=force,verbose=2,compiler='gcc')

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def list_copy_c(a,b):
    code = """
           for(int i = 0; i < a.length();i++)
           {
               int res = PySequence_SetItem(py_b,i,PyList_GET_ITEM(py_a,i));
               if(res == -1)
               {
                 PyErr_Clear();  //Python sets one
                 throw_error(PyExc_RuntimeError, "append failed");
               }
           }
           """
    weave.inline(code,['a','b'],force=force,compiler='gcc')

<<<<<<< HEAD
def list_copy_py(a,b):
    for item in a:
        b[i] = item
=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def time_list_copy(N):
    """ Compare the list append method from scxx to using the Python API
        directly.
    """
<<<<<<< HEAD
    print 'list copy times:',
=======
    print('list copy times:', end=' ')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    a = [0] * N
    b = [1] * N
    t1 = time.time()
    list_copy_c(a,b)
    t2 = time.time()
<<<<<<< HEAD
    print 'py api: ', t2 - t1, '<note: first time takes longer -- repeat below>'
=======
    print('py api: ', t2 - t1, '<note: first time takes longer -- repeat below>')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    a = [0] * N
    b = [1] * N
    t1 = time.time()
    list_copy_c(a,b)
    t2 = time.time()
<<<<<<< HEAD
    print 'py api: ', t2 - t1
=======
    print('py api: ', t2 - t1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    a = [0] * N
    b = [1] * N
    t1 = time.time()
    list_copy_scxx(a,b)
    t2 = time.time()
<<<<<<< HEAD
    print 'scxx:   ', t2 - t1
=======
    print('scxx:   ', t2 - t1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    a = [0] * N
    b = [1] * N
    t1 = time.time()
    list_copy_c(a,b)
    t2 = time.time()
<<<<<<< HEAD
    print 'python: ', t2 - t1

if __name__ == "__main__":
    #time_list_append(N)
=======
    print('python: ', t2 - t1)


if __name__ == "__main__":
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    time_list_copy(N)
