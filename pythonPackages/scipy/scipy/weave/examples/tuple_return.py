<<<<<<< HEAD
=======
from __future__ import absolute_import, print_function

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
import sys
sys.path.insert(0,'..')
import inline_tools

<<<<<<< HEAD
def multi_return():
    return 1, '2nd'

def c_multi_return():

    code =  """
=======

def multi_return():
    return 1, '2nd'


def c_multi_return():

    code = """
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                py::tuple results(2);
                results[0] = 1;
                results[1] = "2nd";
                return_val = results;
            """
    return inline_tools.inline(code,[])


def compare(m):
    import time
    t1 = time.time()
    for i in range(m):
        py_result = multi_return()
    t2 = time.time()
    py = t2 - t1
<<<<<<< HEAD
    print 'python speed:', py
=======
    print('python speed:', py)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    #load cache
    result = c_multi_return()
    t1 = time.time()
    for i in range(m):
        c_result = c_multi_return()
    t2 = time.time()
    c = t2-t1
<<<<<<< HEAD
    print 'c speed:', c
    print 'speed up:', py / c
    print 'or slow down (more likely:', c / py
    print 'python result:', py_result
    print 'c result:', c_result
=======
    print('c speed:', c)
    print('speed up:', py / c)
    print('or slow down (more likely:', c / py)
    print('python result:', py_result)
    print('c result:', c_result)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

if __name__ == "__main__":
    compare(10000)
