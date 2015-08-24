# Typical run:
# C:\home\eric\wrk\scipy\weave\examples>python fibonacci.py
# Recursively computing the first 30 fibonacci numbers:
#  speed in python: 4.31599998474
#  speed in c: 0.0499999523163
#  speed up: 86.32
# Looping to compute the first 30 fibonacci numbers:
#  speed in python: 0.000520999908447
#  speed in c: 5.00000715256e-005
#  speed up: 10.42
# fib(30) 832040 832040 832040 832040
<<<<<<< HEAD
=======
from __future__ import absolute_import, print_function
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

import sys
sys.path.insert(0,'..')
import ext_tools

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def build_fibonacci():
    """ Builds an extension module with fibonacci calculators.
    """
    mod = ext_tools.ext_module('fibonacci_ext')
<<<<<<< HEAD
    a = 1 # this is effectively a type declaration
=======
    a = 1  # this is effectively a type declaration
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    # recursive fibonacci in C
    fib_code = """
                   int fib1(int a)
                   {
                       if(a <= 2)
                           return 1;
                       else
                           return fib1(a-2) + fib1(a-1);
                   }
               """
    ext_code = """
                   return_val = fib1(a);
               """
    fib = ext_tools.ext_function('c_fib1',ext_code,['a'])
    fib.customize.add_support_code(fib_code)
    mod.add_function(fib)

    # looping fibonacci in C
    fib_code = """
                    int fib2( int a )
                    {
                        int last, next_to_last, result;

                        if( a <= 2 )
                            return 1;
                        last = next_to_last = 1;
                        for(int i = 2; i < a; i++ )
                        {
                            result = last + next_to_last;
                            next_to_last = last;
                            last = result;
                        }

                        return result;
                    }
               """
    ext_code = """
                   return_val = fib2(a);
               """
    fib = ext_tools.ext_function('c_fib2',ext_code,['a'])
    fib.customize.add_support_code(fib_code)
    mod.add_function(fib)
    mod.compile()

try:
    import fibonacci_ext
except ImportError:
    build_fibonacci()
    import fibonacci_ext
c_fib1 = fibonacci_ext.c_fib1
c_fib2 = fibonacci_ext.c_fib2

#################################################################
# This where it might normally end, but we've added some timings
# below. Recursive solutions are much slower, and C is 10-50x faster
# than equivalent in Python for this simple little routine
#
#################################################################

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def py_fib1(a):
    if a <= 2:
        return 1
    else:
        return py_fib1(a-2) + py_fib1(a-1)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def py_fib2(a):
    if a <= 2:
        return 1
    last = next_to_last = 1
    for i in range(2,a):
        result = last + next_to_last
        next_to_last = last
        last = result
<<<<<<< HEAD
    return result;

import time

def recurse_compare(n):
    print 'Recursively computing the first %d fibonacci numbers:' % n
=======
    return result

import time


def recurse_compare(n):
    print('Recursively computing the first %d fibonacci numbers:' % n)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    t1 = time.time()
    for i in range(n):
        py_fib1(i)
    t2 = time.time()
<<<<<<< HEAD
    py = t2- t1
    print ' speed in python:', t2 - t1

    #load into cache
=======
    py = t2 - t1
    print(' speed in python:', t2 - t1)

    # load into cache
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    c_fib1(i)
    t1 = time.time()
    for i in range(n):
        c_fib1(i)
    t2 = time.time()
<<<<<<< HEAD
    print ' speed in c:',t2 - t1
    print ' speed up: %3.2f' % (py/(t2-t1))

def loop_compare(m,n):
    print 'Looping to compute the first %d fibonacci numbers:' % n
=======
    print(' speed in c:',t2 - t1)
    print(' speed up: %3.2f' % (py/(t2-t1)))


def loop_compare(m,n):
    print('Looping to compute the first %d fibonacci numbers:' % n)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    t1 = time.time()
    for i in range(m):
        for i in range(n):
            py_fib2(i)
    t2 = time.time()
    py = (t2-t1)
<<<<<<< HEAD
    print ' speed in python:', (t2 - t1)/m

    #load into cache
=======
    print(' speed in python:', (t2 - t1)/m)

    # load into cache
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    c_fib2(i)
    t1 = time.time()
    for i in range(m):
        for i in range(n):
            c_fib2(i)
    t2 = time.time()
<<<<<<< HEAD
    print ' speed in c:',(t2 - t1)/ m
    print ' speed up: %3.2f' % (py/(t2-t1))
=======
    print(' speed in c:',(t2 - t1) / m)
    print(' speed up: %3.2f' % (py/(t2-t1)))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

if __name__ == "__main__":
    n = 30
    recurse_compare(n)
<<<<<<< HEAD
    m= 1000
    loop_compare(m,n)
    print 'fib(30)', c_fib1(30),py_fib1(30),c_fib2(30),py_fib2(30)
=======
    m = 1000
    loop_compare(m,n)
    print('fib(30)', c_fib1(30),py_fib1(30),c_fib2(30),py_fib2(30))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
