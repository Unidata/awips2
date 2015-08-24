<<<<<<< HEAD
=======
from __future__ import absolute_import, print_function

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
import sys
sys.path.insert(0,'..')
import inline_tools

import time

<<<<<<< HEAD
def print_compare(n):
    print 'Printing %d integers:'%n
    t1 = time.time()
    for i in range(n):
        print i,
=======

def print_compare(n):
    print('Printing %d integers:' % n)
    t1 = time.time()
    for i in range(n):
        print(i, end=' ')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    t2 = time.time()
    py = (t2-t1)

    # get it in cache
    inline_tools.inline('printf("%d",i);',['i'])
    t1 = time.time()
    for i in range(n):
        inline_tools.inline('printf("%d",i);',['i'])
    t2 = time.time()
<<<<<<< HEAD
    print ' speed in python:', py
    print ' speed in c:',(t2 - t1)
    print ' speed up: %3.2f' % (py/(t2-t1))
=======
    print(' speed in python:', py)
    print(' speed in c:',(t2 - t1))
    print(' speed up: %3.2f' % (py/(t2-t1)))

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

def cout_example(lst):
    # get it in cache
    i = lst[0]
    inline_tools.inline('std::cout << i << std::endl;',['i'])
    t1 = time.time()
    for i in lst:
        inline_tools.inline('std::cout << i << std::endl;',['i'])
    t2 = time.time()

if __name__ == "__main__":
    n = 3000
    print_compare(n)
<<<<<<< HEAD
    print "calling cout with integers:"
    cout_example([1,2,3])
    print "calling cout with strings:"
=======
    print("calling cout with integers:")
    cout_example([1,2,3])
    print("calling cout with strings:")
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    cout_example(['a','bb', 'ccc'])
