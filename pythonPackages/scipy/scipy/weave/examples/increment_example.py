# examples/increment_example.py

<<<<<<< HEAD
#from weave import ext_tools

# use the following so that development version is used.
=======
# from weave import ext_tools

# use the following so that development version is used.
from __future__ import absolute_import, print_function

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
import sys
sys.path.insert(0,'..')
import ext_tools

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
def build_increment_ext():
    """ Build a simple extension with functions that increment numbers.
        The extension will be built in the local directory.
    """
    mod = ext_tools.ext_module('increment_ext')

<<<<<<< HEAD
    a = 1 # effectively a type declaration for 'a' in the
          # following functions.
=======
    # Effectively a type declaration for 'a' in the following functions.
    a = 1
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    ext_code = "return_val = PyInt_FromLong(a+1);"
    func = ext_tools.ext_function('increment',ext_code,['a'])
    mod.add_function(func)

    ext_code = "return_val = PyInt_FromLong(a+2);"
    func = ext_tools.ext_function('increment_by_2',ext_code,['a'])
    mod.add_function(func)

    mod.compile()

if __name__ == "__main__":
    try:
        import increment_ext
    except ImportError:
        build_increment_ext()
        import increment_ext
    a = 1
<<<<<<< HEAD
    print 'a, a+1:', a, increment_ext.increment(a)
    print 'a, a+2:', a, increment_ext.increment_by_2(a)
=======
    print('a, a+1:', a, increment_ext.increment(a))
    print('a, a+2:', a, increment_ext.increment_by_2(a))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
