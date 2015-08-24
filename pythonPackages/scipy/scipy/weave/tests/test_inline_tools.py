<<<<<<< HEAD
from numpy import *
from numpy.testing import *

from scipy.weave import inline_tools

class TestInline(TestCase):
    """ These are long running tests...

         I'd like to benchmark these things somehow.
=======
from __future__ import absolute_import, print_function

from numpy.testing import TestCase, assert_, run_module_suite

from scipy.weave import inline_tools

from weave_test_utils import dec


class TestInline(TestCase):
    """These are long running tests...

    Would be useful to benchmark these things somehow.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    """
    @dec.slow
    def test_exceptions(self):
        a = 3
        code = """
               if (a < 2)
                  throw_error(PyExc_ValueError,
                              "the variable 'a' should not be less than 2");
               else
                   return_val = PyInt_FromLong(a+1);
               """
        result = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert(result == 4)
=======
        assert_(result == 4)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

## Unfortunately, it is not always possible to catch distutils compiler
## errors, since SystemExit is used.  Until that is fixed, these tests
## cannot be run in the same process as the test suite.

##         try:
##             a = 1
##             result = inline_tools.inline(code,['a'])
<<<<<<< HEAD
##             assert(1) # should've thrown a ValueError
=======
##             assert_(1) # should've thrown a ValueError
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
##         except ValueError:
##             pass

##         from distutils.errors import DistutilsError, CompileError
##         try:
##             a = 'string'
##             result = inline_tools.inline(code,['a'])
<<<<<<< HEAD
##             assert(1) # should've gotten an error
=======
##             assert_(1) # should've gotten an error
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
##         except:
##             # ?CompileError is the error reported, but catching it doesn't work
##             pass

<<<<<<< HEAD
if __name__ == "__main__":
    import nose
    nose.run(argv=['', __file__])
=======

if __name__ == "__main__":
    run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
