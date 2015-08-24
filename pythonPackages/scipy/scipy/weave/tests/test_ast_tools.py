<<<<<<< HEAD
from numpy.testing import *

from scipy.weave import ast_tools
from weave_test_utils import *

class TestHarvestVariables(TestCase):
    """ Not much testing going on here, but
        at least it is a flame test.
    """
=======
from __future__ import absolute_import, print_function

from numpy.testing import TestCase, assert_equal, run_module_suite

from scipy.weave import ast_tools


class TestHarvestVariables(TestCase):
    """ Not much testing going on here, but at least it is a flame test."""
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    def generic_check(self,expr,desired):
        import parser
        ast_list = parser.suite(expr).tolist()
        actual = ast_tools.harvest_variables(ast_list)
        assert_equal(actual,desired,expr)

    def test_simple_expr(self):
<<<<<<< HEAD
        """convert simple expr to blitz

           a[:1:2] = b[:1+i+2:]
        """
=======
        # Convert simple expr to blitz
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        expr = "a[:1:2] = b[:1+i+2:]"
        desired = ['a','b','i']
        self.generic_check(expr,desired)

if __name__ == "__main__":
<<<<<<< HEAD
    import nose
    nose.run(argv=['', __file__])
=======
    run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
