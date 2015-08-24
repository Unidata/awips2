<<<<<<< HEAD
from numpy.testing import *

from scipy.weave import slice_handler
from scipy.weave.slice_handler import indexed_array_pattern
from scipy.weave.ast_tools import *
=======
from __future__ import absolute_import, print_function

import parser

from numpy.testing import TestCase, assert_equal, run_module_suite

from scipy.weave import slice_handler
from scipy.weave.slice_handler import indexed_array_pattern
from scipy.weave.ast_tools import ast_to_string, find_first_pattern
from weave_test_utils import remove_whitespace

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

class TestBuildSliceAtom(TestCase):
    def generic_check(self,slice_vars,desired):
        pos = slice_vars['pos']
        ast_list = slice_handler.build_slice_atom(slice_vars,pos)
        actual = ast_to_string(ast_list)
        assert_equal(actual,desired)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    def test_exclusive_end(self):
        slice_vars = {'begin':'1', 'end':'2', 'step':'_stp',
                      'single_index':'_index','pos':0}
        desired = 'slice(1,2-1)'
        self.generic_check(slice_vars,desired)

<<<<<<< HEAD
class TestSlice(TestCase):

    def generic_check(self,suite_string,desired):
        import parser
        ast_tuple = parser.suite(suite_string).totuple()
        found, data = find_first_pattern(ast_tuple,indexed_array_pattern)
        subscript = data['subscript_list'][1] #[0] is symbol, [1] is the supscript
=======

class TestSlice(TestCase):

    def generic_check(self,suite_string,desired):
        ast_tuple = parser.suite(suite_string).totuple()
        found, data = find_first_pattern(ast_tuple,indexed_array_pattern)
        subscript = data['subscript_list'][1]  # [0] is symbol, [1] is the subscript
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        actual = slice_handler.slice_ast_to_dict(subscript)
        assert_equal(actual,desired,suite_string)

    def test_empty_2_slice(self):
<<<<<<< HEAD
        """match slice from a[:]"""
        test ="a[:]"
        desired = {'begin':'_beg', 'end':'_end', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_begin_2_slice(self):
        """match slice from a[1:]"""
        test ="a[1:]"
        desired = {'begin':'1', 'end':'_end', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_end_2_slice(self):
        """match slice from a[:2]"""
        test ="a[:2]"
        desired = {'begin':'_beg', 'end':'2', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_begin_end_2_slice(self):
        """match slice from a[1:2]"""
        test ="a[1:2]"
        desired = {'begin':'1', 'end':'2', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_empty_3_slice(self):
        """match slice from a[::]"""
        test ="a[::]"
        desired = {'begin':'_beg', 'end':'_end', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_begin_3_slice(self):
        """match slice from a[1::]"""
        test ="a[1::]"
        desired = {'begin':'1', 'end':'_end', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_end_3_slice(self):
        """match slice from a[:2:]"""
        test ="a[:2:]"
        desired = {'begin':'_beg', 'end':'2', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_stp3_slice(self):
        """match slice from a[::3]"""
        test ="a[::3]"
        desired = {'begin':'_beg', 'end':'_end', 'step':'3',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_begin_end_3_slice(self):
        """match slice from a[1:2:]"""
        test ="a[1:2:]"
        desired = {'begin':'1', 'end':'2','step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_begin_step_3_slice(self):
        """match slice from a[1::3]"""
        test ="a[1::3]"
        desired = {'begin':'1', 'end':'_end','step':'3',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_end_step_3_slice(self):
        """match slice from a[:2:3]"""
        test ="a[:2:3]"
        desired = {'begin':'_beg', 'end':'2', 'step':'3',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_begin_end_stp3_slice(self):
        """match slice from a[1:2:3]"""
        test ="a[1:2:3]"
        desired = {'begin':'1', 'end':'2', 'step':'3','single_index':'_index'}
        self.generic_check(test,desired)
    def test_expr_3_slice(self):
        """match slice from a[:1+i+2:]"""
        test ="a[:1+i+2:]"
        desired = {'begin':'_beg', 'end':"1+i+2",'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)
    def test_single_index(self):
        """match slice from a[0]"""
        test ="a[0]"
=======
        # match slice from a[:]
        test = "a[:]"
        desired = {'begin':'_beg', 'end':'_end', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_begin_2_slice(self):
        # match slice from a[1:]
        test = "a[1:]"
        desired = {'begin':'1', 'end':'_end', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_end_2_slice(self):
        # match slice from a[:2]
        test = "a[:2]"
        desired = {'begin':'_beg', 'end':'2', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_begin_end_2_slice(self):
        # match slice from a[1:2]
        test = "a[1:2]"
        desired = {'begin':'1', 'end':'2', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_empty_3_slice(self):
        # match slice from a[::]
        test = "a[::]"
        desired = {'begin':'_beg', 'end':'_end', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_begin_3_slice(self):
        # match slice from a[1::]
        test = "a[1::]"
        desired = {'begin':'1', 'end':'_end', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_end_3_slice(self):
        # match slice from a[:2:]
        test = "a[:2:]"
        desired = {'begin':'_beg', 'end':'2', 'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_stp3_slice(self):
        # match slice from a[::3]
        test = "a[::3]"
        desired = {'begin':'_beg', 'end':'_end', 'step':'3',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_begin_end_3_slice(self):
        # match slice from a[1:2:]
        test = "a[1:2:]"
        desired = {'begin':'1', 'end':'2','step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_begin_step_3_slice(self):
        # match slice from a[1::3]
        test = "a[1::3]"
        desired = {'begin':'1', 'end':'_end','step':'3',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_end_step_3_slice(self):
        # match slice from a[:2:3]
        test = "a[:2:3]"
        desired = {'begin':'_beg', 'end':'2', 'step':'3',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_begin_end_stp3_slice(self):
        # match slice from a[1:2:3]
        test = "a[1:2:3]"
        desired = {'begin':'1', 'end':'2', 'step':'3','single_index':'_index'}
        self.generic_check(test,desired)

    def test_expr_3_slice(self):
        # match slice from a[:1+i+2:]
        test = "a[:1+i+2:]"
        desired = {'begin':'_beg', 'end':"1+i+2",'step':'_stp',
                   'single_index':'_index'}
        self.generic_check(test,desired)

    def test_single_index(self):
        # match slice from a[0]
        test = "a[0]"
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        desired = {'begin':'_beg', 'end':"_end",'step':'_stp',
                   'single_index':'0'}
        self.generic_check(test,desired)

<<<<<<< HEAD
def replace_whitespace(in_str):
    out = in_str.replace(" ","")
    out = out.replace("\t","")
    out = out.replace("\n","")
    return out

class TestTransformSlices(TestCase):
    def generic_check(self,suite_string,desired):
        import parser
        ast_list = parser.suite(suite_string).tolist()
        slice_handler.transform_slices(ast_list)
        actual = ast_to_string(ast_list)
        # Remove white space from expressions so that equivelant
        # but differently formatted string will compare equally
        actual = replace_whitespace(actual)
        desired = replace_whitespace(desired)
        assert_equal(actual,desired,suite_string)

    def test_simple_expr(self):
        """transform a[:] to slice notation"""
        test ="a[:]"
        desired = 'a[slice(_beg,_end,_stp)]'
        self.generic_check(test,desired)
    def test_simple_expr(self):
        """transform a[:,:] = b[:,1:1+2:3] *(c[1-2+i:,:] - c[:,:])"""
        test ="a[:,:] = b[:,1:1+2:3] *(c[1-2+i:,:] - c[:,:])"
=======

class TestTransformSlices(TestCase):

    def generic_check(self,suite_string,desired):
        ast_list = parser.suite(suite_string).tolist()
        slice_handler.transform_slices(ast_list)
        actual = ast_to_string(ast_list)
        # Remove white space from expressions so that equivalent
        # but differently formatted string will compare equally
        actual = remove_whitespace(actual)
        desired = remove_whitespace(desired)
        assert_equal(actual,desired,suite_string)

    def test_simple_expr1(self):
        # transform a[:] to slice notation
        test = "a[:]"
        desired = 'a[slice(_beg,_end)]'
        self.generic_check(test,desired)

    def test_simple_expr2(self):
        test = "a[:,:] = b[:,1:1+2:3] *(c[1-2+i:,:] - c[:,:])"
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        desired = " a[slice(_beg,_end),slice(_beg,_end)] = "\
                                    " b[slice(_beg,_end), slice(1,1+2-1,3)] *"\
                                    " (c[slice(1-2+i,_end), slice(_beg,_end)] -"\
                                    "  c[slice(_beg,_end), slice(_beg,_end)])"
        self.generic_check(test,desired)


if __name__ == "__main__":
<<<<<<< HEAD
    import nose
    nose.run(argv=['', __file__])
=======
    run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
