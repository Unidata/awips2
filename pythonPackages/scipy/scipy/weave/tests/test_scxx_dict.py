""" Test refcounting and behavior of SCXX.
"""
<<<<<<< HEAD

import sys

from numpy.testing import *

from scipy.weave import inline_tools

=======
from __future__ import absolute_import, print_function

import sys

from numpy.testing import TestCase, assert_, assert_raises, run_module_suite

from scipy.weave import inline_tools

from weave_test_utils import dec

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

class TestDictConstruct(TestCase):
    #------------------------------------------------------------------------
    # Check that construction from basic types is allowed and have correct
    # reference counts
    #------------------------------------------------------------------------
    @dec.slow
    def test_empty(self):
        # strange int value used to try and make sure refcount is 2.
        code = """
               py::dict val;
               return_val = val;
               """
        res = inline_tools.inline(code)
<<<<<<< HEAD
        assert sys.getrefcount(res) == 2
        assert res == {}


class TestDictHasKey(TestCase):
=======
        assert_(sys.getrefcount(res) == 2)
        assert_(res == {})


class TestDictHasKey(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_obj(self):
        class Foo:
            pass
        key = Foo()
        a = {}
        a[key] = 12345
        code = """
               return_val =  a.has_key(key);
               """
        res = inline_tools.inline(code,['a','key'])
<<<<<<< HEAD
        assert res
=======
        assert_(res)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_int(self):
        a = {}
        a[1234] = 12345
        code = """
               return_val = a.has_key(1234);
               """
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res
=======
        assert_(res)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_double(self):
        a = {}
        a[1234.] = 12345
        code = """
               return_val = a.has_key(1234.);
               """
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res
=======
        assert_(res)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_complex(self):
        a = {}
        a[1+1j] = 12345
        key = 1+1j
        code = """
               return_val = a.has_key(key);
               """
        res = inline_tools.inline(code,['a','key'])
<<<<<<< HEAD
        assert res
=======
        assert_(res)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_string(self):
        a = {}
        a["b"] = 12345
        code = """
               return_val = a.has_key("b");
               """
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res
=======
        assert_(res)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_std_string(self):
        a = {}
        a["b"] = 12345
        key_name = "b"
        code = """
               return_val = a.has_key(key_name);
               """
        res = inline_tools.inline(code,['a','key_name'])
<<<<<<< HEAD
        assert res
=======
        assert_(res)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_string_fail(self):
        a = {}
        a["b"] = 12345
        code = """
               return_val = a.has_key("c");
               """
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert not res
=======
        assert_(not res)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

class TestDictGetItemOp(TestCase):

    def generic_get(self,code,args=['a']):
        a = {}
        a['b'] = 12345

        res = inline_tools.inline(code,args)
<<<<<<< HEAD
        assert res == a['b']
=======
        assert_(res == a['b'])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_char(self):
        self.generic_get('return_val = a["b"];')

    @dec.knownfailureif(True)
    @dec.slow
    def test_char_fail(self):
        # We can't through a KeyError for dicts on RHS of
        # = but not on LHS.  Not sure how to deal with this.
<<<<<<< HEAD
        try:
            self.generic_get('return_val = a["c"];')
        except KeyError:
            pass
=======
        assert_raises(KeyError, self.generic_get, 'return_val = a["c"];')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_string(self):
        self.generic_get('return_val = a[std::string("b")];')

<<<<<<< HEAD

=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_obj(self):
        code = """
               py::object name = "b";
               return_val = a[name];
               """
        self.generic_get(code,['a'])

    @dec.knownfailureif(True)
    @dec.slow
    def test_obj_fail(self):
        # We can't through a KeyError for dicts on RHS of
        # = but not on LHS.  Not sure how to deal with this.
<<<<<<< HEAD
        try:
            code = """
                   py::object name = "c";
                   return_val = a[name];
                   """
            self.generic_get(code,['a'])
        except KeyError:
            pass

class TestDictSetOperator(TestCase):
=======
        code = """
               py::object name = "c";
               return_val = a[name];
               """
        assert_raises(KeyError, self.generic_get, code, ['a'])


class TestDictSetOperator(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    def generic_new(self,key,val):
        # test that value is set correctly and that reference counts
        # on dict, key, and val are being handled correctly.
        a = {}
        # call once to handle mysterious addition of one ref count
        # on first call to inline.
        inline_tools.inline("a[key] = val;",['a','key','val'])
<<<<<<< HEAD
        assert a[key] == val
        before = sys.getrefcount(a), sys.getrefcount(key), sys.getrefcount(val)
        inline_tools.inline("a[key] = val;",['a','key','val'])
        assert a[key] == val
        after = sys.getrefcount(a), sys.getrefcount(key), sys.getrefcount(val)
        assert before == after
    def generic_overwrite(self,key,val):
        a = {}
        overwritten = 1
        a[key] = overwritten # put an item in the dict to be overwritten
=======
        assert_(a[key] == val)
        before = sys.getrefcount(a), sys.getrefcount(key), sys.getrefcount(val)
        inline_tools.inline("a[key] = val;",['a','key','val'])
        assert_(a[key] == val)
        after = sys.getrefcount(a), sys.getrefcount(key), sys.getrefcount(val)
        assert_(before == after)

    def generic_overwrite(self,key,val):
        a = {}
        overwritten = 1
        a[key] = overwritten  # put an item in the dict to be overwritten
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        # call once to handle mysterious addition of one ref count
        # on first call to inline.
        before_overwritten = sys.getrefcount(overwritten)
        inline_tools.inline("a[key] = val;",['a','key','val'])
<<<<<<< HEAD
        assert a[key] == val
        before = sys.getrefcount(a), sys.getrefcount(key), sys.getrefcount(val)
        inline_tools.inline("a[key] = val;",['a','key','val'])
        assert a[key] == val
        after = sys.getrefcount(a), sys.getrefcount(key), sys.getrefcount(val)
        after_overwritten = sys.getrefcount(overwritten)
        assert before == after
        assert before_overwritten == after_overwritten
=======
        assert_(a[key] == val)
        before = sys.getrefcount(a), sys.getrefcount(key), sys.getrefcount(val)
        inline_tools.inline("a[key] = val;",['a','key','val'])
        assert_(a[key] == val)
        after = sys.getrefcount(a), sys.getrefcount(key), sys.getrefcount(val)
        after_overwritten = sys.getrefcount(overwritten)
        assert_(before == after)
        assert_(before_overwritten == after_overwritten)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_new_int_int(self):
        key,val = 1234,12345
        self.generic_new(key,val)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_new_double_int(self):
        key,val = 1234.,12345
        self.generic_new(key,val)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_new_std_string_int(self):
        key,val = "hello",12345
        self.generic_new(key,val)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_new_complex_int(self):
        key,val = 1+1j,12345
        self.generic_new(key,val)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_new_obj_int(self):
        class Foo:
            pass
        key,val = Foo(),12345
        self.generic_new(key,val)

    @dec.slow
    def test_overwrite_int_int(self):
        key,val = 1234,12345
        self.generic_overwrite(key,val)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_overwrite_double_int(self):
        key,val = 1234.,12345
        self.generic_overwrite(key,val)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_overwrite_std_string_int(self):
        key,val = "hello",12345
        self.generic_overwrite(key,val)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_overwrite_complex_int(self):
        key,val = 1+1j,12345
        self.generic_overwrite(key,val)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_overwrite_obj_int(self):
        class Foo:
            pass
        key,val = Foo(),12345
        self.generic_overwrite(key,val)

<<<<<<< HEAD
class TestDictDel(TestCase):
=======

class TestDictDel(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    def generic(self,key):
        # test that value is set correctly and that reference counts
        # on dict, key, are being handled correctly. after deletion,
        # the keys refcount should be one less than before.
        a = {}
        a[key] = 1
        inline_tools.inline("a.del(key);",['a','key'])
<<<<<<< HEAD
        assert key not in a
        a[key] = 1
        before = sys.getrefcount(a), sys.getrefcount(key)
        inline_tools.inline("a.del(key);",['a','key'])
        assert key not in a
        after = sys.getrefcount(a), sys.getrefcount(key)
        assert before[0] == after[0]
        assert before[1] == after[1] + 1
=======
        assert_(key not in a)
        a[key] = 1
        before = sys.getrefcount(a), sys.getrefcount(key)
        inline_tools.inline("a.del(key);",['a','key'])
        assert_(key not in a)
        after = sys.getrefcount(a), sys.getrefcount(key)
        assert_(before[0] == after[0])
        assert_(before[1] == after[1] + 1)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_int(self):
        key = 1234
        self.generic(key)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_double(self):
        key = 1234.
        self.generic(key)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_std_string(self):
        key = "hello"
        self.generic(key)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_complex(self):
        key = 1+1j
        self.generic(key)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_obj(self):
        class Foo:
            pass
        key = Foo()
        self.generic(key)

<<<<<<< HEAD
class TestDictOthers(TestCase):
=======

class TestDictOthers(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_clear(self):
        a = {}
        a["hello"] = 1
        inline_tools.inline("a.clear();",['a'])
<<<<<<< HEAD
        assert not a
=======
        assert_(not a)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_items(self):
        a = {}
        a["hello"] = 1
        items = inline_tools.inline("return_val = a.items();",['a'])
<<<<<<< HEAD
        assert items == a.items()
=======
        assert_(items == a.items())

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_values(self):
        a = {}
        a["hello"] = 1
        values = inline_tools.inline("return_val = a.values();",['a'])
<<<<<<< HEAD
        assert values == a.values()
=======
        assert_(values == a.values())

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_keys(self):
        a = {}
        a["hello"] = 1
        keys = inline_tools.inline("return_val = a.keys();",['a'])
<<<<<<< HEAD
        assert keys == a.keys()
=======
        assert_(keys == a.keys())

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_update(self):
        a,b = {},{}
        a["hello"] = 1
        b["hello"] = 2
        inline_tools.inline("a.update(b);",['a','b'])
<<<<<<< HEAD
        assert a == b

if __name__ == "__main__":
    import nose
    nose.run(argv=['', __file__])
=======
        assert_(a == b)


if __name__ == "__main__":
    run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
