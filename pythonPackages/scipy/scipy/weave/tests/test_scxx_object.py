""" Test refcounting and behavior of SCXX.
"""
<<<<<<< HEAD

import sys

import nose
from numpy.testing import *

from scipy.weave import inline_tools

=======
from __future__ import absolute_import, print_function

import sys
from UserList import UserList

from numpy.testing import (TestCase, assert_equal, assert_, assert_raises,
                           run_module_suite)

from scipy.weave import inline_tools

from weave_test_utils import debug_print, dec

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

class TestObjectConstruct(TestCase):
    #------------------------------------------------------------------------
    # Check that construction from basic types is allowed and have correct
    # reference counts
    #------------------------------------------------------------------------
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_int(self):
        # strange int value used to try and make sure refcount is 2.
        code = """
               py::object val = 1001;
               return_val = val;
               """
        res = inline_tools.inline(code)
        assert_equal(sys.getrefcount(res),2)
        assert_equal(res,1001)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_float(self):
        code = """
               py::object val = (float)1.0;
               return_val = val;
               """
        res = inline_tools.inline(code)
        assert_equal(sys.getrefcount(res),2)
        assert_equal(res,1.0)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_double(self):
        code = """
               py::object val = 1.0;
               return_val = val;
               """
        res = inline_tools.inline(code)
        assert_equal(sys.getrefcount(res),2)
        assert_equal(res,1.0)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_complex(self):
        code = """
               std::complex<double> num = std::complex<double>(1.0,1.0);
               py::object val = num;
               return_val = val;
               """
        res = inline_tools.inline(code)
        assert_equal(sys.getrefcount(res),2)
        assert_equal(res,1.0+1.0j)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_string(self):
        code = """
               py::object val = "hello";
               return_val = val;
               """
        res = inline_tools.inline(code)
        assert_equal(sys.getrefcount(res),2)
        assert_equal(res,"hello")

    @dec.slow
    def test_std_string(self):
        code = """
               std::string s = std::string("hello");
               py::object val = s;
               return_val = val;
               """
        res = inline_tools.inline(code)
        assert_equal(sys.getrefcount(res),2)
        assert_equal(res,"hello")

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
class TestObjectPrint(TestCase):
    #------------------------------------------------------------------------
    # Check the object print protocol.
    #------------------------------------------------------------------------
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_stringio(self):
        import cStringIO
        file_imposter = cStringIO.StringIO()
        code = """
               py::object val = "how now brown cow";
               val.print(file_imposter);
               """
        res = inline_tools.inline(code,['file_imposter'])
<<<<<<< HEAD
        print file_imposter.getvalue()
=======
        debug_print(file_imposter.getvalue())
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        assert_equal(file_imposter.getvalue(),"'how now brown cow'")

##    @dec.slow
##    def test_failure(self):
##        code = """
##               FILE* file = 0;
##               py::object val = "how now brown cow";
##               val.print(file);
##               """
##        try:
##            res = inline_tools.inline(code)
##        except:
##            # error was supposed to occur.
##            pass


class TestObjectCast(TestCase):
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_int_cast(self):
        code = """
               py::object val = 1;
               int raw_val __attribute__ ((unused)) = val;
               """
        inline_tools.inline(code)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_double_cast(self):
        code = """
               py::object val = 1.0;
               double raw_val __attribute__ ((unused)) = val;
               """
        inline_tools.inline(code)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_float_cast(self):
        code = """
               py::object val = 1.0;
               float raw_val __attribute__ ((unused)) = val;
               """
        inline_tools.inline(code)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_complex_cast(self):
        code = """
               std::complex<double> num = std::complex<double>(1.0, 1.0);
               py::object val = num;
               std::complex<double> raw_val __attribute__ ((unused)) = val;
               """
        inline_tools.inline(code)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_string_cast(self):
        code = """
               py::object val = "hello";
               std::string raw_val __attribute__ ((unused)) = val;
               """
        inline_tools.inline(code)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
# test class used for testing python class access from C++.
class Foo:
    def bar(self):
        return "bar results"
<<<<<<< HEAD
    def bar2(self,val1,val2):
        return val1, val2
    def bar3(self,val1,val2,val3=1):
        return val1, val2, val3

#class StrObj:
#    def __str__(self):
#        return "b"

class TestObjectHasattr(TestCase):
=======

    def bar2(self,val1,val2):
        return val1, val2

    def bar3(self,val1,val2,val3=1):
        return val1, val2, val3

# class StrObj:
#    def __str__(self):
#        return "b"


class TestObjectHasattr(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_string(self):
        a = Foo()
        a.b = 12345
        code = """
               return_val = a.hasattr("b");
               """
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res
=======
        assert_(res)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_std_string(self):
        a = Foo()
        a.b = 12345
        attr_name = "b"
        code = """
               return_val = a.hasattr(attr_name);
               """
        res = inline_tools.inline(code,['a','attr_name'])
<<<<<<< HEAD
        assert res
=======
        assert_(res)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_string_fail(self):
        a = Foo()
        a.b = 12345
        code = """
               return_val = a.hasattr("c");
               """
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert not res
    @dec.slow
    def test_inline(self):
        """ THIS NEEDS TO MOVE TO THE INLINE TEST SUITE
        """
=======
        assert_(not res)

    @dec.slow
    def test_inline(self):
        #TODO: THIS NEEDS TO MOVE TO THE INLINE TEST SUITE
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        a = Foo()
        a.b = 12345
        code = """
               throw_error(PyExc_AttributeError,"bummer");
               """
        try:
            before = sys.getrefcount(a)
<<<<<<< HEAD
            res = inline_tools.inline(code,['a'])
        except AttributeError:
            after = sys.getrefcount(a)
            try:
                res = inline_tools.inline(code,['a'])
            except:
                after2 = sys.getrefcount(a)
            print "after and after2 should be equal in the following"
            print 'before, after, after2:', before, after, after2
            pass
=======
            inline_tools.inline(code,['a'])
        except AttributeError:
            after = sys.getrefcount(a)
            try:
                inline_tools.inline(code,['a'])
            except:
                after2 = sys.getrefcount(a)

            debug_print("after and after2 should be equal in the following")
            debug_print('before, after, after2:', before, after, after2)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_func(self):
        a = Foo()
        a.b = 12345
        code = """
               return_val = a.hasattr("bar");
               """
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res
=======
        assert_(res)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

class TestObjectAttr(TestCase):

    def generic_attr(self,code,args=['a']):
        a = Foo()
        a.b = 12345

        before = sys.getrefcount(a.b)
        res = inline_tools.inline(code,args)
        assert_equal(res,a.b)
        del res
        after = sys.getrefcount(a.b)
        assert_equal(after,before)

    @dec.slow
    def test_char(self):
        self.generic_attr('return_val = a.attr("b");')

    @dec.slow
    def test_char_fail(self):
<<<<<<< HEAD
        try:
            self.generic_attr('return_val = a.attr("c");')
        except AttributeError:
            pass
=======
        assert_raises(AttributeError, self.generic_attr, 'return_val = a.attr("c");')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_string(self):
        self.generic_attr('return_val = a.attr(std::string("b"));')

    @dec.slow
    def test_string_fail(self):
<<<<<<< HEAD
        try:
            self.generic_attr('return_val = a.attr(std::string("c"));')
        except AttributeError:
            pass
=======
        assert_raises(AttributeError, self.generic_attr, 'return_val = a.attr(std::string("c"));')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_obj(self):
        code = """
               py::object name = "b";
               return_val = a.attr(name);
               """
        self.generic_attr(code,['a'])

    @dec.slow
    def test_obj_fail(self):
<<<<<<< HEAD
        try:
            code = """
                   py::object name = "c";
                   return_val = a.attr(name);
                   """
            self.generic_attr(code,['a'])
        except AttributeError:
            pass
=======
        code = """
               py::object name = "c";
               return_val = a.attr(name);
               """
        assert_raises(AttributeError, self.generic_attr, code, ['a'])
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_attr_call(self):
        a = Foo()
        res = inline_tools.inline('return_val = a.attr("bar").call();',['a'])
        first = sys.getrefcount(res)
        del res
        res = inline_tools.inline('return_val = a.attr("bar").call();',['a'])
        second = sys.getrefcount(res)
        assert_equal(res,"bar results")
        assert_equal(first,second)

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
class TestObjectSetAttr(TestCase):

    def generic_existing(self, code, desired):
        args = ['a']
        a = Foo()
        a.b = 12345
<<<<<<< HEAD
        res = inline_tools.inline(code,args)
=======
        inline_tools.inline(code,args)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        assert_equal(a.b,desired)

    def generic_new(self, code, desired):
        args = ['a']
        a = Foo()
<<<<<<< HEAD
        res = inline_tools.inline(code,args)
=======
        inline_tools.inline(code,args)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        assert_equal(a.b,desired)

    @dec.slow
    def test_existing_char(self):
        self.generic_existing('a.set_attr("b","hello");',"hello")
<<<<<<< HEAD
    @dec.slow
    def test_new_char(self):
        self.generic_new('a.set_attr("b","hello");',"hello")
    @dec.slow
    def test_existing_string(self):
        self.generic_existing('a.set_attr("b",std::string("hello"));',"hello")
    @dec.slow
    def test_new_string(self):
        self.generic_new('a.set_attr("b",std::string("hello"));',"hello")
=======

    @dec.slow
    def test_new_char(self):
        self.generic_new('a.set_attr("b","hello");',"hello")

    @dec.slow
    def test_existing_string(self):
        self.generic_existing('a.set_attr("b",std::string("hello"));',"hello")

    @dec.slow
    def test_new_string(self):
        self.generic_new('a.set_attr("b",std::string("hello"));',"hello")

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_existing_object(self):
        code = """
               py::object obj = "hello";
               a.set_attr("b",obj);
               """
        self.generic_existing(code,"hello")
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_new_object(self):
        code = """
               py::object obj = "hello";
               a.set_attr("b",obj);
               """
        self.generic_new(code,"hello")
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_new_fail(self):
        try:
            code = """
                   py::object obj = 1;
                   a.set_attr(obj,"hello");
                   """
            self.generic_new(code,"hello")
        except:
            pass

    @dec.slow
    def test_existing_int(self):
        self.generic_existing('a.set_attr("b",1);',1)
<<<<<<< HEAD
    @dec.slow
    def test_existing_double(self):
        self.generic_existing('a.set_attr("b",1.0);',1.0)
=======

    @dec.slow
    def test_existing_double(self):
        self.generic_existing('a.set_attr("b",1.0);',1.0)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_existing_complex(self):
        code = """
               std::complex<double> obj = std::complex<double>(1,1);
               a.set_attr("b",obj);
               """
        self.generic_existing(code,1+1j)
<<<<<<< HEAD
    @dec.slow
    def test_existing_char1(self):
        self.generic_existing('a.set_attr("b","hello");',"hello")
=======

    @dec.slow
    def test_existing_char1(self):
        self.generic_existing('a.set_attr("b","hello");',"hello")

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_existing_string1(self):
        code = """
               std::string obj = std::string("hello");
               a.set_attr("b",obj);
               """
        self.generic_existing(code,"hello")

<<<<<<< HEAD
class TestObjectDel(TestCase):
=======

class TestObjectDel(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    def generic(self, code):
        args = ['a']
        a = Foo()
        a.b = 12345
<<<<<<< HEAD
        res = inline_tools.inline(code,args)
        assert not hasattr(a,"b")
=======
        inline_tools.inline(code,args)
        assert_(not hasattr(a,"b"))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_char(self):
        self.generic('a.del("b");')
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_string(self):
        code = """
               std::string name = std::string("b");
               a.del(name);
               """
        self.generic(code)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_object(self):
        code = """
               py::object name = py::object("b");
               a.del(name);
               """
        self.generic(code)

<<<<<<< HEAD
class TestObjectCmp(TestCase):
=======

class TestObjectCmp(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_equal(self):
        a,b = 1,1
        res = inline_tools.inline('return_val = (a == b);',['a','b'])
        assert_equal(res,(a == b))
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_equal_objects(self):
        class Foo:
            def __init__(self,x):
                self.x = x
<<<<<<< HEAD
            def __cmp__(self,other):
                return cmp(self.x,other.x)
        a,b = Foo(1),Foo(2)
        res = inline_tools.inline('return_val = (a == b);',['a','b'])
        assert_equal(res,(a == b))
=======

            def __cmp__(self,other):
                return cmp(self.x,other.x)

        a,b = Foo(1),Foo(2)
        res = inline_tools.inline('return_val = (a == b);',['a','b'])
        assert_equal(res,(a == b))

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_lt(self):
        a,b = 1,2
        res = inline_tools.inline('return_val = (a < b);',['a','b'])
        assert_equal(res,(a < b))
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_gt(self):
        a,b = 1,2
        res = inline_tools.inline('return_val = (a > b);',['a','b'])
        assert_equal(res,(a > b))
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_gte(self):
        a,b = 1,2
        res = inline_tools.inline('return_val = (a >= b);',['a','b'])
        assert_equal(res,(a >= b))
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_lte(self):
        a,b = 1,2
        res = inline_tools.inline('return_val = (a <= b);',['a','b'])
        assert_equal(res,(a <= b))
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_not_equal(self):
        a,b = 1,2
        res = inline_tools.inline('return_val = (a != b);',['a','b'])
        assert_equal(res,(a != b))
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_int(self):
        a = 1
        res = inline_tools.inline('return_val = (a == 1);',['a'])
        assert_equal(res,(a == 1))
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_int2(self):
        a = 1
        res = inline_tools.inline('return_val = (1 == a);',['a'])
        assert_equal(res,(a == 1))
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_unsigned_long(self):
        a = 1
        res = inline_tools.inline('return_val = (a == (unsigned long)1);',['a'])
        assert_equal(res,(a == 1))
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_double(self):
        a = 1
        res = inline_tools.inline('return_val = (a == 1.0);',['a'])
        assert_equal(res,(a == 1.0))
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_char(self):
        a = "hello"
        res = inline_tools.inline('return_val = (a == "hello");',['a'])
        assert_equal(res,(a == "hello"))
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_std_string(self):
        a = "hello"
        code = """
               std::string hello = std::string("hello");
               return_val = (a == hello);
               """
        res = inline_tools.inline(code,['a'])
        assert_equal(res,(a == "hello"))

<<<<<<< HEAD
class TestObjectRepr(TestCase):
=======

class TestObjectRepr(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_repr(self):
        class Foo:
            def __str__(self):
                return "str return"
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            def __repr__(self):
                return "repr return"
        a = Foo()
        res = inline_tools.inline('return_val = a.repr();',['a'])
        first = sys.getrefcount(res)
        del res
        res = inline_tools.inline('return_val = a.repr();',['a'])
        second = sys.getrefcount(res)
        assert_equal(first,second)
        assert_equal(res,"repr return")

<<<<<<< HEAD
class TestObjectStr(TestCase):
=======

class TestObjectStr(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_str(self):
        class Foo:
            def __str__(self):
                return "str return"
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
            def __repr__(self):
                return "repr return"
        a = Foo()
        res = inline_tools.inline('return_val = a.str();',['a'])
        first = sys.getrefcount(res)
        del res
        res = inline_tools.inline('return_val = a.str();',['a'])
        second = sys.getrefcount(res)
        assert_equal(first,second)
<<<<<<< HEAD
        print res
        assert_equal(res,"str return")

class TestObjectUnicode(TestCase):
    # This ain't going to win awards for test of the year...
    @dec.slow
    def test_unicode(self):
        class Foo:
            def __repr__(self):
                return "repr return"
            def __str__(self):
                return "unicode"
        a= Foo()
=======
        assert_equal(res,"str return")


class TestObjectUnicode(TestCase):

    # This ain't going to win awards for test of the year...

    @dec.slow
    def test_unicode(self):
        class Foo:

            def __repr__(self):
                return "repr return"

            def __str__(self):
                return "unicode"
        a = Foo()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        res = inline_tools.inline('return_val = a.unicode();',['a'])
        first = sys.getrefcount(res)
        del res
        res = inline_tools.inline('return_val = a.unicode();',['a'])
        second = sys.getrefcount(res)
        assert_equal(first,second)
        assert_equal(res,"unicode")

<<<<<<< HEAD
class TestObjectIsCallable(TestCase):
=======

class TestObjectIsCallable(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_true(self):
        class Foo:
            def __call__(self):
                return 0
<<<<<<< HEAD
        a= Foo()
        res = inline_tools.inline('return_val = a.is_callable();',['a'])
        assert res
=======

        a = Foo()
        res = inline_tools.inline('return_val = a.is_callable();',['a'])
        assert_(res)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_false(self):
        class Foo:
            pass
<<<<<<< HEAD
        a= Foo()
        res = inline_tools.inline('return_val = a.is_callable();',['a'])
        assert not res

class TestObjectCall(TestCase):
=======

        a = Foo()
        res = inline_tools.inline('return_val = a.is_callable();',['a'])
        assert_(not res)


class TestObjectCall(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_noargs(self):
        def Foo():
            return (1,2,3)
        res = inline_tools.inline('return_val = Foo.call();',['Foo'])
        assert_equal(res,(1,2,3))
<<<<<<< HEAD
        assert_equal(sys.getrefcount(res),3) # should be 2?
=======
        assert_equal(sys.getrefcount(res),3)  # should be 2?

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_args(self):
        def Foo(val1,val2):
            return (val1,val2)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        code = """
               py::tuple args(2);
               args[0] = 1;
               args[1] = "hello";
               return_val = Foo.call(args);
               """
        res = inline_tools.inline(code,['Foo'])
        assert_equal(res,(1,"hello"))
        assert_equal(sys.getrefcount(res),2)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_args_kw(self):
        def Foo(val1,val2,val3=1):
            return (val1,val2,val3)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        code = """
               py::tuple args(2);
               args[0] = 1;
               args[1] = "hello";
               py::dict kw;
               kw["val3"] = 3;
               return_val = Foo.call(args,kw);
               """
        res = inline_tools.inline(code,['Foo'])
        assert_equal(res,(1,"hello",3))
        assert_equal(sys.getrefcount(res),2)
<<<<<<< HEAD
    @dec.slow
    def test_noargs_with_args(self):
        # calling a function that does take args with args
        # should fail.
        def Foo():
            return "blah"
=======

    @dec.slow
    def test_noargs_with_args_not_instantiated(self):
        # calling a function that doesn't take args with args should fail.
        # Note: difference between this test add ``test_noargs_with_args``
        # below is that here Foo is not instantiated.
        def Foo():
            return "blah"

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        code = """
               py::tuple args(2);
               args[0] = 1;
               args[1] = "hello";
               return_val = Foo.call(args);
               """
        try:
            first = sys.getrefcount(Foo)
<<<<<<< HEAD
            res = inline_tools.inline(code,['Foo'])
        except TypeError:
            second = sys.getrefcount(Foo)
            try:
                res = inline_tools.inline(code,['Foo'])
            except TypeError:
                third = sys.getrefcount(Foo)
        # first should == second, but the weird refcount error
        assert_equal(second,third)

class TestObjectMcall(TestCase):
=======
            inline_tools.inline(code,['Foo'])
        except TypeError:
            second = sys.getrefcount(Foo)
            try:
                inline_tools.inline(code,['Foo'])
            except TypeError:
                third = sys.getrefcount(Foo)

        # first should == second, but the weird refcount error
        assert_equal(second,third)


class TestObjectMcall(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_noargs(self):
        a = Foo()
        res = inline_tools.inline('return_val = a.mcall("bar");',['a'])
        assert_equal(res,"bar results")
        first = sys.getrefcount(res)
        del res
        res = inline_tools.inline('return_val = a.mcall("bar");',['a'])
        assert_equal(res,"bar results")
        second = sys.getrefcount(res)
        assert_equal(first,second)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_args(self):
        a = Foo()
        code = """
               py::tuple args(2);
               args[0] = 1;
               args[1] = "hello";
               return_val = a.mcall("bar2",args);
               """
        res = inline_tools.inline(code,['a'])
        assert_equal(res,(1,"hello"))
        assert_equal(sys.getrefcount(res),2)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_args_kw(self):
        a = Foo()
        code = """
               py::tuple args(2);
               args[0] = 1;
               args[1] = "hello";
               py::dict kw;
               kw["val3"] = 3;
               return_val = a.mcall("bar3",args,kw);
               """
        res = inline_tools.inline(code,['a'])
        assert_equal(res,(1,"hello",3))
        assert_equal(sys.getrefcount(res),2)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_std_noargs(self):
        a = Foo()
        method = "bar"
        res = inline_tools.inline('return_val = a.mcall(method);',['a','method'])
        assert_equal(res,"bar results")
        first = sys.getrefcount(res)
        del res
        res = inline_tools.inline('return_val = a.mcall(method);',['a','method'])
        assert_equal(res,"bar results")
        second = sys.getrefcount(res)
        assert_equal(first,second)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_std_args(self):
        a = Foo()
        method = "bar2"
        code = """
               py::tuple args(2);
               args[0] = 1;
               args[1] = "hello";
               return_val = a.mcall(method,args);
               """
        res = inline_tools.inline(code,['a','method'])
        assert_equal(res,(1,"hello"))
        assert_equal(sys.getrefcount(res),2)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_std_args_kw(self):
        a = Foo()
        method = "bar3"
        code = """
               py::tuple args(2);
               args[0] = 1;
               args[1] = "hello";
               py::dict kw;
               kw["val3"] = 3;
               return_val = a.mcall(method,args,kw);
               """
        res = inline_tools.inline(code,['a','method'])
        assert_equal(res,(1,"hello",3))
        assert_equal(sys.getrefcount(res),2)
<<<<<<< HEAD
    @dec.slow
    def test_noargs_with_args(self):
        # calling a function that does take args with args
        # should fail.
=======

    @dec.slow
    def test_noargs_with_args(self):
        # calling a function that doesn't take args with args should fail.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        a = Foo()
        code = """
               py::tuple args(2);
               args[0] = 1;
               args[1] = "hello";
               return_val = a.mcall("bar",args);
               """
        try:
            first = sys.getrefcount(a)
<<<<<<< HEAD
            res = inline_tools.inline(code,['a'])
        except TypeError:
            second = sys.getrefcount(a)
            try:
                res = inline_tools.inline(code,['a'])
            except TypeError:
                third = sys.getrefcount(a)
        # first should == second, but the weird refcount error
        assert_equal(second,third)

class TestObjectHash(TestCase):
=======
            inline_tools.inline(code,['a'])
        except TypeError:
            second = sys.getrefcount(a)
            try:
                inline_tools.inline(code,['a'])
            except TypeError:
                third = sys.getrefcount(a)

        # first should == second, but the weird refcount error
        assert_equal(second,third)


class TestObjectHash(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_hash(self):
        class Foo:
            def __hash__(self):
                return 123
<<<<<<< HEAD
        a= Foo()
        res = inline_tools.inline('return_val = a.hash(); ',['a'])
        print 'hash:', res
        assert_equal(res,123)

class TestObjectIsTrue(TestCase):
=======

        a = Foo()
        res = inline_tools.inline('return_val = a.hash(); ',['a'])
        debug_print('hash:', res)
        assert_equal(res,123)


class TestObjectIsTrue(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_true(self):
        class Foo:
            pass
<<<<<<< HEAD
        a= Foo()
        res = inline_tools.inline('return_val = a.is_true();',['a'])
        assert_equal(res,1)
    @dec.slow
    def test_false(self):
        a= None
        res = inline_tools.inline('return_val = a.is_true();',['a'])
        assert_equal(res,0)

class TestObjectType(TestCase):
=======

        a = Foo()
        res = inline_tools.inline('return_val = a.is_true();',['a'])
        assert_equal(res,1)

    @dec.slow
    def test_false(self):
        a = None
        res = inline_tools.inline('return_val = a.is_true();',['a'])
        assert_equal(res,0)


class TestObjectType(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_type(self):
        class Foo:
            pass
<<<<<<< HEAD
        a= Foo()
        res = inline_tools.inline('return_val = a.type();',['a'])
        assert_equal(res,type(a))

class TestObjectSize(TestCase):
=======

        a = Foo()
        res = inline_tools.inline('return_val = a.type();',['a'])
        assert_equal(res,type(a))


class TestObjectSize(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_size(self):
        class Foo:
            def __len__(self):
                return 10
<<<<<<< HEAD
        a= Foo()
        res = inline_tools.inline('return_val = a.size();',['a'])
        assert_equal(res,len(a))
=======

        a = Foo()
        res = inline_tools.inline('return_val = a.size();',['a'])
        assert_equal(res,len(a))

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_len(self):
        class Foo:
            def __len__(self):
                return 10
<<<<<<< HEAD
        a= Foo()
        res = inline_tools.inline('return_val = a.len();',['a'])
        assert_equal(res,len(a))
=======

        a = Foo()
        res = inline_tools.inline('return_val = a.len();',['a'])
        assert_equal(res,len(a))

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_length(self):
        class Foo:
            def __len__(self):
                return 10
<<<<<<< HEAD
        a= Foo()
        res = inline_tools.inline('return_val = a.length();',['a'])
        assert_equal(res,len(a))

from UserList import UserList
class TestObjectSetItemOpIndex(TestCase):
=======

        a = Foo()
        res = inline_tools.inline('return_val = a.length();',['a'])
        assert_equal(res,len(a))


class TestObjectSetItemOpIndex(TestCase):

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_list_refcount(self):
        a = UserList([1,2,3])
        # temporary refcount fix until I understand why it incs by one.
        inline_tools.inline("a[1] = 1234;",['a'])
        before1 = sys.getrefcount(a)
        after1 = sys.getrefcount(a)
        assert_equal(after1,before1)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_set_int(self):
        a = UserList([1,2,3])
        inline_tools.inline("a[1] = 1234;",['a'])
        assert_equal(sys.getrefcount(a[1]),2)
        assert_equal(a[1],1234)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_set_double(self):
        a = UserList([1,2,3])
        inline_tools.inline("a[1] = 123.0;",['a'])
        assert_equal(sys.getrefcount(a[1]),2)
        assert_equal(a[1],123.0)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_set_char(self):
        a = UserList([1,2,3])
        inline_tools.inline('a[1] = "bubba";',['a'])
        assert_equal(sys.getrefcount(a[1]),2)
        assert_equal(a[1],'bubba')
<<<<<<< HEAD
    @dec.slow
    def test_set_string(self):
=======

    @dec.slow
    def test_set_string1(self):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        a = UserList([1,2,3])
        inline_tools.inline('a[1] = std::string("sissy");',['a'])
        assert_equal(sys.getrefcount(a[1]),2)
        assert_equal(a[1],'sissy')
<<<<<<< HEAD
    @dec.slow
    def test_set_string(self):
=======

    @dec.slow
    def test_set_string2(self):
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        a = UserList([1,2,3])
        inline_tools.inline('a[1] = std::complex<double>(1,1);',['a'])
        assert_equal(sys.getrefcount(a[1]),2)
        assert_equal(a[1],1+1j)

<<<<<<< HEAD
from UserDict import UserDict
class TestObjectSetItemOpKey(TestCase):
    @dec.slow
    def test_key_refcount(self):
        a = UserDict()
        code =  """
=======

from UserDict import UserDict


class TestObjectSetItemOpKey(TestCase):

    @dec.slow
    def test_key_refcount(self):
        a = UserDict()
        code = """
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
                py::object one = 1;
                py::object two = 2;
                py::tuple ref_counts(3);
                py::tuple obj_counts(3);
                py::tuple val_counts(3);
                py::tuple key_counts(3);
                obj_counts[0] = a.refcount();
                key_counts[0] = one.refcount();
                val_counts[0] = two.refcount();
                a[1] = 2;
                obj_counts[1] = a.refcount();
                key_counts[1] = one.refcount();
                val_counts[1] = two.refcount();
                a[1] = 2;
                obj_counts[2] = a.refcount();
                key_counts[2] = one.refcount();
                val_counts[2] = two.refcount();

                ref_counts[0] = obj_counts;
                ref_counts[1] = key_counts;
                ref_counts[2] = val_counts;
                return_val = ref_counts;
                """
        obj,key,val = inline_tools.inline(code,['a'])
        assert_equal(obj[0],obj[1])
        assert_equal(obj[1],obj[2])
        assert_equal(key[0] + 1, key[1])
        assert_equal(key[1], key[2])
        assert_equal(val[0] + 1, val[1])
        assert_equal(val[1], val[2])

    @dec.slow
    def test_set_double_exists(self):
        a = UserDict()
        key = 10.0
        a[key] = 100.0
        inline_tools.inline('a[key] = 123.0;',['a','key'])
        first = sys.getrefcount(key)
        inline_tools.inline('a[key] = 123.0;',['a','key'])
        second = sys.getrefcount(key)
        assert_equal(first,second)
        # !! I think the following should be 3
        assert_equal(sys.getrefcount(key),5)
        assert_equal(sys.getrefcount(a[key]),2)
        assert_equal(a[key],123.0)
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_set_double_new(self):
        a = UserDict()
        key = 1.0
        inline_tools.inline('a[key] = 123.0;',['a','key'])
<<<<<<< HEAD
        assert_equal(sys.getrefcount(key),4) # should be 3
        assert_equal(sys.getrefcount(a[key]),2)
        assert_equal(a[key],123.0)
=======
        assert_equal(sys.getrefcount(key),4)  # should be 3
        assert_equal(sys.getrefcount(a[key]),2)
        assert_equal(a[key],123.0)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_set_complex(self):
        a = UserDict()
        key = 1+1j
        inline_tools.inline("a[key] = 1234;",['a','key'])
<<<<<<< HEAD
        assert_equal(sys.getrefcount(key),4) # should be 3
        assert_equal(sys.getrefcount(a[key]),2)
        assert_equal(a[key],1234)
=======
        assert_equal(sys.getrefcount(key),4)  # should be 3
        assert_equal(sys.getrefcount(a[key]),2)
        assert_equal(a[key],1234)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_set_char(self):
        a = UserDict()
        inline_tools.inline('a["hello"] = 123.0;',['a'])
        assert_equal(sys.getrefcount(a["hello"]),2)
        assert_equal(a["hello"],123.0)

    @dec.slow
    def test_set_class(self):
        a = UserDict()
<<<<<<< HEAD
        class Foo:
            def __init__(self,val):
                self.val = val
            def __hash__(self):
                return self.val
=======

        class Foo:
            def __init__(self,val):
                self.val = val

            def __hash__(self):
                return self.val

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        key = Foo(4)
        inline_tools.inline('a[key] = "bubba";',['a','key'])
        first = sys.getrefcount(key)
        inline_tools.inline('a[key] = "bubba";',['a','key'])
        second = sys.getrefcount(key)
        # I don't think we're leaking if this is true
        assert_equal(first,second)
        # !! BUT -- I think this should be 3
        assert_equal(sys.getrefcount(key),4)
        assert_equal(sys.getrefcount(a[key]),2)
        assert_equal(a[key],'bubba')
<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_set_from_member(self):
        a = UserDict()
        a['first'] = 1
        a['second'] = 2
        inline_tools.inline('a["first"] = a["second"];',['a'])
        assert_equal(a['first'],a['second'])

<<<<<<< HEAD
if __name__ == "__main__":
    import nose
    nose.run(argv=['', __file__])
=======

if __name__ == "__main__":
    run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
