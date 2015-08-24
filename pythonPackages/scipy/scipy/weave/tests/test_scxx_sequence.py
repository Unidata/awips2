""" Test refcounting and behavior of SCXX.
"""
<<<<<<< HEAD
=======
from __future__ import absolute_import, print_function
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

import time
import sys

<<<<<<< HEAD
from numpy.testing import *

from scipy.weave import inline_tools


# Test:
#     append            DONE
#     insert            DONE
#     in                DONE
#     count             DONE
#     setItem           DONE
#     operator[] (get)
#     operator[] (set)  DONE
=======
from numpy.testing import (TestCase, assert_, assert_raises,
                           run_module_suite)

from scipy.weave import inline_tools

from weave_test_utils import debug_print, dec

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

class _TestSequenceBase(TestCase):
    seq_type = None

    @dec.slow
    def test_conversion(self):
        a = self.seq_type([1])
        before = sys.getrefcount(a)
        inline_tools.inline(" ",['a'])
<<<<<<< HEAD
        #print 'first:',before
=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        # first call is goofing up refcount.
        before = sys.getrefcount(a)
        inline_tools.inline(" ",['a'])
        after = sys.getrefcount(a)
<<<<<<< HEAD
        #print '2nd,3rd:', before, after
        assert(after == before)

    @dec.slow
    def test_in(self):
        """ Test the "in" method for lists.  We'll assume
            it works for sequences if it works here.
        """
=======
        assert_(after == before)

    @dec.slow
    def test_in(self):
        # Test the "in" method for lists.  We'll assume it works for
        # sequences if it works here.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        a = self.seq_type([1,2,'alpha',3.1416])

        item = 1
        code = "return_val = a.in(item);"
        res = inline_tools.inline(code,['a','item'])
<<<<<<< HEAD
        assert res == 1
        item = 0
        res = inline_tools.inline(code,['a','item'])
        assert res == 0
=======
        assert_(res == 1)
        item = 0
        res = inline_tools.inline(code,['a','item'])
        assert_(res == 0)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # check overloaded in(int val) method
        code = "return_val = a.in(1);"
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res == 1
        code = "return_val = a.in(0);"
        res = inline_tools.inline(code,['a'])
        assert res == 0
=======
        assert_(res == 1)
        code = "return_val = a.in(0);"
        res = inline_tools.inline(code,['a'])
        assert_(res == 0)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # check overloaded in(double val) method
        code = "return_val = a.in(3.1416);"
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res == 1
        code = "return_val = a.in(3.1417);"
        res = inline_tools.inline(code,['a'])
        assert res == 0
=======
        assert_(res == 1)
        code = "return_val = a.in(3.1417);"
        res = inline_tools.inline(code,['a'])
        assert_(res == 0)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # check overloaded in(char* val) method
        code = 'return_val = a.in("alpha");'
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res == 1
        code = 'return_val = a.in("beta");'
        res = inline_tools.inline(code,['a'])
        assert res == 0
=======
        assert_(res == 1)
        code = 'return_val = a.in("beta");'
        res = inline_tools.inline(code,['a'])
        assert_(res == 0)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # check overloaded in(std::string val) method
        code = """
               std::string val = std::string("alpha");
               return_val = a.in(val);
               """
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res == 1
=======
        assert_(res == 1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        code = """
               std::string val = std::string("beta");
               return_val = a.in(val);
               """
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res == 0

    @dec.slow
    def test_count(self):
        """ Test the "count" method for lists.  We'll assume
            it works for sequences if it works hre.
        """
=======
        assert_(res == 0)

    @dec.slow
    def test_count(self):
        # Test the "count" method for lists.  We'll assume it works for
        # sequences if it works here.
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        a = self.seq_type([1,2,'alpha',3.1416])

        item = 1
        code = "return_val = a.count(item);"
        res = inline_tools.inline(code,['a','item'])
<<<<<<< HEAD
        assert res == 1
=======
        assert_(res == 1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # check overloaded count(int val) method
        code = "return_val = a.count(1);"
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res == 1
=======
        assert_(res == 1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # check overloaded count(double val) method
        code = "return_val = a.count(3.1416);"
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res == 1
=======
        assert_(res == 1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # check overloaded count(char* val) method
        code = 'return_val = a.count("alpha");'
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res == 1
=======
        assert_(res == 1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # check overloaded count(std::string val) method
        code = """
               std::string alpha = std::string("alpha");
               return_val = a.count(alpha);
               """
        res = inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert res == 1
=======
        assert_(res == 1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_access_speed(self):
        N = 1000000
<<<<<<< HEAD
        print '%s access -- val = a[i] for N =', (self.seq_type, N)
=======
        debug_print('%s access -- val = a[i] for N =', (self.seq_type, N))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        a = self.seq_type([0]) * N
        val = 0
        t1 = time.time()
        for i in xrange(N):
            val = a[i]
        t2 = time.time()
<<<<<<< HEAD
        print 'python1:', t2 - t1
=======
        debug_print('python1:', t2 - t1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        t1 = time.time()
        for i in a:
            val = i
        t2 = time.time()
<<<<<<< HEAD
        print 'python2:', t2 - t1
=======
        debug_print('python2:', t2 - t1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        code = """
               const int N = a.length();
               py::object val;
               for(int i=0; i < N; i++)
                   val = a[i];
               """
        # compile not included in timing
        inline_tools.inline(code,['a'])
        t1 = time.time()
        inline_tools.inline(code,['a'])
        t2 = time.time()
<<<<<<< HEAD
        print 'weave:', t2 - t1

# Fails
    @dec.slow
    def test_access_set_speed(self):
        N = 1000000
        print '%s access/set -- b[i] = a[i] for N =', (self.seq_type,N)
=======
        debug_print('weave:', t2 - t1)

    @dec.slow
    def test_access_set_speed(self):
        N = 1000000
        debug_print('%s access/set -- b[i] = a[i] for N =', (self.seq_type,N))
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        a = self.seq_type([0]) * N
        # b is always a list so we can assign to it.
        b = [1] * N
        t1 = time.time()
        for i in xrange(N):
            b[i] = a[i]
        t2 = time.time()
<<<<<<< HEAD
        print 'python:', t2 - t1
=======
        debug_print('python:', t2 - t1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        a = self.seq_type([0]) * N
        b = [1] * N
        code = """
               const int N = a.length();
               for(int i=0; i < N; i++)
                   b[i] = a[i];
               """
        # compile not included in timing
        inline_tools.inline(code,['a','b'])
        t1 = time.time()
        inline_tools.inline(code,['a','b'])
        t2 = time.time()
<<<<<<< HEAD
        print 'weave:', t2 - t1
        assert list(b) == list(a)
=======
        debug_print('weave:', t2 - t1)
        assert_(list(b) == list(a))

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

class TestTuple(_TestSequenceBase):
    seq_type = tuple

    @dec.slow
    def test_set_item_operator_equal_fail(self):
        # Tuples should only allow setting of variables
        # immediately after creation.
        a = (1,2,3)
<<<<<<< HEAD
        try:
            inline_tools.inline("a[1] = 1234;",['a'])
        except TypeError:
            pass
=======
        assert_raises(TypeError, inline_tools.inline, "a[1] = 1234;",['a'])

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_set_item_operator_equal(self):
        code = """
               py::tuple a(3);
               a[0] = 1;
               a[1] = 2;
               a[2] = 3;
               return_val = a;
               """
        a = inline_tools.inline(code)
<<<<<<< HEAD
        assert a == (1,2,3)
        # returned value should only have a single refcount
        assert sys.getrefcount(a) == 2
=======
        assert_(a == (1,2,3))
        # returned value should only have a single refcount
        assert_(sys.getrefcount(a) == 2)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_set_item_index_error(self):
        code = """
               py::tuple a(3);
               a[4] = 1;
               return_val = a;
               """
<<<<<<< HEAD
        try:
            a = inline_tools.inline(code)
            assert 0
        except IndexError:
            pass
=======
        assert_raises(IndexError, inline_tools.inline, code)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_get_item_operator_index_error(self):
        code = """
               py::tuple a(3);
               py::object b = a[4]; // should fail.
               """
<<<<<<< HEAD
        try:
            a = inline_tools.inline(code)
            assert 0
        except IndexError:
            pass

class TestList(_TestSequenceBase):
    seq_type = list
=======
        assert_raises(IndexError, inline_tools.inline, code)


class TestList(_TestSequenceBase):

    seq_type = list

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_append_passed_item(self):
        a = []
        item = 1

        # temporary refcount fix until I understand why it incs by one.
        inline_tools.inline("a.append(item);",['a','item'])
        del a[0]

        before1 = sys.getrefcount(a)
        before2 = sys.getrefcount(item)
        inline_tools.inline("a.append(item);",['a','item'])
<<<<<<< HEAD
        assert a[0] is item
        del a[0]
        after1 = sys.getrefcount(a)
        after2 = sys.getrefcount(item)
        assert after1 == before1
        assert after2 == before2
=======
        assert_(a[0] is item)
        del a[0]
        after1 = sys.getrefcount(a)
        after2 = sys.getrefcount(item)
        assert_(after1 == before1)
        assert_(after2 == before2)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_append(self):
        a = []
        # temporary refcount fix until I understand why it incs by one.
        inline_tools.inline("a.append(1);",['a'])
        del a[0]

        before1 = sys.getrefcount(a)

        # check overloaded append(int val) method
        inline_tools.inline("a.append(1234);",['a'])
<<<<<<< HEAD
        assert sys.getrefcount(a[0]) == 2
        assert a[0] == 1234
=======
        assert_(sys.getrefcount(a[0]) == 2)
        assert_(a[0] == 1234)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        del a[0]

        # check overloaded append(double val) method
        inline_tools.inline("a.append(123.0);",['a'])
<<<<<<< HEAD
        assert sys.getrefcount(a[0]) == 2
        assert a[0] == 123.0
=======
        assert_(sys.getrefcount(a[0]) == 2)
        assert_(a[0] == 123.0)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        del a[0]

        # check overloaded append(char* val) method
        inline_tools.inline('a.append("bubba");',['a'])
<<<<<<< HEAD
        assert sys.getrefcount(a[0]) == 2
        assert a[0] == 'bubba'
=======
        assert_(sys.getrefcount(a[0]) == 2)
        assert_(a[0] == 'bubba')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        del a[0]

        # check overloaded append(std::string val) method
        inline_tools.inline('a.append(std::string("sissy"));',['a'])
<<<<<<< HEAD
        assert sys.getrefcount(a[0]) == 2
        assert a[0] == 'sissy'
        del a[0]

        after1 = sys.getrefcount(a)
        assert after1 == before1
=======
        assert_(sys.getrefcount(a[0]) == 2)
        assert_(a[0] == 'sissy')
        del a[0]

        after1 = sys.getrefcount(a)
        assert_(after1 == before1)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_insert(self):
        a = [1,2,3]

        a.insert(1,234)
        del a[1]

        # temporary refcount fix until I understand why it incs by one.
        inline_tools.inline("a.insert(1,1234);",['a'])
        del a[1]

        before1 = sys.getrefcount(a)

        # check overloaded insert(int ndx, int val) method
        inline_tools.inline("a.insert(1,1234);",['a'])
<<<<<<< HEAD
        assert sys.getrefcount(a[1]) == 2
        assert a[1] == 1234
=======
        assert_(sys.getrefcount(a[1]) == 2)
        assert_(a[1] == 1234)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        del a[1]

        # check overloaded insert(int ndx, double val) method
        inline_tools.inline("a.insert(1,123.0);",['a'])
<<<<<<< HEAD
        assert sys.getrefcount(a[1]) == 2
        assert a[1] == 123.0
=======
        assert_(sys.getrefcount(a[1]) == 2)
        assert_(a[1] == 123.0)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        del a[1]

        # check overloaded insert(int ndx, char* val) method
        inline_tools.inline('a.insert(1,"bubba");',['a'])
<<<<<<< HEAD
        assert sys.getrefcount(a[1]) == 2
        assert a[1] == 'bubba'
=======
        assert_(sys.getrefcount(a[1]) == 2)
        assert_(a[1] == 'bubba')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        del a[1]

        # check overloaded insert(int ndx, std::string val) method
        inline_tools.inline('a.insert(1,std::string("sissy"));',['a'])
<<<<<<< HEAD
        assert sys.getrefcount(a[1]) == 2
        assert a[1] == 'sissy'
        del a[0]

        after1 = sys.getrefcount(a)
        assert after1 == before1
=======
        assert_(sys.getrefcount(a[1]) == 2)
        assert_(a[1] == 'sissy')
        del a[0]

        after1 = sys.getrefcount(a)
        assert_(after1 == before1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_set_item_operator_equal(self):
        a = self.seq_type([1,2,3])
        # temporary refcount fix until I understand why it incs by one.
        inline_tools.inline("a[1] = 1234;",['a'])
        before1 = sys.getrefcount(a)

        # check overloaded insert(int ndx, int val) method
        inline_tools.inline("a[1] = 1234;",['a'])
<<<<<<< HEAD
        assert sys.getrefcount(a[1]) == 2
        assert a[1] == 1234

        # check overloaded insert(int ndx, double val) method
        inline_tools.inline("a[1] = 123.0;",['a'])
        assert sys.getrefcount(a[1]) == 2
        assert a[1] == 123.0

        # check overloaded insert(int ndx, char* val) method
        inline_tools.inline('a[1] = "bubba";',['a'])
        assert sys.getrefcount(a[1]) == 2
        assert a[1] == 'bubba'
=======
        assert_(sys.getrefcount(a[1]) == 2)
        assert_(a[1] == 1234)

        # check overloaded insert(int ndx, double val) method
        inline_tools.inline("a[1] = 123.0;",['a'])
        assert_(sys.getrefcount(a[1]) == 2)
        assert_(a[1] == 123.0)

        # check overloaded insert(int ndx, char* val) method
        inline_tools.inline('a[1] = "bubba";',['a'])
        assert_(sys.getrefcount(a[1]) == 2)
        assert_(a[1] == 'bubba')
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        # check overloaded insert(int ndx, std::string val) method
        code = """
               std::string val = std::string("sissy");
               a[1] = val;
               """
        inline_tools.inline(code,['a'])
<<<<<<< HEAD
        assert sys.getrefcount(a[1]) == 2
        assert a[1] == 'sissy'

        after1 = sys.getrefcount(a)
        assert after1 == before1
=======
        assert_(sys.getrefcount(a[1]) == 2)
        assert_(a[1] == 'sissy')

        after1 = sys.getrefcount(a)
        assert_(after1 == before1)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_set_item_operator_equal_created(self):
        code = """
               py::list a(3);
               a[0] = 1;
               a[1] = 2;
               a[2] = 3;
               return_val = a;
               """
        a = inline_tools.inline(code)
<<<<<<< HEAD
        assert a == [1,2,3]
        # returned value should only have a single refcount
        assert sys.getrefcount(a) == 2
=======
        assert_(a == [1,2,3])
        # returned value should only have a single refcount
        assert_(sys.getrefcount(a) == 2)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_set_item_index_error(self):
        code = """
               py::list a(3);
               a[4] = 1;
               """
<<<<<<< HEAD
        try:
            a = inline_tools.inline(code)
            assert 0
        except IndexError:
            pass
=======
        assert_raises(IndexError, inline_tools.inline, code)

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    @dec.slow
    def test_get_item_index_error(self):
        code = """
               py::list a(3);
               py::object o = a[4];
               """
<<<<<<< HEAD
        try:
            a = inline_tools.inline(code)
            assert 0
        except IndexError:
            pass
=======
        assert_raises(IndexError, inline_tools.inline, code)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

    @dec.slow
    def test_string_add_speed(self):
        N = 1000000
<<<<<<< HEAD
        print 'string add -- b[i] = a[i] + "blah" for N =', N
=======
        debug_print('string add -- b[i] = a[i] + "blah" for N =', N)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        a = ["blah"] * N
        desired = [1] * N
        t1 = time.time()
        for i in xrange(N):
            desired[i] = a[i] + 'blah'
        t2 = time.time()
<<<<<<< HEAD
        print 'python:', t2 - t1
=======
        debug_print('python:', t2 - t1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        a = ["blah"] * N
        b = [1] * N
        code = """
               const int N = a.length();
               std::string blah = std::string("blah");
               for(int i=0; i < N; i++)
                   b[i] = convert_to_string(a[i],"a") + blah;
               """
        # compile not included in timing
        inline_tools.inline(code,['a','b'])
        t1 = time.time()
        inline_tools.inline(code,['a','b'])
        t2 = time.time()
<<<<<<< HEAD
        print 'weave:', t2 - t1
        assert b == desired
    @dec.slow
    def test_int_add_speed(self):
        N = 1000000
        print 'int add -- b[i] = a[i] + 1 for N =', N
=======
        debug_print('weave:', t2 - t1)
        assert_(b == desired)

    @dec.slow
    def test_int_add_speed(self):
        N = 1000000
        debug_print('int add -- b[i] = a[i] + 1 for N =', N)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        a = [0] * N
        desired = [1] * N
        t1 = time.time()
        for i in xrange(N):
            desired[i] = a[i] + 1
        t2 = time.time()
<<<<<<< HEAD
        print 'python:', t2 - t1
=======
        debug_print('python:', t2 - t1)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        a = [0] * N
        b = [0] * N
        code = """
               const int N = a.length();
               for(int i=0; i < N; i++)
                   b[i] = (int)a[i] + 1;
               """
        # compile not included in timing
        inline_tools.inline(code,['a','b'])
        t1 = time.time()
        inline_tools.inline(code,['a','b'])
        t2 = time.time()
<<<<<<< HEAD
        print 'weave:', t2 - t1
        assert b == desired

if __name__ == "__main__":
    import nose
    nose.run(argv=['', __file__])
=======
        debug_print('weave:', t2 - t1)
        assert_(b == desired)


if __name__ == "__main__":
    run_module_suite()
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
