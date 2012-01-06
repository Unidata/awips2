#!/usr/bin/env python
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

import unittest
from qpid.codec import Codec
from qpid.spec import load
from cStringIO import StringIO
from qpid.reference import ReferenceId

__doc__ = """

    This is a unit test script for qpid/codec.py

    It can be run standalone or as part of the existing test framework.

    To run standalone:
    -------------------

        Place in the qpid/python/tests/ directory and type...

        python codec.py

        A brief output will be printed on screen. The verbose output will be placed inn a file called
        codec_unit_test_output.txt. [TODO: make this filename configurable]

    To run as part of the existing test framework:
    -----------------------------------------------

        python run-tests tests.codec

    Change History:
    -----------------
        Jimmy John  05/19/2007  Initial draft
        Jimmy John  05/22/2007  Implemented comments by Rafael Schloming


"""

from qpid_config import amqp_spec_0_8
SPEC = load(amqp_spec_0_8)

# --------------------------------------
# --------------------------------------
class BaseDataTypes(unittest.TestCase):


    """
    Base class containing common functions
    """

    # ---------------
    def setUp(self):
        """
        standard setUp for unitetest (refer unittest documentation for details)
        """
        self.codec = Codec(StringIO(), SPEC)

    # ------------------
    def tearDown(self):
        """
        standard tearDown for unitetest (refer unittest documentation for details)
        """
        self.codec.stream.flush()
        self.codec.stream.close()

    # ----------------------------------------
    def callFunc(self, functionName, *args):
        """
        helper function - given a function name and arguments, calls the function with the args and
        returns the contents of the stream
        """
        getattr(self.codec, functionName)(args[0])
        return self.codec.stream.getvalue()

    # ----------------------------------------
    def readFunc(self, functionName, *args):
        """
        helper function - creates a input stream and then calls the function with arguments as have been
        supplied
        """
        self.codec.stream = StringIO(args[0])
        return getattr(self.codec, functionName)()


# ----------------------------------------
# ----------------------------------------
class IntegerTestCase(BaseDataTypes):

    """
    Handles octet, short, long, long long

    """

    # -------------------------
    def __init__(self, *args):
        """
        sets constants for use in tests
        """

        BaseDataTypes.__init__(self, *args)
        self.const_integer = 2
        self.const_integer_octet_encoded = '\x02'
        self.const_integer_short_encoded = '\x00\x02'
        self.const_integer_long_encoded = '\x00\x00\x00\x02'
        self.const_integer_long_long_encoded = '\x00\x00\x00\x00\x00\x00\x00\x02'

    # -------------------------- #
    # Unsigned Octect - 8 bits   #
    # -------------------------- #

    # --------------------------
    def test_unsigned_octet(self):
        """
        ubyte format requires 0<=number<=255
        """
        self.failUnlessEqual(self.callFunc('encode_octet', self.const_integer), self.const_integer_octet_encoded, 'octect encoding FAILED...')

    # -------------------------------------------
    def test_octet_out_of_upper_range(self):
        """
        testing for input above acceptable range
        """
        self.failUnlessRaises(Exception, self.codec.encode_octet, 256)

    # -------------------------------------------
    def test_uoctet_out_of_lower_range(self):
        """
        testing for input below acceptable range
        """
        self.failUnlessRaises(Exception, self.codec.encode_octet, -1)

    # ---------------------------------
    def test_uoctet_with_fraction(self):
        """
        the fractional part should be ignored...
        """
        self.failUnlessEqual(self.callFunc('encode_octet', 2.5), self.const_integer_octet_encoded, 'octect encoding FAILED with fractions...')

    # ------------------------------------
    def test_unsigned_octet_decode(self):
        """
        octet decoding
        """
        self.failUnlessEqual(self.readFunc('decode_octet', self.const_integer_octet_encoded), self.const_integer, 'octect decoding FAILED...')

    # ----------------------------------- #
    # Unsigned Short Integers - 16 bits   #
    # ----------------------------------- #

    # -----------------------
    def test_ushort_int(self):
        """
        testing unsigned short integer
        """
        self.failUnlessEqual(self.callFunc('encode_short', self.const_integer), self.const_integer_short_encoded, 'short encoding FAILED...')

    # -------------------------------------------
    def test_ushort_int_out_of_upper_range(self):
        """
        testing for input above acceptable range
        """
        self.failUnlessRaises(Exception, self.codec.encode_short, 65536)

    # -------------------------------------------
    def test_ushort_int_out_of_lower_range(self):
        """
        testing for input below acceptable range
        """
        self.failUnlessRaises(Exception, self.codec.encode_short, -1)

    # ---------------------------------
    def test_ushort_int_with_fraction(self):
        """
        the fractional part should be ignored...
        """
        self.failUnlessEqual(self.callFunc('encode_short', 2.5), self.const_integer_short_encoded, 'short encoding FAILED with fractions...')

    # ------------------------------------
    def test_ushort_int_decode(self):
        """
        unsigned short decoding
        """
        self.failUnlessEqual(self.readFunc('decode_short', self.const_integer_short_encoded), self.const_integer, 'unsigned short decoding FAILED...')


    # ---------------------------------- #
    # Unsigned Long Integers - 32 bits   #
    # ---------------------------------- #

    # -----------------------
    def test_ulong_int(self):
        """
        testing unsigned long iteger
        """
        self.failUnlessEqual(self.callFunc('encode_long', self.const_integer), self.const_integer_long_encoded, 'long encoding FAILED...')

    # -------------------------------------------
    def test_ulong_int_out_of_upper_range(self):
        """
        testing for input above acceptable range
        """
        self.failUnlessRaises(Exception, self.codec.encode_long, 4294967296)

    # -------------------------------------------
    def test_ulong_int_out_of_lower_range(self):
        """
        testing for input below acceptable range
        """
        self.failUnlessRaises(Exception, self.codec.encode_long, -1)

    # ---------------------------------
    def test_ulong_int_with_fraction(self):
        """
        the fractional part should be ignored...
        """
        self.failUnlessEqual(self.callFunc('encode_long', 2.5), self.const_integer_long_encoded, 'long encoding FAILED with fractions...')

    # -------------------------------
    def test_ulong_int_decode(self):
        """
        unsigned long decoding
        """
        self.failUnlessEqual(self.readFunc('decode_long', self.const_integer_long_encoded), self.const_integer, 'unsigned long decoding FAILED...')


    # --------------------------------------- #
    # Unsigned Long Long Integers - 64 bits   #
    # --------------------------------------- #

    # -----------------------
    def test_ulong_long_int(self):
        """
        testing unsinged long long integer
        """
        self.failUnlessEqual(self.callFunc('encode_longlong', self.const_integer), self.const_integer_long_long_encoded, 'long long encoding FAILED...')

    # -------------------------------------------
    def test_ulong_long_int_out_of_upper_range(self):
        """
        testing for input above acceptable range
        """
        self.failUnlessRaises(Exception, self.codec.encode_longlong, 18446744073709551616)

    # -------------------------------------------
    def test_ulong_long_int_out_of_lower_range(self):
        """
        testing for input below acceptable range
        """
        self.failUnlessRaises(Exception, self.codec.encode_longlong, -1)

    # ---------------------------------
    def test_ulong_long_int_with_fraction(self):
        """
        the fractional part should be ignored...
        """
        self.failUnlessEqual(self.callFunc('encode_longlong', 2.5), self.const_integer_long_long_encoded, 'long long encoding FAILED with fractions...')

    # ------------------------------------
    def test_ulong_long_int_decode(self):
        """
        unsigned long long decoding
        """
        self.failUnlessEqual(self.readFunc('decode_longlong', self.const_integer_long_long_encoded), self.const_integer, 'unsigned long long decoding FAILED...')

# -----------------------------------
# -----------------------------------
class BitTestCase(BaseDataTypes):

    """
    Handles bits
    """

    # ----------------------------------------------
    def callFunc(self, functionName, *args):
        """
        helper function
        """
        for ele in args:
            getattr(self.codec, functionName)(ele)

        self.codec.flush()
        return self.codec.stream.getvalue()

    # -------------------
    def test_bit1(self):
        """
        sends in 11
        """
        self.failUnlessEqual(self.callFunc('encode_bit', 1, 1), '\x03', '11 bit encoding FAILED...')

    # -------------------
    def test_bit2(self):
        """
        sends in 10011
        """
        self.failUnlessEqual(self.callFunc('encode_bit', 1, 1, 0, 0, 1), '\x13', '10011 bit encoding FAILED...')

    # -------------------
    def test_bit3(self):
        """
        sends in 1110100111 [10 bits(right to left), should be compressed into two octets]
        """
        self.failUnlessEqual(self.callFunc('encode_bit', 1,1,1,0,0,1,0,1,1,1), '\xa7\x03', '1110100111(right to left) bit encoding FAILED...')

    # ------------------------------------
    def test_bit_decode_1(self):
        """
        decode bit 1
        """
        self.failUnlessEqual(self.readFunc('decode_bit', '\x01'), 1, 'decode bit 1 FAILED...')

    # ------------------------------------
    def test_bit_decode_0(self):
        """
        decode bit 0
        """
        self.failUnlessEqual(self.readFunc('decode_bit', '\x00'), 0, 'decode bit 0 FAILED...')

# -----------------------------------
# -----------------------------------
class StringTestCase(BaseDataTypes):

    """
    Handles short strings, long strings
    """

    # ------------------------------------------------------------- #
    # Short Strings - 8 bit length followed by zero or more octets  #
    # ------------------------------------------------------------- #

    # ---------------------------------------
    def test_short_string_zero_length(self):
        """
        0 length short string
        """
        self.failUnlessEqual(self.callFunc('encode_shortstr', ''), '\x00', '0 length short string encoding FAILED...')

    # -------------------------------------------
    def test_short_string_positive_length(self):
        """
        positive length short string
        """
        self.failUnlessEqual(self.callFunc('encode_shortstr', 'hello world'), '\x0bhello world', 'positive length short string encoding FAILED...')

    # -------------------------------------------
    def test_short_string_out_of_upper_range(self):
        """
        string length > 255
        """
        self.failUnlessRaises(Exception, self.codec.encode_shortstr, 'x'*256)

    # ------------------------------------
    def test_short_string_decode(self):
        """
        short string decode
        """
        self.failUnlessEqual(self.readFunc('decode_shortstr', '\x0bhello world'), 'hello world', 'short string decode FAILED...')


    # ------------------------------------------------------------- #
    # Long Strings - 32 bit length followed by zero or more octets  #
    # ------------------------------------------------------------- #

    # ---------------------------------------
    def test_long_string_zero_length(self):
        """
        0 length long string
        """
        self.failUnlessEqual(self.callFunc('encode_longstr', ''), '\x00\x00\x00\x00', '0 length long string encoding FAILED...')

    # -------------------------------------------
    def test_long_string_positive_length(self):
        """
        positive length long string
        """
        self.failUnlessEqual(self.callFunc('encode_longstr', 'hello world'), '\x00\x00\x00\x0bhello world', 'positive length long string encoding FAILED...')

    # ------------------------------------
    def test_long_string_decode(self):
        """
        long string decode
        """
        self.failUnlessEqual(self.readFunc('decode_longstr', '\x00\x00\x00\x0bhello world'), 'hello world', 'long string decode FAILED...')


# --------------------------------------
# --------------------------------------
class TimestampTestCase(BaseDataTypes):

    """
    No need of any test cases here as timestamps are implemented as long long which is tested above
    """
    pass

# ---------------------------------------
# ---------------------------------------
class FieldTableTestCase(BaseDataTypes):

    """
    Handles Field Tables

    Only S/I type messages seem to be implemented currently
    """

    # -------------------------
    def __init__(self, *args):
        """
        sets constants for use in tests
        """

        BaseDataTypes.__init__(self, *args)
        self.const_field_table_dummy_dict = {'$key1':'value1','$key2':'value2'}
        self.const_field_table_dummy_dict_encoded = '\x00\x00\x00\x22\x05$key2S\x00\x00\x00\x06value2\x05$key1S\x00\x00\x00\x06value1'

    # -------------------------------------------
    def test_field_table_name_value_pair(self):
        """
        valid name value pair
        """
        self.failUnlessEqual(self.callFunc('encode_table', {'$key1':'value1'}), '\x00\x00\x00\x11\x05$key1S\x00\x00\x00\x06value1', 'valid name value pair encoding FAILED...')

    # ---------------------------------------------------
    def test_field_table_multiple_name_value_pair(self):
        """
        multiple name value pair
        """
        self.failUnlessEqual(self.callFunc('encode_table', self.const_field_table_dummy_dict), self.const_field_table_dummy_dict_encoded, 'multiple name value pair encoding FAILED...')

    # ------------------------------------
    def test_field_table_decode(self):
        """
        field table decode
        """
        self.failUnlessEqual(self.readFunc('decode_table', self.const_field_table_dummy_dict_encoded), self.const_field_table_dummy_dict, 'field table decode FAILED...')


# ------------------------------------
# ------------------------------------
class ContentTestCase(BaseDataTypes):

    """
    Handles Content data types
    """

    # -----------------------------
    def test_content_inline(self):
        """
        inline content
        """
        self.failUnlessEqual(self.callFunc('encode_content', 'hello inline message'), '\x00\x00\x00\x00\x14hello inline message', 'inline content encoding FAILED...')

    # --------------------------------
    def test_content_reference(self):
        """
        reference content
        """
        self.failUnlessEqual(self.callFunc('encode_content', ReferenceId('dummyId')), '\x01\x00\x00\x00\x07dummyId', 'reference content encoding FAILED...')

    # ------------------------------------
    def test_content_inline_decode(self):
        """
        inline content decode
        """
        self.failUnlessEqual(self.readFunc('decode_content', '\x00\x00\x00\x00\x14hello inline message'), 'hello inline message', 'inline content decode FAILED...')

    # ------------------------------------
    def test_content_reference_decode(self):
        """
        reference content decode
        """
        self.failUnlessEqual(self.readFunc('decode_content', '\x01\x00\x00\x00\x07dummyId').id, 'dummyId', 'reference content decode FAILED...')

# ------------------------ #
# Pre - existing test code #
# ------------------------ #

# ---------------------
def test(type, value):
    """
    old test function cut/copy/paste from qpid/codec.py
    """
    if isinstance(value, (list, tuple)):
      values = value
    else:
      values = [value]
    stream = StringIO()
    codec = Codec(stream, SPEC)
    for v in values:
      codec.encode(type, v)
    codec.flush()
    enc = stream.getvalue()
    stream.reset()
    dup = []
    for i in xrange(len(values)):
      dup.append(codec.decode(type))
    if values != dup:
      raise AssertionError("%r --> %r --> %r" % (values, enc, dup))

# -----------------------
def dotest(type, value):
    """
    old test function cut/copy/paste from qpid/codec.py
    """
    args = (type, value)
    test(*args)

# -------------
def oldtests():
    """
    old test function cut/copy/paste from qpid/codec.py
    """
    for value in ("1", "0", "110", "011", "11001", "10101", "10011"):
      for i in range(10):
        dotest("bit", map(lambda x: x == "1", value*i))

    for value in ({}, {"asdf": "fdsa", "fdsa": 1, "three": 3}, {"one": 1}):
      dotest("table", value)

    for type in ("octet", "short", "long", "longlong"):
      for value in range(0, 256):
        dotest(type, value)

    for type in ("shortstr", "longstr"):
      for value in ("", "a", "asdf"):
        dotest(type, value)

# -----------------------------------------
class oldTests(unittest.TestCase):

	"""
	class to handle pre-existing test cases
	"""

	# ---------------------------
	def test_oldtestcases(self):
		"""
		call the old tests
		"""
		return oldtests()

# ---------------------------
# ---------------------------
if __name__ == '__main__':

    codec_test_suite = unittest.TestSuite()

    #adding all the test suites...
    codec_test_suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(IntegerTestCase))
    codec_test_suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(BitTestCase))
    codec_test_suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(StringTestCase))
    codec_test_suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(TimestampTestCase))
    codec_test_suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(FieldTableTestCase))
    codec_test_suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(ContentTestCase))

    #loading pre-existing test case from qpid/codec.py
    codec_test_suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(oldTests))

    run_output_stream = StringIO()
    test_runner = unittest.TextTestRunner(run_output_stream, '', '')
    test_result = test_runner.run(codec_test_suite)

    print '\n%d test run...' % (test_result.testsRun)

    if test_result.wasSuccessful():
        print '\nAll tests successful\n'

    if test_result.failures:
        print '\n----------'
        print '%d FAILURES:' % (len(test_result.failures))
        print '----------\n'
        for failure in test_result.failures:
            print str(failure[0]) + ' ... FAIL'

    if test_result.errors:
        print '\n---------'
        print '%d ERRORS:' % (len(test_result.errors))
        print '---------\n'

        for error in test_result.errors:
            print str(error[0]) + ' ... ERROR'

    f = open('codec_unit_test_output.txt', 'w')
    f.write(str(run_output_stream.getvalue()))
    f.close()
