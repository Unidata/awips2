// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// stringTest.C
// Text String Class Test Program
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const stringTest_C_Id =
"$Id: .stringTest.C__temp27950,v 1.3 2003/05/06 23:12:00 fluke Exp $";
#endif

// -- module ------------------------------------------------------------------
// The stringTest program tests most of the TextString class functions.
//-----------------------------------------------------------------------------

#include <iostream>
#include <stdlib.h>
#include <string.h>
#include "commonDefs.h"
#include "StopWatch.H"
#include <string>

// main test file

#include "TextString.H"
#include "LogStream.H"

bool text_constructorTest();
bool text_lengthTest();
bool text_operatorsTest();
bool text_pointeropsTest();
bool text_comparisonTest();
bool text_subindexTest();
bool text_insertionTest();
bool text_patternSearchTest();
bool text_charPointerTest();
bool text_removeTest();
void text_constnessTest();
bool text_charTest();
bool text_appendSpeed();

// -- global ------------------------------------------------------------------
// main()
// Main driver for testing TextString class.
//-----------------------------------------------------------------------------
int main()
    {
    bool fail = false;

    logVerbose << "TextString Class Test Program " << std::endl;

    // test constructors, including null (no parameters),
    // standard (character string or non-null pointer),
    // and copy constructor.
    logVerbose << "text_constructorTest" << std::endl;
    if (!text_constructorTest())
        {
        logBug << "text constructor test fail" << std::endl;
        fail = true;
        }

    // test string-length function
    logVerbose << "text_lengthTest" << std::endl;
    if (!text_lengthTest())
        {
        logBug << "TextString length test fail" << std::endl;
        fail = true;
        }

    // test text-specific overloaded operators; these
    // are assignment (=) and  catenation (+ and +=)
    // with strings as both arguments.
    logVerbose << "text_operatorsTest" << std::endl;
    if (!text_operatorsTest())
        {
        logBug << "TextString 2-string operators test fail" << std::endl;
        fail = true;
        }

    // test pointers and text-specific overloaded operators;
    // these are assignment (=) and  catenation (+ and +=)
    // with a string as one argument and a character pointer
    // as the other.
    logVerbose << "text_pointeropsTest" << std::endl;
    if (!text_pointeropsTest())
        {
        logBug << "TextString pointer operators test fail" << std::endl;
        fail = true;
        }

    // test textstring comparison operations, each of which
    // will have three forms: string-string, string-pointer
    // and pointer-string.
    logVerbose << "text_comparisonTest" << std::endl;
    if (!text_comparisonTest())
        {
        logBug << "TextString comparisons test fail" << std::endl;
        fail = true;
        }

    // index and substring tests: replacing and extracting
    // a character or characters from specific positions within
    // a string.  Note that substring with length "1" should
    // produce the same results as index.
    logVerbose << "text_subindexTest()" << std::endl;
    if (!text_subindexTest())
        {
        logBug << "TextString subString or index test fail" << std::endl;
        fail = true;
        }

    // insertion (before and after) tests.
    logVerbose << "text_insertionTest" << std::endl;
    if (!text_insertionTest())
        {
        logBug << "TextString insertions test fail" << std::endl;
        fail = true;
        }

    // test searching for a pattern within a text string.
    logVerbose << "text_patternSearchTest" << std::endl;
    if (!text_patternSearchTest())
        {
        logBug << "TextString pattern search test fail" << std::endl;
        fail = true;
        }

    // test returning a character pointer to the textstring
    logVerbose << "text_charPointerTest" << std::endl;
    if (!text_charPointerTest())
        {
        logBug << "TextString character pointer test fail" << std::endl;
        fail = true;
        }

    // test removing a group of characters from the TextString
    logVerbose << "text_removeTest" << std::endl;
    if (!text_removeTest())
        {
        logBug << "TextString remove test fail" << std::endl;
        fail = true;
        }

    // test some of the char functions
    logVerbose << "text_charTest" << std::endl;
    if (!text_charTest())
        {
        logBug << "TextString char test fail" << std::endl;
        fail = true;
        }

    // test constness of TextString.  Verify by checking compiler errors..
    text_constnessTest();


    text_appendSpeed();

    //-----------------------------------------------------------
    // end of tests of TextString class functionality.
    //-----------------------------------------------------------


    //-----------------------------------------------------------
    // wrap-up
    //-----------------------------------------------------------

    if (fail)
        {
        logBug << "TextString tests failed" << std::endl;
        return 1;
        }

    else
        {
        logVerbose << "TextString tests passed successfully" << std::endl;
        return 0;
        }

    }   // end main

// -- global ------------------------------------------------------------------
// text_appendSpeed()
// Tests how fast TextString appends relative to STL strings.
//-----------------------------------------------------------------------------
bool text_appendSpeed()
    {
    logVerbose << "appendTest" << std::endl;
    TextString appendStr;
    StopWatch sw;
    sw.start();
    int total = 100000;
    int i;
    for (i = 0 ; i < total ; i++)
        appendStr += "a";
    sw.stop();
    std::cerr << "time for " << total << " appends(): "
      << sw.wallClockTime() << std::endl;
    std::cerr << "len appendStr: " << appendStr.length() << std::endl;

    std::string stlas;
    StopWatch sw2;
    sw2.start();
    for (i = 0 ; i < total ; i++)
        stlas += "a";
    sw2.stop();

    std::cerr << "time for " << total << " appends(): "
      << sw2.wallClockTime() << std::endl;
    std::cerr << "len appendStr: " << stlas.length() << std::endl;

    return true;
    }

//-------------------------------------------------------------------
// Note on TextString testing.
// In the following subroutines, many TextStrings will be declared,
// as well as many character pointers.  To help in reading of the
// code, I will try to create distinct types of names to keep
// the reader from confusing these two types.
// TextStrings will be named "string_n" where n is a number.
// character pointers will be named "char_x" where x is a letter.
//   for example, string_1, string_2, string_3; char_a, char_b, etc.
//     - jkl
//-------------------------------------------------------------------

// -- global ------------------------------------------------------------------
// text_constructorTest()
// Tests TextString constructors and returns true on success.
//-----------------------------------------------------------------------------
bool text_constructorTest()
    {
    bool return_val = true;
    const TextString NULLString;

    // test of null constructor:
    TextString string_1;
    if (string_1 != NULLString)
        {
        logBug << "problem found with default constructor, should be null, is "
             << string_1 << std::endl;
        return_val = false;
        }
    if (string_1.length() != 0)
        {
        logBug << "length of default null constructor result should be 0, is "
             << string_1.length() << std::endl;
        return_val = false;
        }

    // test of basic constructor; pass in a string.
    TextString string_2("second constructor test");
    if (string_2 != "second constructor test")
        {
        logBug << "problem found with basic constructor, returned "
             << string_2 << std::endl;
        return_val = false;
        }

    // test of basic constructor with char * passed.
    char *char_a = "third constructor test";
    TextString string_3(char_a);
    if (string_3 != "third constructor test")
        {
        logBug << "problem found with basic char * constructor, returned "
             << string_3 << std::endl;
        return_val = false;
        }

    // test of copy constructor with null
    TextString string_4(string_1);
    if (string_4 != NULLString)
        {
        logBug << "problem with copy constructor and null, should be "
             << string_1 << ", is: " << string_4  << std::endl;
        return_val = false;
        }
    if (!(""==string_4))
        {
        logBug << "problem with copy constructor and null"
          << "!(\"\"==string_4)" << std::endl;
        return_val = false;
        }
    if (!(string_4==""))
        {
        logBug << "problem with copy constructor and null"
          << "!(string_4==\"\")" << std::endl;
        return_val = false;
        }

    // test of copy constructor with a value
    TextString string_5 = string_2;
    if (string_5 != string_2)
        {
        logBug << "problem with copy constructor, should be "
             << string_2 << ", is: " << string_5  << std::endl;
        return_val = false;
        }

    if (return_val == true)
        logVerbose << "TextString constructor tests ran successfully." << std::endl;

    return(return_val);
    }

// -- global ------------------------------------------------------------------
// text_lengthTest()
// Test function to return correct length of a TextString.  Returns true
// on success.
//-----------------------------------------------------------------------------
bool text_lengthTest()
    {
    bool return_val = true;

    TextString string_1("longlonglonglong string with length of 41");
    TextString string_2;  // null string, length 0
    TextString string_3("1");

    if (string_1.length() != 41 || string_2.length() != 0 ||
        string_3.length() != 1)
        {
        logBug << "Problem with length function, predicted lengths not given."
             << std::endl;
        return_val = false;
        }

    if (return_val == true)
        logVerbose << "TextString length tests ran successfully." << std::endl;

    return(return_val);
    }


// -- global ------------------------------------------------------------------
// text_operatorsTest()
// Tests overloaded operators for TextStrings.  The operators tested are
// =, +, and +=.  Returns true for success.
//-----------------------------------------------------------------------------
bool text_operatorsTest()
    {
    bool return_val = true;
    const TextString NULLString;
    TextString string_1("simple string");
    TextString string_2("more complicated string");
    TextString string_3;  // null string
    TextString string_4;  // null string
    char * char_A = "more complicated stringmore complicated string";

    // start with ASSIGNMENT operator:

    string_1 = string_1;  // make sure this doesn't do the weird thing
    if (string_1 != "simple string")
        {
        logBug << "problem with a=a type operation with 2 TextStrings;"
             << " resulting string is " << string_1 << std::endl;
        return_val = false;
        }

    string_1 = string_2;  // straightforward assignment
    if (string_1 != "more complicated string")
        {
        logBug << "problem with a=b type operation with 2 TextStrings;"
             << " resulting string is [" << string_1 << ']' << std::endl;
        return_val = false;
        }

    string_1 = string_3; // assignment of a NULLString string
    if (string_1 != NULLString)
        {
        logBug << "problem with a=NULLString type operation with 2 TextStrings;"
             << " resulting string is " << string_1 << std::endl;
        return_val = false;
        }

    string_3 = string_2;  // assign NULLString string with a non-NULL
    if (string_3 != "more complicated string")
        {
        logBug << "problem with NULLString=b type operation with 2 TextStrings;"
             << " resulting string is " << string_3 << std::endl;
        return_val = false;
        }

    TextString abc("abc");
    char c = 'c';
    abc = c;
    if (abc.length() != 1 || abc[0] != 'c')
        {
        logBug << "problem with operator=(char)" << std::endl;
        return_val = false;
        }

    // next do ASSIGNMENT/CATENATION operator: +=

    // string_3 is now "more complicated string"
    // string_2 is "more complicated string"
    // string_1 is NULLString

    string_3 += string_2;
    // now string_3 is "more complicated stringmore complicated string"
    if (string_3 != "more complicated stringmore complicated string")
        {
        logBug << "problem1 with += operator between 2 TextStrings. "
             << " resulting string is " << string_3 << std::endl;

        return_val = false;
        }

    if ((strcmp(string_3.stringPtr(), char_A)))
        {
        logBug << "problem2 with += operator between 2 TextStrings. "
             << " resulting string is " << string_3 << std::endl;

        return_val = false;
        }

    string_2 += string_2;
    // now string2 is "more complicated stringmore complicated string"
    if (string_2 != "more complicated stringmore complicated string")
        {
        logBug << "problem3 with += operator, same TextString: A+=A; "
             << " resulting string is " << string_2 << std::endl;

        return_val = false;
        }

    if ((strcmp(string_2.stringPtr(), char_A)))
        {
        logBug << "problem4 with += operator, same TextString: A+=A; "
             << " resulting string is " << string_2 << std::endl;

        return_val = false;
        }

    string_1 += string_3;
    // now string1 is "more complicated stringmore complicated string"
    if (string_1 != "more complicated stringmore complicated string")
        {
        logBug << "problem5 with += operator between 2 TextStrings "
             << "where 1st is NULLString.  resulting string is "
             << string_3 << std::endl;

        return_val = false;
        }

    if ((strcmp(string_1.stringPtr(), char_A)))
        {
        logBug << "problem6 with += operator between 2 TextStrings "
             << "where 1st is NULLString.  resulting string is "
             << string_3 << std::endl;

        return_val = false;
        }

    // and straight CATENATION: +
    string_1 = "first";
    string_2 = "second";
    string_3 = "third";

    string_1 = string_1 + string_1;
    // should be "firstfirst"
    if (string_1 != "firstfirst")
        {
        logBug << "problem with + (concatenation) operator in A=A+A. "
             << "resulting string is " << string_1 << std::endl;
        return_val = false;
        }

    string_1 = string_1 + string_2 + string_2;
    // should be "firstfirstsecondsecond"
    if (string_1 != "firstfirstsecondsecond")
        {
        logBug << "problem with + (concatenation) operator in A=A+B+B. "
             << "resulting string is " << string_1 << std::endl;
        return_val = false;
        }

    string_2 = string_1 + string_3 + string_3;
    // should be "firstfirstsecondsecondthirdthird"
    if (string_2 != "firstfirstsecondsecondthirdthird")
        {
        logBug << "problem with + (concatenation) operator in A=B+C+C. "
             << "resulting string is " << string_2 << std::endl;
        return_val = false;
        }

    string_4 = string_4 + string_4;
    // result should be NULLString
    if (string_4 != NULLString)
        {
        logBug << "problem with + operator in A=A+A where A is NULLString.  "
             << "resulting string is " << string_4 << std::endl;
        return_val = false;
        }

    string_4 = string_4 + string_3 + string_4 + string_4 + string_3;
    // result should be "thirdthird"
    if (string_4 != "thirdthird")
        {
        logBug << "problem with + operator in A=A+B+A+A+B where A is "
          << "NULLString. Resulting string is " << string_4 << std::endl;
        return_val = false;
        }

    // Check concatenation with a const string
    const TextString string_c("const");

    string_1 = "first";
    string_1 = string_1 + string_c;
    // should be "firstconst"
    if (string_1 != "firstconst")
        {
        logBug << "problem with + (concatenation) operator in A=A+Const. "
             << "resulting string is " << string_1 << std::endl;
        return_val = false;
        }

    string_1 = "first";
    string_1 = string_c + string_1;
    // should be "constfirst"
    if (string_1 != "constfirst")
        {
        logBug << "problem with + (concatenation) operator in A=Const+A. "
             << "resulting string is " << string_1 << std::endl;
        return_val = false;
        }


    if (return_val == true)
        logVerbose << "TextString operators tests ran successfully." << std::endl;

    return(return_val);
    }

// -- global ------------------------------------------------------------------
// text_pointeropsTest()
// Tests overloaded operators for TextStrings and character pointers.
// Returns true for success.
//-----------------------------------------------------------------------------
bool text_pointeropsTest()
    {
    bool return_val = true;

    const TextString NULLString;
    TextString string_1("simple string");
    TextString string_2("more complicated string");
    TextString string_3;  // null string
    TextString string_4;  // null string
    char * char_A = "more complicated stringanother character string";
    char * char_B = ""; // null string
    char * char_C = "another character string";
    char * char_D = NULL;  // null string
    char * char_E = "first";
    char * char_F = "second";
    char * char_G = "third";

    // start with ASSIGNMENT operator:

    string_1 = char_C;  // straightforward assignment
    if (string_1 != "another character string")
        {
        logBug << "problem with string = pointer operation;"
             << " resulting string is " << string_1 << std::endl;
        return_val = false;
        }

    string_1 = char_D; // assignment of a NULL string
    if (string_1 != NULLString)
        {
        logBug << "problem with a=NULLString operation with string = pointer; "
             << " resulting string is " << string_1 << std::endl;
        return_val = false;
        }

    // next do ASSIGNMENT/CATENATION operator: +=

    // string_3 is NULLString
    // string_2 is "more complicated string"
    // string_1 is NULLString
    // char_D is NULL

    string_2 += char_C;
    // now string_2 is "more complicated stringanother character string"
    if (string_2 != "more complicated stringanother character string")
        {
        logBug << "problem1 with += operator, TextString += char *. "
             << " resulting string is " << string_2 << std::endl;
        return_val = false;
        }

    if ((strcmp(string_2.stringPtr(), char_A)))
        {
        logBug << "problem2 with += operator between 2 TextStrings. "
             << " resulting string is " << string_3 << std::endl;
        return_val = false;
        }

    string_2 += char_B;
    // now string_2 is "more complicated stringanother character string"
    if (string_2 != "more complicated stringanother character string")
        {
        logBug << "problem4 with += operator between string += pointer "
             << "where pointer is NULL. resulting string: "
             << string_2 << std::endl;
        return_val = false;
        }

    if ((strcmp(string_2.stringPtr(), char_A)))
        {
        logBug << "problem5 with += operator between string += pointer "
             << "where pointer is NULL. resulting string: "
             << string_2 << std::endl;

        return_val = false;
        }

    if (string_2 != char_A)
        {
        logBug << "problem6 with += operator between string += pointer "
             << "where pointer is NULL. resulting string: " << string_2
             << std::endl;
        return_val = false;
        }

    // and straight CATENATION: +
    string_1 = "first";   // also char_E
    string_2 = "second";  // also char_F
    string_3 = "third";   // also char_G
    // char_B is NULL

    string_1 = string_1 + char_E;
    // should be "firstfirst"
    if (string_1 != "firstfirst")
        {
        logBug << "problem with + (concatenation) operator in A=A+P. "
             << "resulting string is " << string_1 << std::endl;
        return_val = false;
        }

    string_1 = string_1 + char_F + string_2;
    // should be "firstfirstsecondsecond"
    if (string_1 != "firstfirstsecondsecond")
        {
        logBug << "problem with + (concatenation) operator in A=A+P+B. "
             << "resulting string is " << *char_A << std::endl;
        return_val = false;
        }

    string_2 = string_1 + "third" + string_3;
    // should be "firstfirstsecondsecondthirdthird"
    if (string_2 != "firstfirstsecondsecondthirdthird")
        {
        logBug << "problem with + (concatenation) operator in A=P+P+B. "
             << "resulting string is " << string_2 << std::endl;
        return_val = false;
        }

    string_4 = char_B + string_4;
    // result should be NULL.
    if (string_4 != NULLString)
        {
        logBug << "problem with + operator in A=P+A where A&P are NULL.  "
             << "resulting string is " << string_4 << std::endl;
        return_val = false;
        }

    string_4 = string_4 + char_G + string_4 + "third" + string_3;
    // result should be "thirdthirdthird"
    if (string_4 != "thirdthirdthird")
        {
        logBug << "problem with + operator in A=A+P+A+P+B where A, P are NULL."
             << " resulting string is " << string_4 << std::endl;
        return_val = false;
        }

    if (return_val == true)
        logVerbose << "TextString string/pointer ops tests ran successfully."
          << std::endl;

    return(return_val);
    }

// -- global ------------------------------------------------------------------
// text_comparisonTest()
// Tests comparison operations for TextStrings and char pointers.  Returns
// true for success.
//-----------------------------------------------------------------------------
bool text_comparisonTest()
    {
    bool return_val = true;

    TextString string_1("aBcDeFgHiJkLmNoPq");
    TextString string_2("aBcDeFgHiJkLmNoPq");
    TextString string_3("aBcD");  // less than string_1 & 2.
    TextString string_4;          // null string, less than anything.

    if ((string_1 != string_2)    ||
        (!(string_1 == string_2)) ||
        (string_1 < string_2)     ||
        (string_1 > string_2)     ||
        (string_2 > string_1)     ||
        (string_2 < string_1))
        {
        return_val = false;
        logBug << "error comparing 2 equal strings, " << string_1 << " and "
             << string_2 << std::endl;
        }

    if ((string_1 == string_3)    ||
        (!(string_1 != string_3)) ||
        (string_1 < string_3)     ||
        (string_1 <= string_3)    ||
        (string_2 == string_3)    ||
        (string_2 <= string_3)    ||
        (string_3 > string_1)     ||
        (string_3 >= string_2))
        {
        return_val = false;
        logBug << "error comparing 2 unequal strings, " << string_1 << " and "
             << string_3 << std::endl;
        }

    if ((string_1 != string_1)    ||
        (!(string_1 == string_1)) ||
        (string_1 < string_1)     ||
        (string_1 > string_1))
        {
        return_val = false;
        logBug << "error comparing same string with itself: "
             << string_1 << std::endl;
        }

    if ((string_1 == string_4)    ||
        (!(string_4 != string_3)) ||
        (!(string_4 < string_3))  ||
        (string_1 <= string_4)    ||
        (string_4 == string_3)    ||
        (!(string_4 <= string_3)) ||
        (string_4 > string_4)     ||
        (string_4 > string_1)     ||
        (string_4 >= string_2))
        {
        return_val = false;
        logBug << "error comparing strings and null string." << std::endl;
        }

    if (return_val == true)
        logVerbose << "TextString comparison tests ran successfully." << std::endl;

    return(return_val);
    }


// -- global ------------------------------------------------------------------
// text_subindexTest()
// Tests substring and index functionality.  Returns true for success.
// -- implementation ----------------------------------------------------------
// A section of code has been commented out.  It should be included to cause
// the code to test failure conditions.
//-----------------------------------------------------------------------------
bool text_subindexTest()
    {
    bool return_val = true;
    const TextString NULLString;
    TextString string_1("abcdefghijklmnop");
    TextString string_2("0123456789");
    TextString string_3;

    string_3 = string_1.left(3);
    if (string_3 != "abc")
        {
        return_val = false;
        logBug << "left function error, string should be 'abc', is "
             << string_3 << std::endl;
        }

    string_3 = string_1.mid(9,3) + string_2.mid(5,3);
    if (string_3 != "jkl567")
        {
        return_val = false;
        logBug << "mid function error, string should be 'jkl567', is "
             << string_3 << std::endl;
        }

    // the following test will do mid(mid(mid()))
    string_3 = ((string_1.mid(5,5)).mid(0,4)).mid(2,2);
    if (string_3 != "hi")
        {
        return_val = false;
        logBug << "mid function error, string should be 'hi', is "
             << string_3 << std::endl;
        }

    string_3 = "XXXXX";

    string_3[0] = 'j';
    string_3[1] = string_1[10];
    string_3[2] = 'l';
    string_3[3] = string_2[5];
    string_3.remove(4,100); // remove 100 char starting at pos 4
    if (string_3 != "jkl5")
        {
        return_val = false;
        logBug << "Indexing [] function1 error, string should be 'jkl5', is "
             << string_3 << std::endl;
        }

    string_3[3] = string_2[1];
    string_3[0] = *((string_1.mid(5,1)).stringPtr());
    if (string_3 != "fkl1")
        {
        return_val = false;
        logBug << "Indexing []2 function error, string should be 'fkl1', is "
             << string_3 << std::endl;
        }

    // now test some error cases for the [] functions

    logVerbose << "The following problems test out bad indexes" << std::endl;
    logVerbose << " You should see one logBug" << std::endl;
    string_3[8] = 'X';  // will actually change the last character
    if (string_3 != "fkl1")
        {
        return_val = false;
        logBug << "LHS Index Error, position out of bounds, string should "
             << "remain 'fkl1', but is " << string_3 << std::endl;
        }

    if (string_3.right(2) != "l1")
        {
        return_val = false;
        logBug << "right() didn't work for partial string, should be l1, was "
          << string_3.right(2) << std::endl;
        }

    if (string_3.right(200) != string_3)
        {
        return_val = false;
        logBug << "right() didn't work for full string" << std::endl;
        }

    if (string_1.mid(14, 4) != "op")
        {
        return_val = false;
        logBug << "Illegal length should have truncated and returned 'op'."
             << " start pos is 14, string is " << string_1 << std::endl;
        }

    if (string_1.mid(string_1.length()/2, string_1.length())
        != "ijklmnop")
        {
        return_val = false;
        logBug << "Illegal length should have truncated and "
          << "returned 'ijklmnop'. string is " << string_1 << std::endl;
        }

    if (string_1.mid(5, 0) != NULLString)
        {
        return_val = false;
        logBug << "Error: length of 0 in mid() should have "
             << "returned NULLString." << std::endl;
        }

    if (return_val == true)
        logVerbose << "TextString index/substr tests ran successfully."
          << std::endl;

    return(return_val);
    }

// -- global ------------------------------------------------------------------
// text_insertionTest()
// Tests insertion (before or after a string) of characters.  Returns
// true for success.
//-----------------------------------------------------------------------------
bool text_insertionTest()
    {
    bool return_val = true;

    TextString string_1("ABCDEFG");
    TextString string_2("abcdefg");

    string_1.insertBefore(3, string_2.mid(0,3));
    if (string_1 != "ABCabcDEFG")
        {
        return_val = false;
        logBug << "insertBefore error, string should be 'ABCabcDEFG', is "
             << string_1 << std::endl;
        }

    string_1.insertAfter(6,"hijkl");
    if (string_1 != "ABCabcDhijklEFG")
        {
        logBug << "insertAfter error, string should be 'ABCabcDhijklEFG', is "
             << string_1 << std::endl;
        }


    // test error handling for auto index correction.
    logVerbose << "The next line should be a logbug" << std::endl;
    string_1.insertAfter(200, "B");
    if (string_1 != "ABCabcDhijklEFGB")
        {
        logBug << "insertAfter worked incorrectly with a large position"
             << std::endl;
        return_val = false;
        }

    if (return_val == true)
        logVerbose << "TextString insertion tests ran successfully." << std::endl;

    return(return_val);
    }

// -- global ------------------------------------------------------------------
// text_patternSearchTest()
// Tests search for a specific string pattern within a TextString.
// Returns true for success.
//-----------------------------------------------------------------------------
bool text_patternSearchTest()
    {
    bool return_val = true;

    TextString string_1("abcdefghijkljkl");

    unsigned int foundPos;
    if (!string_1.found("jkl", foundPos) || foundPos != 9)
        {
        logBug << "error in search for pattern 'jkl' in string "
             << string_1 << " beginning at pos 0 " << std::endl;
        return_val = false;
        }

    if (!string_1.found("jkl", foundPos, 9) || foundPos != 9)
        {
        logBug << "error in search for pattern 'jkl' in string "
             << string_1 << " beginning at pos 9 " << std::endl;
        return_val = false;
        }

    // check error cases.  search pos 14 is after matching part...
    // this won't cause an error message, it will just return "fail", legally.
    if (string_1.found("jkl", foundPos, 14))
        {
        logBug << "search for pattern 'jkl' after pos 14 should have "
             << "caused error: string is " << string_1 << std::endl;
        return_val = false;
        }

    // check error cases.  search pos 16 is out of bounds...
    logVerbose << "next line is a logBug" << std::endl;
    if (string_1.found('p', foundPos, 16))
        {
        logBug << "search after position 16 is out of bounds, should have "
             << "caused error: string is " << string_1 << std::endl;
        return_val = false;
        }

    // check error cases.  search for a pattern not in the string...
    // this won't cause an error message, just a "fail" return legally.
    if (string_1.found("jkL", foundPos))
        {
        logBug << "search for pattern 'jkL' should have caused error, "
             << "that pattern is not in string " << string_1 << std::endl;
        return_val = false;
        }

    // check reverse search.  search for the position of the last pattern
    // in the string.  Should be 12.
    if (!string_1.found("jk", foundPos, 14, TextString::SEARCH_BCK) ||
      foundPos != 12)
        {
        logBug << "error in search for last pattern 'jk' in string "
            << string_1 << ", last occurrence was pos 12, not " <<
            foundPos << std::endl;
        return_val = false;
        }

    // check reverse search, search for the position of the last pattern
    // in the string, but search pos only allows first 13 characters in
    // string to be searched.  Should be 9.
    if (!string_1.found("jk", foundPos, 12, TextString::SEARCH_BCK) ||
      foundPos != 9)
        {
        logBug << "error in search for last pattern 'jk' in string "
            << string_1 <<
            ", ending at position 12.  occurrence should be 9, was "
            << foundPos << std::endl;
        return_val = false;
        }

    if (return_val == true)
        logVerbose << "TextString pattern-search tests ran successfully."
          << std::endl;




    return(return_val);
    }

// -- global ------------------------------------------------------------------
// text_charPointerTest()
// Tests the return of a character pointer to a TextString.  Returns true
// if successful.
//-----------------------------------------------------------------------------
bool text_charPointerTest()
    {
    bool return_val = true;

    TextString string_1("abcd1234");
    char * char_A = "abcd1234";

    if ((strcmp(string_1.stringPtr(),"abcd1234")))
        {
        return_val = false;
        logBug << "Error 1 returning character pointer, string is "
             << string_1 << std::endl;
        }

    if ((strcmp(string_1.stringPtr(), char_A)))
        {
        return_val = false;
        logBug << "Error 2 returning character pointer, string is "
             << string_1 << std::endl;
        }

    if (return_val == true)
        logVerbose << "TextString char-pointer tests ran successfully."
          << std::endl;

    return(return_val);
    }

// -- global ------------------------------------------------------------------
// text_removeTest()
// Tests removing a specific group of characters from a TextString.  Returns
// true for success.
//-----------------------------------------------------------------------------
bool text_removeTest()
    {
    bool return_val = true;
    TextString string_1("ABC12345678DEF");

    string_1.remove(3,8);
    if (string_1 != "ABCDEF")
        {
        logBug << "remove error1, string should be 'ABCDEF', is "
             << string_1 << std::endl;
        return_val = false;
        }

    string_1.remove(0,1);
    if (string_1 != "BCDEF")
        {
        logBug << "remove error2, string should be 'BCDEF', is "
             << string_1 << std::endl;
        return_val = false;
        }

    string_1.remove(string_1.length() -1, 1);
    if (string_1 != "BCDE")
        {
        logBug << "remove error3, string should be 'BCDE', is "
             << string_1 << std::endl;
        return_val = false;
        }

    logVerbose << "next line is a logbug" << std::endl;
    string_1.remove(100, 50);
    if (string_1 != "BCDE")
        {
        logBug << "remove error4, string should be unchanged at 'BCDE' but is "
             << string_1 << std::endl;
        return_val = false;
        }

    string_1.remove(2, 8);
    if (string_1 != "BC") // should have truncated the length anyway...
        {
        logBug << "remove error5, string should be 'BC' but is " << string_1
             << std::endl;
        return_val = false;
        }

    if (return_val == true)
        logVerbose << "TextString remove tests ran successfully." << std::endl;

    return(return_val);
    }

// -- global ------------------------------------------------------------------
// text_constnessTest()
// Tests to see that nonconst TextString functions called with const
// TextString objects produce compiler errors as they should.  No return
// value.
//-----------------------------------------------------------------------------
void text_constnessTest()
    {
    const TextString string_1("DoMeSoSoLa");
#ifdef TEST_CONSTNESS

    // Function calls which should (and do) produce compiler warnings.
    //
    string_1.remove(6,2);
    string_1.insertAfter(1, "Ra");
    string_1.insertBefore(6, "Fa");
    string_1 += "Te";
    string_1 = "Do";
#endif
    } // end of constness test.

// -- global ------------------------------------------------------------------
// text_charTest()
// Tests TextString char operators. Returns true if successful.
//-----------------------------------------------------------------------------
bool text_charTest()
    {
    bool retValue = true;
    TextString a("1234567890");
    TextString b("123x4567890");
    TextString c = a;

    // test insertAfter with character
    c.insertAfter(2,'x');
    if (c != b)
        {
        logBug << "insertAfter(pos,char) failed" << std::endl;
        logBug << "insertAfter(2,'x') for " << a << " was " << c << std::endl;
        retValue = false;
        }

    // test insertBefore with character
    c = a;
    c.insertBefore(3, 'x');
    if (c != b)
        {
        logBug << "insertBefore(pos,char) failed" << std::endl;
        logBug << "insertBefore(3,'x') for " << a << " was " << c << std::endl;
        retValue = false;
        }

    // test TS = TS + char
    TextString d = "abcd";
    TextString e = "abcde";
    TextString f = d + 'e';
    if (f != e)
        {
        logBug << "operator+(char) failed" << std::endl;
        logBug << "abcd + e = " << f << std::endl;
        retValue = false;
        }

    // test TS = char + TS
    TextString g = "xabcd";
    TextString h = 'x' + d;
    if (g != h)
        {
        logBug << "operator+(char,TS) failed" << std::endl;
        logBug << "x + abcd  = " << h << std::endl;
        retValue = false;
        }

    // test found with character - forward search
    unsigned int foundPos;
    bool foundIt = g.found('a', foundPos);
    if (!foundIt || foundPos != 1)
        {
        logBug << "found(char) forward search  failed" << std::endl;
        retValue = false;
        }

    // test found with character - back search
    foundIt = g.found('a', foundPos, 3, TextString::SEARCH_BCK);
    if (!foundIt || foundPos != 1)
        {
        logBug << "found(char) backwards search failed" << std::endl;
        retValue = false;
        }
    return retValue;
    }
