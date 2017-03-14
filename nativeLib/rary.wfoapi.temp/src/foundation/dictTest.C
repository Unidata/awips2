// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// dictTest.C
// Dictionary Class Test Program
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const dictTest_C_Id =
 "$Id: .dictTest.C__temp27950,v 1.2 2003/05/06 23:11:57 fluke Exp $";
#endif

// -- module ------------------------------------------------------------------
// The dictionaryTest program fully exercises the functions available
// in the %Type%Dictionary classes.  The two classes tested are 
// Dict<TextString,float> and Dict<TextString,SeqOfPtr<float*>>.
//-----------------------------------------------------------------------------

#include <iostream>
#include <stdlib.h>
#include "commonDefs.h"
#include "Dict.H"
#include "DictKP.H"
#include "DictPP.H"
#include "DictPV.H"
#include "TextString.H"

bool dictionaryTest();
bool ptrTest();

// -- global ------------------------------------------------------------------
// main()
// Main driver for testing dictionary classes.  Program returns 0 if all
// tests were successfully completed.
//-----------------------------------------------------------------------------
int main()
    {
    std::cout << "Dictionary Test Program " << std::endl;

    std::cout << "Testing Float Dictionary class" << std::endl;
    if(!dictionaryTest())
        {
        std::cout << "..Dictionary test failed" << std::endl;
        return 1;
        }
    std::cout << "Testing pointer dictionary class" << std::endl;
    if (!ptrTest())
        {
        std::cout << "..Ptr Dictionary test failed" << std::endl;
        return 1;
        }

    std::cout << "Testing Completed Successfully" << std::endl;
    return 0;
    }


// -- global ------------------------------------------------------------------
// dictionaryTest()
// Tests dictionary containing floats.  Returns 0 if all
// tests were successfully completed.
//-----------------------------------------------------------------------------
bool dictionaryTest()
    {
    Dict<TextString,float> fd;
    int i;
    float lookupValue;

    // check size
    if (fd.size() != 0)
        {
        std::cout << "Non-zero size() returned on zero size dictionary" << std::endl;
        return false;
        }

    // assignment operator on empty dictionary
    Dict<TextString,float> fd1;
    fd1 = fd;
    if (fd1.size() != 0)
        {
        std::cout << "operator= fail on empty dictionary" << std::endl;
        return false;
        }

    // copy constructor on empty dictionary
    Dict<TextString,float> fd2(fd1);
    if (fd2.size() != 0)
        {
        std::cout << "copy constructor fail on empty dictionary" << std::endl;
        return false;
        }

    // try to find a key with an empty dictionary
    if (fd.map("AKey"))
        {
        std::cout << "map() incorrect when key does not exist" << std::endl;
        return false;
        }

    // add elements
    fd.add("Zero", 0.0);
    fd.add("One", 1.0);
    fd.add("Two", 2.0);
    fd.add("Three", 3.0);
    fd.add("Four", 4.0);
    fd.add("Five", 5.0);
    fd.add("Six", 6.0);
    fd.add("Seven", 7.0);
    fd.add("Eight", 8.0);
    fd.add("Nine", 9.0);
    fd.add("Ten", 10.0);
    // check size
    if (fd.size() != 11)
        {
        std::cout << "size incorrect after adding elements" << std::endl;
        }

    // check for existing key and get value
    if (!fd.map("Seven", lookupValue) || lookupValue != 7.0)
        {
       std::cout << "data wrong after lookup()" << std::endl;
       return false;
       }

    // index through the dictionary and get each key and value
    bool error = false;
    static int count[11] = {0,0,0,0,0,0,0,0,0,0,0};
    for (i = 0; i < fd.size(); i++)
        {
        const TextString& key = fd.key(i);
        const float& lv = fd.value(i);
        int intValue = (int)lv;
        if (intValue >= 0 && intValue < 11)
            count[intValue]++;
        switch (intValue)
            {
            case 0:  if (key != "Zero")
                         error = true;
                     break;
            case 1:  if (key != "One")
                         error = true;
                     break;
            case 2:  if (key != "Two")
                         error = true;
                     break;
            case 3:  if (key != "Three")
                         error = true;
                     break;
            case 4:  if (key != "Four")
                         error = true;
                     break;
            case 5:  if (key != "Five")
                         error = true;
                     break;
            case 6:  if (key != "Six")
                         error = true;
                     break;
            case 7:  if (key != "Seven")
                         error = true;
                     break;
            case 8:  if (key != "Eight")
                         error = true;
                     break;
            case 9:  if (key != "Nine")
                         error = true;
                     break;
            case 10: if (key != "Ten")
                         error = true;
                     break;
            default: error = true;
                     break;
             }
        }
     if (error)
         {
         std::cout << "key and value did not match" << std::endl;
         return false;
         }
     for (i = 0; i < 11; i++)
         if (count[i] != 1)
             {
             std::cout << "either duplicate or missing keys" << std::endl;
             return false;
             }

    // removal test
    fd.remove("Nineteen");
    if (fd.size() != 11)
        {
        std::cout << "size() incorrect after attempted removal of non-existant"
          << std::endl;
        return false;
        }
    TextString nine("Nine"), ten("Ten");
    fd.remove(nine);
    if (fd.size() != 10 || fd.map(nine))
        {
        std::cout << "remove() fail on key that exists" << std::endl;
        return false;
        }
    
    // modify a key
    fd.add("Ten", 100.0);

    fd.map(ten, lookupValue);
    if (lookupValue != 100.0)
        {
        std::cout << "map() or data incorrect after add() modification" 
          << std::endl;
        return false;
        }

    // add an entry to another dictionary and then assign it over this one
    TextString pi("PI");
    float piValue = 3.14159;
    fd1.add(pi, piValue);

    fd = fd1;
    if (fd.size() != 1)
        {
        std::cout << "assignment of one dictionary over another in error" << std::endl;
        std::cout << "size() is wrong" << std::endl;
        return false;
        }
    if (!fd.map(pi, lookupValue) || piValue != lookupValue)
        {
        std::cout << "assignment of one directory over another in error" << std::endl;
        std::cout << "data is wrong" << std::endl;
        return false;
        }

     return true;
    }

// -- global ------------------------------------------------------------------
// ptrTest()
// Tests some of the pointer functions for dictionary.
//-----------------------------------------------------------------------------
bool ptrTest()
    {
    DictKP<TextString, TextString*> kp;
    DictPV<TextString*, TextString> pv;
    DictPP<TextString*, TextString*> pp;

    TextString s = "abcdefg";

    TextString *sp = &s;

    // add one entry
    kp.add(s, sp);
    pv.add(sp, s);
    pp.add(sp, sp);

    // remove the entry
    kp.remove(s);
    pv.remove(sp);
    pp.remove(sp);

    // add the entry back in
    kp.add(s, sp);
    pv.add(sp, s);
    pp.add(sp, sp);
 
    // test key and value
    TextString smap;
    TextString *spmap;
    if (!kp.map(s, spmap))
        {
        std::cout << "kp map fail" << std::endl;
        return false;
        }
    if (spmap != sp)
        {
        std::cout << "kp map pointers match fail" << std::endl;
        return false;
        }
    if (!pp.map(sp, spmap))
        {
        std::cout << "pp map fail" << std::endl;
        return false;
        }
    if (spmap != sp)
        {
        std::cout << "pp map pointers match fail" << std::endl;
        return false;
        }
    if (!pv.map(sp, smap))
        {
        std::cout << "pv map fail" << std::endl;
        return false;
        }
    if (s != smap)
        {
        std::cout << "pv map values match fail" << std::endl;
        return false;
        }
    smap = kp.key(0);
    spmap = pp.key(0);
    spmap = pv.key(0);
    spmap = kp.value(0);
    smap = pv.value(0);
    spmap = pp.value(0);
    return true;

    }

