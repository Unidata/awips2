// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// BitArray Test Program
//
// Author: mathewson
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const bitArrayTest_C_Id =
  "$Id: .bitArrayTest.C__temp21481,v 1.3 2003/05/28 15:12:49 fluke Exp $";
#endif

// -- module ------------------------------------------------------------------
// The bitArrayTest program tests the BitArray class.
//-----------------------------------------------------------------------------

#include <iostream>
#include <stdlib.h>

#include "utilityFuncs.H"
#include "BitArray.H"
#include "commonDefs.h"

#include "LogStream.H"

bool bitTest();

// -- global ------------------------------------------------------------------
// main()
// Main test driver for BitArray class.  Program returns 0 if all
// tests were successful.
//-----------------------------------------------------------------------------
int main()
    {
    bool bitTestStatus;

    std::cout << "BitArray Test Program " << std::endl;
    if (!(bitTestStatus = bitTest()))
        std::cout << "..Bit Test failed" << std::endl;
    if (!bitTestStatus)
        {
        std::cout << "Test Failed....." << std::endl;
        return 1;
        }
    else
        {
        std::cout << "All tests successfully passed" << std::endl;
        return 0;
        }
    }


// -- global ------------------------------------------------------------------
// bitTest()
// Tests BitArray class.  Returns true if all tests are successful.
//-----------------------------------------------------------------------------
bool bitTest()
    {
    unsigned int index;
    unsigned int arrSize;
    byte *fp;

    std::cout << "..Testing Array of Bits" << std::endl;

    // Create NULL array
    std::cout << "....NULL array test : ";
    BitArray f1;

    // checking size for invalid array
    if (f1.isValid())
        {
        std::cout << " isValid() incorrect" << std::endl;
        return false;
        }
    if (f1.size() != 0)
        {
        std::cout << "size() incorrect" << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    // create 22 bit array without any data, index through it
    // testing for 0 values

    std::cout << "....Array with no data test: ";
    BitArray f2(22);

    // checking dimensions for 22 array
    if (!f2.isValid())
        {
        std::cout << ": isValid() incorrect " << std::endl;
        return false;
        }
    if (f2.size() != 22)
        {
        std::cout << "size() incorrect " << std::endl;
        return false;
        }
    fp = (byte *)f2.dataPtr();
    arrSize = f2.dataSize();
    if (arrSize != 3)
        {
        std::cout << "dataSize() incorrect " << std::endl;
        return false;
        }
    for (index = 0; index < arrSize; index++, fp++)
       {
       if (*fp != 0)
           {
           std::cout << "null data initialized improperly or" << std::endl;
           std::cout << "dataPtr() is incorrect" << std::endl;
           return false;
           }
        }
    unsigned int pos;
    for (pos = 0; pos < 22; pos++)
        {
        if (f2.get(pos) != false)
            {
            std::cout << "null data initialized improperly" << std::endl;
            std::cout << " or access via get() is incorrect" << std::endl;
            return false;
            }
        }
    
    std::cout << "Success" << std::endl;
    
    std::cout << "....Array with data test :";
    // create 9 bit array with alternating bits set

    fp = new byte [2];
    if (!fp)
        {
        std::cout << "new memory allocation fail" << std::endl;
        return false;
        }
    fp[0] = 0x55;
    fp[1] = 0x1;
    BitArray f3(9, fp);
    if (memcmp(f3.dataPtr(), fp, 2))
        {
        std::cout << "ramp data initialization fail" << std::endl;
        return false;
        }
    static bool answer[9] = {true, false, true, false, true, false, 
      true, false, true};
    for (pos = 0; pos < 9; pos++)
        {
        if (f3.get(pos) != answer[pos])
            {
            std::cout << "ramp data differs from get access" << std::endl;
            return false;
            }
        }

     std::cout << "Success" << std::endl;

     std::cout << "....Assignment operator test: ";

     // test out assignment operator
     f1 = f3;
     if(!f1.isValid() || f1.size() != 9 )
         {
         std::cout << "size wrong"  << std::endl;
         return false;
         }
    if (memcmp(f3.dataPtr(), f1.dataPtr(), 2))
        {
        std::cout << "data compare fail using dataPtr()" << std::endl;
        return false;
        }
    for(pos = 0; pos < f1.size(); pos++)
       {
       if (f3.get(pos) != f1.get(pos))
           {
           std::cout << "data compare fail using get operator " << std::endl;
           return false;
           }
        }

    std::cout << "Success" << std::endl;

    std::cout << "....Copy constructor test: " ;

     // test out copy constructor
     BitArray f4(f1);
     if(!f4.isValid() || f4.size() != 9)
         {
         std::cout << "dimensions wrong" << std::endl;
         return false;
         }
    if (memcmp(f4.dataPtr(), f1.dataPtr(), 2))
        {
        std::cout << "data compare using dataPtr()" << std::endl;
        return false;
        }
   for(pos = 0; pos< f1.size(); pos++)
       {
       if (f4.get(pos) != f1.get(pos))
           {
           std::cout << "data compare fail using get operator " << std::endl;
           return false;
           }
        }

    std::cout << "Success" << std::endl;

    std::cout << "....set bit test: ";

    // test storage capability with () operators
    f1.set(0);
    f1.set(1);
    f1.set(5);
    f1.clear(4);
    f1.clear(8);
    f1.set(7);
    f1.clear(1);
    fp[0] = 0xe5;
    fp[1] = 0x0;
    if (memcmp(fp, f1.dataPtr(), 2))
        {
        std::cout << "set or clear failed " << std::endl;
        return false;
        }

    // solid comparision between set and get
    if (f1.get(0) != true || f1.get(1) != false || f1.get(2) != true 
     || f1.get(3) != false || f1.get(4) != false || f1.get(5) != true
     || f1.get(6) != true || f1.get(7) != true || f1.get(8) != false)
        {
        std::cout << "set/get comparision failed " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    std::cout << "....output test: " << f1 << std::endl;

    // test the logical OR
    std::cout << ".... Testing logical OR: ";
    BitArray f6(9);
    f6.set(1);
    f6.set(0);
    f6.set(8);
    f6.set(5);
    f1 |= f6;
    fp[0] = 0xe7;
    fp[1] = 0x1;
    if (memcmp(fp, f1.dataPtr(), 2))
        {
        std::cout << "logical OR command  failed " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    std::cout << "....Assignment of invalid grid to good grid test: " ;

    // check assignment from invalid array to good array
    BitArray f5;
    f1=f5;

    // checking dimensions for invalid array
    if (f1.isValid())
        {
        std::cout << "isValid() incorrect" << std::endl;
        return false;
        }
    if (f1.size() != 0)
        {
        std::cout << "size() incorrect " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    std::cout << "....output test: " << f1 << std::endl;
                
    std::cout << "..Bit Grid Test Completed Successfully" << std::endl;
                
    delete [] fp;
    return true;
    }

