// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// Two Dimensional Grid Test Program
//
// Author: mathewson
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const twoDGridTest_C_Id =
  "$Id: .twoDGridTest.C__temp27950,v 1.2 2003/05/06 23:12:02 fluke Exp $";
#endif


// -- module ------------------------------------------------------------------
// The twoDGridTest program tests the template Grid2D<T> classes
// Grid2DBit, and CartCoord2D<float> classes.
//-----------------------------------------------------------------------------

#include <iostream>
#include <stdlib.h>

#include "Grid2D.H"
#include "Grid2DBit.H"
#include "CartCoord2D.H"
#include "commonDefs.h"
#include "Grid3D.H"
#include "CartCoord3D.H"

#include "LogStream.H"

bool floatTest();
bool intTest();
bool bitTest();
bool areaTest();
bool coordTest();
bool threeDTest();

// -- global ------------------------------------------------------------------
// main()
// Main test driver for TwoD Grid class.  Program returns 0 if all
// tests were successful.
//-----------------------------------------------------------------------------
int main()
    {
    bool floatTestStatus, intTestStatus, coordTestStatus;
    bool bitTestStatus, areaTestStatus, threeDTestStatus;

    std::cout << "Two Dimensional Grid Test Program " << std::endl;
    if (!(coordTestStatus = coordTest()))
        std::cout << "..CartCoord2D<T> Test failed" << std::endl;
    if (!(floatTestStatus = floatTest()))
        std::cout << "..Float Test failed" << std::endl;
    if (!(intTestStatus = intTest()))
        std::cout << "..Int Test failed" << std::endl;
    if (!(bitTestStatus = bitTest()))
        std::cout << "..Bit Test failed" << std::endl;
    if (!(areaTestStatus = areaTest()))
        std::cout << "..Area Bit Test failed" << std::endl;
    if (!(threeDTestStatus = threeDTest()))
        std::cout << "..Three D Grid/Coord Test failed" << std::endl;
    if (!floatTestStatus || !intTestStatus || !coordTestStatus
      || !bitTestStatus || !areaTestStatus || !threeDTestStatus)
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
// floatTest()
// Tests Grid2D<float> class.  Returns true if all tests are successful.
//-----------------------------------------------------------------------------
bool floatTest()
    {
    float *fp1;
    const float *fp;
    unsigned int xpos, ypos, index;

    std::cout << "..Testing Two-Dimensional Grid of Floats" << std::endl;

    // Create invalid array
    std::cout << "....invalid array test : ";
    Grid2D<float> f1;

    // checking dimensions for invalid array
    if (f1.isValid())
        {
        std::cout << " isValid() incorrect" << std::endl;
        return false;
        }
    if (f1.xDim() != 0)
        {
        std::cout << "xDim() incorrect" << std::endl;
        return false;
        }
    if (f1.yDim() != 0)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    // create 10 x 20 float array without any data, index through it
    // testing for 0.0 values

    std::cout << "....Array with no data test: ";
    Grid2D<float> f2(10, 20);

    // checking dimensions for 10x20 array
    if (!f2.isValid())
        {
        std::cout << ": isValid() incorrect " << std::endl;
        return false;
        }
    if (f2.xDim() != 10)
        {
        std::cout << "xDim() incorrect " << std::endl;
        return false;
        }
    if (f2.yDim() != 20)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    fp = f2.dataPtr();
    for (index = 0; index < 200; index++, fp++)
       {
       if (*fp != 0.0)
           {
           std::cout << "null data initialized improperly or" << std::endl;
           std::cout << "dataPtr() is incorrect" << std::endl;
           return false;
           }
        }
    for (xpos = 0; xpos < 10; xpos++)
        for (ypos = 0; ypos < 20; ypos++)
            {
            if (f2(xpos,ypos) != 0.0)
                {
                std::cout << "null data initialized improperly" << std::endl;
                std::cout << " or access via () operators is incorrect" << std::endl;
                std::cout <<"xpos, ypos = " <<xpos <<", " <<ypos << std::endl;
                std::cout <<f2.xDim() <<", " <<f2.yDim() << std::endl;
                return false;
                }
            }
    
    std::cout << "Success" << std::endl;
    
    std::cout << "....Array with data test :";
    // create 20 x 30 float array with row/col numbers, index and check
    fp = new float [20 * 30];
    fp1 = (float *) fp;
    if (!fp)
        {
        std::cout << "new memory allocation fail" << std::endl;
        return false;
        }
    for (index = 0; index < 20*30; index++, fp1++)
        *fp1 = index;
    Grid2D<float> f3(20, 30, fp);
    if (memcmp(f3.dataPtr(), fp, 20*30*sizeof(float)))
        {
        std::cout << "ramp data initialization fail" << std::endl;
        return false;
        }
    for (index = 0, ypos = 0; ypos < 30; ypos++)
        for (xpos = 0; xpos < 20; xpos++)
            {
            if (f3(xpos,ypos) != (float)index)
                {
                std::cout << "ramp data differs from () access" << std::endl;
                return false;
                }
            index++;
            }

     std::cout << "Success" << std::endl;

     std::cout << "....Assignment operator test: ";

     // test out assignment operator
     f1 = f3;
     if(!f1.isValid() || f1.xDim() != 20 || f1.yDim() != 30)
         {
         std::cout << "dimensions wrong"  << std::endl;
         return false;
         }
    if (memcmp(f3.dataPtr(), f1.dataPtr(), 20*30*sizeof(float)))
        {
        std::cout << "data compare fail using dataPtr()" << std::endl;
        return false;
        }
    for(xpos = f1.xDim(); xpos>0; xpos--)
       {
       unsigned int x = xpos - 1;
       for(ypos = 0; ypos< f1.yDim(); ypos++)
           {
           if (f3(x,ypos) != f1(x,ypos))
               {
               std::cout << "data compare fail using () operator " << std::endl;
               return false;
               }
            }
       }

    std::cout << "Success" << std::endl;

    std::cout << "....Copy constructor test: " ;

     // test out copy constructor
     Grid2D<float> f4(f1);
     if(!f4.isValid() || f4.xDim() != 20 || f4.yDim() != 30)
         {
         std::cout << "dimensions wrong" << std::endl;
         return false;
         }
    if (memcmp(f4.dataPtr(), f1.dataPtr(), 20*30*sizeof(float)))
        {
        std::cout << "data compare using dataPtr()" << std::endl;
        return false;
        }
    for(xpos = 0; xpos < f1.xDim(); xpos++)
       for(ypos = 0; ypos< f1.yDim(); ypos++)
           {
           if (f4(xpos,ypos) != f1(xpos,ypos))
               {
               std::cout << "data compare fail using () operator " << std::endl;
               return false;
               }
            }

    std::cout << "Success" << std::endl;

    std::cout << "....() operator test: ";

    // test storage capability with () operators
    for(xpos=0; xpos < f2.xDim(); xpos++)
        for(ypos = 0; ypos < f2.yDim(); ypos++)
            f2(xpos,ypos) = (100.0*ypos)+xpos;
    for(ypos=0; ypos < f2.yDim(); ypos++)
        for(xpos = 0; xpos < f2.xDim(); xpos++)
            if(f2(xpos,ypos) != (100.0*ypos)+xpos)
                {   
                std::cout << "() storage/retrieval fail with ramp data" << std::endl;
                return false;
                }

    std::cout << "Success" << std::endl;

    std::cout << "....Assignment of invalid grid to good grid test: " ;

    // check assignment from invalid grid to good 
    Grid2D<float> f5;
    f1=f5;

    // checking dimensions for invalid array
    if (f1.isValid())
        {
        std::cout << "isValid() incorrect" << std::endl;
        return false;
        }
    if (f1.xDim() != 0)
        {
        std::cout << "xDim() incorrect " << std::endl;
        return false;
        }
    if (f1.yDim() != 0)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    std::cout << "....Subgrid test: ";

    // subarray test
    f5 = f2.subGrid(5, 4, 9, 10);
    if(!f5.isValid() || f5.xDim() != 5 || f5.yDim() != 7)
        {
        std::cout << "subarray dimensions are incorrect" << std::endl;
        return false;
        }
    for(xpos = 0; xpos < 5; xpos++)
        for(ypos = 0; ypos < 7; ypos++)
            if(f5(xpos,ypos) != (100.0*(ypos+4))+(xpos+5))
                {
                std::cout << "subarray data is incorrect" << std::endl;
                return false;
                }

    std::cout << "Success" << std::endl;


    std::cout << "....output test: " << f2 << std::endl;


    std::cout << "Success" << std::endl;
                
    std::cout << "..Two-D Float Grid Test Completed Successfully" << std::endl;
                
    delete [] (float *)fp;
    return true;
    }

// -- global ------------------------------------------------------------------
// intTest()
// Tests Grid2D<int> class.  Returns true if all tests were successful.
//-----------------------------------------------------------------------------
bool intTest()
    {
    int *ip1;
    const int *ip;
    unsigned int xpos, ypos, index;

    std::cout << "..Testing Two-Dimensional Array of Ints" << std::endl;

    // Create invalid array
    std::cout << "....invalid array test : ";
    Grid2D<int> f1;

    // checking dimensions for invalid grid
    if (f1.isValid())
        {
        std::cout << " isValid() incorrect" << std::endl;
        return false;
        }
    if (f1.xDim() != 0)
        {
        std::cout << "xDim() incorrect" << std::endl;
        return false;
        }
    if (f1.yDim() != 0)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    // create 10 x 20 int array without any data, index through it
    // testing for 0 values

    std::cout << "....Array with no data test: ";
    Grid2D<int> f2(10, 20);

    // checking dimensions for 10x20 array
    if (!f2.isValid())
        {
        std::cout << ": isValid() incorrect " << std::endl;
        return false;
        }
    if (f2.xDim() != 10)
        {
        std::cout << "xDim() incorrect " << std::endl;
        return false;
        }
    if (f2.yDim() != 20)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    ip = f2.dataPtr();
    for (index = 0; index < 200; index++, ip++)
       {
       if (*ip != 0)
           {
           std::cout << "null data initialized improperly or" << std::endl;
           std::cout << "dataPtr() is incorrect" << std::endl;
           return false;
           }
        }
    for (xpos = 0; xpos < 10; xpos++)
        for (ypos = 0; ypos < 20; ypos++)
            {
            if (f2(xpos,ypos) != 0)
                {
                std::cout << "null data initialized improperly" << std::endl;
                std::cout << " or access via () operators is incorrect" << std::endl;
                return false;
                }
            }
    
    std::cout << "Success" << std::endl;
    
    std::cout << "....Array with data test :";
    // create 20 x 30 int array with row/col numbers, index and check
    ip = new int [20 * 30];
    ip1 = (int *) ip;
    if (!ip)
        {
        std::cout << "new memory allocation fail" << std::endl;
        return false;
        }
    for (index = 0; index < 20*30; index++, ip1++)
        *ip1 = index;
    Grid2D<int> f3(20, 30, ip);
    if (memcmp(f3.dataPtr(), ip, 20*30*sizeof(int)))
        {
        std::cout << "ramp data initialization fail" << std::endl;
        return false;
        }
    for (index = 0, ypos = 0; ypos < 30; ypos++)
        for (xpos = 0; xpos < 20; xpos++)
            {
            if (f3(xpos,ypos) != (int)index)
                {
                std::cout << "ramp data differs from () access" << std::endl;
                return false;
                }
            index++;
            }

     std::cout << "Success" << std::endl;

     std::cout << "....Assignment operator test: ";

     // test out assignment operator
     f1 = f3;
     if(!f1.isValid() || f1.xDim() != 20 || f1.yDim() != 30)
         {
         std::cout << "dimensions wrong"  << std::endl;
         return false;
         }
    if (memcmp(f3.dataPtr(), f1.dataPtr(), 20*30*sizeof(int)))
        {
        std::cout << "data compare fail using dataPtr()" << std::endl;
        return false;
        }
    for(xpos = f1.xDim()-1; xpos>0; xpos--)
       {
       unsigned int x = xpos - 1;
       for(ypos = 0; ypos< f1.yDim(); ypos++)
           {
           if (f3(x,ypos) != f1(x,ypos))
               {
               std::cout << "data compare fail using () operator " << std::endl;
               return false;
               }
            }
       }

    std::cout << "Success" << std::endl;

    std::cout << "....Copy constructor test: ";

     // test out copy constructor
     Grid2D<int> f4(f1);
     if(!f4.isValid() || f4.xDim() != 20 || f4.yDim() != 30)
         {
         std::cout << "dimensions wrong" << std::endl;
         return false;
         }
    if (memcmp(f4.dataPtr(), f1.dataPtr(), 20*30*sizeof(int)))
        {
        std::cout << "data compare using dataPtr()" << std::endl;
        return false;
        }
    for(xpos = 0; xpos < f1.xDim(); xpos++)
       for(ypos = 0; ypos< f1.yDim(); ypos++)
           {
           if (f4(xpos,ypos) != f1(xpos,ypos))
               {
               std::cout << "data compare fail using () operator " << std::endl;
               return false;
               }
            }

    std::cout << "Success" << std::endl;

    std::cout << "....() operator test: ";

    // test storage capability with () operators
    for(xpos=0; xpos < f2.xDim(); xpos++)
        for(ypos = 0; ypos < f2.yDim(); ypos++)
            f2(xpos,ypos) = (100*ypos)+xpos;
    for(ypos=0; ypos < f2.yDim(); ypos++)
        for(xpos = 0; xpos < f2.xDim(); xpos++)
            if(f2(xpos,ypos) != (int)((100*ypos)+xpos))
                {   
                std::cout << "() storage/retrieval fail with ramp data" << std::endl;
                return false;
                }

    std::cout << "Success" << std::endl;

    std::cout << "....Assignment of invalid to good grid test: ";

    // check assignment from invalid grid to good grid
    Grid2D<int> f5;
    f1=f5;

    // checking dimensions for invalid array
    if (f1.isValid())
        {
        std::cout << "isValid() incorrect" << std::endl;
        return false;
        }
    if (f1.xDim() != 0)
        {
        std::cout << "xDim() incorrect " << std::endl;
        return false;
        }
    if (f1.yDim() != 0)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    std::cout << "....Sub Grid test: ";

    // subarray test
    f5 = f2.subGrid(5, 4, 9, 10);
    if(!f5.isValid() || f5.xDim() != 5 || f5.yDim() != 7)
        {
        std::cout << "subGrid dimensions are incorrect" << std::endl;
        return false;
        }
    for(xpos = 0; xpos < 5; xpos++)
        for(ypos = 0; ypos < 7; ypos++)
            if(f5(xpos,ypos) != (int)((100*(ypos+4))+(xpos+5)))
                {
                std::cout << "subGrid data is incorrect" << std::endl;
                return false;
                }
    std::cout << "Success" << std::endl;

    std::cout << "....output test: " << f2 << std::endl;

    std::cout << "Success" << std::endl;
                
    std::cout << "..Int Array Test Completed Successfully" << std::endl;

    delete [] (int *)ip;
    return true;
    }


// -- global ------------------------------------------------------------------
// coordTest()
// Tests the CartCoord2D<T> class.  Returns true if all tests were successful.
//-----------------------------------------------------------------------------
bool coordTest()
    {
    std::cout << "..FloatCoord tests" << std::endl;

    CartCoord2D<float> i1,i3;
    CartCoord2D<float> i2(10.0,20.0);

    if(i1.x != 0 || i1.y != 0)
        {
        std::cout << "default constructor failed.." << std::endl;
        return false;
        }

    if (i2.x != 10.0 || i2.y != 20.0)
        {
        std::cout << "constructor (x,y) failed.." << std::endl;
        return false;
        }

    i1.x = 10;
    i1.y = 20;
    if (!(i1==i2))
        {
        std::cout << "== operator fail when equal" << std::endl;
        return false;
        }
    if ((i1==i3))
        {
        std::cout << "== operator fail when not-equal" << std::endl;
        return false;
        }
    if ((i1!=i2))
        {
        std::cout << "!= operator fail when equal" << std::endl;
        return false;
        }
    if (!(i1!=i3))
        {
        std::cout << "!= operator fail when not-equal" << std::endl;
        return false;
        }
    
    std::cout << "Success." << std::endl;
    std::cout << "....output test: should be (10,20): " << i2 << std::endl;

    std::cout << "..CartCoord2D<float> Test Success" << std::endl;
    return true;
    }

// -- global ------------------------------------------------------------------
// bitTest()
// Tests Grid2DBit class.  Returns true if all tests are successful.
//-----------------------------------------------------------------------------
bool bitTest()
    {
    unsigned int xpos, ypos, index;
    unsigned int arrSize;
    bool *fp;

    std::cout << "..Testing Two-Dimensional Array of Bits" << std::endl;

    // Create NULL array
    std::cout << "....NULL array test : ";
    Grid2DBit f1;

    // checking dimensions for invalid array
    if (f1.isValid())
        {
        std::cout << " isValid() incorrect" << std::endl;
        return false;
        }
    if (f1.xDim() != 0)
        {
        std::cout << "xDim() incorrect" << std::endl;
        return false;
        }
    if (f1.yDim() != 0)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    // create 10 x 22 bit array without any data, index through it
    // testing for 0 values

    std::cout << "....Array with no data test: ";
    Grid2DBit f2(10, 22);

    // checking dimensions for 10x22 array
    if (!f2.isValid())
        {
        std::cout << ": isValid() incorrect " << std::endl;
        return false;
        }
    if (f2.xDim() != 10)
        {
        std::cout << "xDim() incorrect " << std::endl;
        return false;
        }
    if (f2.yDim() != 22)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    fp = (bool *)f2.dataPtr();
    arrSize = f2.dataSize();
    if (arrSize != 10*22*sizeof(bool))
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
    for (xpos = 0; xpos < 10; xpos++)
        for (ypos = 0; ypos < 22; ypos++)
            {
            if (f2.get(xpos,ypos) != false)
                {
                std::cout << "null data initialized improperly" << std::endl;
                std::cout << " or access via get() is incorrect" << std::endl;
                return false;
                }
            }
    
    std::cout << "Success" << std::endl;
    
    std::cout << "....Array with data test :";
    // create 3 x 9 bit array with alternating rows set

    fp = new bool[27];
    if (!fp)
        {
        std::cout << "new memory allocation fail" << std::endl;
        return false;
        }
    static bool answer[9] = {true, false, true, false, true, false, 
      true, false, true};
    for (ypos = 0; ypos < 9; ypos++)
        for (xpos = 0; xpos < 3; xpos++)
            fp[xpos + (ypos*3)] = answer[ypos];
    Grid2DBit f3(3, 9, fp);
    if (memcmp(f3.dataPtr(), fp, 27*sizeof(bool)))
        {
        std::cout << "ramp data initialization fail" << std::endl;
        return false;
        }
    for (ypos = 0; ypos < 9; ypos++)
        for (xpos = 0; xpos < 3; xpos++)
            {
            if (f3.get(xpos,ypos) != answer[ypos])
                {
                std::cout << "ramp data differs from get access" << std::endl;
                return false;
                }
            }

     std::cout << "Success" << std::endl;

     std::cout << "....Assignment operator test: ";

     // test out assignment operator
     f1 = f3;
     if(!f1.isValid() || f1.xDim() != 3 || f1.yDim() != 9)
         {
         std::cout << "dimensions wrong"  << std::endl;
         return false;
         }
    if (memcmp(f3.dataPtr(), f1.dataPtr(), 4))
        {
        std::cout << "data compare fail using dataPtr()" << std::endl;
        return false;
        }
    for(xpos = 0; xpos < f1.xDim(); xpos++)
       for(ypos = 0; ypos< f1.yDim(); ypos++)
           {
           if (f3.get(xpos,ypos) != f1.get(xpos,ypos))
               {
               std::cout << "data compare fail using get operator " << std::endl;
               return false;
               }
            }

    std::cout << "Success" << std::endl;

    std::cout << "....Copy constructor test: " ;

     // test out copy constructor
     Grid2DBit f4(f1);
     if(!f4.isValid() || f4.xDim() != 3 || f4.yDim() != 9)
         {
         std::cout << "dimensions wrong" << std::endl;
         return false;
         }
    if (memcmp(f4.dataPtr(), f1.dataPtr(), 4))
        {
        std::cout << "data compare using dataPtr()" << std::endl;
        return false;
        }
    for(xpos = 0; xpos < f1.xDim(); xpos++)
       for(ypos = 0; ypos< f1.yDim(); ypos++)
           {
           if (f4.get(xpos,ypos) != f1.get(xpos,ypos))
               {
               std::cout << "data compare fail using get operator " << std::endl;
               return false;
               }
            }

    std::cout << "Success" << std::endl;

    std::cout << "....set bit test: ";

    // test storage capability with () operators
    f1.set(0,0);
    f1.set(1,1);
    f1.set(1,5);
    f1.clear(0,4);
    f1.clear(2,8);
    f1.set(0,7);
    f1.clear(0,1);

    // solid comparision between set and get
    if (f1.get(0,0) != true  || f1.get(1,0) != true  || f1.get(2,0) != true
     || f1.get(0,1) != false || f1.get(1,1) != true  || f1.get(2,1) != false
     || f1.get(0,2) != true  || f1.get(1,2) != true  || f1.get(2,2) != true
     || f1.get(0,3) != false || f1.get(1,3) != false || f1.get(2,3) != false
     || f1.get(0,4) != false || f1.get(1,4) != true  || f1.get(2,4) != true
     || f1.get(0,5) != false || f1.get(1,5) != true  || f1.get(2,5) != false
     || f1.get(0,6) != true  || f1.get(1,6) != true  || f1.get(2,6) != true
     || f1.get(0,7) != true  || f1.get(1,7) != false || f1.get(2,7) != false
     || f1.get(0,8) != true  || f1.get(1,8) != true  || f1.get(2,8) != false)
        {
        std::cout << "set/get comparision failed " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    std::cout << "....output test: " << f1 << std::endl;

    // test the logical OR
    std::cout << ".... Testing logical OR: ";
    Grid2DBit f6(3,9);
    f6.set(0, 1);
    f6.set(0, 0);
    f6.set(1, 8);
    f6.set(2, 8);
    Grid2DBit orTest = f6 | f1;
    f1 |= f6;
    if (orTest != f1)
        {
        std::cout << "OR test failed for | operator" << std::endl;
        return false;
        }
    fp[0] = 0xdf;
    fp[1] = 0x61;
    fp[2] = 0x3d;
    fp[3] = 0x7;
    if (memcmp(fp, f1.dataPtr(), 4))
        {
        std::cout << "logical OR command  failed " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    std::cout << "....Assignment of invalid grid to good grid test: " ;

    // check assignment from invalid array to good array
    Grid2DBit f5;
    f1=f5;

    // checking dimensions for invalid array
    if (f1.isValid())
        {
        std::cout << "isValid() incorrect" << std::endl;
        return false;
        }
    if (f1.xDim() != 0)
        {
        std::cout << "xDim() incorrect " << std::endl;
        return false;
        }
    if (f1.yDim() != 0)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    std::cout << "....output test: " << f1 << std::endl;
                
    std::cout << "....Subgrid test: " ;
    Grid2DBit biggrid(3,5);
    biggrid.set(1,4);
    biggrid.set(2,1);
    biggrid.set(0,3);
    biggrid.set(2,4);
    biggrid.set(1,1);
    biggrid.set(1,2);
    Grid2DBit sg = biggrid.subGrid(1, 1, 2, 3);
    if (sg.get(0,0) != true  || sg.get(1,0) != true  
     || sg.get(0,1) != true  || sg.get(1,1) != false
     || sg.get(0,2) != false || sg.get(1,2) != false)
        {
        std::cout << "subgrid failed " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    std::cout << "....FindNearestSet test: ";
    Grid2DBit h(3,5);
    h.set(1,4);
    h.set(1,3);
    h.set(2,1);
    h.set(0,3);
    CartCoord2D<int> loc;
    bool t1 = h.findNearestSet(CartCoord2D<int>(60,60), loc);
    if (t1)
       {
       std::cout << "findNearestSet bad coord failed" << std::endl;
       return false;
       }
    t1 = h.findNearestSet(CartCoord2D<int>(1,4), loc);
    if (!t1 || loc != CartCoord2D<int>(1,4))
       {
       std::cout << "findNearestSet match failed1 " << loc << std::endl;
       return false;
       }

    t1 = h.findNearestSet(CartCoord2D<int>(0,0), loc);
    if (!t1 || loc != CartCoord2D<int>(2,1))
       {
       std::cout << "findNearestSet match failed2 " << loc << std::endl;
       return false;
       }

    t1 = h.findNearestSet(CartCoord2D<int>(2,3), loc);
    if (!t1 || loc != CartCoord2D<int>(1,4))
       {
       std::cout << "findNearestSet match failed3 " << loc << std::endl;
       return false;
       }
    std::cout << "Success" << std::endl;


    std::cout << "..Bit Grid Test Completed Successfully" << std::endl;
                
    delete [] fp;
    return true;
    }

// -- global ------------------------------------------------------------------
// areaTest()
// Tests Grid2DBit class.  Returns true if all tests are successful.
//-----------------------------------------------------------------------------
bool areaTest()
    {
    std::cout << "..Testing Two-Dimensional Array of Bits - Area version" << std::endl;

    Grid2DBit f1(7, 5);
    f1.set(2,1);
    f1.set(2,2);
    f1.set(3,2);
    f1.set(2,3);
    f1.set(3,3);
    Grid2DBit contigAnswer = f1;
    f1.set(5,1);

    CartCoord2D<int> pos(0,0);
    Grid2DBit contig = f1.contiguousBitArray(pos);
    if (contig.xDim() != f1.xDim() || contig.yDim() != f1.yDim())
        {
        std::cout << "wrong size grid returned from contiguousBitArray" << std::endl;
        return false;
        }

    for (unsigned int i = 0; i < contig.dataSize(); i++)
        if (contig.dataPtr()[i])
            {
            std::cout << "bad data from contiguousBitArray for zero case" << std::endl;
            return false;
            }

    pos.x = 2; 
    pos.y = 3;
    std::cout << "Source of contig bit array: " << f1 << std::endl;
    contig = f1.contiguousBitArray(pos);
    std::cout << "Contig Bit Array for point (2,3) is: " << contig << std::endl;
    if (contig != contigAnswer)
        {
        std::cout << "bad contig area from contiguousBitArray for non-zero case"
          << std::endl;
        return false;
        }

    CartCoord2D<int> lowerLeft, upperRight;
    CartCoord2D<int> lowerLeftAnswer(2,1), upperRightAnswer(5,3);
    if (!f1.extremaOfSetBits(lowerLeft, upperRight))
        {
        std::cout << "false returned when should be true for extremaOfSetBits"
          << std::endl;
        return false;
        }

    if (lowerLeft != lowerLeftAnswer || upperRight != upperRightAnswer)
        {
        std::cout << "bad results from extremaOfSetBits when data exists" 
          << std::endl;
        return false;
        }

    Grid2DBit empty(20,20);
    if (empty.extremaOfSetBits(lowerLeft, upperRight))
        {
        std::cout << "true returned incorrectly from extremaOfSetBits" << std::endl;
        return false;
        }

    Grid2DBit emptyAndInvalid;
    if (emptyAndInvalid.extremaOfSetBits(lowerLeft, upperRight))
        {
        std::cout << "true returned incorrectly from invalid grid "
          << "extremaOfSetBits" << std::endl;
        return false;
        }

    // translate test
    Grid2DBit translateOriginal(5,5);
    translateOriginal.set(1,2);
    translateOriginal.set(1,4);
    translateOriginal.set(3,0);
    Grid2DBit trans1 = translateOriginal.translate(CartCoord2D<int>(2,1));
    Grid2DBit trans2 = translateOriginal.translate(CartCoord2D<int>(-1,-1));
    Grid2DBit trans3 = translateOriginal.translate(CartCoord2D<int>(3,-2));

    {
    static bool set1[25] = {0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,1,0, 
      0,0,0,0,0};
    static bool set2[25] = {0,0,0,0,0, 1,0,0,0,0, 0,0,0,0,0, 1,0,0,0,0, 
      0,0,0,0,0};
    static bool set3[25] = {0,0,0,0,1, 0,0,0,0,0, 0,0,0,0,1, 0,0,0,0,0,
      0,0,0,0,0};
    for (int a = 0; a < 4; a++)
        for (int b = 0; b < 4; b++)
            {
            if (trans1.get(a,b) != set1[a+(b*trans1.yDim())]
              || trans2.get(a,b) != set2[a+(b*trans2.yDim())]
              || trans3.get(a,b) != set3[a+(b*trans3.yDim())])
                {
                std::cout << "translate failed " << std::endl;
                return false;
                }
            }
    translateOriginal.translateMe(CartCoord2D<int>(3,-2));
    if (translateOriginal != trans3)
        {
        std::cout << "translateMe() failed " << std::endl;
        return false;
        }

    }

    // contigiousBitArrayLocations test
    {
    Grid2DBit base(9,7);
    static bool baseSet[63] = {0,0,1,1,0,0,0,1,0,
      0,0,1,0,0,0,0,0,0, 0,0,0,0,0,1,1,1,1, 1,0,0,0,0,1,0,0,1,
      1,0,1,0,0,1,1,0,0, 1,1,1,0,0,0,1,1,1, 0,1,0,0,0,0,0,0,1};

    static bool base1Set[63] = {0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0, 1,0,0,0,0,0,0,0,0,
      1,0,1,0,0,0,0,0,0, 1,1,1,0,0,0,0,0,0, 0,1,0,0,0,0,0,0,0};
    static bool base2Set[63] = {0,0,1,1,0,0,0,0,0,
      0,0,1,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0};
    static bool base3Set[63] = {0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0, 0,0,0,0,0,1,1,1,1, 0,0,0,0,0,1,0,0,1,
      0,0,0,0,0,1,1,0,0, 0,0,0,0,0,0,1,1,1, 0,0,0,0,0,0,0,0,1};
    static bool base4Set[63] = {0,0,0,0,0,0,0,1,0,
      0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0};
    // initialize it
    int i,j;
    for (i = 0; i < 9; i++)
        for (j = 0; j < 7; j++)
            if (baseSet[i + (j*base.xDim())])
                base.set(i,j);

    SeqOf<CartCoord2D<int> > areas = base.contiguousBitArrayLocations();
    if (areas.length() != 4)
        {
        std::cout << "Wrong number of areas from contiguosBitArrayLocations()"
          << std::endl;
        return false;
        }

    Grid2DBit answer1 = base.contiguousBitArray(areas[0]);
    Grid2DBit answer2 = base.contiguousBitArray(areas[1]);
    Grid2DBit answer3 = base.contiguousBitArray(areas[2]);
    Grid2DBit answer4 = base.contiguousBitArray(areas[3]);
    for (i = 0; i < 9; i++)
        for (j = 0; j < 7; j++)
            if (answer1.get(i,j) != base1Set[i + (j*9)] 
              || answer2.get(i,j) != base2Set[i + (j*9)] 
              || answer3.get(i,j) != base3Set[i + (j*9)] 
              || answer4.get(i,j) != base4Set[i + (j*9)] )
                {
                std::cout << "contigiuousBitArrayLocations() failed" << std::endl;
                return false;
                }
    }
    return true;
    }

// -- global ------------------------------------------------------------------
// threeDTest()
// Tests Grid3D<float> class.  Returns true if all tests are successful.
//-----------------------------------------------------------------------------
bool threeDTest()
    {
    float *fp1;
    const float *fp;
    unsigned int xpos, ypos, zpos, index;

    std::cout << "..Testing Three-Dimensional Grid of Floats" << std::endl;

    // Create invalid array
    std::cout << "....invalid array test : ";
    Grid3D<float> f1;

    // checking dimensions for invalid array
    if (f1.isValid())
        {
        std::cout << " isValid() incorrect" << std::endl;
        return false;
        }
    if (f1.xDim() != 0)
        {
        std::cout << "xDim() incorrect" << std::endl;
        return false;
        }
    if (f1.yDim() != 0)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    if (f1.zDim() != 0)
        {
        std::cout << "zDim() incorrect " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    // create 10 x 20 x 2 float array without any data, index through it
    // testing for 0.0 values

    std::cout << "....Array with no data test: ";
    Grid3D<float> f2(10, 20, 2);

    // checking dimensions for 10x20x2 array
    if (!f2.isValid())
        {
        std::cout << ": isValid() incorrect " << std::endl;
        return false;
        }
    if (f2.xDim() != 10)
        {
        std::cout << "xDim() incorrect " << std::endl;
        return false;
        }
    if (f2.yDim() != 20)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    if (f2.zDim() != 2)
        {
        std::cout << "zDim() incorrect " << std::endl;
        return false;
        }
    fp = f2.dataPtr();
    for (index = 0; index < 400; index++, fp++)
       {
       if (*fp != 0.0)
           {
           std::cout << "null data initialized improperly or" << std::endl;
           std::cout << "dataPtr() is incorrect" << std::endl;
           return false;
           }
        }
    for (xpos = 0; xpos < 10; xpos++)
        for (ypos = 0; ypos < 20; ypos++)
            for (zpos = 0; zpos < 2; zpos++)
                {
                if (f2(xpos,ypos, zpos) != 0.0)
                    {
                    std::cout << "null data initialized improperly" << std::endl;
                    std::cout << " or access via () operators is incorrect" 
                      << std::endl;
                    return false;
                    }
                }
    
    std::cout << "Success" << std::endl;
    
    std::cout << "....Array with data test :";
    // create 20 x 30 x 2 float array with row/col/level numbers, index 
    // and check
    fp = new float [20 * 30 * 2];
    fp1 = (float *) fp;
    if (!fp)
        {
        std::cout << "new memory allocation fail" << std::endl;
        return false;
        }
    for (index = 0; index < 20*30*2; index++, fp1++)
        *fp1 = index;
    Grid3D<float> f3(20, 30, 2, fp);
    if (memcmp(f3.dataPtr(), fp, 20*30*2*sizeof(float)))
        {
        std::cout << "ramp data initialization fail" << std::endl;
        return false;
        }
    for (index = 0, zpos = 0; zpos < 2; zpos++)
        for (ypos = 0; ypos < 30; ypos++)
            for (xpos = 0; xpos < 20; xpos++)
                {
                if (f3(xpos,ypos,zpos) != (float)index)
                    {
                    std::cout << "ramp data differs from () access" << std::endl;
                    return false;
                    }
                index++;
                }

     std::cout << "Success" << std::endl;

     std::cout << "....Assignment operator test: ";

     // test out assignment operator
     f1 = f3;
     if(!f1.isValid() || f1.xDim() != 20 || f1.yDim() != 30 || f1.zDim() != 2)
         {
         std::cout << "dimensions wrong"  << std::endl;
         return false;
         }
    if (memcmp(f3.dataPtr(), f1.dataPtr(), 20*30*2*sizeof(float)))
        {
        std::cout << "data compare fail using dataPtr()" << std::endl;
        return false;
        }
    for (zpos = f1.zDim(); zpos < 2; zpos++)
        for(xpos = f1.xDim(); xpos>0; xpos--)
           {
           unsigned int x = xpos - 1;
           for(ypos = 0; ypos< f1.yDim(); ypos++)
               {
               if (f3(x,ypos,zpos) != f1(x,ypos,zpos))
                   {
                   std::cout << "data compare fail using () operator " << std::endl;
                   return false;
                   }
                }
           }

    std::cout << "Success" << std::endl;

    std::cout << "....Copy constructor test: " ;

     // test out copy constructor
     Grid3D<float> f4(f1);
     if(!f4.isValid() || f4.xDim() != 20 || f4.yDim() != 30 || f4.zDim() != 2)
         {
         std::cout << "dimensions wrong" << std::endl;
         return false;
         }
    if (memcmp(f4.dataPtr(), f1.dataPtr(), 20*30*2*sizeof(float)))
        {
        std::cout << "data compare using dataPtr()" << std::endl;
        return false;
        }
    for (zpos = 0; zpos < f1.zDim(); zpos++)
        for(xpos = 0; xpos < f1.xDim(); xpos++)
           for(ypos = 0; ypos< f1.yDim(); ypos++)
               {
               if (f4(xpos,ypos,zpos) != f1(xpos,ypos,zpos))
                   {
                   std::cout << "data compare fail using () operator " << std::endl;
                   return false;
                   }
                }

    std::cout << "Success" << std::endl;

    std::cout << "....() operator test: ";

    // test storage capability with () operators
    for(xpos=0; xpos < f2.xDim(); xpos++)
        for(ypos = 0; ypos < f2.yDim(); ypos++)
            for (zpos = 0; zpos < f2.zDim(); zpos++)
                f2(xpos,ypos,zpos) = (10000.0*zpos) + (100.0*ypos) + xpos;
    for(ypos=0; ypos < f2.yDim(); ypos++)
        for(xpos = 0; xpos < f2.xDim(); xpos++)
            for (zpos = 0; zpos < f2.zDim(); zpos++)
                if(f2(xpos,ypos,zpos) != (10000.0*zpos) + (100.0*ypos) + xpos)
                    {   
                    std::cout << "() storage/retrieval fail with ramp data" << std::endl;
                    return false;
                    }

    std::cout << "Success" << std::endl;

    std::cout << "....Assignment of invalid grid to good grid test: " ;

    // check assignment from invalid grid to good 
    Grid3D<float> f5;
    f1=f5;

    // checking dimensions for invalid array
    if (f1.isValid())
        {
        std::cout << "isValid() incorrect" << std::endl;
        return false;
        }
    if (f1.xDim() != 0)
        {
        std::cout << "xDim() incorrect " << std::endl;
        return false;
        }
    if (f1.yDim() != 0)
        {
        std::cout << "yDim() incorrect " << std::endl;
        return false;
        }
    if (f1.zDim() != 0)
        {
        std::cout << "zDim() incorrect " << std::endl;
        return false;
        }
    std::cout << "Success" << std::endl;

    std::cout << "....Subgrid test: ";

    // subarray test
    f5 = f2.subGrid(5, 4, 1, 9, 10, 1);
    if(!f5.isValid() || f5.xDim() != 5 || f5.yDim() != 7 || f5.zDim() != 1)
        {
        std::cout << "subarray dimensions are incorrect" << std::endl;
        return false;
        }
    for(xpos = 0; xpos < 5; xpos++)
        for(ypos = 0; ypos < 7; ypos++)
            for (zpos = 0; zpos < 1; zpos++)
                if(f5(xpos,ypos,zpos) != (10000.0*(zpos+1)) + 
                  (100.0*(ypos+4))+(xpos+5))
                    {
                    std::cout << "subarray data is incorrect" << std::endl;
                    return false;
                    }

    std::cout << "Success" << std::endl;


    std::cout << "....output test: " << f2 << std::endl;


    std::cout << "Success" << std::endl;
                
    std::cout << "..Three-D Float Grid Test Completed Successfully" << std::endl;
                
    delete [] (float *)fp;
    return true;
    }

