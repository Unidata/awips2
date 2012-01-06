// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// seqTest.C
// Array Class Test Program
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const seqTest_C_Id =
 "$Id: .seqTest.C__temp27950,v 1.2 2003/05/06 23:11:59 fluke Exp $";
#endif
//-----------------------------------------------------------------------------

// -- module ------------------------------------------------------------------
// arrayTest is a program to fully exercise the array class.  The array
// class is created using a template.  This test program tests an array
// containing data and an array containing pointers.  
//-----------------------------------------------------------------------------

#include <iostream>
#include <stdlib.h>
#include "commonDefs.h"
#include "SeqOf.H"
#include "SeqOfPtr.H"
#include "TextString.H"
#include <assert.h>

// main test file

bool ptrArrayTest();
bool arrayTest();
bool nowArrayTest();


// -- global ------------------------------------------------------------------
// main()
// Main driver program to test array class.
//-----------------------------------------------------------------------------
int main()
    {
    std::cout << "Array Test Program " << std::endl;
    std::cout << "Testing Float Array class" << std::endl;
    if(!arrayTest())
        {
        std::cout << "..Array test failed" << std::endl;
        return 1;
        }

    std::cout << "Testing Float Ptr Array class" << std::endl;
    if(!ptrArrayTest())
        {
        std::cout << "..Ptr Array test failed" << std::endl;
        return 1;
        }

    std::cout << "Testing Float Array class with setLength() allocation" << std::endl;
    if(!nowArrayTest())
        {
        std::cout << "..NOW Array test failed" << std::endl;
        return 1;
        }

    std::cout << "Testing Completed Successfully" << std::endl;
    return 0;
    }


// -- global ------------------------------------------------------------------
// ptrArrayTest()
// Array of pointers test routines.  Returns true if all tests were successful.
// -- implementation ----------------------------------------------------------
// Does very little.
//-----------------------------------------------------------------------------
bool ptrArrayTest()
    {
    SeqOfPtr<float *> fp;
    static float f[4] = {1, 2, 3, 4};
    fp.append(&f[0]);
    fp.append(&f[1]);
    fp.append(&f[2]);
    fp.append(&f[3]);
    std::cout << "ptrArray output test: " << std::endl;
    std::cout << fp << std::endl;

    // try the not in list command
    SeqOfPtr<float*> fp1;
    fp1.append(&f[2]);
    fp1.append(&f[3]);
    SeqOfPtr<float*> notList = fp.notInList(fp1);
    if (notList.length() != 2)
        {
        std::cout << "notInList error" << std::endl;
        return false;
        }
        
    // try the in list command
    SeqOfPtr<float*> common = fp1.in(fp);
    if (common.length() != 2)
        {
        std::cout << "in error" << std::endl;
        return false;
        }

    return true;
    }

// -- global ------------------------------------------------------------------
// arrayTest()
// Array test routines.  Returns true if all tests were successful.
//-----------------------------------------------------------------------------
bool arrayTest()
    {
    SeqOf<float> array; 
    int i;

    if (array.length() != 0)
        {
        std::cout << "length() incorrect on empty array" << std::endl;
        return false;
        }
    
    // append test - single elements
    for (i = 0; i < 100; i++)
        array.append((float)i*2);
    if (array.length() != 100)
        {
        std::cout << "length() incorrect on array" << std::endl;
        return false;
        }

    // [] operator test
    for (i = 0; i < 100; i++)
        if(array[i] != 2.0 * i)
            {
            std::cout << "operator[] retrieval data incorrect" << std::endl;
            return false;
            }

    // [] operator test
    for (i = 0; i < 99; i++)
        {
        array[i] = array[i+1];
        if (array[i] != array[i+1] || array[i] != 2.0 * (i+1))
            {
            std::cout << "operator[] storage data incorrect" << std::endl;
            return false;
            }
        }

    // squeeze test
    for (i = 0; i < 100; i++)
        array[i] = (float) i;
    const float *p = array.dataPtr();
    for (i = 0; i < 100; i++)
        if (array[i] != (float)i)
            {
            std::cout << "squeeze changed data, it shouldn't have" << std::endl;
            return false;
            }

     // pointer test
     float *pf = new float [100];
     assert(pf);
     float *px = pf;
     for (i = 0; i < 100; i++)
         *px++ = (float)i;
     if(memcmp(p, pf, 100 * sizeof(float)))
         {
         for (i=0;i<100;i++)
             std::cout << *(p+i) << ' ' << *(pf+i) << ' ' << i << std::endl;
         std::cout << "incorrect response to dataPtr()" << std::endl;
         return false;
         }
    
     // assignment operator
     SeqOf<float> array1;
     array1 = array;
     if (array1.length() != array.length())
         {
         std::cout << "length() incorrect after assignment" << std::endl;
         return false;
         }
     for (i = 0; i < array1.length(); i++)
         if (array1[i] != array[i])
             {
             std::cout << "data different after assignment" << std::endl;
             return false;
             }

     // copy constructor test
     SeqOf<float> array2(array1);
     if (array1.length() != array2.length())
         {
         std::cout << "length() incorrect after copy constructor" << std::endl;
         return false;
         }
     for (i = 0; i < array1.length(); i++)
         if (array1[i] != array2[i])
             {
             std::cout << "data different after copy constructor" << std::endl;
             return false;
             }

     // data initializer constructor test
     SeqOf<float> array3(pf, 100);
     if (array3.length() != array2.length())
         {
         std::cout << "length() incorrect after constructor(dataptr,length)" << std::endl;
         return false;
         }
     for (i = 0; i < array3.length(); i++)
         if (array3[i] != array2[i])
             {
             std::cout << "data different after constructor(dataptr,length)" << std::endl;
             return false;
             }
     p = array3.dataPtr();
     if(memcmp(p, pf, 100 * sizeof(float)))
         {
         std::cout << "incorrect response to dataPtr() after constructor" << std::endl;
         std::cout << "    (dataptr,length)" << std::endl;
         return false;
         }
     delete [] pf;
    
    // array manipulations setup
    SeqOf<float> a;  // force many copies
    static float fp[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
    for (i = 0; i < 16; i++)
        a.append(fp[i]);  // fill up test array
    std::cout << a << std::endl;  // output test

    // remove - single element test 
    a.remove(15,15);
    a.remove(0,0);
    a.remove(10,10);
    static float answersA[] = {1,2,3,4,5,6,7,8,9,10,12,13,14};
    if (a.length() != 13)
        {
        std::cout << "length() incorrect after remove(single)" << std::endl;
        return false;
        }
    for (i = 0; i < 13; i++)
        if(answersA[i] != a[i])
            {
            std::cout << "data wrong after remove(single)" << std::endl;
            return false;
            }

    // insertBefore - single element test
    a.insertBefore(0, 100.0);
    a.insertBefore(13, 200.0);
    a.insertBefore(5, 300.0);
    static float answersB[] = {100,1,2,3,4,300,5,6,7,8,9,10,12,13,200,14};
    if (a.length() != 16)
        {
        std::cout << "length() incorrect after insertBefore(single)" << std::endl;
        return false;
        }
    for (i = 0; i < 16; i++)
        if(answersB[i] != a[i])
            {
            std::cout << "data wrong after insertBefore(single)" << std::endl;
            return false;
            }

    // insertAfter - single element test
    a.insertAfter(0, 20.0);
    a.insertAfter(16, 30.0);
    a.insertAfter(5, 40.0);
    static float answersC[] = 
      {100,20,1,2,3,4,40,300,5,6,7,8,9,10,12,13,200,14,30};
    if (a.length() != 19)
        {
        std::cout << "length() incorrect after insertAfter(single)" << std::endl;
        return false;
        }
    for (i = 0; i < 19; i++)
        if(answersC[i] != a[i])
            {
            std::cout << "data wrong after insertAfter(single)" << std::endl;
            return false;
            }

    // append - multiple elements
    a.append(SeqOf<float>(fp, 3));
    static float answersD[] = 
      {100,20,1,2,3,4,40,300,5,6,7,8,9,10,12,13,200,14,30,0,1,2};
    if (a.length() != 22)
        {
        std::cout << "length() wrong after append(multiple)" << std::endl;
        return false;
        }
    for (i = 0; i < 22; i++)
        if(answersD[i] != a[i])
            {
            std::cout << "data wrong after append(multiple)" << std::endl;
            return false;
            }

    // insertBefore - multiple elements
    a.insertBefore(2, SeqOf<float>(fp, 3));
    static float answersE[] = 
      {100,20,0,1,2,1,2,3,4,40,300,5,6,7,8,9,10,12,13,200,14,30,0,1,2};
    if (a.length() != 25)
        {
        std::cout << "length() wrong after insertBefore(multiple)" << std::endl;
        return false;
        }
    for (i = 0; i < 25; i++)
        if(answersE[i] != a[i])
            {
            std::cout << "data wrong after insertBefore(multiple)" << std::endl;
            return false;
            }

    // insertAfter - multiple elements
    a.insertAfter(2, SeqOf<float>(fp, 3));
    static float answersF[] = 
      {100,20,0,0,1,2,1,2,1,2,3,4,40,300,5,6,7,8,9,10,12,13,200,14,30,0,1,2};
    if (a.length() != 28)
        {
        std::cout << "length() wrong after insertAfter(multiple)" << std::endl;
        return false;
        }
    for (i = 0; i < 28; i++)
        if(answersF[i] != a[i])
            {
            std::cout << "data wrong after insertAfter(multiple)" << std::endl;
            return false;
            }

    // remove - multiple elements
    a.remove(4,15);
    static float answersG[] = 
      {100,20,0,0,7,8,9,10,12,13,200,14,30,0,1,2};
    if (a.length() != 16)
        {
        std::cout << "length() wrong after remove(multiple)" << std::endl;
        return false;
        }
    for (i = 0; i < 16; i++)
        if(answersG[i] != a[i])
            {
            std::cout << "data wrong after remove(multiple)" << std::endl;
            return false;
            }

    // array test for const arrays with dataPtr()
    SeqOf<float> faconst;
    for (i = 0; i < 10; i++)
        faconst.append((float)i);
    const SeqOf<float> faConst = faconst;
    if (faConst.length() != 10)
        {
        std::cout << "length wrong for const array after assignment" << std::endl;
        return false;
        }
    const float *fconstPtr = faConst.dataPtr();
    for (i = 0; i < 10; i++)
        if (fconstPtr[i] != (float)i)
            {
            std::cout << "bad data from dataPtr() for const array" << std::endl;
            return false;
            }

    // try the find command
    SeqOf<TextString> findTest;
    findTest.append("Alpha");
    findTest.append("Beta");
    findTest.append("AC");
    if (findTest.find("Beta") != 1 || findTest.find("XX") != -1)
        {
        std::cout << "find() error" << std::endl;
        return false;
        }

    // try the sort command
    findTest.sort();
    if (findTest[0] != "AC" || findTest[1] != "Alpha" 
      || findTest[2] != "Beta")
        {
        std::cout << "sort(ascending) error" << std::endl;
        return false;
        }
    findTest.sort(false);
    if (findTest[0] != "Beta" || findTest[1] != "Alpha" 
      || findTest[2] != "AC")
        {
        std::cout << "sort(decending) error" << std::endl;
        return false;
        }

    // try the not in list command
    SeqOf<int> flistOne, flistTwo;
    flistOne.append(1);
    flistOne.append(2);
    flistOne.append(3);
    flistTwo.append(4);
    flistTwo.append(2);
    SeqOf<int> flistThree = flistOne.notInList(flistTwo);
    if (flistThree[0] != 1 || flistThree[1] != 3 || flistThree.length() != 2)
        {
        std::cout << "notInList error" << std::endl;
        return false;
        }

    // try the in list command
    SeqOf<int> common = flistOne.in(flistTwo);
    if (common.length() != 1 || common[0] != 2)
        {
        std::cout << "in error" << std::endl;
        return false;
        }
        
    return true;
    }



// -- global ------------------------------------------------------------------
// nowArrayTest()
// Tests arrays using NOW memory allocation.  Returns true if all tests
// were successful.
//-----------------------------------------------------------------------------
bool nowArrayTest()
    {
    SeqOf<float> array;
    array.setLength(10);

    int i;
    if (array.length() != 10)
        {
    std::cout << "length() incorrect on empty array" << std::endl;
    return false;
    }
    for (i = 0; i < 10; i++)    //define the first 10 elements
       array[i] = ((float)i*2);

    // append test - single elements
    for (i = 10; i < 100; i++)
        array.append((float)i*2);
    if (array.length() != 100)
        {
        std::cout << "length() incorrect on array" << std::endl;
        return false;
        }

    // [] operator test
    for (i = 0; i < 100; i++)
        if(array[i] != 2.0 * i)
            {
            std::cout << "operator[] retrieval data incorrect" << std::endl;
            return false;
            }

    // [] operator test
    for (i = 0; i < 99; i++)
        {
        array[i] = array[i+1];
        if (array[i] != array[i+1] || array[i] != 2.0 * (i+1))
            {
            std::cout << "operator[] storage data incorrect" << std::endl;
            return false;
            }
        }
    
    // array manipulations setup
    SeqOf<float> a;  // force many copies
    static float fp[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
    for (i = 0; i < 16; i++)
        a.append(fp[i]);  // fill up test array

    // remove - single element test 
    a.remove(15, 15);
    a.remove(0,0);
    a.remove(10,10);
    static float answersA[] = {1,2,3,4,5,6,7,8,9,10,12,13,14};
    if (a.length() != 13)
        {
        std::cout << "length() incorrect after remove(single)" << std::endl;
        return false;
        }
    for (i = 0; i < 13; i++)
        if(answersA[i] != a[i])
            {
            std::cout << "data wrong after remove(single)" << std::endl;
            return false;
            }
    //replace test

    SeqOf<float> r;
    static float rfp[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
    for (i = 0; i < 16; i++)
        r.append(rfp[i]);  // fill up test array
    r.remove(11,15);
    static float replaceArray[2] = {115,116};
    static float answersR[16] = {0,1,2,3,4,115,116,7,8,9,10};

    r.replace(5, 6, SeqOf<float>(replaceArray, 2));
    for (i = 0; i < 11; i++)
        if(answersR[i] != r[i])
        {
        std::cout << "data wrong after replace" << std::endl;
            for (i = 0; i < 11; i++)
                std::cout << r[i] << "  "  << std::endl;
             std::cout << "\n" << std::endl;
        return false;
        }

    //replace test with array syntax

    SeqOf<float> ar;
    static float arfp[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
    for (i = 0; i < 16; i++)
        ar.append(arfp[i]);  // fill up test array
    ar.remove(11,15);
    SeqOf<float> areplaceArray;
    areplaceArray.setLength(2);
    areplaceArray[0] = 115;
    areplaceArray[1] = 116;
    static float answersaR[16] = {0,1,2,3,4,115,116,7,8,9,10};

    ar.replace(5, 6, areplaceArray);
    for (i = 0; i < 11; i++)
        if(answersaR[i] != r[i])
        {
        std::cout << "data wrong after replace" << std::endl;
            for (i = 0; i < 11; i++)
                std::cout << ar[i] << "  "  << std::endl;
             std::cout << "\n" << std::endl;
        return false;
        }


    return true;
    }
