// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// Time Classes Test Program
//
// Author: Mark Mathewson
//-----------------------------------------------------------------------------

// -- module ------------------------------------------------------------------
// The timeTest program tests the AbsTime and TimeRange classes.
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const timeTest_C_Id =
  "$Id: .timeTest.C__temp10480,v 1.3 2004/02/04 23:14:33 fluke Exp $";
#endif

#include <iostream>
#include <stdlib.h>

#include "AbsTime.H"
#include "TimeRange.H"
#include "commonDefs.h"

bool timeTest();
bool year2000CompliantTest();
bool rangeTest();

// -- global ------------------------------------------------------------------
// main()
// Main driver program for testing the AbsTime and TimeRange classes.
//-----------------------------------------------------------------------------
int main()
    {
    bool fail = false;
    
    std::cout << "Time Classes Test Program " << std::endl;
    if(!timeTest())
        fail = true;
    if(!rangeTest())
        fail = true;
    if (!year2000CompliantTest())
        fail = true;
    if(fail)
        {
        std::cout << "Time Tests Failed" << std::endl;
        return 1;
        }
    else
        {
        std::cout << "Time Tests Passed Successfully" << std::endl;
        return 0;
        }
    }


// -- global ------------------------------------------------------------------
// timeTest()
// Tests the AbsTime class.  Returns true if all tests are successful.
//-----------------------------------------------------------------------------
bool timeTest()
    {
    bool fail = false;
    std::cout << "..Testing AbsTime class" << std::endl;

    // constructor tests
    std::cout << "....Constructor/component extractor test: " << std::endl;
    AbsTime t1(1999, 12, 20, 10, 11, 12);
    AbsTime t2(1999,  6, 20, 10, 11, 12);

    if(t1.year() != 1999)
        {
        std::cout << t1.year() << std::endl;
        std::cout << "....year() or AbsTime(y,m,d,h,m,s) failed" << std::endl;
        fail = true;
        }
    if(t1.month() != 12)
        {
        std::cout << "....month() or AbsTime(y,m,d,h,m,s) failed" << std::endl;
        fail = true;
        }
    if(t1.day() != 20)
        {
        std::cout << "....day() or AbsTime(y,m,d,h,m,s) failed" << std::endl;
        fail = true;
        }
    if(t1.hour() != 10)
        {
        std::cout << "....hour() or AbsTime(y,m,d,h,m,s) failed" << std::endl;
        fail = true;
        }
    if(t1.minute() != 11)
        {
        std::cout << "....minute() or AbsTime(y,m,d,h,m,s) failed" << std::endl;
        fail = true;
        }
    if(t1.second() != 12)
        {
        std::cout << "....second() or AbsTime(y,m,d,h,m,s) failed" << std::endl;
        fail = true;
        }
    if(t1.weekday() != 1)
        {
        std::cout << "....weekday() or AbsTime(y,m,d,h,m,s) failed" << std::endl;
        fail = true;
        }
    
    // trying unixTime constructor
    std::cout << "....unixTime constructor test: " << std::endl;
    AbsTime t3(45678990);
    if (t3.unixTime() != 45678990)
        {
        std::cout << "unixTime() or AbsTime(time_t) fail" << std::endl;
        fail = true;
        }

    // trying assignment and copy constructors
    std::cout << "....assignment and copy constructor test: " << std::endl;
    AbsTime t4(t3);
    if(t4.unixTime() != t3.unixTime())
        {
        std::cout << "copy constructor fail" << std::endl;
        fail = true;
        }

    AbsTime t5;
    t5 = t3;
    if(t5.unixTime() != t3.unixTime())
        {
        std::cout << "assignment operator fail" << std::endl;
        fail = true;
        }

    // trying relational operators
    std::cout << "....relational operator test: " << std::endl;
    AbsTime t6(1000);
    AbsTime t7(1000);
    AbsTime t8(1001);
    AbsTime t9(999);
    if(!(t6 == t7))
        {
        std::cout << "== operator fail" << std::endl;
        fail = true;
        }
    if(!(t6 != t9))
        {
        std::cout << "!= operator fail" << std::endl;
        fail = true;
        }
    if(!(t9 <= t6 && t9 <= t9))
        {
        std::cout << "<= operator fail" << std::endl;
        fail = true;
        }
    if(!(t6 < t8))
        {
        std::cout << "< operator fail" << std::endl;
        fail = true;
        }
    if(!(t6 >= t9 && t6 >= t7))
        {
        std::cout << ">= operator fail" << std::endl;
        fail = true;
        }
    if(!(t6 > t9))
        {
        std::cout << "> operator fail" << std::endl;
        fail = true;
        }

    // arithmetic operators
    AbsTime t11(1000000);
    AbsTime t12(1234567);
    int d = 5000;
    int e = 6000;
    std::cout << "....arithmetic operators: " << std::endl;
    t1 = t11 + d;
    t2 = e + t12;
    if(t1.unixTime() != 1000000+5000 || t2.unixTime() != 1234567+6000)
        {
        std::cout << "operator+() fail" << std::endl;
        fail = true;
        }
    t2 = t12 - e;
    if(t2.unixTime() != t12.unixTime()-6000)
        {
        std::cout << "operator-() for time and duration fail" << std::endl;
        fail = true;
        }
    d = t12 - t11;
    if(d != 1234567-1000000)
        {
        std::cout << "operator-() for two times fail" << std::endl;
        fail = true;
        }
    t11 += 3;
    if(t11.unixTime() != 1000000+3)
        {
        std::cout << "operator+=() fail" << std::endl;
        fail = true;
        }
    t11 -= 3;
    if(t11.unixTime() != 1000000)
        {
        std::cout << "operator-=() fail" << std::endl;
        fail = true;
        }


    // current time constructor format test
    std::cout << "....Current time and string format test" << std::endl;
    std::cout << "      Please manually verify proper format and time" << std::endl;
    AbsTime ctime = AbsTime::current();
    TextString asciiCTime = ctime.string("%D %T GMT");
    std::cout << "      time: " << asciiCTime << std::endl;

    // << operator test
    std::cout << "....Output operator test: " << ctime << std::endl;

    if (fail)
        std::cout << "..AbsTime Tests Failed" << std::endl;
    else
        std::cout << "..AbsTime Tests Successful" << std::endl;

    return !fail;
    }

// -- global ------------------------------------------------------------------
// rangeTest()
// Tests TimeRange class.  Returns true if all tests were successful.
//-----------------------------------------------------------------------------
bool rangeTest()
    {
    bool fail = false;
    int i,j,k;  // loop indexing
    std::cout << "..Testing TimeRange class" << std::endl;

    // invalid time range test
    std::cout << "..invalid time range test: " << std::endl;
    TimeRange n1,n2;
    TimeRange tr(100000, 200);  // good time range
    if(n1.isValid())
        {
        std::cout << "isValid() incorrect" << std::endl;
        fail = true;
        }
    if(!(n1 == n2) || n1 == tr || tr == n1)
        {
        std::cout << "== operator incorrect" << std::endl;
        fail = true;
        }
    if((n1 != n2) || !(n1 != tr) || !(tr != n1))
        {
        std::cout << "!= operator incorrect" << std::endl;
        fail = true;
        }
    TimeRange x = n1.join(n2);
    TimeRange y = n1.join(tr);
    TimeRange z = tr.join(n1);
    if(x.isValid() || y.isValid() || z.isValid())
        {
        std::cout << "join() incorrect" << std::endl;
        fail = true;
        }
    x = n1.intersection(n2);
    y = n1.intersection(tr);
    z = tr.intersection(n1);
    if(x.isValid() || y.isValid() || z.isValid())
        {
        std::cout << "intersection() incorrect" << std::endl;
        fail = true;
        }
    x = n1.gap(n2);
    y = n1.gap(tr);
    z = tr.gap(n1);
    if(x.isValid() || y.isValid() || z.isValid())
        {
        std::cout << "gap() incorrect" << std::endl;
        fail = true;
        }
    x = n1.span(n2);
    y = n1.span(tr);
    z = tr.span(n1);
    if(x.isValid() || y.isValid() || z.isValid())
        {
        std::cout << "span() incorrect" << std::endl;
        fail = true;
        }
    x = n1.combineWith(n2);
    y = n1.combineWith(tr);
    z = tr.combineWith(n1);
    if(x.isValid() || !y.isValid() || !z.isValid())
        {
        std::cout << "combineWith() incorrect" << std::endl;
        fail = true;
        }


    // Constructor test
    std::cout << "....Constructor tests:  "  << std::endl;
    AbsTime t1=AbsTime::current();
    AbsTime t2=t1 + 1000;
    std::cout << t1 << ' ' << t2 << std::endl;
    TimeRange tr10(t1, t2);
    TimeRange tr11(t2, t1);
    TimeRange tr12(t1, 1000);
    TimeRange tr13(t2, -1000);
    if (tr10.duration() != 1000 || tr11.duration() != 1000 
      || tr12.duration() != 1000 || tr13.duration() != 1000)
        {
        std::cout << "duration() or constructor fail" << std::endl;
        fail = true;
        }
    if (tr10.startTime() != t1 || tr11.startTime() != t1
      || tr12.startTime() != t1)
        {
        std::cout << "startTime() or constructor fail" << std::endl;
        fail = true;
        }
    if (tr10.endTime() != t2 || tr11.endTime() != t2
      || tr12.endTime() != t2)
        {
        std::cout << "endTime() or constructor fail" << std::endl;
        fail = true;
        }
    if (tr10 != tr11 || tr10 != tr12 || tr10 != tr13)
        {
        std::cout << "!= or constructor fail" << std::endl;
        fail = true;
        }
    if (!(tr10 == tr11))
        {
        std::cout << "== fail" << std::endl;
        fail = true;
        }

    // comparision routines
    // 0   1    2  3  4    5    6   7         time points
    //          XXXXXXXXXXX                   base time range
    // trtest[28] contains all the combinations of time ranges
    // initialize test data sets
    std::cout << "....Comparision routines: " << std::endl;
    AbsTime t[8];
    TimeRange trtest[29];  // combinations of time ranges (29th is a NULLtr)
    for (i = 0; i < 8; i++)
        {
        AbsTime temporary(i*100);
        t[i] = temporary;
        }
    for (i = 0, k = 0; i < 7; i++)
        for (j = i+1; j < 8; j++)
            {
            TimeRange temporary(t[i], t[j]);
            trtest[k++] = temporary;
            }
    TimeRange trbase(t[2], t[5]);
        {
        static bool answers[28] = 
          { 0,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0 };
        for(k = 0; k < 28; k++)
            if(trbase.overlaps(trtest[k]) ^ answers[k])
                {
                std::cout << "overlaps() fail";
                std::cout << "    base=" << (trbase.startTime()).unixTime();
                std::cout << "," << (trbase.endTime()).unixTime();
                std::cout << "  check=" << (trtest[k].startTime()).unixTime();
                std::cout << "," << (trtest[k].endTime()).unixTime() << std::endl;
                fail = true;
                }
        }
              
        {
        static bool answers[28] = 
          { 0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0 };
        for(k = 0; k < 28; k++)
            if(trbase.isAdjacentTo(trtest[k]) ^ answers[k])
                {
                std::cout << "isAdjacentTo() fail";
                std::cout << "    base=" << (trbase.startTime()).unixTime();
                std::cout << "," << (trbase.endTime()).unixTime();
                std::cout << "  check=" << (trtest[k].startTime()).unixTime();
                std::cout << "," << (trtest[k].endTime()).unixTime() << std::endl;
                fail = true;
                }
        }
              
        {
        static bool answers[8] = 
          { 0,0,1,1,1,0,0,0 };
        for(k = 0; k < 8; k++)
            if(trbase.contains(t[k]) ^ answers[k])
                {
                std::cout << "contains() for time fail";
                std::cout << "    base=" << (trbase.startTime()).unixTime();
                std::cout << "," << (trbase.endTime()).unixTime();
                std::cout << "  check=" << t[k].unixTime() << std::endl;
                fail = true;
                }
        }          
        {
        TimeRange trjoin;
        static int answers[28] =    
          { 28,4,4,4,4,5,6,10,10,10,10,11,12,15,15,15,16,17,15,15,16,17,
             15,16,17,16,17,28 };
        for(k = 0; k < 28; k++)
           {
           trjoin = trbase.join(trtest[k]);
           if(answers[k]==28 && (trjoin.isValid()))  //not Valid but was Valid
               {
               std::cout << "join() fail - should be NULL ";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime();
               std::cout << "    join=" << (trjoin.startTime()).unixTime();
               std::cout << "," << (trjoin.endTime()).unixTime() << std::endl;
                fail = true;
               }
           else if(answers[k]!=28 && !trjoin.isValid()) // not valid but should
               {
               std::cout << "join() fail - was not valid, should be valid";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime() << std::endl;
                fail = true;
               }
           else if(!trjoin.isValid())
               continue;
           else if(trjoin != trtest[answers[k]])  // nrm tr 
               {   
               std::cout << "join() fail ";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime();
               std::cout << "    join=" << (trjoin.startTime()).unixTime();
               std::cout << "," << (trjoin.endTime()).unixTime() << std::endl;
                fail = true;
               }
           }
        }

        {
        TimeRange trint;
        static int answers[28] =    
          { 28,28,13,14,15,15,15,28,13,14,15,15,15,13,14,15,15,15,
            18,19,19,19,22,22,22,28,28,28 };
        for(k = 0; k < 28; k++)
           {
           trint = trbase.intersection(trtest[k]);
           if(answers[k]==28 && (trint.isValid()))  
               {
               std::cout << "intersection() fail - should be not valid ";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime();
               std::cout << "    inters=" << (trint.startTime()).unixTime();
               std::cout << "," << (trint.endTime()).unixTime() << std::endl;
                fail = true;
               }
           else if(answers[k]!=28 && !trint.isValid()) // invalid but shouldn't
               {
               std::cout << "intersection() fail - was not valid, should be";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime() << std::endl;
                fail = true;
               }
           else if(!trint.isValid())
               continue;
           else if(trint != trtest[answers[k]])  // nrm tr 
               {   
               std::cout << "intersection() fail ";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime();
               std::cout << "    inters=" << (trint.startTime()).unixTime();
               std::cout << "," << (trint.endTime()).unixTime() << std::endl;
                fail = true;
               }
           }
        }

        {
        TimeRange trgap;
        static int answers[28] =    
          { 7,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,
             28,28,28,28,28,28,28,28,28,25 };
        for(k = 0; k < 28; k++)
           {
           trgap = trbase.gap(trtest[k]);
           if(answers[k]==28 && (trgap.isValid())) 
               {
               std::cout << "gap() fail - should not be valid ";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime();
               std::cout << "    gap=" << (trgap.startTime()).unixTime();
               std::cout << "," << (trgap.endTime()).unixTime() << std::endl;
                fail = true;
               }
           else if(answers[k]!=28 && !trgap.isValid()) 
               {
               std::cout << "gap() fail - was not valid, should be";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime() << std::endl;
                fail = true;
               }
           else if(!trgap.isValid())
               continue;
           else if(trgap != trtest[answers[k]])  // nrm tr 
               {   
               std::cout << "gap() fail ";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime();
               std::cout << "    gap=" << (trgap.startTime()).unixTime();
               std::cout << "," << (trgap.endTime()).unixTime() << std::endl;
                fail = true;
               }
           }
        }
        {
        TimeRange trcombine;
        static int answers[28] =    
          { 4,4,4,4,4,5,6,10,10,10,10,11,12,15,15,15,16,17,15,15,16,
            17,15,16,17,16,17,17 };
        for(k = 0; k < 28; k++)
           {
           trcombine = trbase.combineWith(trtest[k]);
           if(answers[k]==28 && (trcombine.isValid()))  
               {
               std::cout << "combineWith() fail - should not be valid ";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime();
               std::cout << "    combine=" << (trcombine.startTime()).unixTime();
               std::cout << "," << (trcombine.endTime()).unixTime() << std::endl;
                fail = true;
               }
           else if(answers[k]!=28 && !trcombine.isValid()) 
               {
               std::cout << "combineWith() fail - was not valid, should be";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime() << std::endl;
                fail = true;
               }
           else if(!trcombine.isValid())
               continue;
           else if(trcombine != trtest[answers[k]])  // nrm tr 
               {   
               std::cout << "combineWith() fail ";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime();
               std::cout << "    combine=" << (trcombine.startTime()).unixTime();
               std::cout << "," << (trcombine.endTime()).unixTime() << std::endl;
                fail = true;
               }
           }
        }

        {
        TimeRange trspan;
        static int answers[28] =    
          { 4,4,4,4,4,5,6,10,10,10,10,11,12,15,15,15,16,17,15,15,16,
            17,15,16,17,16,17,17 };
        for(k = 0; k < 28; k++)
           {
           trspan = trbase.span(trtest[k]);
           if(answers[k]==28 && (trspan.isValid()))  
               {
               std::cout << "span() fail - should not be valid ";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime();
               std::cout << "    span=" << (trspan.startTime()).unixTime();
               std::cout << "," << (trspan.endTime()).unixTime() << std::endl;
                fail = true;
               }
           else if(answers[k]!=28 && !trspan.isValid()) 
               {
               std::cout << "span() fail - was not valid, should be";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime() << std::endl;
                fail = true;
               }
           else if(!trspan.isValid())
               continue;
           else if(trspan != trtest[answers[k]])  // nrm tr 
               {   
               std::cout << "span() fail ";
               std::cout << "    base=" << (trbase.startTime()).unixTime();
               std::cout << "," << (trbase.endTime()).unixTime();
               std::cout << "    other=" << (trtest[k].startTime()).unixTime();
               std::cout << "," << (trtest[k].endTime()).unixTime();
               std::cout << "    span=" << (trspan.startTime()).unixTime();
               std::cout << "," << (trspan.endTime()).unixTime() << std::endl;
                fail = true;
               }
           }
        }

    // simple contains test with TimeRanges
    TimeRange con1(AbsTime(1000), AbsTime(2000));
    TimeRange con2(AbsTime(50), AbsTime(2500));
    TimeRange con3(AbsTime(1000), AbsTime(2500));
    TimeRange con4(AbsTime(20), AbsTime(25));
    TimeRange con5(AbsTime(3000), AbsTime(3500));
    TimeRange con6(AbsTime(200), AbsTime(1500));
    TimeRange con7(AbsTime(1900), AbsTime(2100));
    TimeRange con8(AbsTime(1100), AbsTime(1800));
    TimeRange con9(AbsTime(1000), AbsTime(1400));
    TimeRange con10(AbsTime(1100), AbsTime(2000));
    if (con1.contains(con2))
        {
        std::cout << "tr.contains(tr) fail  " << con1 << ' ' << con2 << std::endl;
        fail = true;
        }
    if (con1.contains(con3))
        {
        std::cout << "tr.contains(tr) fail  " << con1 << ' ' << con3 << std::endl;
        fail = true;
        }
    if (con1.contains(con4))
        {
        std::cout << "tr.contains(tr) fail  " << con1 << ' ' << con4 << std::endl;
        fail = true;
        }
    if (con1.contains(con5))
        {
        std::cout << "tr.contains(tr) fail  " << con1 << ' ' << con5 << std::endl;
        fail = true;
        }
    if (con1.contains(con6))
        {
        std::cout << "tr.contains(tr) fail  " << con1 << ' ' << con6 << std::endl;
        fail = true;
        }
    if (con1.contains(con7))
        {
        std::cout << "tr.contains(tr) fail  " << con1 << ' ' << con7 << std::endl;
        fail = true;
        }
    if (!con1.contains(con8))
        {
        std::cout << "tr.contains(tr) fail  " << con1 << ' ' << con8 << std::endl;
        fail = true;
        }
    if (!con1.contains(con9))
        {
        std::cout << "tr.contains(tr) fail  " << con1 << ' ' << con9 << std::endl;
        fail = true;
        }
    if (!con1.contains(con10))
        {
        std::cout << "tr.contains(tr) fail  " << con1 << ' ' << con10 << std::endl;
        fail = true;
        }
    if (!con1.contains(con1))
        {
        std::cout << "tr.contains(tr) fail  " << con1 << ' ' << con1 << std::endl;
        fail = true;
        }

    // special tests with some time ranges of zero duration
    TimeRange zero(AbsTime(1000), AbsTime(1000));
    TimeRange nonZeroBefore(AbsTime(900), AbsTime(1000));
    TimeRange nonZeroAfter(AbsTime(1001), AbsTime(1100));
    TimeRange nonZeroIncAfter(AbsTime(1000), AbsTime(1100));
    TimeRange bigSpan(AbsTime(900), AbsTime(1100));
    AbsTime beforeBaseTime = AbsTime(999);
    AbsTime baseTime = AbsTime(1000);
    AbsTime afterBaseTime = AbsTime(1001);
    // test contains(abstime)
    if (!zero.contains(baseTime))
        {
        std::cout << "trZero.contains(abstime) fail " << zero << ' ' << baseTime
          << std::endl;
        fail = true;
        }
    if (zero.contains(beforeBaseTime))  
        {
        std::cout << "trZero.contains(abstime) fail " << zero << ' ' << baseTime
          << std::endl;
        fail = true;
        }
    if (zero.contains(afterBaseTime))   
        {
        std::cout << "trZero.contains(abstime) fail " << zero << ' ' << baseTime
          << std::endl;
        fail = true;
        }

    // test contains(tr)
    if (nonZeroBefore.contains(zero))
        {
        std::cout << "nonZero.contains(zerotr) fail " << zero << ' ' 
          << nonZeroBefore << std::endl;
        fail = true;
        }
    if (nonZeroAfter.contains(zero))
        {
        std::cout << "nonZero.contains(zerotr) fail " << zero << ' ' 
          << nonZeroBefore << std::endl;
        fail = true;
        }
    if (!nonZeroIncAfter.contains(zero))
        {
        std::cout << "nonZero.contains(zerotr) fail " << zero << ' ' 
          << nonZeroIncAfter << std::endl;
        fail = true;
        }
    if (!zero.contains(zero))
        {
        std::cout << "zero.contains(zerotr) fail " << zero << ' ' 
          << zero << std::endl;
        fail = true;
        }
    if (zero.contains(nonZeroBefore))
        {
        std::cout << "zero.contains(nonZero) fail " << zero << ' ' 
          << nonZeroBefore << std::endl;
        fail = true;
        }
    if (zero.contains(nonZeroAfter))
        {
        std::cout << "zero.contains(nonZero) fail " << zero << ' ' 
          << nonZeroAfter << std::endl;
        fail = true;
        }
    if (zero.contains(bigSpan))
        {
        std::cout << "zero.contains(nonZero) fail " << zero << ' ' 
          << bigSpan << std::endl;
        fail = true;
        }
    if (zero.contains(nonZeroIncAfter))
        {
        std::cout << "zero.contains(nonZero) fail " << zero << ' ' 
          << nonZeroIncAfter << std::endl;
        fail = true;
        }

    // overlaps(zeroTR) test
    if (nonZeroBefore.overlaps(zero))
        {
        std::cout << "nonzero.overlaps(zeroTR) fail " << nonZeroBefore << ' ' 
          << zero << std::endl;
        fail = true;
        }
    if (nonZeroAfter.overlaps(zero))
        {
        std::cout << "nonzero.overlaps(zeroTR) fail " << nonZeroAfter << ' ' 
          << zero << std::endl;
        fail = true;
        }
    if (!nonZeroIncAfter.overlaps(zero))
        {
        std::cout << "nonzero.overlaps(zeroTR) fail " << nonZeroIncAfter << ' ' 
          << zero << std::endl;
        fail = true;
        }
    if (!bigSpan.overlaps(zero))
        {
        std::cout << "nonzero.overlaps(zeroTR) fail " << bigSpan << ' ' 
          << zero << std::endl;
        fail = true;
        }
    if (!zero.overlaps(zero))
        {
        std::cout << "nonzero.overlaps(zeroTR) fail " << zero << ' ' 
          << zero << std::endl;
        fail = true;
        }
    if (zero.overlaps(nonZeroBefore))
        {
        std::cout << "nonzero.overlaps(zeroTR) fail " << zero << ' ' 
          << nonZeroBefore << std::endl;
        fail = true;
        }
    if (zero.overlaps(nonZeroAfter))
        {
        std::cout << "nonzero.overlaps(zeroTR) fail " << zero << ' ' 
          << nonZeroAfter << std::endl;
        fail = true;
        }
    if (!zero.overlaps(nonZeroIncAfter))
        {
        std::cout << "nonzero.overlaps(zeroTR) fail " << zero << ' ' 
          << nonZeroIncAfter << std::endl;
        fail = true;
        }

    // intersection with 0 duration test
    TimeRange interLess(AbsTime(900), AbsTime(1000));
    TimeRange interAfter(AbsTime(1002), AbsTime(2000));
    TimeRange interAfterInc(AbsTime(1000), AbsTime(2000));
    TimeRange interSpan(AbsTime(500), AbsTime(2000));

    TimeRange answer;
    answer = interLess.intersection(zero);
    if (answer.isValid())
        {
        std::cout << "nonZeroTr.intersect(zeroTR) fail " << interLess << ' ' 
          << zero << ' ' << answer << std::endl;
        fail = true;
        }
    answer = interAfter.intersection(zero);
    if (answer.isValid())
        {
        std::cout << "nonZeroTr.intersect(zeroTR) fail " << interAfter << ' ' 
          << zero << ' ' << answer << std::endl;
        fail = true;
        }
    answer = interAfterInc.intersection(zero);
    if (answer.isValid())
        {
        std::cout << "nonZeroTr.intersect(zeroTR) fail " << interAfterInc << ' ' 
          << zero << ' ' << answer << std::endl;
        fail = true;
        }
    answer = interSpan.intersection(zero);
    if (answer.isValid())
        {
        std::cout << "nonZeroTr.intersect(zeroTR) fail " << interSpan << ' ' 
          << zero << ' ' << answer << std::endl;
        fail = true;
        }
    answer = zero.intersection(interLess);
    if (answer.isValid())
        {
        std::cout << "nonZeroTr.intersect(zeroTR) fail " << zero << ' ' 
          << interLess << ' ' << answer << std::endl;
        fail = true;
        }
    answer = zero.intersection(interAfter);
    if (answer.isValid())
        {
        std::cout << "nonZeroTr.intersect(zeroTR) fail " << zero << ' ' 
          << interAfter << ' ' << answer << std::endl;
        fail = true;
        }
    answer = zero.intersection(interAfterInc);
    if (answer.isValid())
        {
        std::cout << "nonZeroTr.intersect(zeroTR) fail " << zero << ' ' 
          << interAfterInc << ' ' << answer << std::endl;
        fail = true;
        }
    answer = zero.intersection(interSpan);
    if (answer.isValid())
        {
        std::cout << "nonZeroTr.intersect(zeroTR) fail " << zero << ' ' 
          << interSpan << ' ' << answer << std::endl;
        fail = true;
        }

    // operator << test
    TimeRange trnull;
    TimeRange trgood(AbsTime::current(), 3600);
    std::cout << "....operator<< test: " << trnull << std::endl;
    std::cout << "    operator<< test: " << trgood << std::endl;

    // centerTime() test
    std::cout << "....TimeRange::centerTime() test: " << std::endl;
    AbsTime halfMax(INT_MAX/2);
    TimeRange trvalid(halfMax, 3600);
    AbsTime trcenter = halfMax + 1800;
    if (trvalid.centerTime() != trcenter)
        {
        std::cout << "TimeRange::centerTime() failed, trvalid.centerTime() = "
                  << trvalid.centerTime() << ", trcenter = " << trcenter
                  << std::endl;
        fail = true;
        }

  
    if (fail)
        std::cout << "..TimeRange Tests Fail" << std::endl;   
    else
        std::cout << "..TimeRange Tests Successful = "  << std::endl;
    return !fail;
    }

// -- global ------------------------------------------------------------------
// year2000CompliantTest()
// Tests AbsTime for year 2000 compliant.  Returns true if successful.
//-----------------------------------------------------------------------------
bool year2000CompliantTest()
    {
    TextString format("%b %d %Y %H:%M:%S");
    std::cout << "..Testing Year 2000 Compliance" << std::endl;

    // Dec 31, 1999 at 2100z, add 4 hours
    AbsTime t1(1999, 12, 31, 21, 0, 0);
    AbsTime t2 = t1 + 4*3600;
    std::cout << t1.string(format) << " + 4 hours = " << t2.string(format) << std::endl;
    if (t2.year() != 2000 || t2.month() != 1 || t2.day() != 1
      || t2.hour() != 1 || t2.minute() != 0 || t2.second() != 0)
          {
          std::cout << "Failure with Dec31 1999 crossover to Jan1 2000" << std::endl;
          return false;
          }

    // January 1, 2000 at 0000z, subtract 1 minute
    t1 = AbsTime(2000, 1, 1, 0, 0, 0);
    t2 = t1 - 60;
    std::cout << t1.string(format) << " - 1 minute  = " << t2.string(format) << std::endl;
    if (t2.year() != 1999 || t2.month() != 12 || t2.day() != 31
      || t2.hour() != 23 || t2.minute() != 59 || t2.second() != 0)
          {
          std::cout << "Failure with Jan 1 2000 crossover to Dec31 1999" << std::endl;
          return false;
          }


    // February 28, 2000 at 2300z, add 1 hour
    t1 = AbsTime(2000, 2, 28, 23, 0, 0);
    t2 = t1 + 3600;
    std::cout << t1.string(format) << " + 1 hour  = " << t2.string(format) << std::endl;
    if (t2.year() != 2000 || t2.month() != 2 || t2.day() != 29
      || t2.hour() != 0 || t2.minute() != 0 || t2.second() != 0)
          {
          std::cout << "Failure with Feb28 2000 crossover to Feb29" << std::endl;
          return false;
          }
    if (t2.weekday() != 2)
        {
        std::cout << "Failure with Feb29 2000 not being Tuesday" << std::endl;
        return false;
        }

    // March 1, 2000 at 0000z, subtract 1 second
    t1 = AbsTime(2000, 3, 1, 0, 0, 0);
    t2 = t1 - 1;
    std::cout << t1.string(format) << " - 1 second  = " << t2.string(format) << std::endl;
    if (t2.year() != 2000 || t2.month() != 2 || t2.day() != 29
      || t2.hour() != 23 || t2.minute() != 59 || t2.second() != 59)
          {
          std::cout << "Failure with March1 2000 crossover to Feb29" << std::endl;
          return false;
          }

    return true;
    }
