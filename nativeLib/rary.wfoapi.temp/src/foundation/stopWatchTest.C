//-------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// stopWatchTest.C
// Test program for StopWatch: a code timing class.
//
// Author:  Gerry Murray
//-------------------------------------------------------------------

#ifdef IDENT_C
static const char* const stopWatchTest_C_Id =
  "$Id: .stopWatchTest.C__temp27950,v 1.2 2003/05/06 23:12:00 fluke Exp $";
#endif


#include "commonDefs.h"
#include "StopWatch.H"
#include <iostream>
#include <math.h>

// -- global -----------------------------------------------------------------
// main()
// Main test program for the StopWatch class.
// ---------------------------------------------------------------------------
int main(void)
    {
    //declare the stop watch.
    static StopWatch sw;

    //test the error condition of stopping the watch before its started.
    sw.stop ();
    
    //time the code that performs a trig calculation a million times
    //and do five different timings so we can test the averages.
    for (int j = 0; j < 5; j++)
        {
        std::cout << "Trial " << j+1 << std::endl;
        sw.start ();
        for (int i = 0; i < 1000000; i++)
            {
            tan (34 * DEG_TO_RAD);
            if (!(i % 10000) && i)
                std::cout << "Split: " << sw.getSplit() << std::endl;
            }
        sw.stop ();

        //print the results of a timing.  
        std::cout << "Wall Clock Time: "
             << sw.wallClockTime ()
             << std::endl;
        std::cout << "User CPU Time: "
             << sw.userCPU_Time ()
             << std::endl;
        std::cout << "System CPU Time: "
             << sw.systemCPU_Time ()
             << std::endl;
        std::cout << "Average Wall Clock Time thus far: "
             << sw.avgWallClockTime ()
             << std::endl << std::endl;
        }
    return 0;
    }
