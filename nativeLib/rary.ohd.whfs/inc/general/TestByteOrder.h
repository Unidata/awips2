/*******************************************************************************
* FILENAME:            TestByteOrder.h
* GENERAL INFORMATION:
* DESCRIPTION:         Contains prototype and enumeration information for
*                      the "TestByteOrder" routine.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       May 24, 2002
* ORGANIZATION:        HSEB / OHD
* MACHINE:             HP-UX / Dell Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   5/24/2002    Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef TESTBYTEORDER_H
#define TESTBYTEORDER_H

#ifdef __cplusplus
extern "C" {
#endif

enum TestByteResult { DontFlipBytes , FlipBytes , FlipTestFailed } ;

void TestByteOrder_ ( const char * filename ,
                      const void * value ,
                      const int * word_position ,
                      enum TestByteResult * result ) ;
#ifdef __cplusplus
}
#endif

#endif /* #ifndef TESTBYTEORDER_H */
