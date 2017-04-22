/*******************************************************************************
* FILENAME:            TestXmrg.h
* GENERAL INFORMATION:
* DESCRIPTION:         This file contains the prototype for the 
*                      TestXmrgOS_ routine. 
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       May 30, 2002
* ORGANIZATION:        OHD / HSEB
* MACHINE/OS:          HP-UX / Dell Redhat Linux
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   5/30/2002    Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef TESTXMRG_H
#define TESTXMRG_H

#include "GetOS.h" /* For the "OperSys" definition. */
#include "TestByteOrder.h"

void TestXmrgOS_ ( const char * filename , OperSys * oper ) ;
void TestXmrgByteOrder_ ( const char * filename , const int * XOR , 
                          enum TestByteResult * result ) ;

#endif /* #ifndef TESTXMRG_H */
