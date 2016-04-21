/*******************************************************************************
* FILENAME:            GetOS.h
* DESCRIPTION:         This file contains prototype and symbolic constant
*                      information for the the "GetOS" routine.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       October 24, 2001
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP / Linux Workstations
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        10/24/01      Bryon Lawrence    Original Coding
*
********************************************************************************
*/

#ifndef GETOS_H
#define GETOS_H

/* Enumeration Defintion(s). */
typedef enum opersys { OS_UNIX ,
                       OS_LINUX ,
                       OS_ERROR } OperSys ;
/* Prototype(s). */
OperSys GetOS ( ) ;
void get_oper_sys ( int * opersys ) ;

#endif /* #ifndef GETOS_H */
