/*******************************************************************************
* FILENAME:            openlog.h
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*         DESCRIPTION: This file contains prototype of openlog routine
*                      defined within the openlog.c file.
*
* ORIGINAL AUTHOR:     Moria Shebsovich 
* CREATION DATE:       July 7, 2004 
* ORGANIZATION:        HSEB
* MACHINE:             HP-UX 9000 / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        7//2001      Moria Shebsovich  Original Coding
*********************************************************************************/

#ifndef OPENLOG_H
#define OPENLOG_H

#define MAX_LOG_LENGTH 512

/* Function prototype. */
int closelog ( ) ;
int openlog ( int * idat , const char * log_out , int * logday ) ;
int writelog ( const char * log_message ) ;
 
#endif /* #ifndef OPENLOG_H */


