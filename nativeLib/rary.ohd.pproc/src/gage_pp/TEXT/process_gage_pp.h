/*******************************************************************************
* FILENAME:            process_gage_pp.h
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*         DESCRIPTION: This file contains constant declarations and prototype 
*                      of the process_gage_pp routine
*                      defined within the process_gage_pp.c file.
*
* ORIGINAL AUTHOR:     Moria Shebsovich 
* CREATION DATE:       July 7, 2004 
* ORGANIZATION:        HSEB
* MACHINE:             HP-UX 9000 / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        7//2001      Moria Shebsovich  Original Coding
*********************************************************************************/

#ifndef PROCESS_GAGE_PP_H
#define PROCESS_GAGE_PP_H

#include "gage_pp_write_rec.h"

#define MAXFILES   100
#define ERRFILE_LEN 256

int process_gage_pp ( const char * datadir , const char * stopfilename ,
		      const GagePPoptions * pOptions ) ;
 
#endif /* #ifndef PROCESS_GAGE_PP_H */


