/*******************************************************************************
* FILENAME:            gage_pp_process_file.h
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*         DESCRIPTION: This file contains constant declarations and prototype 
*                      of the process_file routine
*                      defined within the gage_pp_process_file.c file.
*
* ORIGINAL AUTHOR:     Moria Shebsovich 
* CREATION DATE:       July 7, 2004 
* ORGANIZATION:        HSEB
* MACHINE:             HP-UX 9000 / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        7//2001      Moria Shebsovich  Original Coding
*********************************************************************************/

#ifndef GAGE_PP_PROCESS_FILE_H
#define GAGE_PP_PROCESS_FILE_H

#include "gage_pp_write_rec.h"
#include "time_defs.h"

#define NUM_MINUTES_PER_6HOUR_OFFSET 10
#define NUM_SECONDS_PER_6HOUR_OFFSET ( NUM_MINUTES_PER_6HOUR_OFFSET * \
                                       SECONDS_PER_MINUTE )

/* Type definitions. */
struct PrecipRecord
{
	char * pLid;
	char * pPe;
	int    shef_duration;
	char * pTs;
	char * pObstime;
	float value;
	char shef_qual_code;
	short revision;
	char * pPostingTime;
	time_t data_duration;
};

/* Function prototype. */
int gage_pp_process_file ( const char * filename , 
                           const GagePPoptions * pOptions ) ;
 
#endif /* #ifndef GAGE_PP_PROCESS_FILE_H */


