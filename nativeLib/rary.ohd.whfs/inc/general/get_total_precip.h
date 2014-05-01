
/*******************************************************************************
* FILENAME:            get_total_precip.h
* GENERAL INFORMATION:
* DESCRIPTION:         Contains the prototypes and user-defined types used
*                      the get_total_precip and get_total_raw_precip routines.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       July 9, 2004
* ORGANIZATION:        OHD/HSEB
* MACHINE:             i686, Redhat Linux version 2.4.9-31 enterprise 
* *
* MODIFICATION HISTORY:
*     DATE              PROGRAMMER        DESCRIPTION/REASON
*     July 9, 2004      Bryon Lawrence    Original Coding 
********************************************************************************
*/

#ifndef GET_TOTAL_PRECIP_H
#define GET_TOTAL_PRECIP_H

#include "DbmsDefs.h"   /* For SHEF_PE_LEN and SHEF_TS_LEN definitions. */
#include "HourlyPC.h"   /* DbGen header file. */
#include "HourlyPP.h"   /* DbGen header file. */
#include "precip_total.h" /* For data_err_struct definition. */
#include "RawPC.h" /* DbGen Header file. */
#include "RawPP.h" /* DbGen Header file. */

#define PRECIP_TS_SINGLE                 0
#define PRECIP_NO_ACCUM                  1
#define REPORT_MISSING_BELOW_MIN_PERCENT 4
#define PRECIP_PE_BEST                   8
#define PRECIP_PP                        16
#define PRECIP_PC                        32
#define PRECIP_TS_BEST                   64
#define PRECIP_TS_RANK                   128

#define EXACT_ENDINGTIME_MATCH   -1
#define CLOSEST_ENDINGTIME_MATCH -2
#define LATEST_ENDINGTIME_MATCH  -3

enum PrecipTSmode { PrecipTSbest , PrecipTSrank, PrecipTSsingle } ;
enum PrecipPEmode { PrecipPEbest , PrecipPEPP , PrecipPEPC } ;

struct total_precip {
   char lid [ LOC_ID_LEN + 1 ] ;
   char PE [ SHEF_PE_LEN + 1 ] ;
   char TS [ SHEF_TS_LEN + 1 ] ;
   float value ;
   short int summed_flag ;
   float hours_covered ;
   float percent_filled ;
   char value_indicator ;
   char qc ;
   data_err_struct err ; 
   short int reported_missing ;
   time_t match_time ;
} ;

struct total_precip get_total_hourly_precip ( HourlyPC ** pHourlyPC ,
                                              HourlyPP ** pHourlyPP ,
                                              time_t ending_time ,
					      short int num_hours ,
                                              float min_percent ,
                                              unsigned char settings ,
                                              short int advance ,
	                                      int * pc_records ,
	                                      int * pp_records ) ;

struct total_precip get_total_raw_precip ( RawPC ** pRawPC ,
		                           RawPP ** pRawPP ,
			                   time_t starting_time ,
			                   time_t ending_time ,
					   short int ending_time_match ,
			                   float min_percent ,
			                   unsigned char settings ,
	                                   short int advance ,
	                                   int * pc_records ,
	                                   int * pp_records ) ;

#endif /*#ifndef GET_TOTAL_PRECIP_H */
