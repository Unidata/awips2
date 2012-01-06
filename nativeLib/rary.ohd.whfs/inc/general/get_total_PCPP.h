/*******************************************************************************
* FILENAME:            get_total_PCPP.h 
* GENERAL INFORMATION:
* DESCRIPTION:         Contains the prototypes for the APIs to compute
*                      precipitation totals from the HourlyPP, HourlyPC,
*                      and RawPPother tables in the IHFS database.
*
*                      These routines were designed to be used with
*                      data produced by the Gage Precipitation Processor.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       July 21, 2004
* ORGANIZATION:        HSEB/OHD/NWS/NOAA/Dept Commerce
* MACHINE:             i686, Redhat Linux Version 2.4.9 - 31 Enterprise
* MODIFICATION HISTORY:
*   DATE            PROGRAMMER        DESCRIPTION/REASON
*   July 21, 2004   Bryon Lawrence    Original Coding      
********************************************************************************
*/

#ifndef GET_TOTAL_PCPP_H
#define GET_TOTAL_PCPP_H

#include  <time.h>

#include "HourlyPC.h"
#include "HourlyPP.h"
#include "RawPC.h"
#include "RawPP.h"

enum PPdurationMatch { PPdurationMatchFound , PPdurationMatchNotFound } ;

float get_total_hourly_PC ( const HourlyPC * pHourlyPC ,
                            time_t ending_time ,
                            int num_hours ,
                            int num_pc_records ,
                            int * seconds_covered ,
			    char * pc_qc ) ;

float get_total_hourly_PP ( const HourlyPP * pHourlyPP ,
                            time_t ending_time ,
                            int num_hours ,
                            int num_pp_records ,
                            int * seconds_covered ,
                            char * pp_qc ,
                            int * reported_missing ) ;

float get_total_raw_PC ( const RawPC * pRawPC ,
                         time_t starting_time ,
                         time_t ending_time ,
			 int num_records ,
                         int sum_pc_reports ,
                         int * seconds_covered ) ;

float get_total_raw_PP ( const RawPP * pRawPP ,
                         time_t starting_time ,
                         time_t ending_time ,
			 int num_pp_records ,
                         short int no_accum_flag , 
                         short int ending_time_match ,
                         time_t * pMatchTime ,
                         int * seconds_covered ,
                         int * summed_flag ) ;

#endif /* #ifndef GET_TOTAL_PCPP_H */
