/*******************************************************************************
* FILENAME:              load_PCPP_data.h
* GENERAL INFORMATION: 
* DESCRIPTION:           Header file containing the prototypes of routines
*                        to load data from the HourlyPC, HourlyPP, RawPC,
*                        RawPPhourly, and RawPPhourly IHFS database tables.
*
* ORIGINAL AUTHOR:       Bryon Lawrence
* CREATION DATE:         July 27, 2004
* ORGANIZATION:          HSEB/OHD
* MACHINE:               i686, Redhat Linux version 2.4.9-31 enterprise
* MODIFICATION HISTORY:
* DATE         PROGRAMMER        DESCRIPTION/REASON
* 7/27/2004    Bryon Lawrence    First Coding
********************************************************************************
*/

#include <time.h>

#include "CurPC.h"
#include "CurPP.h"
#include "HourlyPC.h"
#include "HourlyPP.h"
#include "RawPC.h"
#include "RawPP.h"

#ifndef LOAD_PCPP_DATA_H
#define LOAD_PCPP_DATA_H

typedef enum RawPrecipTable { RawPrecip , CurRawPrecip } RawPrecipTable ;

const char * get_pcpp_query ( ) ;

HourlyPC * load_PC_hourly ( time_t query_being_time ,
                            time_t query_end_time ,
                            const char * lid ,
                            const char ** ts ,
                            int num_ts ,
                            int * pc_rec_cnt ) ;
HourlyPP * load_PP_hourly ( time_t query_begin_time ,
                            time_t query_end_time ,
                            const char * lid ,
                            const char ** ts ,
                            int num_ts ,
                            int * pp_rec_cnt ) ;
RawPC * load_PC_raw ( time_t query_begin_time ,
                      time_t query_end_time ,
                      const char * lid ,
                      const char ** ts ,
                      int num_ts ,
		      RawPrecipTable table ,
                      int * pc_rec_cnt ) ;
RawPP * load_PP_raw ( time_t query_begin_time ,
                      time_t query_end_time ,
                      const char * lid ,
                      const char ** ts ,
                      int num_ts ,
		      RawPrecipTable table ,
                      int * pp_rec_cnt ) ;

#endif /* #ifndef LOAD_PCPP_DATA_H */
