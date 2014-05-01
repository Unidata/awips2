/*******************************************************************************
* FILENAME:              dqc_preproc.h
*
* DESCRIPTION:         This file contains parameters and
*                      user-defined types for the QPEMapper_preproc program.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         Feburary 15, 2006
* ORGANIZATION:          HSEB / OHD
* MACHINE:               Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef DQC_PREPROC_H
#define DQC_PREPROC_H
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <ctype.h>

#include "List.h"
#include "time_convert.h"		/* Time conversion utilities. */
#include "time_defs.h"			/* Time constant utilities. */
#include "BinarySearch.h"
#include "DbmsUtils.h"
#include "LocDataLimits.h"
#include "DataLimits.h"

#include "DbmsDefs.h"
#include "get_loc_latlon.h"
#include "get_total_precip.h"
#include "DailyPP.h"
#include "HourlyPC.h"
#include "HourlyPP.h"
#include "Temperature.h"
#include "load_PCPP_data.h"
#include "time_convert.h"
#include "time_defs.h"


/*--------------------------------*/
/*  definition of constants       */
/*--------------------------------*/
#define PATH_LEN	            256
/*#define FNAME_LEN	             24*/
#define GAGE_ID_LEN	              8
#define DB_LEN		             20
#define DATA_STRING_LEN	        256
#define MAX_TOKEN_NUMBER	     50
#define SITENAME_LEN	          8
#define DEFAULT_DAY	             10
#define MAXIMUM_DAY	            100
#define PRECIP_MISSING	      -99.0
#define TEMPERATURE_MISSING	-9999.0
#define TEMPERATURE_WINDOW_DEFAULT 60
#define MAX_DIFF_TIME	              24 * SECONDS_PER_HOUR
#define MISSING_MIN_TEMPERATURE	     999.0
#define MISSING_MAX_TEMPERATURE	     -999.0
#define MAXMIN_TEMPERATURE_HOUR_WINDOW   2

#define DEFAULT_ENDING_6HOUR_OBS_TIME 6 /* The default ending obs time for
                                           temperature and freezing level
                                           data. */
/* Don't change these numbers.  They are
   used to index an array. */
#define DQC_PREPROCESSOR_ENDING_OBSTIME_06Z 0
#define DQC_PREPROCESSOR_ENDING_OBSTIME_12Z 1

#define SECONDS_PER_3_HOURS 3*SECONDS_PER_HOUR
#define SECONDS_PER_6_HOURS 6*SECONDS_PER_HOUR
#define SECONDS_PER_9_HOURS 9*SECONDS_PER_HOUR
#define SECONDS_PER_12_HOURS 12*SECONDS_PER_HOUR
#define SECONDS_PER_15_HOURS 15*SECONDS_PER_HOUR
#define SECONDS_PER_18_HOURS 18*SECONDS_PER_HOUR
#define SECONDS_PER_21_HOURS 21*SECONDS_PER_HOUR
#define SECONDS_PER_24_HOURS 24*SECONDS_PER_HOUR

/*--------------------------------*/
/*  definition of variables       */
/*--------------------------------*/

struct precip_info
{
    char lid [ GAGE_ID_LEN + 1];
    char source ;
    char ** pPPQPE ;

    double lat;
    double lon;

    double * pPPD;
    double ** pPPQ;
};

struct temperature_info
{
    char lid [ GAGE_ID_LEN + 1];
    char source ;
    char extremum ;

    double lat;
    double lon;
    int ** diffTime;
    double ** value;
    double ** maxHourlyValue;
    double ** minHourlyValue;
};

typedef struct dqcInitStruct
{
   int window_time_t;
   int temperature_hour_window;
   char strDate[MAXIMUM_DAY][11];
   int dqc_ending_6hour_obstime_flag;
   short hourSlotMap[2][24];
   int localMidnightInZ;
   int ending6HourObsTime;
} dqcInitStruct;

extern char  mpe_station_list_dir[PATH_LEN] ;
extern char  mpe_point_precip_dir[PATH_LEN];
extern char  mpe_site_id[SITENAME_LEN] ;
extern char  mpe_area_names[DATA_STRING_LEN]  ;
extern char  db_name[DB_LEN] ;

extern char  mpe_point_temperature_dir[PATH_LEN];

extern struct precip_info * pPrecipInfo ;
extern struct temperature_info * pTempInfo;

extern int precip_count ;
extern int temperature_count ;

/*-----------------------------*/
/*  function prototypes        */
/*-----------------------------*/

void initPrecipArray ( const int stationSize, const int numDays );
void releasePrecipArray (const int stationSize, const int numDays );
void initTempArray ( const int stationSize, const int numDays );
void releaseTempArray (const int stationSize, const int numDays );

int dqc_preprocessor_getAppsDefaults(const char* strToken, char* strTokenValue);

void loadAppsDefaults() ;

char** stringTokenize(char* strValue, char delims[], int * listNum) ;

double dqc_preprocessor_readPrecipLimit( const char * gageID ,
                        const time_t datetime,
                        int runHours) ;
void readTemperatureLimit( const char * gageID ,
                           const time_t datetime,
                           double * maxValue,
                           double * minValue);

int read_station_file ( const int numDays, const char *areaName);

void processHourlyPPPC (const time_t start_time_t, const int numDays ) ;

void processDailyPP (const time_t start_time_t, const int numDays );

void processTemperature (time_t start_time_t, int numDays );

int compare_lid_source ( void * search_value,
                        void * array_value ) ;
int compare_lid_source_ext ( void * search_value,
					    void * array_value );

void write_precip_data (const time_t start_time_t,
					    const int numDays,
                        const char * areaName,
                        const bool blnSetZero );

void write_temperature_data (const time_t start_time_t,
                      const int numDays,
                      const char * areaName ) ;

void getClosestSynopticHour ( const char * strCurrDate,
                              const dqcInitStruct * initStruct,
                              int numDays,
                              int * pClosest6HourSynopticTime,
                              int * diffTime);

int getDayOf12HrObs ( const char * strCurrDate,
                      const dqcInitStruct * pInitStruct,
		      int numDays );

int getDay_forHourlyT ( const char * strCurrDate,
                        const dqcInitStruct * pInitStruct,
     	                int numDays );

int getDay_forMaxMinT ( const char * strCurrDate,
                        const dqcInitStruct * pInitStruct,
     	                int numDays );

void processTemperatureRecord (char *strCurrDate,
                               const dqcInitStruct * pInitStruct,
                               const Temperature * pTemperatureNode,
                               int indexOfDQCDayToComputeMaxMinFor,
			       int indexOfDQCDayToComputeHourlyFor,
                               int indexOfDQCDayToStore12HrObs,
                               int closest6HourSynopticTime,
                               int diffTime,
                               struct temperature_info *pStationInfo);

void initTokens ( time_t start_time_t, int numDays );

inline int getSearchWindow ( );

inline int getBasetimeFlag ( );

inline dqcInitStruct getInitStruct ( );

#endif /* #ifndef DQC_PREPROC_H */

