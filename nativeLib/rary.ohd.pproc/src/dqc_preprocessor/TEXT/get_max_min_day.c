#include "dqc_preproc.h"
/*******************************************************************************
* FILENAME:           get_max_min_day.c
* DESCRIPTION:        Contains the getDay_forHoulyT function.  This
*                     function examines the hour of the temperature
*                     report and returns the hydrologic day for which
*                     it should be used to compute max/min temperature
*                     values.
*
*                     The hydrologic day is determined as follows:
*                     0 < obstime <= 12   :  Applies to current DQC day max/min.
*                    12 < obstime <= 24  :   Applies to next DQC day max/min.
*
*                    The DQC date is advanced acccording to the following rule:
*                    increment the DQC date for any obstime greater than 12z.
*
*                    For example, a DQC hydrologic day is 12z-12z.  An observation
*                    with an obstime 2007-12-03 06:00:00 will be in the Dec 3rd DQC
*                    day.  An obstime of 2007-12-03 13:00:00 would be in the Dec 4th
*                    DQC day.
*
*
*                    contains the getDay_forMaxMinT function.
*                    The hydrologic day is determined as following:
*                     0 < obstime <= 12+mpe_maxminT_hour_window   :  Applies to current DQC day.
*                    12+mpe_maxminT_hour_window < obstime <= 24  :   Applies to next DQC day.
*
* ORIGINAL AUTHOR:   Bryon Lawrence
* CREATION DATE:     June 11, 2008
* ORGANIZATION:      OHD/HSEB
* MACHINE:           IBM Linux
* MODIFICATION HISTORY:
* DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
/*******************************************************************************

getDay_forMaxMinT(): Determine the date for max/min temperature record (extremum code as X/N).

The hydrologic day is determined as follows:
                    0 < obstime <= 12+mpe_maxminT_hour_window   :  Applies to current DQC day.
                    12+mpe_maxminT_hour_window < obstime <= 24  :   Applies to next DQC day.
*********************************************************************************/
int getDay_forMaxMinT ( const char * strCurrDate,
                              const dqcInitStruct * pInitStruct,
     	                      int numDays )
{
   char strTempDate[11] = { '\0' };
   int compareResult;
   int dateIndex;
   int hours;
   int i;
   int minutes;
   int obsTime;
   int seconds;

   /**
    * determine the candidate date index value
    */
   dateIndex = -1 ;

   for ( i = 0; i < numDays; i++ )
   {
      /* Compare the YYYY-MM-DD portion of the dates. */
      compareResult = strncmp(strCurrDate, pInitStruct->strDate[i], 10);

      if ( compareResult == 0 )
      {
         dateIndex = i;
         break;
      }
   }

   if ( dateIndex != -1 )
   {
      /* Compute the observation time of the current report being examined. */
      sscanf(strCurrDate, "%s %d:%d:%d", strTempDate, &hours, &minutes, &seconds);
      obsTime = hours * SECONDS_PER_HOUR + minutes * 60 + seconds;

      if ( obsTime > (SECONDS_PER_12_HOURS + (pInitStruct->temperature_hour_window)*SECONDS_PER_HOUR ))
      {
         dateIndex++;

	 if ( dateIndex == numDays )
	 {
	    dateIndex = -1;
	 }
      }
   }

   return dateIndex;
}


/*******************************************************************************

getDay_forHourlyT(): Determine the date for hourly temperature record (extremum code as Z).

The hydrologic day is determined as follows:
                    0 < obstime <= 12   :  Applies to current DQC day.
                    12 < obstime <= 24  :   Applies to next DQC day.
*********************************************************************************/

int getDay_forHourlyT ( const char * strCurrDate,
                              const dqcInitStruct * pInitStruct,
     	                      int numDays )
{
   char strTempDate[11] = { '\0' };
   int compareResult;
   int dateIndex;
   int hours;
   int i;
   int minutes;
   int obsTime;
   int seconds;

   /**
    * determine the candidate date index value
    */
   dateIndex = -1 ;

   for ( i = 0; i < numDays; i++ )
   {
      /* Compare the YYYY-MM-DD portion of the dates. */
      compareResult = strncmp(strCurrDate, pInitStruct->strDate[i], 10);

      if ( compareResult == 0 )
      {
         dateIndex = i;
         break;
      }
   }

   if ( dateIndex != -1 )
   {
      /* Compute the observation time of the current report being examined. */
      sscanf(strCurrDate, "%s %d:%d:%d", strTempDate, &hours, &minutes, &seconds);
      obsTime = hours * SECONDS_PER_HOUR + minutes * 60 + seconds;

      if ( obsTime > SECONDS_PER_12_HOURS )
      {
         dateIndex++;

	 if ( dateIndex == numDays )
	 {
	    dateIndex = -1;
	 }
      }
   }

   return dateIndex;
}



