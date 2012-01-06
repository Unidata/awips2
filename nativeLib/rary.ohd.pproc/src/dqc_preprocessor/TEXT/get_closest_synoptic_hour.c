#include "dqc_preproc.h"
/*******************************************************************************
* FILENAME:           get_closest_synoptic_hour.c
* DESCRIPTION:        Contains the getClosestSynopticHour function.  This 
*                     function examines the hour of the temperature
*                     report and returns the synoptic hour (00,06,12,18) it 
*                     falls closest to.  It also returns the difference in
*                     seconds between the observed time and the synoptic
*                     hour and the dailyQC day whose 24 hour max/min temperature
*                     value this temperature observation will apply to.
*
*                     The synoptic hour is determined as follows:
*                     0 < obstime <= 3   : 00z  Applies to current DQC day max/min.
*                     3 < obstime <= 9   : 06z  ...
*                     9 < obstime <= 12  : 06z  ...
*                    12 < obstime <= 15  : 12z  Applies to next DQC day max/min.
*                    15 < obstime <= 21  : 18z  ...
*                    21 < obstime <= 24  : 00z  ...
*
*                    The DQC date is advanced acccording to the following rule:
*                    increment the DQC date for any obstime greater than 12z.
*
*                    For example, a DQC hydrologic day is 12z-12z.  An observation
*                    with an obstime 2007-12-03 06:00:00 will be in the Dec 3rd DQC
*                    day.  An obstime of 2007-12-03 13:00:00 would be in the Dec 4th
*                    DQC day.
*
* ORIGINAL AUTHOR:   Bryon Lawrence
* CREATION DATE:     December 4, 2007
* ORGANIZATION:      OHD/HSEB
* MACHINE:           IBM Linux
* MODIFICATION HISTORY:
* DATE         PROGRAMMER        DESCRIPTION/REASON
* 12/2007      Lawrence          Fixed and modularized dqc_preprocessor code
*                                to correctly process 6 hourly, max and min
*                                temperatures.     
* 6/6/2008     Lawrence          Revised so that a hydrologic day is always
*                                12Z-12Z.  Added code to support different
*                                obstimes for the 6 hour temperature and
*                                freezing level data.
********************************************************************************
*/

void getClosestSynopticHour ( const char * strCurrDate,
                              const dqcInitStruct * pInitStruct,
                              int numDays,
                              int * pClosest6HourSynopticTime,
                              int * diffTime)

{
   char strTempDate[11] = { '\0' };
   int compareResult;
   int hours;
   int i;
   int minutes;
   int obsTime;
   int seconds;
   int sixHourTimeDiff;
   int twelveHourTimeDiff;
   int eighteenHourTimeDiff;
   int twentyfourHourTimeDiff;

   /* Compute the observation time of the current report being examined. */
   sscanf(strCurrDate, "%s %d:%d:%d", strTempDate, &hours, &minutes, &seconds);
   obsTime = hours * SECONDS_PER_HOUR + minutes * 60 + seconds ;
   sixHourTimeDiff = abs ( SECONDS_PER_6_HOURS - obsTime );
   twelveHourTimeDiff = abs ( SECONDS_PER_12_HOURS - obsTime );
   eighteenHourTimeDiff = abs ( SECONDS_PER_18_HOURS - obsTime );
   twentyfourHourTimeDiff = abs ( SECONDS_PER_24_HOURS - obsTime );

   /**
    * determine the candidate hour index value
    * and the diff time,
    * and adjust the date index if necessary.
    **/
   *pClosest6HourSynopticTime = -1;
   *diffTime = MAX_DIFF_TIME ;

   /* Determine the synoptic hour the obstime is closest to. */

   if ( obsTime <= SECONDS_PER_3_HOURS )
   {
      *pClosest6HourSynopticTime = 0;
      *diffTime = obsTime;
   }
   else if ((obsTime > SECONDS_PER_3_HOURS) &&
            (obsTime <= SECONDS_PER_9_HOURS ))
   {
      *pClosest6HourSynopticTime = 6;
      *diffTime = sixHourTimeDiff;  
   }
   else if ((obsTime > SECONDS_PER_9_HOURS) &&
            (obsTime <= SECONDS_PER_15_HOURS ))
   {
      *pClosest6HourSynopticTime = 12;
      *diffTime = twelveHourTimeDiff;
   }
   else if ((obsTime > SECONDS_PER_15_HOURS) &&
            (obsTime <= SECONDS_PER_21_HOURS ))
   {
      *pClosest6HourSynopticTime = 18;
      *diffTime = eighteenHourTimeDiff;
   }
   else /* Time between 21z and 00z. */
   {
      *pClosest6HourSynopticTime = 0;
      *diffTime = twentyfourHourTimeDiff;
   }
}
