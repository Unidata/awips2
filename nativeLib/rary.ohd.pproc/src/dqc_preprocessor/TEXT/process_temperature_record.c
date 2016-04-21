#include "dqc_preproc.h"

/*******************************************************************************
* FILENAME:    process_temperature_record.c
* DESCRIPTION: Processes a temperature record and store it in the correct DQC
*              day.  The following temperature values are processed and stored:
*              00z, 06z, 12z, 18z observed temperatures.
*              Max/Min temperature for the DailyQC period.
*
*              Max/min temperatures are determined by both looking at the 
*              highest and lowest observed temperatures and looking at
*              temperature reports with an extremum of 'X' (daily maximum)
*              or 'N' (daily minimum). Only 'X' or 'N' values whose duration
*              is completely contained within the DQC day are used.
*              
*
* ORIGINAL AUTHOR:  Bryon Lawrence
* CREATION DATE:    December 4, 2007
* ORGANIZATION:     OHD/HSEB
* MACHINE:          IBM/Linux
* MODIFICATION HISTORY:
*  DATE         PROGRAMMER        DESCRIPTION/REASON
*  12/4/2007    Lawrence          Written to modularize the original
*                                 dqc_preprocessor and fix a problem
*                                 estimating daily max and min
*                                 temperatures.
*  6/11/2008    Lawrence
********************************************************************************
*/
void processTemperatureRecord (char *strCurrDate,
                               const dqcInitStruct * pInitStruct,
			       const Temperature * pTemperatureNode,
			       int indexOfDQCDayToComputeMaxMinFor,
			       int indexOfDQCDayToComputeHourlyFor,
			       int indexOfDQCDayToStore12HrObs,
			       int closest6HourSynopticTime,
			       int diffTime,
			       struct temperature_info *pStationInfo)
{
   double maxValue; /* Contains the maximum value allowed by the DataLimits and
                       LocDataLimits table. */
   double minValue; /* Contains the minimum value allowed by the DataLimits and
                       LocDataLimits table. */
   int hourIndex;   /* The index of the element in the diffTime and value arrays
                       of the temperature_info structure representing the 
                       synoptic hour being processed. */
   int obsHour;     /* The hour of the observed data. */
   time_t timet;    /* The obstime of the temperature report in 
                       UNIX ticks. */

   /* Check to make sure the temperature value is within
      acceptable limits. */
   yearsec_ansi_to_timet (strCurrDate, &timet);
   obsHour = atoi ( &strCurrDate[11] );

   readTemperatureLimit (pTemperatureNode->lid, timet, &maxValue, &minValue);

   if ((IsNull (DOUBLE, (void *) &(pTemperatureNode->value)) == NOTNULL)
       && (pTemperatureNode->value != TEMPERATURE_MISSING)
       && (pTemperatureNode->value >= minValue)
       && (pTemperatureNode->value <= maxValue))
   {
      /* Process and store this temperature observation. */
      if ((pTemperatureNode->extremum[0] != 'X') &&
	  (pTemperatureNode->extremum[0] != 'N'))
      {
	 /* Process the hourly max/min values. Max/Min temperature values are always
	    computed for a hydrologic day which spans 12z-12z. */
	 if (pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][6] < pTemperatureNode->value)
	 {
	    pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][6] = pTemperatureNode->value;

            if ( pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][6] >
                 pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][4] )
            {
               pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][4] =
	                pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][6];
            } 
	 }

	 if (pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][7] > pTemperatureNode->value)
	 {
	    pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][7] = pTemperatureNode->value;

            if ( pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][7] <
                 pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][5] )
            {
               pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][5] = pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][7];
            } 

	 }

	 /*
	  * store the 00z, 06z, 12z, 18z temperature value
	  * Take into consideration that the 6 hour observed values
	  * for a 12z-12z hydrologic day may be 12z, 18z, 00z, 06z
	  * or it may be 18z, 00z, 06z, 12z.
	  */
	 if (diffTime <= pInitStruct->window_time_t)
	 {
	    hourIndex =
                  pInitStruct->hourSlotMap[pInitStruct->dqc_ending_6hour_obstime_flag][closest6HourSynopticTime];
	    pStationInfo->source = pTemperatureNode->ts[1];

            /* Check if the closest6HourSynopticTime is 12.
               If it is, then check which DQC day receives this 12hour observation.
               This depends on what the specified last 6 hour obstime is. */
            if ( closest6HourSynopticTime == 12 )
            {
               /* Use the temperature report which is closest to the
	          12Z synoptic hour. */
	       if (diffTime < pStationInfo->diffTime[indexOfDQCDayToStore12HrObs][hourIndex])
	       {
	          pStationInfo->value[indexOfDQCDayToStore12HrObs][hourIndex] =
		     pTemperatureNode->value;
	          pStationInfo->diffTime[indexOfDQCDayToStore12HrObs][hourIndex] = diffTime;
	       }

            }
            else
            {

               /* Use the temperature report which is closest to the
	          00Z, 06Z, or 18Z synoptic hour. */
	       if (diffTime < pStationInfo->diffTime[indexOfDQCDayToComputeHourlyFor][hourIndex])
	       {
	          pStationInfo->value[indexOfDQCDayToComputeHourlyFor][hourIndex] =
		     pTemperatureNode->value;
	          pStationInfo->diffTime[indexOfDQCDayToComputeHourlyFor][hourIndex] = diffTime;
	       }
            }
	 }
      }
      else
      {
	 /*
          * Process the max and min temperatures.
	  */

	  if (obsHour > pInitStruct->localMidnightInZ )
	  {

	      if (pTemperatureNode->extremum[0] == 'X')
	      {
		  if (pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][4] < pTemperatureNode->value)
		  {
	             pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][4] = pTemperatureNode->value;
		  }
              }
	      /*
	      * process the min temperature value
	      */
	      else
	      {
		  if (pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][5] > pTemperatureNode->value)
		  {
	             pStationInfo->value[indexOfDQCDayToComputeMaxMinFor][5] = pTemperatureNode->value;
		  }
              }
          }
      }
   }
}
