
/**********************************************************************
 * getTemperatureRecords()
 * 
 * This function loads the temperature data
 * from Temperature table.
 *
 * Modified Dave Miller Nov 2009.  See reason in compare_lid_ts_ext.c
 *********************************************************************/

#include "dqc_preproc.h"

static Temperature *getTemperatureRecords (const time_t start_time_t,
   				           const time_t end_time_t)
{
   Temperature *pTemperatureHead = NULL;

   char query[200] = { '\0' };
   char start_time[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
   char end_time[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };

   timet_to_yearsec_ansi (start_time_t, start_time);
   timet_to_yearsec_ansi (end_time_t, end_time);

   sprintf (query, "WHERE obstime >= '%s'"
	    " AND obstime <= '%s'"
	    " AND pe = 'TA'"
	    " ORDER BY lid ASC, obstime ASC", start_time, end_time);

   pTemperatureHead = GetTemperature (query);

   return pTemperatureHead;
}

/*************************************************************************
 * processTemperature()
 * 
 * This function fills in the temperature array with the data loaded
 * from Temperature table. Including four time periods 00z, 06z, 12z, 18z,
 * and max & min values.
 ***************************************************************************/

void processTemperature (time_t start_time_t, int numDays)
{
   static int first = 0;

   static Temperature *pTemperatureHead = NULL;
   Temperature *pTemperatureNode = NULL;

   int i, dateIndex;
   char strCompare[12] = { '\0' };

   char strCurrDate[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
   dtime_t dt;
   double maxValue, minValue;
   int diffTime = MAX_DIFF_TIME;
   int closest6HourSynopticTime = -1;
   int indexOfDQCDayToComputeMaxMinFor;
   int indexOfDQCDayToComputeHourlyFor;
   int indexOfDQCDayToStore12HrObs;
   int status;
   struct dqcInitStruct initStruct;
   struct temperature_info *pStationInfo = NULL;
   time_t timet, end_time_t;

    /*
     * Load the mpe_temperature_window token value.
     */
   if (first == 0)
   {
      initTokens (start_time_t, numDays);
      first = 1;
   }

   initStruct = getInitStruct ( );
   
   /*
    * Load the temperature data from the IHFS Temperature table. 
    */

   /* Compute the ending time of the interval to retrieve
      temperature data for.  Add extra look ahead time. */
   end_time_t =
      start_time_t + numDays * 24 * SECONDS_PER_HOUR + initStruct.window_time_t;

   /* Adjust the start time to include the extra look back time
      for temperature reports. */
   timet = start_time_t - initStruct.window_time_t;

   if (pTemperatureHead == NULL)
   {
      pTemperatureHead = getTemperatureRecords (timet, end_time_t);
   }

   if (pTemperatureHead != NULL)
   {
      pTemperatureNode = (Temperature *) ListFirst (&pTemperatureHead->list);
   }

   while (pTemperatureNode != NULL)
   {
      /*
       * get the info for the matching identifier,
       * if there is a match.
       * otherwise, skip this record.
       */

      strcpy(strCompare,pTemperatureNode->lid);


      /* Must normalize lengths of strings in the linked list before comparing.
	 This is also done in the station lid and source search value within
	 compare_lid_source.
      */


      while (strlen(strCompare)<8)
	   strcat(strCompare,"0");

      sprintf(strCompare,"%s%c",strCompare,pTemperatureNode->ts[1]);

      pStationInfo =
	 (struct temperature_info *) binary_search (pTempInfo, strCompare,
						    temperature_count,
						    sizeof (struct
						    temperature_info),
						    compare_lid_source);
      if (pStationInfo != NULL)
      {

	 dt = pTemperatureNode->obstime;
	 yearsec_dt_to_ansi (dt, strCurrDate);

	 /* Determine the synoptic 6-hour the obstime is closest to. */
	 getClosestSynopticHour (strCurrDate,
                                 &initStruct,
		                 numDays,
			         &closest6HourSynopticTime,
			         &diffTime );

         /* Retrieve the DQC hydrologic day to compute max/min
	    temperatures for. */
	 indexOfDQCDayToComputeMaxMinFor = getDay_forMaxMinT( strCurrDate,
	                                                      &initStruct,
							      numDays );

	 /* Retrieve the DQC hydrologic day to compute hourly
	    temperatures for. */
	 indexOfDQCDayToComputeHourlyFor = getDay_forHourlyT( strCurrDate,
	                                                      &initStruct,
							      numDays );

         /* Retrieve the DQC hydrologic day to store the 12z obs
	    in. */
         indexOfDQCDayToStore12HrObs = getDayOf12HrObs ( strCurrDate,
	                                                 &initStruct,
							 numDays );

	 /* 
	  * Mismatch pTemperatureNode->obstime value, skip it.
	  */
	 if ((indexOfDQCDayToComputeMaxMinFor >= 0) &&
	     (indexOfDQCDayToStore12HrObs >= 0))
	 {

	    processTemperatureRecord (strCurrDate,
                                      & initStruct,
				      pTemperatureNode,
				      indexOfDQCDayToComputeMaxMinFor,
				      indexOfDQCDayToComputeHourlyFor,
				      indexOfDQCDayToStore12HrObs,
				      closest6HourSynopticTime,
				      diffTime,
				      pStationInfo);

	 }

      }

      pTemperatureNode = (Temperature *) ListNext (&pTemperatureNode->node);
   }
}
