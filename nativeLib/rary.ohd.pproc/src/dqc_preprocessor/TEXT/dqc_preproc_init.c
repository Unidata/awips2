
/**********************************************************************
 * initPrecipArray ( )
 * 
 * This function allocates memory and
 * initializes the precip station data array.
 *********************************************************************/

#include "dqc_preproc.h"

static dqcInitStruct initStruct;

inline int getSearchWindow ( )
{
   return initStruct.window_time_t;
}

inline int getEndingObstimeFlag ( )
{
   return initStruct.dqc_ending_6hour_obstime_flag;
}

inline dqcInitStruct getInitStruct ( )
{
   return initStruct;
}

void initTokens (time_t start_time_t, int numDays )
{
   const char *MPE_TEMPERATURE_WINDOW_TOKEN = "mpe_temperature_window";
   const char *MPE_MAXMINT_HOUR_WINDOW_TOKEN = "mpe_maxminT_hour_window";
   char mpe_temperature_window[4] = { '\0' };
   char mpe_maxminT_hour_window[3]={'\0'};

   struct tm * GMTtime = NULL;
   struct tm * localTime = NULL;
   int endingObsTime;
   int GMThour;
   int i;
   int j;
   int localHour;
   int status;
   time_t currentGMTtime;
   time_t timet;

   initStruct.window_time_t = TEMPERATURE_WINDOW_DEFAULT;
   initStruct.temperature_hour_window = MAXMIN_TEMPERATURE_HOUR_WINDOW;
   initStruct.dqc_ending_6hour_obstime_flag = DQC_PREPROCESSOR_ENDING_OBSTIME_12Z;
   initStruct.ending6HourObsTime = DEFAULT_ENDING_6HOUR_OBS_TIME;

   /* Determine the what local time is in GMT.  For example, in EST,
      local midnight is 05Z. */
   currentGMTtime = time ( NULL );
   GMTtime = gmtime (&currentGMTtime);
   GMThour = GMTtime->tm_hour;
   localTime = localtime (&currentGMTtime);
   localHour = localTime->tm_hour;

   if ( GMThour < localHour )
   {
      GMThour += 24;
   }

   initStruct.localMidnightInZ = GMThour - localHour;

   for ( i = 0; i < MAXIMUM_DAY; ++i )
   {
     memset ( initStruct.strDate[i], '\0', 11 );
   }

   for ( i = 0; i < 2; ++i )
   {
      for ( j = 0; j < 24; ++j )
      {
         initStruct.hourSlotMap[i][j] = -1;
      }
   }

   status =
		   dqc_preprocessor_getAppsDefaults (MPE_TEMPERATURE_WINDOW_TOKEN, mpe_temperature_window);
   if (status == -1)
   {
      printf ("WARNING: Token \"%s\" is not defined. Application will use "
	      "default value -- %d minutes.\n",
	      MPE_TEMPERATURE_WINDOW_TOKEN, initStruct.window_time_t);
   }
   else if (strlen (mpe_temperature_window) > 0)
   {
      initStruct.window_time_t = atoi (mpe_temperature_window);
      printf ("STATUS: Set the temperature window value to %d minutes.\n",
	      initStruct.window_time_t);
   }

   initStruct.window_time_t *= 60;		// convert to seconds


   /* get value from token mpe_maxminT_hour_window, the default value is defined from
   MAXMIN_TEMPERATURE_HOUR_WINDOW*/

   status =
		   dqc_preprocessor_getAppsDefaults (MPE_MAXMINT_HOUR_WINDOW_TOKEN, mpe_maxminT_hour_window);
   if (status == -1)
   {
      printf ("WARNING: Token \"%s\" is not defined. Application will use "
	      "default value -- %d hours.\n",
	      MPE_MAXMINT_HOUR_WINDOW_TOKEN, initStruct.temperature_hour_window);
   }
   else if (strlen (mpe_maxminT_hour_window) > 0)
   {
      initStruct.temperature_hour_window = atoi (mpe_maxminT_hour_window);
      printf ("STATUS: Set the max/min temperature hour window value to %d hours.\n",
	      initStruct.temperature_hour_window);
   }



   /*
    * Compute the date string array.
    */
   timet = start_time_t;
   for (i = 0; i < numDays; i++)
   {
      timet += 24 * SECONDS_PER_HOUR;
      timet_to_yearday_ansi (timet, initStruct.strDate[i]);
   }

   /* get the token get_ending_6hour_obstime, the default value is 06Z which
      means to generate temperature point level1 data at 12Z, 18Z, 00Z
      and 06Z.
      If the token is set as 12Z, then generate temperature point level1 data
      at 18Z, 00Z, 06Z and 12Z.
    */

   endingObsTime = getEnding6HourObsTime();

   if ( endingObsTime == 6 )
   {
      initStruct.dqc_ending_6hour_obstime_flag = DQC_PREPROCESSOR_ENDING_OBSTIME_06Z;
      initStruct.ending6HourObsTime = endingObsTime;
   }
   else
   {
      /* The ending obstime is 12z. */
      initStruct.dqc_ending_6hour_obstime_flag = DQC_PREPROCESSOR_ENDING_OBSTIME_12Z;
      initStruct.ending6HourObsTime = endingObsTime;
   }

   printf ("STATUS: the token dqc_ending_6hour_obstime is set as %d.\n",
	    endingObsTime);

   /* Initialize the arrays containing the six hour slot indexes for
      the two possible 6 hour ending observation times. */
      /* 6 Hour Ending Obstime of 06Z. */
   initStruct.hourSlotMap[DQC_PREPROCESSOR_ENDING_OBSTIME_06Z][12] = 0;
   initStruct.hourSlotMap[DQC_PREPROCESSOR_ENDING_OBSTIME_06Z][18] = 1;
   initStruct.hourSlotMap[DQC_PREPROCESSOR_ENDING_OBSTIME_06Z][0] = 2;
   initStruct.hourSlotMap[DQC_PREPROCESSOR_ENDING_OBSTIME_06Z][6] = 3;

   /* 6 Hour Ending Obstime of 12z. */
   initStruct.hourSlotMap[DQC_PREPROCESSOR_ENDING_OBSTIME_12Z][18] = 0;
   initStruct.hourSlotMap[DQC_PREPROCESSOR_ENDING_OBSTIME_12Z][0] = 1;
   initStruct.hourSlotMap[DQC_PREPROCESSOR_ENDING_OBSTIME_12Z][6] = 2;
   initStruct.hourSlotMap[DQC_PREPROCESSOR_ENDING_OBSTIME_12Z][12] = 3;

}

void initPrecipArray (const int stationSize, const int numDays)
{
   int i, j, k;

   pPrecipInfo = (struct precip_info *) malloc (stationSize *
						sizeof (struct precip_info));

   if (pPrecipInfo == NULL)
   {
      printf ("Could not allocate memory for the precip array."
	      "\n\tProgram exit.\n");
      exit (-1);
   }

   for (i = 0; i < stationSize; i++)
   {
      pPrecipInfo[i].pPPD = (double *) malloc (numDays * sizeof (double));

      if (pPrecipInfo[i].pPPD == NULL)
      {
	 printf ("Could not allocate memory for the precip array."
		 "\n\tProgram exit.\n");
	 exit (-1);
      }

      for (j = 0; j < numDays; j++)
      {
	 pPrecipInfo[i].pPPD[j] = PRECIP_MISSING;
      }

      pPrecipInfo[i].pPPQ = (double **) malloc (numDays * sizeof (double *));

      if (pPrecipInfo[i].pPPQ == NULL)
      {
	 printf ("Could not allocate memory for the precip array."
		 "\n\tProgram exit.\n");
	 exit (-1);
      }

      for (j = 0; j < numDays; j++)
      {
	 pPrecipInfo[i].pPPQ[j] = (double *) malloc (4 * sizeof (double));;
	 if (pPrecipInfo[i].pPPQ[j] == NULL)
	 {
	    printf ("Could not allocate memory for the precip array."
		    "\n\tProgram exit.\n");
	    exit (-1);
	 }

	 for (k = 0; k < 4; k++)
	 {
	    pPrecipInfo[i].pPPQ[j][k] = PRECIP_MISSING;
	 }
      }

      pPrecipInfo[i].pPPQPE = (char **) malloc (numDays * sizeof (char *));

      if (pPrecipInfo[i].pPPQPE == NULL)
      {
	 printf ("Could not allocate memory for the precip array."
		 "\n\tProgram exit.\n");
	 exit (-1);
      }

      for (j = 0; j < numDays; j++)
      {
	 pPrecipInfo[i].pPPQPE[j] = (char *) malloc (3 * sizeof (char));;
	 if (pPrecipInfo[i].pPPQPE[j] == NULL)
	 {
	    printf ("Could not allocate memory for the precip array."
		    "\n\tProgram exit.\n");
	    exit (-1);
	 }

	 strcpy (pPrecipInfo[i].pPPQPE[j], "PP");
      }

      pPrecipInfo[i].source = 'Z';
   }
}


void releasePrecipArray (const int stationSize, const int numDays)
{
   int i, j;

   if (pPrecipInfo != NULL)
   {
      for (i = 0; i < stationSize; i++)
      {
	 if (pPrecipInfo[i].pPPQ != NULL)
	 {
	    for (j = 0; j < numDays; j++)
	    {
	       if (pPrecipInfo[i].pPPQ[j] != NULL)
	       {
		  free (pPrecipInfo[i].pPPQ[j]);
		  pPrecipInfo[i].pPPQ[j] = NULL;
	       }
	    }
	 }
	 if (pPrecipInfo[i].pPPQPE != NULL)
	 {
	    for (j = 0; j < numDays; j++)
	    {
	       if (pPrecipInfo[i].pPPQPE[j] != NULL)
	       {
		  free (pPrecipInfo[i].pPPQPE[j]);
		  pPrecipInfo[i].pPPQPE[j] = NULL;
	       }
	    }
	 }
	 if (pPrecipInfo[i].pPPD != NULL)
	 {
	    free (pPrecipInfo[i].pPPD);
	    pPrecipInfo[i].pPPD = NULL;
	 }
      }
      free (pPrecipInfo);
      pPrecipInfo = NULL;
   }
}


void initTempArray (const int stationSize, const int numDays)
{
   int i, j, k;

   pTempInfo = (struct temperature_info *) malloc (stationSize *
						   sizeof (struct
							   temperature_info));

   if (pTempInfo == NULL)
   {
      printf ("Could not allocate memory for the temperature array."
	      "\n\tProgram exit.\n");
      exit (-1);
   }

   for (i = 0; i < stationSize; i++)
   {
      pTempInfo[i].value = (double **) malloc (numDays * sizeof (double *));

      if (pTempInfo[i].value == NULL)
      {
	 printf ("Could not allocate memory for the temperature array."
		 "\n\tProgram exit.\n");
	 exit (-1);
      }

      for (j = 0; j < numDays; j++)
      {
	 /* For each row, allocate eight columns.  These eight columns are
	    four 6 hour slots, 1 daily max slot, 1 daily min slot, 1 hourly
	    max slot, 1 hourly min slot. */
	 pTempInfo[i].value[j] = (double *) malloc (8 * sizeof (double));;
	 if (pTempInfo[i].value[j] == NULL)
	 {
	    printf ("Could not allocate memory for the temperature array."
		    "\n\tProgram exit.\n");
	    exit (-1);
	 }

	 for (k = 0; k < 4; k++)
	 {
	    pTempInfo[i].value[j][k] = TEMPERATURE_MISSING;
	 }

	 pTempInfo[i].value[j][4] = MISSING_MAX_TEMPERATURE;
	 pTempInfo[i].value[j][5] = MISSING_MIN_TEMPERATURE;
	 pTempInfo[i].value[j][6] = MISSING_MAX_TEMPERATURE;
	 pTempInfo[i].value[j][7] = MISSING_MIN_TEMPERATURE;
      }

      pTempInfo[i].diffTime = (int **) malloc (numDays * sizeof (int *));

      if (pTempInfo[i].diffTime == NULL)
      {
	 printf ("Could not allocate memory for the temperature array."
		 "\n\tProgram exit.\n");
	 exit (-1);
      }

      for (j = 0; j < numDays; j++)
      {
	 pTempInfo[i].diffTime[j] = (int *) malloc (6 * sizeof (int));;
	 if (pTempInfo[i].diffTime[j] == NULL)
	 {
	    printf ("Could not allocate memory for the temperature array."
		    "\n\tProgram exit.\n");
	    exit (-1);
	 }

	 for (k = 0; k < 6; k++)
	 {
	    pTempInfo[i].diffTime[j][k] = MAX_DIFF_TIME;
	 }
      }
   }
}


void releaseTempArray (const int stationSize, const int numDays)
{
   int i, j;

   if (pTempInfo != NULL)
   {
      for (i = 0; i < stationSize; i++)
      {
	 if (pTempInfo[i].value != NULL)
	 {
	    for (j = 0; j < numDays; j++)
	    {
	       if (pTempInfo[i].value[j] != NULL)
	       {
		  free (pTempInfo[i].value[j]);
		  pTempInfo[i].value[j] = NULL;
	       }
	    }
	 }
	 if (pTempInfo[i].diffTime != NULL)
	 {
	    for (j = 0; j < numDays; j++)
	    {
	       if (pTempInfo[i].diffTime[j] != NULL)
	       {
		  free (pTempInfo[i].diffTime[j]);
		  pTempInfo[i].diffTime[j] = NULL;
	       }
	    }
	 }
      }
      free (pTempInfo);
      pTempInfo = NULL;
   }
}
