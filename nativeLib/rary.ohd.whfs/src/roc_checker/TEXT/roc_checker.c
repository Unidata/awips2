#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <sqlca.h>
#include <stdio.h>
#include <math.h>

#include "roc_checker.h"
#include "DbmsAccess.h"
#include "GeneralUtil.h"
#include "get_limits.h"
#include "time_convert.h"

#include <sqlca.h>
/*extern struct sqlca_s sqlca;
extern long SQLCODE;
*/
/**************************************************************************

   PURPOSE
   Computes the rate of change for a specified time series of observational
   data.  This ROC value is then checked against three ROC thresholds:
   a QC threshold, an alarm threshold, and an alert threshold.  If
   it fails the QC threshold, the quality code in the appropriate
   database table containing the record is set accordingly.  If it fails
   either alarm or alert thresholds, the record is written to the
   AlertAlarmVal table.

   Note that there is currently no means to check forecast data for
   ROCs.  Because of this, there is no need to clean out old basis
   times for ROC data.


   ***********************************************************************/

int roc_checker_main(int argc, const char ** argv)
{

   FILE        *logfilePtr;
   char         dbname[25];
   char 	lid[LOC_ID_LEN + 1];
   char		pe[SHEF_PE_LEN + 1];
   char		tabname[TABLE_NAME_LEN + 1];
   /* char 	start_time[ANSI_TIME_LEN + 1]; */
   time_t       start_timet;
   time_t	end_timet;
   int		numhrs;
   int		show_errs_only;
   int          usedata_mode;
   double	alert_upper_limit;
   int          dbstatus;
   char		logfilePath[200];
   time_t	tnow;
   struct tm   *tm_struct;
   char         timestr[25];

   int          len=0, rlen=0, istatus=0;
   char         rdir_path[128];


   /* initialization of memory blocks */

   memset(&lid,     0, sizeof(lid));
   memset(&pe,      0, sizeof(pe));
   memset(&tabname, 0, sizeof(tabname));


   /* initialization of variables */

   logfilePtr     = NULL;
   show_errs_only = 0;
   alert_upper_limit	  = MISSING_VAL;


   /* get the current time for later use, including using it
      to name the log file. Use this silly string to avoid problems with SCCS
      messing up the %characters when doing a get */

   time(&tnow);
   tm_struct = gmtime(&tnow);


   /* The get_check_options() function handles the following:
      - Insures proper number of command line arguments
      - Retrieves the command line arguments
      - Opens the database  */

   get_check_options(argc, argv, dbname, tabname, &end_timet, &numhrs,
		     &show_errs_only, &usedata_mode, lid, pe);


   /* open the Rate-of-Change Checker log file for dynamic input of
      diagnostic messages. */

   len = strlen("qcalarm_log_dir");
   istatus = get_apps_defaults("qcalarm_log_dir", &len, rdir_path, &rlen);

   if (istatus != 0)
   {
      printf("qcalarm_log_dir is undefined.  Program aborting.\n");
      exit(-1);
   }

   sprintf(logfilePath, "%s/roc_%s.%02d%02d.%02d%02d%02d", rdir_path, tabname,
           tm_struct->tm_mon + 1, tm_struct->tm_mday, tm_struct->tm_hour,
           tm_struct->tm_min, tm_struct->tm_sec);

   logfilePtr = fopen(logfilePath, "w");

   if (logfilePtr == NULL)
   {
      fprintf(logfilePtr, "Error opening logfile: %s.\n", logfilePath);
      exit(-1);
   }


   fprintf(logfilePtr, "Rate-of-Change Checker  ** V5.1.2 July 17, 2001 **");


   /* log the following command line information to the logfile */

   strftime(timestr, 25, "%a %b %d %H:%M:%S %Y", tm_struct);

   fprintf(logfilePtr, "\nCurrent Time     = %s\n", timestr);

   fprintf(logfilePtr, "\nDatabase Name    = %s", dbname);

   fprintf(logfilePtr, "\nPE Table Name    = %s", tabname);

   fprintf(logfilePtr, "\nQuery End Time   = %s", asctime(gmtime(&end_timet)));

   start_timet = end_timet - numhrs * 3600;
   fprintf(logfilePtr, "Query Start Time = %s", asctime(gmtime(&start_timet)));

   fprintf(logfilePtr, "Number of Hours  = %d", numhrs);


   if (usedata_mode == USE_GOOD_DATA)
      fprintf(logfilePtr, "%s",
	      "\nData Used        = GOOD DATA ONLY, NO QUESTIONABLE DATA");

   else if (usedata_mode == USE_GOOD_QUESTIONABLE_DATA)
      fprintf(logfilePtr, "%s",
	      "\nData Used        = GOOD AND QUESTIONABLE DATA");

   if (strlen(lid) != 0)
      fprintf(logfilePtr, "\nLocation ID      = %s", lid);

   if (strlen(pe) != 0)
      fprintf(logfilePtr, "\nPhysical Element = %s", pe);


   /* The do_roc_checker() function handles the following:
      - Retrieves data from the database table(s)
      - Performs some message handling
      - Begins processing of selected data
      - Writes data events to logfile */

   do_roc_checker(tabname, end_timet, numhrs, lid, pe, show_errs_only,
		  &usedata_mode, &tnow, logfilePtr);


   /* close the database.  Upon successful execution,
      normally return from main() routine. */

   dbstatus = CloseDbms();


   /* closure of the data review logfile and error handling */

   if (fclose(logfilePtr) != 0)
   {
      perror("Error encountered upon closure of the roc_checker logfile.");
      exit(-1);
   }

   return(0);

}


/*********************************************************************
   get_check_options()

   PURPOSE
   Get the options specified for the application.
   This function opens the database.

   *******************************************************************/

void get_check_options(int		argc,
		       char		**argv,
                       char             *dbname,
		       char		*tabname,
		       time_t		*end_timet,
		       int		*numhrs,
		       int		*show_errs_only,
                       int              *usedata_mode,
		       char		*lid,
		       char		*pe)

{
   extern char 	*optarg;
   extern int	 optind;
   extern int    optopt;
   int		 c;
   int           dbstatus;
   int		 slen;
   int           status;
   int		 lookback_hrs;
   int           udate;
   int           uhour;
   int           num_items;
   int		 db_given, lid_given, pe_given;
   int           time_given, numhrs_given;
   int           table_given, show_err_given, usedata_given;

   struct tm     utm;
   time_t 	 utimet;
   extern long   timezone;


   /* initialization of automatic (local) variables */

   db_given       = 0;
   lid_given      = 0;
   pe_given       = 0;
   numhrs_given   = 0;
   time_given     = 0;
   table_given    = 0;
   show_err_given = 0;
   usedata_given  = 0;


   /* check the number of command line arguments.  There are two required
      arguments, database name and table name.  In argv[] array, the first
      argument is the name of the program thus this program checks for
      argument count of 3. */

   if (argc < 3)
   {
      printf("Usage: roc_checker -d<dbase> -t<table> "
	     "[-n<hrs>] [-h<endhr>] [-l<lid>] [-p<pe>] [-u<G | GQ>]\n");
      exit(-1);
   }


   /*process and load the number of command line arguments */

   while ((c = getopt(argc, argv, "d:t:p:n:e:l:h:u:")) != -1)

   {  /* Opening brace of while((c = getopt()) loop */

      switch (c)

      {  /* Opening brace of switch(c) structure */

         case 'd':

            /* open the INFORMIX database */

	    if ((dbstatus = OpenDbms(optarg)) != 0)
	    {
	       printf("Error %d opening database: %s\n", dbstatus, optarg);
	       exit(-1);
	    }

	    else
            {
	       db_given = 1;
               strcpy(dbname, optarg);
            }

	    break;

	 case 't':

	    status = 1;
	    slen = strlen(optarg);

	    if (slen == 0)
	       status = -1;

	    if (status < 0)
	    {
	       printf("Invalid length for table name  %s\n", optarg);
	       exit(-1);
	    }

	    else
	    {
	       strcpy(tabname, optarg);
	       table_given = 1;
	    }

	    break;

	 case 'l':

	    status = 1;
	    slen = strlen(optarg);

	    if (slen == 0 || slen > LOC_ID_LEN)
	       status = -1;

	    if (status < 0)
	    {
	       printf("Invalid length for location id %s\n", optarg);
	       exit(-1);
	    }

	    else
	    {
	       strcpy(lid, optarg);
	       lid_given = 1;
	    }

	    break;

	 case 'p':

	    status = 1;
	    slen = strlen(optarg);

	    if (slen == 0 || slen > SHEF_PE_LEN)
	       status = -1;

	    if (status < 0)
	    {
	       printf("Invalid length for physical element %s\n", optarg);
	       exit(-1);
	    }

	    else
	    {
	       strcpy(pe, optarg);
	       pe_given = 1;
	    }

	    break;

	 case 'n':

	    status = 1;
	    slen = strlen(optarg);

	    if (slen == 0)
	       status = -1;

	    else
	    {
	       *numhrs = atoi(optarg);

	       if (*numhrs <= 0 || *numhrs > 960)
		  status = -1;
	    }

	    if (status < 0)
	    {
	       printf("Number of hours must be between 1-960 %s\n", optarg);
	       exit(-1);
	    }

	    else
	    {
	       numhrs_given = 1;
	    }

	    break;

	 case 'h':

	    status       =  1;
	    lookback_hrs = -1;
            udate        = -1;
            uhour        = -1;
	    slen         = strlen(optarg);

	    if (slen == 0)
	       status = -1;

	    else
	    {
	       if (slen <= 4)
	       {
		  lookback_hrs = atoi(optarg);

		  if (lookback_hrs < 0 || lookback_hrs > 9999)
		     status = -1;
	       }

	       else if (slen == 10)
	       {
		  num_items = sscanf(optarg, "%8d%2d", &udate, &uhour);

		  if (num_items != 2 || udate <= 0 || uhour <= 0 || uhour > 23)
		     status = -1;
	       }

	       else
		  status = -1;
	    }

	    if (status < 0)
	    {
	       printf("Specify a relative endtime as hours between 0-9999 or\n"
		      "an absolute endtime as YYYYMMDDHH %s\n", optarg);
	       exit(-1);
	    }

	    else
	    {
	       if (lookback_hrs >= 0)
	       {
		  time(&utimet);
		  utimet = utimet - (lookback_hrs * 3600);
	       }

	       else if (uhour >= 0)
	       {
		  utm.tm_year = (udate / 10000) - 1900;
		  utm.tm_mon  = ((udate % 10000) / 100) - 1;
		  utm.tm_mday = (udate % 100);
		  utm.tm_hour = uhour;
		  utm.tm_min  = 0;
		  utm.tm_sec  = 0;
		  utimet      = mktime(&utm);
		  utimet      = utimet - timezone;
	       }

	       *end_timet = utimet;
	       time_given = 1;
	    }

	    break;

	 case 'e':

	    slen = strlen(optarg);

	    if (slen == 0 || slen > 4)
	    {
	       printf("Invalid -err option given %s\n", optarg);
	       exit(-1);
	    }

	    else
	       *show_errs_only = 1;

	    break;

	 case 'u':

	    slen = strlen(optarg);

	    if (slen == 0 || slen > 2)
	    {
	       printf("Invalid -u option given: '%s'. "
		      "Specify 'G' or 'GQ'.\n", optarg);
	       exit(-1);
	    }

	    else if ( (strcmp(optarg, "G") != 0) &&
		     (strcmp(optarg, "GQ") != 0) )

            {
	       printf("Invalid -u option given: '%s'. "
		      "Specify 'G' or 'GQ'.\n", optarg);
	       exit(-1);
            }

	    else
            {
	       *usedata_mode = (strcmp(optarg, "G") == 0) ? USE_GOOD_DATA :
		  USE_GOOD_QUESTIONABLE_DATA;

	       usedata_given = 1;
            }

	    break;

         default:

	    break;

      }  /* Closing brace of switch(c) structure */

   }  /* Closing brace of while((c = getopt()) loop */

   /* ensure the mandatory command line arguments were given */

   if (!db_given || !table_given)
   {
      printf("Usage: roc_checker -d<dbase> -t<table> "
	     "[-n<hrs>] [-h<endhr>] [-l<lid>] [-p<pe>] [-u<G | GQ>]\n");
      exit(-1);
   }

   /* assign any defaults */

   if (!numhrs_given)
      *numhrs = DEFAULT_HOURS;

   if (!time_given)
   {
      time(&utimet);
      *end_timet = utimet;
   }

   if (!usedata_given)
      *usedata_mode = USE_GOOD_DATA;

   return;

}


/***************************************************************************
   The do_roc_checker() function.
   *************************************************************************/

void do_roc_checker(char       *tabname,
		    time_t	end_timet,
		    int	        numhrs,
		    char       *lid,
		    char       *pe,
		    int		show_errs_only,
                    int        *usedata_mode,
		    time_t     *tnow,
                    FILE       *logfilePtr)

{
   Observation	    *obsHead;
   LocDataLimits    *LocDataLimitsHead;
   DataLimits       *DataLimitsHead;
   int		    cnt;
   time_t	    curtime1;
   time_t           curtime2;


   /* retrieve all data from the LocDataLimits table */

   LocDataLimitsHead = GetLocDataLimits("ORDER BY lid ASC, pe, dur");


   /* retrieve all data from the DataLimits table */

   DataLimitsHead = GetDataLimits("ORDER BY pe ASC, dur");


   /* retrieve data from PE table and track time */

   time(&curtime1);

   obsHead = get_review_data(tabname, end_timet, numhrs, usedata_mode,
			     lid, pe, logfilePtr);


   /* if data is found, then process. */

   if (obsHead == NULL)
   	cnt = 0;
   else
   	cnt = ListCount(&obsHead->list);

   if (cnt > 0)
   {

      /* log messages */

      fprintf(logfilePtr, "\n\nProcessing %d observations.", cnt);

      time(&curtime2);

      fprintf(logfilePtr, "\nRetrieval time = %ld seconds\n",
	      (curtime2 - curtime1));

      fprintf(logfilePtr,
	      "\nIgnoring any values designated as missing or as extremes.");


      /* subheader for QC threshold, Alarm, and Alert limit data
	 recordings in the Rate-Of-Change logfile */

      fprintf(logfilePtr, "%s\n",
              "\nThe QC, Alarm, and Alert threshold values (-9999 denotes missing value) for each\n"
              "location id, physical element combination are given as follows: ");


      /* process the quality control check and the rate-of-change for this batch
	 of data via a call to the perform_roc_checker() function */

      perform_roc_checker(tabname, obsHead, LocDataLimitsHead, DataLimitsHead,
			  show_errs_only, tnow, logfilePtr);


      /* free memory structure */
      if (obsHead != NULL)
      {
      	FreeObservation(obsHead);
      	obsHead = NULL;
      }


      /* procure end time from system calendar clock */

      time(&curtime2);

      fprintf(logfilePtr, "\n\nTotal time = %ld second(s)\n",
	      (curtime2 - curtime1));

   }

   return;

}


/**********************************************************************
   The get_review_data() function.
   *******************************************************************/

Observation *get_review_data(char       *tabname,
			     time_t	 end_timet,
			     int	 numhrs,
                             int        *usedata_mode,
			     char       *lid,
			     char       *pe,
                             FILE       *logfilePtr)

{

   Observation	*obsHead;
   int	         status;
   char		 where[400];
   char          pe_str[50];
   char          usedata_str[50];
   char	 	 begin_time[ANSI_TIME_LEN + 1];
   char          end_time[ANSI_TIME_LEN + 1];
   int	         cnt;


   /* define the time window */

   status = timet_to_yearsec_ansi(end_timet, end_time);
   status = timet_to_yearsec_ansi(end_timet - (numhrs * 3600), begin_time);


   /* first, form the where clause to get the data */

   /* construct the where subclause that regards the physical element (pe) */

   if (strlen(pe) > 0)
      sprintf(pe_str, " AND pe='%s' ", pe);

   else
      sprintf(pe_str," ");


   /* construct the where subclause that regards the quality code of the data */

   if (*usedata_mode == USE_GOOD_DATA)
      build_qc_where(QC_PASSED, usedata_str);

   else if (*usedata_mode == USE_GOOD_QUESTIONABLE_DATA)
      build_qc_where(QC_NOT_FAILED, usedata_str);

   if (strlen(lid) > 0)
   {
      sprintf(where, "WHERE lid='%s' AND obstime >= '%s' AND "
	      " obstime <= '%s' %s AND %s AND value != %f ORDER BY "
	      " lid, pe, obstime ASC ", lid, begin_time,
	      end_time, pe_str, usedata_str, MISSING_VAL);
   }

   else
   {
      sprintf(where, "WHERE obstime >= '%s' AND "
	      " obstime <= '%s' %s AND %s AND value != %f "
	      " ORDER BY lid, pe, obstime ASC ",
	      begin_time, end_time, pe_str, usedata_str, MISSING_VAL);
   }


   /* now get the data */

   obsHead = GetObservation(where, tabname);

   if (obsHead == NULL)
   	cnt = 0;
   else
   	cnt = ListCount(&obsHead->list);

   if (cnt == 0)
   {
      fprintf(logfilePtr, "\n\nSpecified data between %s and %s not found.\n",
	      begin_time, end_time);
   }


   return(obsHead);
}


/**********************************************************************
   perform_roc_checker()
   control the processing for the multiple lid,pe combinations
   *********************************************************************/

void perform_roc_checker(char 	               *tabname,
			 Observation 	       *obsHead,
			 LocDataLimits	       *LocDataLimitsHead,
			 DataLimits	       *DataLimitsHead,
			 int     		show_errs_only,
			 time_t		       *tnow,
			 FILE                  *logfilePtr)

{
   Observation	*obsPtr;
   Observation  *tempPtr;


   /* loop on the values */

   obsPtr = (Observation *) ListFirst(&obsHead->list);

   while (obsPtr)
   {

      /* perform the analysis for this location id (lid)/physical
	 element (pe) combination */

      tempPtr = process_lidpe_data(tabname, obsPtr,
				   LocDataLimitsHead, DataLimitsHead,
				   show_errs_only, tnow, logfilePtr);

      /* In the course of processing the data for this lid, pe
	 combination, the function marches through the linked list
	 until the next lid, pe combination is found, or the end of the
	 list is reached.  update the location of the pointer in this.
	 if not at the end, then continue with the new combination. */

      obsPtr = tempPtr;
   }


   return;
}


/*********************************************************************

   process_lidpe_data()
   do the actual analysis for a single lid-pe combination

   ********************************************************************/

Observation *process_lidpe_data(char 	        *tabname,
				Observation 	*obsHead,
				LocDataLimits   *LocDataLimitsHead,
				DataLimits      *DataLimitsHead,
				int		 show_errs_only,
				time_t		*tnow,
				FILE            *logfilePtr)

{
   Observation	*obsPtr;
   char		 cur_lid[LOC_ID_LEN + 1];
   char          cur_pe[SHEF_PE_LEN + 1];
   char		 ansi_time[ANSI_TIME_LEN + 1];
   char		 where[240];
   char         *indentation = "  ";
   time_t	 time_diff, obs_timet, prev_timet=0;
   double	 prev_value;
   double	 value_diff;
   double        roc;
   int		 use_current_value;
   int		 roc_status=0;
   int		 qcalarm_found;
   int		 num_minutes;
   int		 status, returnValue;
   double	 roc_prev;
   int		 lid_cnt;
   double        threshold_roc_max;
   double        threshold_alert_roc_limit, threshold_alarm_roc_limit;
   int		 perform_checks;
   char		 valuestr[10];


   /* initialization of local variables */

   prev_value  = MISSING_VAL;
   qcalarm_found = 0;
   roc_prev    = MISSING_VAL;
   lid_cnt     = 0;

   strcpy(cur_lid, obsHead->lid);
   strcpy(cur_pe,  obsHead->pe);


   /* loop on the obs */

   obsPtr = obsHead;


   /* get data limits based on lid/pe/dur/monthdaystart criteria
      and log a message about the values. */

   get_data_limits(LocDataLimitsHead, DataLimitsHead, obsHead,
		   &threshold_roc_max, &threshold_alarm_roc_limit,
		   &threshold_alert_roc_limit);

   fprintf(logfilePtr,
	   "\n%s %s:  %.2f %.2f %.2f",
	   obsPtr->lid, obsPtr->pe, threshold_roc_max,
	   threshold_alarm_roc_limit, threshold_alert_roc_limit);

   if (threshold_roc_max         != MISSING_VAL ||
       threshold_alarm_roc_limit != MISSING_VAL ||
       threshold_alert_roc_limit != MISSING_VAL)
      perform_checks = 1;

   else
      perform_checks = 0;


   /* now loop on each of the observations for this group of data */

   while (obsPtr)
   {

      /* If no longer working with the same location id/pe combination,
	 exit the loop. */

      if (strcmp(obsPtr->lid, cur_lid) != 0 ||
	  strcmp(obsPtr->pe,  cur_pe)  != 0)
	 break;

      lid_cnt++;


      status = yearsec_dt_to_timet(obsPtr->obstime, &obs_timet);
      status = yearsec_dt_to_ansi(obsPtr->obstime, ansi_time);


      /* skip the missing values and extremum values. */

      if (strcmp(obsPtr->extremum, "Z") != 0)
	 use_current_value = 0;

      else
	 use_current_value = 1;


      /* if the current value is okay and a previous value exists,
	 then check the rate of change */

      roc = MISSING_VAL;

      if (use_current_value && prev_value != MISSING_VAL)
      {

	 /* get time difference and absolute value difference  */

	 value_diff = obsPtr->value - prev_value;
	 time_diff  = obs_timet - prev_timet;


	 /* check for identical times but different values */

	 if (time_diff == 0)
	 {
	    if (value_diff != 0)
	    {
	       fprintf(logfilePtr,
		       "\n%s %s: Two values with same time are not equal: %s %.2f %.2f",
		       obsPtr->lid, obsPtr->pe,
		       ansi_time, obsPtr->value, prev_value);

	       qcalarm_found = 1;
	    }
	 }


	 /* determine the rate of change in units/hour */

	 else
	 {
	    roc = (value_diff / (double) time_diff) * 3600.0;

	    /* initialize ROC status.  Check for ROC indicating questionable
	       data. If ROC not questionable, check if ROC is in alert range
	       if Alert ROC test is specified. */

	    roc_status = ROC_OKAY;


            /* for PC data, implement special checking for decreasing data. */

	    if ( ((threshold_roc_max != MISSING_VAL) &&
		  ((double) abs(roc * 100.0) >= (threshold_roc_max * 100.0))) ||
		((strcmp(obsPtr->pe, "PC") == 0) && roc < 0.0 )  )
	    {

	       /* QC_ROC check failed */

	       qcalarm_found = 1;

	       roc_status = ROC_BAD;

               /* if the QC_ROC check reflects failure yet the database reflects
		  success, set the subject quality control code in the database
		  to failure. */

	       if (check_qccode(QC_ROC_PASSED, obsPtr->quality_code) == TRUE_QC )
	       {
		  returnValue = set_qccode(QC_ROC_FAILED, &obsPtr->quality_code);

		  sprintf(where, " WHERE lid= '%s' AND pe= '%s' "
			  "AND obstime= '%s' "
			  "AND dur= %d AND ts = '%s' AND extremum = '%s' ",
			  obsPtr->lid, obsPtr->pe, ansi_time,
			  obsPtr->dur, obsPtr->ts, obsPtr->extremum);

		  returnValue = UpdateObservation(obsPtr, where, tabname);

		  if (returnValue < 0)
		  {
		     fprintf(logfilePtr,
			     "\n%sPostgreSQL error %d updating in %s "
			     "%s,%s,%s,%d,%s,%s\n",
			     indentation, returnValue,
			     tabname, obsPtr->lid, obsPtr->pe,
			     ansi_time,  obsPtr->dur, obsPtr->ts, obsPtr->extremum);
		  }

	       }

	       num_minutes = time_diff / 60;


	       /* log the failed QC_ROC check to logfile */

	       if ( (strcmp(obsPtr->pe, "PC") == 0) && roc < 0.0 )
	       {
		  /* format the value with some smarts to
		     account for the printf's unpleasant habit
		     of formatting small negative numbers as +0.00 */

		  if (roc <= -0.0051)
		     sprintf(valuestr, "%+7.2f", roc);
		  else
		     sprintf(valuestr, "  -0.00");

		  fprintf(logfilePtr,
			  "\n%sroc < 0.0  %s [%s= (%.2f - %.2f) / %d mins]",
			  indentation, ansi_time, valuestr,
			  obsPtr->value, prev_value,  num_minutes);
	       }

	       else

		  fprintf(logfilePtr,
			  "\n%sroc >= qc  %s [%+7.2f= (%.2f - %.2f) / %d mins]",
			  indentation, ansi_time, roc, obsPtr->value, prev_value,
			  num_minutes);

	    }


	    /* QC_ROC check passed or was not performed.
	       Check against Alarm and possibly the Alert roc_limits. */

	    else
	    {

	       if ( threshold_alarm_roc_limit != MISSING_VAL &&
		   (double) abs(roc * 100.0) >= (threshold_alarm_roc_limit * 100.0) )
	       {

		  /* Alarm_roc check failed */

                  qcalarm_found = 1;
		  roc_status = ROC_ALARM;

		  Write_AlertAlarmVal(obsPtr, roc, roc_status, tnow, logfilePtr);

		  num_minutes = time_diff / 60;


		  /* Log the failed ALARM_ROC Check to logfile */

		  fprintf(logfilePtr,
		          "\n%sroc >= alarm %s [%+7.2f = (%.2f - %.2f) / %d mins]",
		          indentation, ansi_time, roc, obsPtr->value, prev_value,
                          num_minutes);

	       }


	       /* if Alarm_ROC check passed, check against Alert_roc_limit
		  if available.  */

	       if ( roc_status == ROC_OKAY &&
		   threshold_alert_roc_limit != MISSING_VAL &&
		   (double) abs(roc * 100.0) >= (threshold_alert_roc_limit * 100.0) )
	       {

		  /* alert_roc check failed */

		  qcalarm_found = 1;

		  roc_status = ROC_ALERT;

		  Write_AlertAlarmVal(obsPtr, roc, roc_status, tnow, logfilePtr);

		  status = yearsec_dt_to_ansi(obsPtr->obstime, ansi_time);
		  num_minutes = time_diff / 60;


		  /* log the failed ALERT_ROC Check to logfile  */

		  fprintf(logfilePtr,
			  "\n%sroc >= alert %s [%+7.2f = (%.2f - %.2f) / %d mins]",
			  indentation, ansi_time, roc, obsPtr->value, prev_value,
                          num_minutes);
	       }
	    }  /* end of else on passed QC ROC check */
	 }     /* end of if on determine rate of change */
      }        /* end of on use current value and prev_value okay */

      /* save values for the next pass, if they are acceptable.
	 Afterwards, retrieve the next record in link list. */

      if (roc_status != ROC_BAD)
      {
	 prev_value = (use_current_value) ? obsPtr->value : prev_value;
	 prev_timet = (use_current_value) ? obs_timet : prev_timet;
      }

      obsPtr = (Observation *) ListNext(&obsPtr->node);

   }  /* Closing brace of while(obsPtr) loop */


   /* if no errors are found and at least one check was performed,
      log a message */

   if (perform_checks)
   {
      if(!qcalarm_found && !show_errs_only)
	 fprintf(logfilePtr, "%s", " - PASSED");
   }


   /* log a "Not Applicable" (N/A) message to the logfile if
      no checks were performed.  Checks are normally not
      performed if all three threshold data limits are
      missing. */

   else
      fprintf(logfilePtr, "%s", " - N/A");


   return(obsPtr);
}


/*******************************************************
   get_data_limits() function


   When checking a station's qc and alert/alarm limits, this
   function uses the full set of location limits if they exist in
   LocDataLimits, even if for example, only the alert/alarm limits
   are defined and the qc limits are not set.  It will NOT attempt
   to get values for those null limits from the general
   DataLimits tables.

   ******************************************************/

void get_data_limits(LocDataLimits *LocDataLimitsHead,
		     DataLimits    *DataLimitsHead,
		     Observation   *obsHead,
		     double        *threshold_roc_max,
		     double        *threshold_alarm_roc_limit,
		     double        *threshold_alert_roc_limit)

{

   Observation          *obsPtr;
   LocDataLimits	*LocDataLimitsPtr;
   DataLimits	        *DataLimitsPtr;
   int                   lid_compare;
   int                   pe_compare;
   int                   dur_match;
   int                   date_within;
   int                   loc_range_found;
   int                   default_range_found;
   time_t                obstime_timet;


   /* initialization of certain local variables */

   obsPtr              = obsHead;
   loc_range_found     = 0;
   default_range_found = 0;

   /* initialization of formal variables */

   *threshold_roc_max         = MISSING_VAL;
   *threshold_alarm_roc_limit = MISSING_VAL;
   *threshold_alert_roc_limit = MISSING_VAL;


   /* extract data limit information, if available,  from the LocDataLimits
      table for a particular location id/pe combination.  First check if
      a specific entry exists for the given location, pe, duration, and
      time window. */

   if (LocDataLimitsHead != NULL)
      LocDataLimitsPtr = (LocDataLimits *) ListFirst(&LocDataLimitsHead->list);

   else
      LocDataLimitsPtr = NULL;

   while (LocDataLimitsPtr != NULL)
   {

      /* check if the location matches. If so, then check the
	 remaining information to see if there is a match. */

      lid_compare = strcmp(obsPtr->lid, LocDataLimitsPtr->lid);

      if (lid_compare == 0)
      {
	 pe_compare  = strcmp(obsPtr->pe, LocDataLimitsPtr->pe);

	 if (obsPtr->dur == LocDataLimitsPtr->dur)
	    dur_match = 1;

	 else
	    dur_match = 0;

         obstime_timet = yearsec_dt_to_timet( obsPtr->obstime, &obstime_timet );

	 date_within = check_date_range(obsPtr->obstime,
					LocDataLimitsPtr->monthdaystart,
					LocDataLimitsPtr->monthdayend);

	 if (pe_compare == 0 && dur_match == 1 && date_within == 1)
	 {
	    loc_range_found = 1;

	    if (IsNull(DOUBLE, &LocDataLimitsPtr->roc_max) == NOTNULL)
	       *threshold_roc_max = LocDataLimitsPtr->roc_max;

	    else
	       *threshold_roc_max = MISSING_VAL;

	    if (IsNull(DOUBLE, &LocDataLimitsPtr->alarm_roc_limit) == NOTNULL)
	       *threshold_alarm_roc_limit = LocDataLimitsPtr->alarm_roc_limit;

	    else
	       *threshold_alarm_roc_limit = MISSING_VAL;

	    if (IsNull(DOUBLE, &LocDataLimitsPtr->alert_roc_limit) == NOTNULL)
	       *threshold_alert_roc_limit = LocDataLimitsPtr->alert_roc_limit;

	    else
	       *threshold_alert_roc_limit = MISSING_VAL;

	    break;
	 }

      }


      /* if we passed the place in the list where the lid
	 should have been, then don't bother continuing */

      else if (lid_compare < 0)
	 break;


      /* get the next entry */

      LocDataLimitsPtr = (LocDataLimits *) ListNext(&LocDataLimitsPtr->node);

   }  /*  Closing brace of while(LocDataLimitsPtr != NULL) loop */


   /* if a location specific range is not defined, then
      check if the default range is defined */

   if (!loc_range_found)
   {

      if (DataLimitsHead != NULL)
	 DataLimitsPtr = (DataLimits *) ListFirst(&DataLimitsHead->list);

      else
	 DataLimitsPtr = NULL;

      while (DataLimitsPtr != NULL)
      {

	 /* check if the pe matches.  If so, then check the remaining
	    information to see if there is a complete match. */

	 pe_compare = strcmp(obsPtr->pe, DataLimitsPtr->pe);

	 if (pe_compare == 0)
	 {
	    if (obsHead->dur == DataLimitsPtr->dur)
	       dur_match = 1;

	    else
	       dur_match = 0;

            obstime_timet = yearsec_dt_to_timet( obsPtr->obstime, &obstime_timet );

	    date_within = check_date_range(obsPtr->obstime,
					   DataLimitsPtr->monthdaystart,
					   DataLimitsPtr->monthdayend);

	    if (dur_match == 1 && date_within == 1)
	    {
	       default_range_found = 1;

	       if (IsNull(DOUBLE, &DataLimitsPtr->roc_max) == NOTNULL)
		  *threshold_roc_max = DataLimitsPtr->roc_max;

	       else
		  *threshold_roc_max = MISSING_VAL;

	       if (IsNull(DOUBLE, &DataLimitsPtr->alarm_roc_limit) == NOTNULL)
		  *threshold_alarm_roc_limit = DataLimitsPtr->alarm_roc_limit;

	       else
		  *threshold_alarm_roc_limit = MISSING_VAL;

	       if (IsNull(DOUBLE, &DataLimitsPtr->alert_roc_limit) == NOTNULL)
		  *threshold_alert_roc_limit = DataLimitsPtr->alert_roc_limit;

	       else
		  *threshold_alert_roc_limit = MISSING_VAL;

	       break;
	    }

	 }

	 /* if we passed the place in the list where the pe
	    should have been, then don't bother continuing */

	 else if (pe_compare < 0)
	    break;

	 /* get the next entry */

	 DataLimitsPtr = (DataLimits *) ListNext(&DataLimitsPtr->node);

      }  /* Closing brace of while(DataLimitsPtr != NULL) loop */

   }     /* Closing brace of if(!loc_range_found) structure */


   return;
}



/*******************************************
   Write_AlertAlarmVal()

   ******************************************/


void Write_AlertAlarmVal(Observation	*obsPtr,
                         double     	roc_value,
			 int		roc_status,
			 time_t 	*tnow,
			 FILE 		*logfilePtr)

{
   AlertAlarmVal AlertAlarmVal_Record;
   int 		status;
   int		returnValue;
   time_t	temptimet;
   char		ansi_time[ANSI_TIME_LEN + 1];

   bool         isUnique;

   strcpy(AlertAlarmVal_Record.lid,      obsPtr->lid);
   strcpy(AlertAlarmVal_Record.pe,       obsPtr->pe);
   AlertAlarmVal_Record.dur =            obsPtr->dur;
   strcpy(AlertAlarmVal_Record.ts,       obsPtr->ts);
   strcpy(AlertAlarmVal_Record.extremum, obsPtr->extremum);
   AlertAlarmVal_Record.probability =    MISSING_VAL;

   status = yearsec_dt_to_timet(obsPtr->obstime, &temptimet);
   status = timet_to_yearsec_dt(temptimet, &AlertAlarmVal_Record.validtime);

   temptimet = 0;
   status = timet_to_yearsec_dt(temptimet, &AlertAlarmVal_Record.basistime);

   AlertAlarmVal_Record.value =                obsPtr->value;
   AlertAlarmVal_Record.suppl_value =          roc_value;

   strcpy(AlertAlarmVal_Record.shef_qual_code, obsPtr->shef_qual_code);
   AlertAlarmVal_Record.quality_code =         obsPtr->quality_code;

   AlertAlarmVal_Record.revision =             obsPtr->revision;
   strcpy(AlertAlarmVal_Record.product_id,     obsPtr->product_id);

   status = yearsec_dt_to_timet(obsPtr->producttime, &temptimet);
   status = timet_to_yearsec_dt(temptimet, &AlertAlarmVal_Record.producttime);

   status = timet_to_yearsec_dt(*tnow, &AlertAlarmVal_Record.postingtime);

   (void) SetNull(DATETIME, (void *)  &AlertAlarmVal_Record.action_time);
   strcpy(AlertAlarmVal_Record.aa_check, ROC_CHECKSTR);


   /* set the category to the keyword value */

   if (roc_status == ROC_ALARM)
      strcpy(AlertAlarmVal_Record.aa_categ, ALARM_CATEGSTR);

   else if (roc_status == ROC_ALERT)
      strcpy(AlertAlarmVal_Record.aa_categ, ALERT_CATEGSTR);

   else
      strcpy(AlertAlarmVal_Record.aa_categ, "undef");

   /* insert the record into the AlertAlarmVal table
      and check its return status.  */

   returnValue = InsertIfUniqueAlertAlarmVal(&AlertAlarmVal_Record, &isUnique);

   if (returnValue < 0)
   {
      status = yearsec_dt_to_ansi(obsPtr->obstime, ansi_time);

      if ( !isUnique)
      {
         /*
	    fprintf(logfilePtr,
	    "\n%sDuplicate insert to AlertAlarmVal ignored for "
	    "%s,%s,%s,%d,%s,%s",
	    indentation, obsPtr->lid, obsPtr->pe, ansi_time,
	    obsPtr->dur, obsPtr->ts, obsPtr->extremum);
           */

	 ;  /* Null statement (Do nothing) */

      }

      else
      {
	 fprintf(logfilePtr,
		 "\nPostgreSQL error %d inserting to AlertAlarmVal "
		 "%s,%s,%s,%d,%s,%s", returnValue,
		 obsPtr->lid, obsPtr->pe, ansi_time,
		 obsPtr->dur, obsPtr->ts, obsPtr->extremum);
      }

   }

   return;

}
