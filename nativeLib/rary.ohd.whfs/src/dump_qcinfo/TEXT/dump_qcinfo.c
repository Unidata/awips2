#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <sqlca.h>

#include "dump_qcinfo.h"

#include "DbmsAccess.h"
#include "GeneralUtil.h"

/* Use these for Informix error checking; the SQLCODE
   is returned in most cases as a function argument; the
   structure is useful for identifying the ISAM errors */

#include <sqlca.h>



/**************************************************************************

   PURPOSE
   Utility program to dump out detailed info on the qc byte.  Two modes
   are supported - one involves providing info for a specific data value
   in the database, the other involves listing of the details for a
   specific decimal encoded value in bit form.


   ***********************************************************************/

int dump_qcinfo_main(int argc,const char ** argv)

{
   FILE        *logfilePtr;
   char         dbname[25];
   time_t       start_timet;
   double	alert_limit;
   int          dbstatus;
   char		logfilePath[200];
   time_t	tnow;
   struct tm   *tm_struct;
   char         timestr[25];
   options      Options;

   int          len=0, rlen=0, istatus=0;
   char         rdir_path[128];


   /* initialization of memory blocks */

   memset(Options.lid,     0, sizeof(Options.lid));
   memset(Options.pe,      0, sizeof(Options.pe));
   memset(Options.tabname, 0, sizeof(Options.tabname));
   Options.show_errs_only = 0;


   /* initialization of variables */

   logfilePtr     = NULL;
   alert_limit	  = MISSING_VAL;


   /* get the current time for later use, including using it
      to name the log file. Use this silly string to avoid problems with SCCS
      messing up the %characters when doing a get */

   time(&tnow);
   tm_struct = gmtime(&tnow);


   /* The get_check_options() function handles the following:
      - Insures proper number of command line arguments
      - Retrieves the command line arguments
      - Opens the Informix database  */

   get_dump_options(argc, argv, dbname, &Options);


   /* open the  log file for dynamic input of
      diagnostic messages. */

   len = strlen("whfs_util_log_dir");
   istatus = get_apps_defaults("whfs_util_log_dir", &len, rdir_path, &rlen);

   if (istatus != 0)
   {
      fprintf(stderr, "whfs_util_log_dir is undefined.  Using current directory.\n");
      sprintf(rdir_path, ".");
   }

   sprintf(logfilePath, "%s/dump_qcinfo.log", rdir_path);
   fprintf(stdout, "Info written to file: %s\n", logfilePath);

   logfilePtr = fopen(logfilePath, "w");

   if (logfilePtr == NULL)
   {
      fprintf(stderr, "Error opening logfile: %s.\n", logfilePath);
      exit(-1);
   }


   fprintf(logfilePtr, "dump_qcinfo  ** Feb 08, 2006 **\n");


   /* if using the test qcode option, do that and get out */

   if (Options.test_qcode != 0)
   {
     fprintf(logfilePtr, "Detailed info for qc value = %d\n",
             Options.test_qcode);
     dump_qc_bit_descr(Options.test_qcode, logfilePtr);
     list_bitdescr(logfilePtr);
   }


   /* if dumping out actual data, first log
      the command line information to the logfile */

   else
   {
      strftime(timestr, 25, "%a %b %d %H:%M:%S %Y", tm_struct);
      fprintf(logfilePtr, "\nCurrent Time     = %s\n", timestr);
      fprintf(logfilePtr, "\nDatabase Name    = %s", dbname);
      fprintf(logfilePtr, "\nPE Table Name    = %s", Options.tabname);
      fprintf(logfilePtr, "\nQuery End Time   = %s",
                           asctime(gmtime(&Options.end_timet)));
      start_timet = Options.end_timet - Options.numhrs * 3600;
      fprintf(logfilePtr, "Query Start Time = %s", asctime(gmtime(&start_timet)));
      fprintf(logfilePtr, "Number of Hours  = %d", Options.numhrs);


      if (strlen(Options.lid) != 0)
         fprintf(logfilePtr, "\nLocation ID      = %s", Options.lid);

      if (strlen(Options.pe) != 0)
         fprintf(logfilePtr, "\nPhysical Element = %s", Options.pe);


      /* process the request */

      do_qcinfo_dump(Options, &tnow, logfilePtr);


      /* close the Informix database.  Upon successful execution,
         normally return from main() routine. */

      dbstatus = CloseDbms();
   }


   /* closure of the data review logfile and error handling */

   if (fclose(logfilePtr) != 0)
   {
      perror("Error encountered upon closure of the logfile.");
      exit(-1);
   }

   return(0);
}

/*********************************************************************
   get_dump_options()

   PURPOSE
   Get the options specified for the application.
   This function opens the Informix database.

   *******************************************************************/

void get_dump_options(int		argc,
		       char		**argv,
                       char             *dbname,
		       options          *Options)

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
   int		 db_given = 0, lid_given = 0, pe_given = 0;
   int           time_given = 0, numhrs_given = 0;
   int           table_given = 0, test_qcode_given = 0;

   struct tm     utm;
   time_t 	 utimet;
   extern long   timezone;

   /* check the number of command line arguments. */

   if (argc != 2 && argc < 5)
   {
      printf("Usage: dump_qcinfo -d<dbase> -t<table> "
	     "-l<lid> -p<pe> [-n<hrs>] [-h<endhr>]\n");
      printf("Usage: dump_qcinfo -v########\n");
      exit(-1);
   }


   Options->test_qcode = 0;

   /*process and load the number of command line arguments */

   while ((c = getopt(argc, argv, "d:t:p:l:n:h:v:")) != -1)

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
	       printf("Invalid length for table name %s\n", optarg);
	       exit(-1);
	    }

	    else
	    {
	       strcpy(Options->tabname, optarg);
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
	       strcpy(Options->lid, optarg);
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
	       strcpy(Options->pe, optarg);
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
	       Options->numhrs = atoi(optarg);

	       if (Options->numhrs <= 0 || Options->numhrs > 960)
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

	       Options->end_timet = utimet;
	       time_given = 1;
	    }

	    break;

         case 'v':
            Options->test_qcode = atoi(optarg);
	    test_qcode_given = 1;
            break;


         default:
	    break;

      }  /* Closing brace of switch(c) structure */

   }     /* Closing brace of while((c = getopt()) loop */


   /* ensure the mandatory command line arguments were given */


   if (test_qcode_given)
      ;

   else if ( (!db_given) || (!table_given) || (!lid_given) || (!pe_given))
   {
      printf("Usage: dump_qcinfo -d<dbase> -t<table> "
	     "-l<lid> -p<pe> [-n<hrs>] [-h<endhr>] [-v]\n");
      printf("Usage: dump_qcinfo -v########\n");
      exit(-1);
   }

   /* assign any defaults */

   if (!numhrs_given)
      Options->numhrs = DEFAULT_HOURS;

   if (!time_given)
   {
      time(&utimet);
      Options->end_timet = utimet;
   }


   return;
}


/***************************************************************************
   The do_qcinfo_dump() function.
   *************************************************************************/

void do_qcinfo_dump(options    Options,
		    time_t     *tnow,
                    FILE       *logfilePtr)

{
   Observation	    *obsHead = NULL, *obsPtr = NULL;
   int		    cnt;
   time_t	    curtime1;
   time_t           curtime2;
   char		    datatime[ANSI_TIME_LEN + 1];

   /* retrieve data from PE table and track time */

   time(&curtime1);
   obsHead = get_dump_data(Options,logfilePtr);


   /* if data is found, then process. */

   if (obsHead == NULL)
      cnt = 0;
   else
   {
      cnt = ListCount(&obsHead->list);
      obsPtr = (Observation *) ListFirst(&obsHead->list);
   }

   fprintf(logfilePtr, "\nRetrieved %d observations for %s %s.\n",
           cnt, Options.lid, Options.pe);

   while(obsPtr)
   {

      yearsec_dt_to_ansi(obsPtr->obstime, datatime);

      /* dump the info */

      fprintf(logfilePtr, "%s  %s  %.2lf  %ld\n",
              datatime, obsPtr->ts, obsPtr->value, obsPtr->quality_code);

      dump_qc_bit_descr(obsPtr->quality_code, logfilePtr);

      obsPtr = (Observation *) ListNext(&obsPtr->node);
   }

   if (obsHead != NULL) FreeObservation(obsHead);

   /* procure end time from system calendar clock */

   time(&curtime2);

   fprintf(logfilePtr, "\n\nTotal processing time = %ld second(s)\n",
	   (curtime2 - curtime1));


   return;
}


/**********************************************************************
   The get_review_data() function.
   *******************************************************************/

Observation *get_dump_data(options    Options,
                           FILE       *logfilePtr)

{

   Observation	*obsHead= NULL;
   int	         status;
   char		 where[400];
   char          pe_str[50];
   char	 	 begin_time[ANSI_TIME_LEN + 1];
   char          end_time[ANSI_TIME_LEN + 1];
   int	         cnt;

   /* define the time window */

   status = timet_to_yearsec_ansi(Options.end_timet, end_time);
   status = timet_to_yearsec_ansi(Options.end_timet - (Options.numhrs * 3600), begin_time);


   /* first, form the where clause to get the data */

   /* construct the where subclause that regards the physical element (pe) */

   if (strlen(Options.pe) > 0)
      sprintf(pe_str, " AND pe='%s' ", Options.pe);

   else
      sprintf(pe_str, " ");


   if (strlen(Options.lid) > 0)
   {
      sprintf(where, "WHERE lid='%s' AND obstime >= '%s' AND "
	      " obstime <= '%s' %s AND value != %f ORDER BY "
	      " lid, pe, ts, obstime ASC ",
	      Options.lid, begin_time,
	      end_time, pe_str, MISSING_VAL);
   }

   else
   {
      sprintf(where, "WHERE obstime >= '%s' AND "
	      " obstime <= '%s' %s AND value != %f "
	      " ORDER BY lid, pe, ts, obstime ASC ",
	      begin_time, end_time, pe_str, MISSING_VAL);
   }


   /* now get the data */

   fprintf(logfilePtr, "\nselect * from %s %s\n", Options.tabname, where);

   obsHead = GetObservation(where, Options.tabname);

   if (obsHead == NULL)
      cnt = 0;
   else
      cnt = ListCount(&obsHead->list);

   if (cnt == 0)
   {
      fprintf(logfilePtr, "\n\nSpecified data between %s and %s not found.\n",
	      begin_time, end_time);
   }
   else
     fprintf(logfilePtr, "\nRetrieved %d records", cnt);


   return(obsHead);
}


/**********************************************************************
   Dumps data as per bit settings.
   *******************************************************************/

void dump_qc_bit_descr(long qcode,
                       FILE *logfilePtr)
{
   int certainty_bit, notquest_bit, testrun_bit, extern_bit,
       manual_bit, grossrange_bit, extern_notquest_bit, reason_bit,
       roc_bit, outlier_bit, scc_bit, msc_bit;


/* ignore bits 31, 24-27, 0-14 since they are unused;
   get values of 12 bits actually used */

   certainty_bit   = check_qcbit(CERTAINTY_QC,  qcode);
   notquest_bit    = check_qcbit(NOTQUEST_QC,   qcode);
   testrun_bit     = check_qcbit(TEST_RUN_QC,   qcode);
   extern_bit      = check_qcbit(EXTERN_QC,     qcode);
   manual_bit      = check_qcbit(MANUAL_QC,     qcode);
   grossrange_bit  = check_qcbit(GROSSRANGE_QC, qcode);
   extern_notquest_bit = check_qcbit(EXTERN_NOTQUEST_QC, qcode);
   reason_bit      = check_qcbit(REASONRANGE_QC,qcode);
   roc_bit         = check_qcbit(ROC_QC,     qcode);
   outlier_bit     = check_qcbit(OUTLIER_QC, qcode);
   scc_bit         = check_qcbit(SCC_QC,     qcode);
   msc_bit         = check_qcbit(MSC_QC,     qcode);


   /* dump summary bits */

   if (certainty_bit && notquest_bit)
     fprintf(logfilePtr, "  Good: ");

   else if (certainty_bit && !notquest_bit)
     fprintf(logfilePtr, "  Questionable: ");

   else if (!certainty_bit)
     fprintf(logfilePtr, "  Bad: ");


   /* ignore tests run bit since not used */


   /* dump the external indicator bits */

   if (extern_bit & extern_notquest_bit)
     fprintf(logfilePtr, "extern G; ");

   else if (extern_bit & !extern_notquest_bit)
     fprintf(logfilePtr, "extern Q; ");

   else
     fprintf(logfilePtr, "extern B; ");


   /* dump the manual indicator*/

   if (manual_bit)
     fprintf(logfilePtr, "man G; ");
   else
     fprintf(logfilePtr, "man Q or Q; ");


   /* dump the gross range bit */

   if (grossrange_bit)
     fprintf(logfilePtr, "gross P; ");
   else
     fprintf(logfilePtr, "gross F; ");


   /* dump the various q bits;
      outlier, scc, msc not currently used in obs tables */

   if (reason_bit)
     fprintf(logfilePtr, "reason P; ");
   else
     fprintf(logfilePtr, "reason F; ");

   if (roc_bit)
     fprintf(logfilePtr, "roc P; ");
   else
     fprintf(logfilePtr, "roc F; ");


   /* dump the zeroes and ones display */

   dump_qcbits(qcode, logfilePtr);


   return;
}

/**********************************************************************
   Dumps data as per bit settings.
   *******************************************************************/

void dump_qcbits(long quality_code,
		 FILE *logfilePtr)
{

   int bit_setting;
   int i;


   fprintf(logfilePtr, "\n ");

   for (i = 31; i >= 0; i--)
   {
       bit_setting = check_qcbit(i, quality_code);

       if (((i + 1) % 5) == 0) fprintf(logfilePtr, " ");
       fprintf(logfilePtr, "%d", bit_setting);
   }

   fprintf(logfilePtr, "\n");


   return;
}


/**********************************************************************
   Dumps data as per bit settings.
   *******************************************************************/

void list_bitdescr(FILE *logfilePtr)
{
   int i;

   /* show headings to help interpret bit 1/0 display */

   fprintf(logfilePtr, " ");

   for (i = 31; i >= 0; i--)
   {
       if (((i + 1) % 5) == 0) fprintf(logfilePtr, " ");
       fprintf(logfilePtr, "%d", (i % 10));
   }

   fprintf(logfilePtr, "\n");

   /* show descriptions */

fprintf(logfilePtr,
"bit\n"
"===\n"
"31: unused\n"
"30: certainty not-bad indicator, summary bit\n"
"29: not questionable indicator, summary bit\n"
"28: tests run\n"
"27-24: not used\n"
"23: external not-bad\n"
"22: manual not-bad\n"
"21: gross range\n"
"20: external not-questionable\n"
"19: reasonable range\n"
"18: rate-of-change\n"
"17: outlier\n"
"16: spatial consistency check\n"
"15: multi-sensor check\n"
"0-14 not used\n");

return;
}


