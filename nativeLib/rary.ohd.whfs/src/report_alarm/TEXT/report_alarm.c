#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <sqlca.h>
#include <stdio.h>
#include <math.h>

#include "report_alarm.h"
#include "DbmsAccess.h"

 /***********************************************************************

    report_alarm

    Reads the AlertAlarmVal table and generates a file which reports/
    summarizes the threats indicated in the table.

    This program returns an unsigned integer representing the number
    of alert/alarm items actually reported.

  ***********************************************************************/

int report_alarm_main(int argc, const char ** argv)
{
   report_options_struct	report_options;
   int				dbstatus;
   int				num_alarms = 0;


   /* log a message stating the version number */

   printf("ReportAlarm: Version OB8.1, March 26, 2007 \n");


   /* get the user options from the command line */

   get_report_options(argc, argv, &report_options);


   /* perform the operations */

   do_report_alarm(report_options, &num_alarms);


   /* close the Informix database. */

   dbstatus = CloseDbms();


   /* return with the count of the number of alarms actually reported */

   return(num_alarms);
}


/*********************************************************************
   get_report_options()

   PURPOSE
   Get the options specified for the application.
   This function opens the Informix database.

 ********************************************************************/

void get_report_options(int			argc,
			char			**argv,
			report_options_struct	*opt)

{
   extern char 	*optarg;
   extern int	optind;
   extern int   optopt;

   int	c;
   int  dbstatus;
   int  slen;
   int	status;
   int	db_given, productid_given, minutes_given;
   int	num;


   /* initialize flags for mandatory options */

   db_given = productid_given =  0;


   /* initialize non-mandatory options */

   minutes_given = 0;
   strcpy(opt->product_id, "");
   strcpy(opt->file_suffix, "");
   strcpy(opt->PEfilter, "");
   opt->mode         = DEFAULT_MODE;
   opt->minutes      = DEFAULT_MINUTES;
   opt->min_val_diff = DEFAULT_MIN_VAL_DIFF;

   /* check the number of arguments */

   if (argc < 3)
   {
       printf("Usage: report_alarm -d<dbase> -p<product_id> "
	      "[-r<report_mode>] [-m<minutes]  [-s<file_suffix>] "
	      "[-f<include_flags>] [-e<PE>]\n");
       exit(0);
   }

   /* process and load the number of command line arguments */

   while ((c = getopt(argc, argv, "d:p:r:m:s:f:e:")) != -1)

   {
      switch (c)
      {

	 case 'd':
	    /* get the database name */

	    if ((dbstatus = OpenDbms(optarg)) != 0)
	    {
	       printf("Error %d opening database: %s\n", dbstatus, optarg);
	       exit(0);
	    }

	    else
	       db_given = 1;

	    break;


	 case 'p':
	    /* get the product id */

	    status = 1;
	    slen = strlen(optarg);

	    if (slen != 9 && slen != 10)
	       status = -1;

	    if (status < 0)
	    {
	       printf("Invalid length for product_id: %s\n", optarg);
	       exit(0);
	    }

	    else
	    {
	       strcpy(opt->product_id, optarg);
	       productid_given = 1;
	    }

	    break;


	 case 'r':
	    /* get the report mode */

	    status = 1;
	    slen = strlen(optarg);

	    if (slen == 0 || slen > 16)
	        status = -1;

	    else
	    {
	       if (strcmp("ALL", optarg) == 0)
		  opt->mode = REPORT_ALL;

	       else if (strcmp("FRESH", optarg) == 0)
		  opt->mode = REPORT_FRESH;

	       else if (strcmp("RECENT", optarg) == 0)
		  opt->mode = REPORT_RECENT;

	       else if (strcmp("UNREPORTED", optarg) == 0)
		  opt->mode = REPORT_UNREPORTED;

	       else if (strcmp("NEAREST", optarg) == 0)
		  opt->mode = REPORT_NEAREST;

	       else if (strcmp("NEAR_NOW", optarg) == 0)
	          opt->mode = REPORT_NEAR_NOW;

	       else if (strcmp("LATEST_MAXFCST", optarg) == 0)
	          opt->mode = REPORT_LATEST_MAXFCST;

	       else if (strcmp("NEW_OR_INCREASED", optarg) == 0)
	          opt->mode = REPORT_NEW_OR_INCREASED;


	       else
		  status = -1;
	    }

	    if (status < 0)
	    {
	        printf("Report mode %s must be either:\n"
		       "ALL, FRESH, RECENT, UNREPORTED, NEAREST, NEAR_NOW, "
		       "LATEST_MAXFCST or NEW_OR_INCREASED\n", optarg);
	        exit(0);
	    }

	    break;

	 case 'f':
	    /* get the query filter specications */

	    status = 1;
	    slen = strlen(optarg);

	    if (slen == 0 || slen > 8)
	       status = -1;

	    else
	    {
	       for (num = 0; num < slen; num++)
	       {
		  if (optarg[num] != 'O' &&
		      optarg[num] != 'F' &&
		      optarg[num] != 'T' &&
		      optarg[num] != 'M' &&
		      optarg[num] != 'R' &&
		      optarg[num] != 'L' &&
		      optarg[num] != 'U' &&
		      optarg[num] != 'D')
		     status = -1;
		  break;
	       }

	       strcpy(opt->filter, optarg);
	    }

	    if (status < 0)
	    {
	       printf("Filter options %s must be either: O,F,T,M,R,L,U,D "
		      "for Observed, Forecast, alertT, alarM, Roc, Lower, Upper, Diff\n",
		      optarg);
	       exit(0);
	    }

	    break;

	 case 'e':
	    /* get the PE filter specifications */
	    status = 1;
	    slen = strlen(optarg);

	    if (slen != 2)
	    {
	       status = -1;
	    }
	    else
	    {
	       strcpy(opt->PEfilter,optarg);
	    }

	    if (status < 0)
	    {
	       printf("PE filter option '%s' must be two characters\n", optarg);
	       exit(0);
	     }

	     break;

	 case 'm':
	    /* get the time window in minutes */

	    status = 1;
	    slen = strlen(optarg);

	    if (slen == 0 || slen > 6)
	    {
	       status = -1;
	    }

	    else
	    {
	       num = atoi(optarg);
	       if (num <= 0 || num > 999999)
	       {
		  status = -1;
	       }
	       else
	       {
		  opt->minutes = num;
		  minutes_given = 1;
	       }
	    }

	    if (status < 0)
	    {
	       printf("Invalid number of minutes: %s; must be between 1-999999\n", optarg);
	       exit(0);
	    }

	    break;


	 case 's':
	    /* get the output report and log file suffix */

	    strcpy(opt->file_suffix, optarg);

	    break;

         default:

	    break;

      }
   }


   /* ensure the mandatory command line arguments were given*/

   if (!db_given || !productid_given)
   {
      printf("Usage: report_alarm -d<dbase> -p<product_id> "
	     "[-r<report_mode>] [-m<minutes] [-s<file_suffix>] [-f<include_flags>]"
	     "[-e<PE>\n");
      exit(0);
   }

   if (minutes_given &&
       (opt->mode == REPORT_ALL     || opt->mode == REPORT_UNREPORTED ||
	opt->mode == REPORT_NEAREST || opt->mode == REPORT_LATEST_MAXFCST))
   {
      printf("Usage: -m<minutes> value ignored for this -r<report_mode>\n");
   }


   return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}


