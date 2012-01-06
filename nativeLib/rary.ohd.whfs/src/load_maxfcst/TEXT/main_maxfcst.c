/*******************************************************************
   main_maxfcst.c

   Modification History
   Date         Name              Description
   12/03/01     Russell Erb       Rewrote some logic and added the -l, -p, -t
   				  options so lid, pe and ts could be entered.
********************************************************************/

#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "DbmsDefs.h"
#include "load_maxfcst.h"
#include "RpfParams.h"
#include "DbmsAccess.h"

/* local prototypes */

void get_startup_options(const  int	argc,
                                char	**argv,
				char	*lid,
				char	*pe_filter,
				char	*ts_filter);

/*********************************************************************
   main()

   PURPOSE
   Main module for the load maximum forecast program.

   NOTES
   If lid, pe and ts are not given as command line arguments then this will
   Loop on ALL the forecast data available and process each location
   and each pe,ts that has forecast height or discharge data.
   Then load the RiverStatus table with the maximum info.
********************************************************************/

int load_maxfcst_main(int argc, const char ** argv)
{

   char lid[LOC_ID_LEN + 1];
   char pe_filter[SHEF_PE_LEN + 1];
   char ts_filter[SHEF_TS_LEN + 1];

   /* get the command line options specified with the program */

   get_startup_options(argc, argv, lid, pe_filter, ts_filter);


   /*
	process and load the max fcst data.
	if lid, pe, or ts is missing then call the load_maxfcst() function
	to do all. If all three are specified then call load_maxfcst_item().
   */

   if ( (strlen(lid) == 0 || (strlen(ts_filter) == 0) || strlen(pe_filter) == 0) )
	load_maxfcst();
   else
	load_maxfcst_item(lid, pe_filter, ts_filter);

   return(0);
}


/*********************************************************************
	This function will read in the command line arguments, verify
	them and attempt to open the database specified
********************************************************************/

void get_startup_options(const  int             argc,
                                char            **argv,
				char		*lid,
				char		*pe_filter,
				char		*ts_filter)

{
   extern char  *optarg;
   char *dbms;
   int c, status;
   int	dbms_given;

   /* initialize  */

   dbms_given = 0;
   memset(lid, 0, LOC_ID_LEN + 1);
   memset(pe_filter, 0, SHEF_PE_LEN + 1);
   memset(ts_filter, 0, SHEF_TS_LEN + 1);


   /* loop on the specified args */

   while ((c = getopt(argc, argv, "d:l:p:t:")) != -1)
   {
      switch (c)
      {
	case 'd':
            /* open the database */

            dbms = optarg;

            status = OpenDbms(dbms);
            if (status != 0)
	    {
	       fprintf(stderr, "Error opening database: %s\n", dbms);
	       exit (-1);
	    }
	    dbms_given =1;
            break;


	case 'l':
		/* define the optional id for the location being considered */

		strcpy(lid, optarg);
		if (strlen(lid) == 0 || strlen(lid) > LOC_ID_LEN)
		    {
			 fprintf(stderr, "Invalid location_id specified: %s\n",
				 lid);
			 exit(-1);
		    }
		break;


         case 't':
		/* define the optional ts for the lid being considered */
		strcpy(ts_filter, optarg);
		if ( (strlen(ts_filter) != SHEF_TS_LEN) || (ts_filter[0] != 'F') )
		    {
		       fprintf(stderr, "Invalid Forecast Type and Source specified: %s\n",
	       			ts_filter);
		       exit(-1);
		    }
		break;


         case 'p':
		/* define the optional pe for the ts being considered */
		strcpy(pe_filter, optarg);
		if (strlen(pe_filter) != SHEF_PE_LEN)
		    {
		       fprintf(stderr, "Invalid Physical Element specified: %s\n",
	       			pe_filter);
		       exit(-1);
		    }
		break;

         default:
            break;
      }
   }

   /* check that a database name is given */

   if (!dbms_given)
   {
	  fprintf(stderr,
		  "\nusage: %s -d<databasename> [-l<location_id>] [-p<pe>] [-t<ts>] \n",
		  argv[0]);

	  fprintf(stderr, "\n-d<databasename> is required."
		          " Selects database name.\n");

	  fprintf(stderr, "-l<location_id> specifies a single station to process -"
		          " the default is to process ALL stations\n");

	  fprintf(stderr, "-p<pe> specifies a single Physical Element for a station\n");

	  fprintf(stderr, "-t<ts> specifies the Type and Source for the"
	  		  " Physical Element specified.\n");

      exit(-1);
   }

   return;
}
