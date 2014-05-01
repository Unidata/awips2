#include <sqlca.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <sqlca.h>
#include <unistd.h>

#include "floodseq.h"
#include "GeneralUtil.h"

/* ***************************************************************

   floodseq

   This is run via a cron-job.  It scans the location and
   height tables to get information required to update the
   FloodTS table.

   ****************************************************************/

int floodseq_main(int argc,const char ** argv)
{
   extern char 	*optarg;
   extern int   optind, optopt;

   char   	cmd[300];
   char		msg[160];
   int    	c;
   char  	*dbms;
   int    	db_given = 0, lid_given = 0;
   int		debug_mode = 0;
   int 		return_code, transaction_log;
   time_t	start_time;
   char		lid[LOC_ID_LEN + 1];

   int          len=0, rlen=0, istatus=0;
   char         rdir_path[128];



   /* check input arguments */

   if (argc < 2 || argc > 4)
      display_usage_and_abort(argv[0]);

   /*  get the proper pathname for whfs_bin_dir */

   len = strlen("whfs_bin_dir");
   istatus = get_apps_defaults("whfs_bin_dir", &len, rdir_path, &rlen);


   /*  process command line argument[s] */

   while ((c = getopt(argc, argv, "d:l:m:")) != -1)
   {
      switch (c)
      {
	 case 'd':
	    dbms = optarg;
	    if (OpenDbms(dbms) != Ok)
	    {
	       printf("unable to open dbms.\n");
	       exit(-2);
	    }

	    db_given = 1;
	    break;

	 case 'l':
	    strcpy(lid, optarg);
	    lid_given = 1;
	    break;

	 case 'm':
	    debug_mode = 1;
	    break;

	 default:
	    display_usage_and_abort(argv[0]);
	    break;
      }
   }



   /* make sure we can proceed */

   if (!db_given)
      display_usage_and_abort(argv[0]);


   /* output version info and start time */

   printf("FloodSeq OB8.1; March 26, 2007\n");

   time(&start_time);
   strftime(msg, 490, "%Y-%m-%d %H:%M:%S", gmtime(&start_time));
   printf("Begin run at: %s Z\n", msg);


   /* lock floodts.  if error, then issue interactive error message.  */

   return_code = executeImmediate("BEGIN WORK");
   if (return_code == 0)
      transaction_log = 1;
   else
   {
      printf("Begin work request not accepted %d\n", return_code);
      transaction_log = 0;
   }

   return_code = executeImmediate("LOCK TABLE floodts IN EXCLUSIVE MODE");
   if (return_code != 0)
   {
      printf("Lock Table failed %d\n", return_code);

      sprintf(cmd, "%s/x_notify -n -tWHFS \"Table FloodTS lock error.\" "
	      "\"Please try later. PostgreSQL return code: %d\"",
	      rdir_path, return_code);
      system(cmd);

      end_msg(start_time);
      exit(1);
   }
   else
   {
      return_code = executeImmediate("COMMIT WORK");
      if (return_code != 0)
	 printf("Commit work failed %d\n", return_code);
   }




   /* initiate the main procesing */

   if (lid_given == 0)
   	lid[0] = '\0';
   do_floodseq(lid, debug_mode);


   /* commit the work or unlock the table, depending on
      whether transaction logging is enabled. */

   if (transaction_log)
   {
   }
   else
   {
      return_code = executeImmediate("UNLOCK TABLE floodts");
      if (return_code != 0)
	 printf("Unlock Table failed %d\n", return_code);
   }


   /* output end time and close log */

   end_msg(start_time);

   exit(0);

}


/****************************************************************

   do_floodseq

   **************************************************************/
void do_floodseq(char	*lid,
		 int	debug_mode)
{
   Location* 	locationHead;
   Location* 	locationPtr;
   int		location_cnt;
   char		where[260];


   int          token_name_len=0;
   int          hsa_list_len=0;
   int		ad_status=0;
   char         hsa_list[1024];

   /* retrieve the token to get the list of HSAs for which to save flood reports */

   token_name_len = strlen("floodseq_hsa_list");
   ad_status = get_apps_defaults("floodseq_hsa_list",
   				 &token_name_len,
				 hsa_list,
				 &hsa_list_len);



   /* define where clause depending on whether processing one location */

   if (strlen(lid) > 0)  /* one lid only */
   {
      sprintf(where, " WHERE lid = '%s' ", lid);
      printf("standard single lid WHERE clause = :%s:\n",where);
   }
   else if (  (hsa_list_len > 0) )  /* custom tokenized hsa list */
   {

      if ( hsa_list_syntax_is_good(hsa_list, hsa_list_len) )
      {

         sprintf(where, " WHERE lid IN  "
                          " (SELECT DISTINCT(lid) from Location "
		                "WHERE hsa in (%s))  AND "
				" lid IN "
				"     (SELECT distinct(lid) FROM ingestfilter "
	     			"           WHERE pe LIKE 'H%%' AND pe != 'HI' AND "
	     			"                 ts LIKE 'R%%' AND ingest='T' )"
				" ORDER BY lid", hsa_list);

         printf("SPECIAL customized WHERE clause = :%s:\n", where);
      }
      else /* error with tokenized hsa list */
      {
         printf("Because of the syntax error, floodseq will run for locations in ALL HSAs\n");


	 /* this where clause needs to be indentical to the normal usage where clause */
	 strcpy(where, " WHERE lid IN "
	     " (SELECT distinct(lid) FROM ingestfilter "
	     "  WHERE pe LIKE 'H%' AND pe != 'HI' AND "
	     " ts LIKE 'R%' AND ingest='T' )"
             " ORDER BY lid ");

         printf("using standard multi-lid WHERE clause = :%s:\n", where);
      }
   }
   else /* normal usage */
   {
       strcpy(where, " WHERE lid IN "
	     " (SELECT distinct(lid) FROM ingestfilter "
	     "  WHERE pe LIKE 'H%' AND pe != 'HI' AND "
	     " ts LIKE 'R%' AND ingest='T' )"
             " ORDER BY lid ");

       printf("standard multi-lid WHERE clause = :%s:\n", where);
   }


   /* get a list of candidate locations */

   locationHead = GetLocation(where);

   if (locationHead != NULL)
   {
      location_cnt = ListCount(&locationHead->list);
      printf("Processing %d stage locations.\n", location_cnt);
   }
   else
   {
      location_cnt = 0;
      if (strlen(lid) > 0)
	 printf("Specified location '%s' not defined.\n", lid);
      else
	 printf("No stage data locations defined.\n");
      return;
   }


   /* log some comments about the subsequent processing */

   printf("Data written to FloodTS for following cases:\n");
   printf("  2a - below fs, preceding rise above flood\n");
   printf("  2b - first value in flood\n");
   printf("  3  - still in flood\n");
   printf("  4  - below fs, following fall below flood\n");



   /* perform the main processing */

   if (location_cnt > 0)
   {
      locationPtr = (Location*) ListFirst(&locationHead->list);
      while (locationPtr != (Location *)NULL)
      {

	 /* now process the data */

	 floodseq_loc(locationPtr->lid, debug_mode);

	 locationPtr = (Location*) ListNext(&locationPtr->node);
      }


      /* free the location memory */

      FreeLocation(locationHead);
   }


   return;
}

/*********************************************************************************/

bool hsa_list_syntax_is_good(const char* hsa_list, int hsa_list_len)
{
    int i = 0;
    char c;


    int before_hsa = 0;
    int inside_hsa = 1;
    int after_hsa = 2;
    int state = before_hsa;

    int alpha_count = 0;

    bool result = false;
    bool foundError = false;
    char message[BUFSIZ];

    memset(message, BUFSIZ, '\0');

    for (i = 0; i < hsa_list_len; i++)
    {
        c = hsa_list[i];

	/*
	printf(" c = %c \n", c);
	*/

	if (state == before_hsa)
	{
	    if (c == '\'')
	    {
	        state = inside_hsa;
	    }
	    else if (isspace(c))
	    {
	        ; /* do nothing */
	    }
	    else
	    {
	        sprintf(message, "Expecting opening single quote at character %d\n", i+1);

		foundError = true;
	        log_syntax_error(message, hsa_list);
		break;

	    }

	}
	else if (state == inside_hsa)
	{
	    if (c == '\'')
	    {
	        if (alpha_count < 3)
		{
		    sprintf(message, "Not enough characters for HSA identifier. \n"
		                     "Expecting alphabetic character at character %d\n", i+1);

		    foundError = true;
		    log_syntax_error(message, hsa_list);
		    break;
		}
	        state = after_hsa;
		alpha_count = 0;

	    }
	    else if (isspace(c))
	    {
	        sprintf(message, "Expecting alphabetic character at %d\n", i+1);

		foundError = true;
		log_syntax_error(message, hsa_list);
		break;

	    }
	    else if (isalpha(c))
	    {
	        alpha_count++;
		if (alpha_count > 3)
		{
		   sprintf(message, "Too many characters for HSA identifier.\n"
		                    "Expecting closing single quote at character %d\n", i+1);
				     result = false;

		   foundError = true;
		   log_syntax_error(message, hsa_list);
		   break;
		}
	    }
	    else
	    {
	        sprintf(message, "Unexpected character %c at %d\n",c, i+1);

  	        foundError = true;
		log_syntax_error(message, hsa_list);
		break;
	    }
	} /* end else if inside_hsa */


	else if (state == after_hsa)
	{


	     if (c == ',')
	     {
	         state = before_hsa;
	     }
	     else if (isspace(c))
	     {
	        ; /* do nothing */


	     }
	     else
	     {
	         sprintf(message, "Expected comma at character %d\n", i+1);

		 foundError = true;
		 log_syntax_error(message, hsa_list);
		 break;
	     }


	}


	else  /* invalid state */
	{

	        sprintf(message, "Invalid state = %d at %d\n", state, i+1);

		foundError = true;
		log_syntax_error(message, hsa_list);
		break;
	}

    }


    /*
        if did not find an error and in the right state
	then the result is true
    */
    if  ( (!foundError ) && (state == after_hsa) )
    {
        result = true;
	printf("The syntax for the floodseq_hsa_list token value :%s: is valid.\n", hsa_list);
    }


    /*
       if the state is wrong for completion and if have not already recorded an error
       then the string has ended too early
    */
    else if ( (state != after_hsa) && ( strlen(message) < 1) )
    {
        sprintf(message, "Unexpected end of string\n");
	log_syntax_error(message, hsa_list);
    }




    return result;
}



/*********************************************************************************/

void log_syntax_error(const char* message, const char *hsa_list)
{

    if (  (message != NULL) &&  (strlen(message) > 0))
    {
        printf("Syntax error: %s\n", message);
    }
    printf("floodseq_hsa_list token value = :%s:\n", hsa_list);
    printf("Hint: string should be of the form :'ABC', 'CDE', 'AAA': "
	   " in which ABC and CDE and AAA are HSA identifiers.\n");

    return;

}
/*********************************************************************************/

/****************************************************************

   get the flood stage

   ************************************************************** */

void get_fldstg(char	*lid,
		int	*status,
		double	*fs,
		char 	*pe)
{

   char		where[140];
   Riverstat* 	riverstatHead = NULL ;
   Riverstat* 	riverstatPtr = NULL ;

   /* initialize */
   *status = 0;

   *fs = MISSING_VALUE;
   strcpy(pe, "");

   /* retrieve the values */

   sprintf(where, " WHERE lid = '%s' ", lid);
   riverstatHead = GetRiverstat(where);

   if ( riverstatHead == NULL )
   {
      printf ( "%-8s - ignoring since river station info undefined.\n" ,
               lid ) ;
      *status = -1;
   }
   else
   {
      riverstatPtr  = (Riverstat*) ListFirst(&riverstatHead->list);

      if (IsNull(FLOAT, (void *)(&riverstatPtr->fs)))
      {
	 printf("%-8s - ignoring since fs = NULL\n", lid);
	 *status = -1;
      }

      else if (riverstatPtr->fs <= 0)
      {
	 printf("%-8s - ignoring since fs <= 0 (%.2f)\n",
		lid, riverstatPtr->fs);
	 *status = -1;
      }

      else if (IsNull(CHAR, (void *)(&riverstatPtr->primary_pe)))
      {
	 printf("%-8s - ignoring since primary_pe not defined\n", lid);
	 *status = -1;
      }

      else if (riverstatPtr->primary_pe[0] != 'H')
      {
	 printf("%-8s - ignoring since primary_pe not stage-based (%s)\n",
		lid, riverstatPtr->primary_pe);
	 *status = -1;
      }

      else
      {
	 printf("%-8s pe=%s fs=%.2f\n",
		lid, riverstatPtr->primary_pe, riverstatPtr->fs);

	 *fs = riverstatPtr->fs;
	 strcpy(pe, riverstatPtr->primary_pe);
      }

      FreeRiverstat(riverstatHead);
   }


   return;
}


/****************************************************************

   display usage message and abort program

   ************************************************************** */

void display_usage_and_abort(char 	*command_name)
{
   printf("usage: %s -d<database> [-l<lid>] [-monitor]\n", command_name);
   exit(-1);
}


/***************************************************************

   Output ending message to log file

   ************************************************************** */

void end_msg(time_t	start_time)
{
   time_t end_time;
   time_t Minutes, Seconds;
   char   msg[200];
   time_t ElapsedTime;


   time(&end_time);

   strftime(msg, 200, "%Y-%m-%d %H:%M:%S", gmtime(&end_time));
   printf("End run at: %s Z\n", msg);

   ElapsedTime = end_time - start_time;
   Minutes     = ElapsedTime / 60;
   Seconds     = ElapsedTime - (Minutes * 60);

   printf("Elapsed time: %ld mins %ld secs\n", Minutes, Seconds);

   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
