#include <stdio.h>
#include <string.h>
#include <stdlib.h>               
#include <time.h>


#include "get_precip_settings.h"    /* for GPP_WORKFILE */
#include "GeneralUtil.h"            /* for get_apps_defaults() */
#include "Observation.h"
#include "gpp_input.h"

FILE	*gpp_workfile_ptr = NULL;

char	filepath[150];
char 	workfilename[200];


/*******************************************************************
  gpp_hourly_precip() 
  Write a single precip report to a file, using the PostgreSQL
  unload format (i.e. pipe-separated columns).  This function 
  opens the file as needed.
      
  ******************************************************************/

void gpp_hourly_precip ( Observation *obsRow )
{
   static int	first = 1;
   int          token_len, string_len;
   char		obstime[ANSI_TIME_LEN];
   char		prodtime[ANSI_TIME_LEN], posttime[ANSI_TIME_LEN];
   time_t   currentTime = 0;
   
   
   /* get the path for the file, the first time this function is called. */
   
   if (first)
   {
      token_len = strlen("gage_pp_data");   
      get_apps_defaults("gage_pp_data", &token_len, filepath, &string_len);
            
      sprintf(workfilename, "%s/%s.%s", 
              filepath, "TIMESERIES_HOURLY", GPP_WORKFILE_SUFFIX);
      
      printf("  Opening GagePP input file for first time:");
      printf("%s", workfilename);
      
      first = 0;
   }
		       

   /* if this is the first record for this product, then 
      open the file. */
      
   gpp_workfile_ptr = fopen(workfilename, "w");
      
   if (gpp_workfile_ptr == NULL)
   {
      printf("Error opening GagePP input file:");
	   printf("%s", workfilename);
   }
   
   
   /* write data to the file in the format of a unload file */
   
   if (gpp_workfile_ptr != NULL)
   {   
      yearsec_dt_to_ansi(obsRow->obstime,     obstime);
      yearsec_dt_to_ansi(obsRow->producttime, prodtime);
      yearsec_dt_to_ansi(obsRow->postingtime, posttime);
      
      fprintf(gpp_workfile_ptr, 
              "%s|%s|%d|%s|%s|%s|%.3lf|%s|%ld|%d|%s|%s|%s\n", 
              obsRow->lid, obsRow->pe, obsRow->dur, 
	           obsRow->ts, obsRow->extremum, 
              obstime, obsRow->value,
              obsRow->shef_qual_code, obsRow->quality_code, obsRow->revision,
              obsRow->product_id, prodtime, posttime);
      
	   /* get current time */
	   time(&currentTime);
      rename_gpp_workfile ( obsRow->product_id, currentTime );
   }
      
   return;
}


/*******************************************************************
  rename_gpp_workfile()
  
  Close and rename the precip work file containing top-of-the-hour
  reports for use by the GagePP server. 
      
   ******************************************************************/

void rename_gpp_workfile ( char *product_id, time_t current_timet )
{
#define DTSTR_LEN 13

   char dtstr[DTSTR_LEN];
   struct tm *tm_ptr;   
   char	newfilename[200];
   int	status;
   
      
   /* check that the file is open */
   
   if (gpp_workfile_ptr == NULL)
   {
      printf("Error - GagePP file not open.  No data written.");
      
      return;
   }
   
   
   /* close the file */
   
   fclose(gpp_workfile_ptr);   
   
        
   /* convert the current time string */

   tm_ptr = gmtime(&current_timet);
   memset(dtstr, 0, DTSTR_LEN);
   sprintf(dtstr, "%2.2d%2.2d.%2.2d%2.2d%2.2d",
           tm_ptr->tm_mon + 1, tm_ptr->tm_mday, 
           tm_ptr->tm_hour,    tm_ptr->tm_min, tm_ptr->tm_sec);

   
   
   /* build the new filename and rename the file to it */
            
   sprintf(newfilename, "%s/%s.%s", filepath, product_id, dtstr); 
   
   status = rename(workfilename, newfilename);   
   if (status != 0)
   {
      sprintf("Error renaming GagePP file to: %s", newfilename);
   }
   
   return;
}
