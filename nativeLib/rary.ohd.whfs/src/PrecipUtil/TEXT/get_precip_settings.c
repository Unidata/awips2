/***********************************************************************/
/*   get_precip_settings.c                                             */
/***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "get_precip_settings.h"

#include "GeneralUtil.h"  /* for get_apps_defaults */

/***********************************************************************
  get_precip_index()
  
  Returns an index to the table that contains the 
  specified precip physical element.
  
***********************************************************************/

int get_precip_index(char   *pe)
{
  int index;
  
  
  if (strcmp(pe, "PC") == 0)
    index = RAWPC;
  
  else if (strcmp(pe, "PP") == 0)
    index = RAWPP;
  
  else if (strcmp(pe, "PM") == 0 ||
           strcmp(pe, "PN") == 0 ||
           strcmp(pe, "PR") == 0 ||
           strcmp(pe, "PT") == 0)
    index = RAWPOTHER;
  
  else 
    index = NOT_PRECIP;
  
  
  return(index);
}

  
/***********************************************************************
  get_precip_window()
  
  Get the token values that specify the time window
  around the top of the hour.
  The tokens it reads are: intpc, intlppp, intuppp 
 
***********************************************************************/
void get_precip_window(int *pc_window,
                       int *pp_beforewindow,
		       int *pp_afterwindow)
{
   int                  token_len, string_len;
   char                 token_string[60];
   

  /* read the tokens intpc, intlppp, intuppp to get the values */
  
  *pc_window       = DEFAULT_PC_WINDOW;
  *pp_beforewindow = DEFAULT_PPBEFORE_WINDOW;
  *pp_afterwindow  = DEFAULT_PPAFTER_WINDOW;


  /* get intpc */
  
  token_len = strlen("intpc");
  get_apps_defaults("intpc", &token_len, token_string, 
                       &string_len);
  if (string_len > 0)
       *pc_window = atoi(token_string);
 
 
  /* get intlppp */
  
  token_len = strlen("intlppp");
  get_apps_defaults("intlppp", &token_len, token_string, &string_len);
  if (string_len > 0)
       *pp_beforewindow = atoi(token_string);
       
       
  /* get intuppp */
  
  token_len = strlen("intuppp");
  get_apps_defaults("intuppp", &token_len, token_string, &string_len);
  if (string_len > 0)
       *pp_afterwindow = atoi(token_string);
		     
  return;
}

/***********************************************************************
  get_6hour_precip_window()
  
  Get the token values that specify the time window
  around the top of the hour.
  The tokens it reads are: intpc, intlppp, intuppp 
 
***********************************************************************/
void get_6hour_precip_window ( float * ppq_window )
{
   int                  token_len, string_len;
   char                 token_string[60];

  /* Set the PPQ window duration to the default value. */
  * ppq_window = DEFAULT_PPQ_WINDOW;
  
  /* Retrieve the value of the intppq token if it exists. */  
  token_len = strlen ( "intppq" );
  get_apps_defaults ( "intppq", &token_len, token_string, 
                       &string_len);
  if (string_len > 0)
  {
       *ppq_window = ( float ) atof ( token_string );
  }
		     
  return;
}


/***********************************************************************
  check_precip_window()
  
  Checks if the value for the specified time and physical element
  are within the top-of-the-hour window.
  
***********************************************************************/
int check_precip_window(char *pe,
                        time_t validtime)
{
   static int first = 1;   
   static int	pc_window;
   static int	pp_beforewindow;
   static int	pp_afterwindow;   
   int 		within_window;
   struct tm    *tm_struct;
   int		minutes;
    
   
   /* initialize */
   
   within_window = 0;
   
   
   /* get the limits the first time */
   
   if (first)
   {
      get_precip_window(&pc_window, &pp_beforewindow, &pp_afterwindow);
      first = 0;
   }
   
   
   /* extract the minutes portion from the time value */
   
   tm_struct = gmtime(&validtime);
   minutes = tm_struct->tm_min;
   
   
   /* if the physical element is not PC type, use the PP settings,
      even if it is not for PP */
      
   if (strcmp(pe, "PC") == 0)
   {
     if (minutes >= (60 - pc_window) || minutes <= pc_window)
	within_window = 1;     
   }
   else 
   {
     if (minutes >= (60 - pp_beforewindow) || minutes <= pp_afterwindow)
	within_window = 1;     
   }
   
   
   return(within_window);
}


/***********************************************************************
  check_sum_pc_reports()

  Checks if the sum_pc_reports token is set to "YES" or "NO".  YES means
  to compute PC-based precipitation totals by adding the PC reports within 
  the accumulation interval.  NO means to subtract the value of the PC report
  closest to the starting time from the value of the PC report closest to the
  ending time.

  This routine will return a value of 1 if the sum_pc_reports token is "YES".
  This routine will return a value of 0 if the sum_pc_reports token is "NO".

***********************************************************************/
int check_sum_pc_reports( )
{
   char reply [ 50 ];
   const static char * sum_pc_reports_token = "sum_pc_reports";
   static int first = 1;
   int reply_len;
   int request_len;
   int status;
   static int sum_pc_reports = DEFAULT_SUM_PC_REPORTS_VALUE ; 

   /* Check the sum_pc_reports token.
    * This will determine if PC-based precipitation totals should be
    * arrived at by a) adding all PC reports over the user-specified interval
    * or b) by subtracting the value of the PC report closest to the 
    * starting time of the accumulation interval from the value of the PC
    * report closest to the ending time of the accumulation interval. */
   if ( first == 1 )
   {
      first = 0 ;

      request_len = strlen ( sum_pc_reports_token );
      status = get_apps_defaults ( ( char * ) sum_pc_reports_token,
                                   & request_len,
                                   ( char * ) reply,
                                   & reply_len ) ;

      if ( ( status == 0 ) && ( reply_len > 0 ) )
      {
         if ( ( reply [ 0 ] == 'Y' ) || ( reply [ 0 ] == 'y' ) )
         {
            sum_pc_reports = 1;   
         }
      }
   }

   return sum_pc_reports;
}

