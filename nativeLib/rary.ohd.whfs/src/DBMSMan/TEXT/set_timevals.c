/*********************************************************************
set_timevals.c

Sets time windows for use in loading cur obs and max forecast data.

*********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "set_timevals.h"

/*********************************************************************
   set_timevals()
   
   PURPOSE
   Set the beginning of the observed window and the end of the
   forecast window.
      
   ********************************************************************/

void set_timevals(const time_t	current_time,
		  	time_t	*obs_btime,
		  	time_t	*fcst_etime,
			time_t	*basis_btime)
{ 
   time_t obshrs, fcsthrs, basishrs;
   
   
   get_hrvals(&obshrs, &fcsthrs, &basishrs);
   
   *obs_btime   = current_time - (3600 * (long )obshrs); 
   *fcst_etime  = current_time + (3600 * (long )fcsthrs);
   *basis_btime = current_time - (3600 * (long )basishrs);

   return;
}


/*********************************************************************
   get_hrvals()
   
   Get the time window hourly offsets for general use 
   throughout the program.
      
   ********************************************************************/

void get_hrvals(time_t	*obshrs,
		time_t	*fcsthrs,
		time_t  *basishrs)
{   
   static int		first = 1;
   static time_t 	db_obshrs, db_fcsthrs, token_basishrs;
   
   RpfParams 		*rpfPtr;
   int             	tlen=0, rlen=0, istatus=0;
   char            	basis_hours_str[128];
   int			hr_value;
   
   
   /* retrieve the look-back, look-forward intervals   */
   
   if (first)
   {       
      /* initialize */
      
      db_obshrs = db_fcsthrs = token_basishrs = 72;
      
      
      /* get the database values */
      
      rpfPtr = GetRpfParams(" ");
      
      if (rpfPtr == NULL)
      {
	 fprintf(stderr, "No records in RpfParams table, using defaults\n");
      }
      
      else
      {
	 db_obshrs  = rpfPtr->obshrs;
	 db_fcsthrs = rpfPtr->fcsthrs;
	 
	 FreeRpfParams(rpfPtr);
      }
      
      
      /* get the token value */
      
      tlen = strlen("basis_hours_filter");
      istatus = get_apps_defaults("basis_hours_filter", &tlen, 
                                   basis_hours_str, &rlen);
      if (rlen > 0)
      {
	 hr_value = atoi(basis_hours_str);
	 if (hr_value <= 0 || hr_value > 480)
	    fprintf(stderr, "invalid value for basis_hours_filter token: %s\n", 
		    basis_hours_str);
	 else
	    token_basishrs = hr_value;
      }
      
      
      first = 0;
   }
   
   *obshrs   = db_obshrs;
   *fcsthrs  = db_fcsthrs;
   *basishrs = token_basishrs;
   
   
   return;
}
