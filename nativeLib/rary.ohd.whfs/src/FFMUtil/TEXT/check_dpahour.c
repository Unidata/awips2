#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>

#include "GeneralUtil.h"
#include "time_defs.h"
#include "time_convert.h"

#include "FfmUtils.h"


/*******************************************************************
   check_recent_onhr()	
   
   PURPOSE
   Checks whether the time is an off-hour grid that is not 
   within the specified number of hours.  The off-hour test allows
   for a window around the top of the hour. Returned value is
   either 1 or 0.
   
   *****************************************************************/

int check_recent_onhr(time_t	data_timet,
		      int	minutes_window,
		      int	max_hrs)
{
   time_t		cur_timet;
   int			grid_status;
   onhour_flagtype	onhour_flag;
   time_t		nearest_hour;
   
   
   /* see if the grid is on the hour */
   
   check_onhour(data_timet, minutes_window, &onhour_flag, &nearest_hour);
   
      
   /* grid is on the hour or near the hour, so it is ok */
   
   if (onhour_flag == ON_HOUR || onhour_flag == NEAR_HOUR)
   {
      grid_status = 1;
   }
   
   
   /* the grid is an off-the-hour grid, so check if it
      is a recent grid */
   
   else
   {
      time(&cur_timet);
            
      if ((cur_timet - data_timet) <= (SECONDS_PER_HOUR * max_hrs))
	 grid_status = 1;
      
      else
	 grid_status = 0;
   }
   
   
   return(grid_status);
}


/*******************************************************************
   check_onhour()	
   
   PURPOSE
   Checks whether the time is on the top-of-the-hour 
   is near the top-of-the-hour, or is not near the top-of-the-hour.
   Returned value is an enumerated variable.
   
   *****************************************************************/
void check_onhour(time_t		data_timet,
		  int			minutes_window,
		  onhour_flagtype	*hr_flag,
		  time_t		*nearest_hour)
{
   struct tm	*data_tm;
   time_t	hour_timet;
   int		minute_value;
   
   
   /* convert the time */
   
   data_tm = gmtime(&data_timet);
   
   
   /* determine the top-of-the-hour characteristics */
   
   if (data_tm->tm_min == 0)
   {
      *hr_flag = ON_HOUR;
      *nearest_hour = data_timet;
   }
   
   
   else
   {
      
      /* save the minute value for use below. */
      
      minute_value = data_tm->tm_min;
      
      
      /* if within the window, then set the flag */
      
      if (minute_value <= minutes_window ||
	  minute_value >= (MINUTES_PER_HOUR - minutes_window))
	 *hr_flag = NEAR_HOUR; 
      
      else
	 *hr_flag = OFF_HOUR;
      
      
      /* set to the nearest hour regardless of whether it is
	 in the window or not. */
      
      data_tm->tm_min = 0;
      hour_timet = gm_mktime(data_tm);
      
      if (minute_value < 30)
	 *nearest_hour = hour_timet;
      
      else
	 *nearest_hour = hour_timet + SECONDS_PER_HOUR;
   }
   
   
   return;
}


/*******************************************************************
   find_best_dpatime()	
   
   PURPOSE
   Given a list of dpa products and a desired top-of-the hour time,
   finds out which product is the closest to the desired time
   and is within an acceptable number of minutes of the window.
   
   This function must also handles the case when it is given a
   NON top-of-the-hour time.  In this case the best time is
   must match the desired time exactly.  This case arises when
   processing single-hour, off-hour grids, from accum_grids
   for example.
   
   *****************************************************************/
DPARadar * find_best_dpatime(int	minutes_window,
			     DPARadar	*dpaHead,
			     time_t	desired_time)
{ 
   DPARadar		*dpaPtr_best = NULL;
   DPARadar	        *dpaPtr = NULL;
   onhour_flagtype	hr_flag;
   time_t		best_timet, obs_timet;
   time_t		nearest_hour;
   int			status;
   
   
   /* initialize */
   
   best_timet  = 0;
   
   
   /* check for null input data */
   
   if (dpaHead == NULL)
      return(NULL);
      
   
   /* loop on the number of products in the list */
   
   dpaPtr = (DPARadar *) ListFirst(&dpaHead->list);
   while (dpaPtr)
   {          
      status = yearsec_dt_to_timet(dpaPtr->obstime, &obs_timet);
      
      
      /* if there is an exact match, then great - we are done.
	 this check covers the special case of exact match for non
	 top-of-the-hour desired time. */
      
      if (obs_timet == desired_time)
      {
	 dpaPtr_best = dpaPtr;
	 break;
      }

      
      /* check if the grid is near the top of the hour, and 
         find out the hour to which it is nearest. */
      
      else
      {
	 check_onhour(obs_timet, minutes_window, &hr_flag, &nearest_hour);
	 
	 
	 /* if it is near the desired hour, then check if it
	    the closest one */
	 
	 if (hr_flag == NEAR_HOUR && nearest_hour == desired_time)
	 {		 
	    if (dpaPtr_best == (DPARadar *)NULL)
	       dpaPtr_best = dpaPtr;
	    
	    else
	    {
	       status = yearsec_dt_to_timet(dpaPtr_best->obstime, &best_timet);
	       if (abs(obs_timet - desired_time) < abs(best_timet - desired_time))
		  dpaPtr_best = dpaPtr;	    
	    }
	 }
      }
      
      
      dpaPtr = (DPARadar *) ListNext(&dpaPtr->node);
   }
   
   
   return(dpaPtr_best);
}


/*******************************************************************
   get_min_duration()	
   
   PURPOSE
   Get the minimum allowable duration.
   
   *****************************************************************/
void get_min_duration(float	*min_duration)
{
   char 	token_string[60];
   int		token_len, string_len;
   float	floatval;
   char		token_name[] = "whfs_min_dur_filled";
   
   
   /* get the token value */
   
   token_len = strlen(token_name);
   get_apps_defaults(token_name, &token_len, token_string, &string_len);
   
   if (string_len > 0)
   {
      floatval = atof(token_string);
      
      if ((floatval <= 0.0) || (floatval > 1.00) )
      {
	 floatval = DEFAULT_MIN_DURATION_FILLED;
	 fprintf(stderr,
		 "Error in specified value for token: %s.\n" 
		 "Using default value of %f percent.\n",
		 token_string, (floatval * 100.));
      }
   }
   
   else
   {
      floatval = DEFAULT_MIN_DURATION_FILLED;
   }
   
   
   /* set the returned value */
   
   *min_duration = floatval;
   
   
   return;
}


/*******************************************************************
   get_stage1_window()	
   
   PURPOSE
   Get the minutes window around the top of the hour
   for considering stage 1 products near the top of the hour.
   
   *****************************************************************/
void get_stage1_window(int	*minute_window)
{
   char 	token_string[60];
   int		token_len, string_len;
   int		intval;
   char		token_name[] = "dpa_wind";
   
   
   /* get the token value */
   
   token_len = strlen(token_name);
   get_apps_defaults(token_name, &token_len, token_string, &string_len);
   
   
   /* extract the value and check it */
   
   if (string_len > 0)
   {
      intval = atoi(token_string);
      if (intval < 0 || intval > 30)
      {
	 intval = DEFAULT_STAGE1_MINUTES_WINDOW;
	 fprintf(stderr,
		 "Error in specified value of %s for token %s.\n" 
		 "Using default value of %d minutes.\n",
		 token_string, token_name, intval);
      }
   }
   
   else
   {
      intval = DEFAULT_STAGE1_MINUTES_WINDOW;      
   }

   
   /* set the returned value */
   
   *minute_window = intval;
      
   
   return;
}


/*******************************************************************
   get_min_area()	
   
   PURPOSE
   Get the minimum allowable area.
   
   *****************************************************************/
void get_min_area(float	*min_area)
{
   char 	token_string[60];
   int		token_len, string_len;
   float	floatval;
   char		token_name[] = "whfs_min_area_covered";
   
   
   /* get the token value */
   
   token_len = strlen(token_name);
   get_apps_defaults(token_name, &token_len, token_string, &string_len);
   
   
   /* extract the value and check it */
   
   if (string_len > 0)
   {
      floatval = atof(token_string);
      
      if ((floatval <= 0.0) || (floatval > 1.00) )
      {
	 floatval = DEFAULT_MIN_AREA_COVERED;
	 fprintf(stderr,
		 "Error in specified value for env var MIN_AREA_COVERED.\n" 
		 "Using default value of %f percent.\n", (floatval * 100.));
      }
   }
   
   else
   {
      floatval = DEFAULT_MIN_AREA_COVERED;
   }
   
   /* set the returned value */
   
   *min_area = floatval;
      
   return;
}

