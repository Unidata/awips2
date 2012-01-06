#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "FfmUtils.h"

#include "time_convert.h"
#include "time_defs.h"

#include "ContingencyValue.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"

/*******************************************************************
   check_for_ffg.c	
   
   PURPOSE
   Checks for the existence of grids of the specified
   resolution since the specified time for the specified
   ffg duration.
   
   Return boolean indicating whether valid data found.
   
   *****************************************************************/

int check_for_ffg(ResolutionLevel	res,
		  time_t		lookbacktime,
		  int			ffg_hours)
{
   char		where[220];
   char		ansitime[ANSI_YEARSEC_TIME_LEN + 1];
   int		status;   
   int 		numfound;
   int		found;
   int		shefdur;
   char 	boundary_type[40];
   time_t 	durtime;
   
     
   /* initialize variables */
   
   found = 0;
   
   
   /* convert the times for the query */
   
   status = timet_to_yearsec_ansi(lookbacktime, ansitime);
   if (status < 0) 
   {
      fprintf(stderr,
	      "Error converting endtime in check_for_ffg(); status = %d\n",
	      status);
      return(found);
   }
   
   durtime = (time_t)((time_t )SECONDS_PER_HOUR * (time_t)ffg_hours);
   status = timet_to_shefdur(durtime, &shefdur);
   
   /* printf("after call: durtime, shefdur = %ld %d\n",
	  durtime, shefdur); */
   
   
   if (status < 0) 
   {
      fprintf(stderr,
	      "Error converting duration in check_for_ffg(); status = %d\n",
	      status);
      return(found);
   }
  
   set_boundary_string(res, boundary_type);
   
   
   
   /* get the count of the number of records matching
      the specified criteria */
   
   if (res != GRID_RES)
      sprintf(where,
	      " WHERE pe = 'PP' AND ts = 'CF' AND "
	      " validtime > '%s' "
	      " AND lid IN (SELECT area_id FROM geoarea where " 
	      " boundary_type = '%s') AND dur = %d",
	      ansitime, boundary_type, shefdur);
   else
      sprintf(where,
	      " WHERE pe = 'PP' AND ts = 'CF' AND "
	      " validtime > '%s'  AND dur = %d",
	      ansitime, shefdur);
      
   
   numfound = recordCount("ContingencyValue", where);

   if (numfound > 0) 
      found = 1;
   else
      found = 0;
   
   return(found);
}


/*******************************************************************
   get_latest_ffgtime()	
   
   PURPOSE
   Gets the latest ffg time available.  This is useful for when
   assembling the gridded data set of FFG from non-gridded data
   with assorted valid times and what is wanted is some sort
   of valid time for this nebulus data set.
   
   *****************************************************************/
int get_latest_ffgtime(time_t	lookbacktime,
			int	ffg_hours,
			time_t	*valid_timet)
{
   char			where[240];   
   time_t 		durtime;
   char			ansitime[ANSI_YEARSEC_TIME_LEN + 1];
   ContingencyValue	*cvHead;
   int 			shefdur;
   int 			status;
   time_t		temp_timet;
   
   
   /* convert the times for the query */
   
   status = timet_to_yearsec_ansi(lookbacktime, ansitime);
   if (status < 0) 
   {
      fprintf(stderr,
	      "Error converting endtime in get_latest_ffgtime(); status = %d\n",
	      status);
      return(-1);
   }
   
   durtime = SECONDS_PER_HOUR * ffg_hours;
   status = timet_to_shefdur(durtime, &shefdur);
   if (status < 0) 
   {
      fprintf(stderr,
	      "Error converting duration in get_latest_ffgtime(); status = %d\n",
	      status);
      return(-1);
   }
   
   
   /* perform the query using a sorted order */
   
   sprintf(where,
	   " WHERE pe = 'PP' AND ts = 'CF' AND "
	   " validtime > '%s'  AND dur = %d ORDER BY validtime DESC",
	   ansitime, shefdur);
   
   cvHead = GetContingencyValue(where);   
   
   
   /* the first item in the list is the latest */
   
   if (cvHead == (ContingencyValue *)(NULL))
   {
      *valid_timet = 0;
      return(-1);
   }
   
   else
   {
      status = yearsec_dt_to_timet(cvHead->validtime, &temp_timet);
      *valid_timet = temp_timet;
      
      FreeContingencyValue(cvHead);
      
      return(0);
   }
   
}
