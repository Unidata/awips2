#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "DbmsDefs.h"
#include "time_series.h"
#include "time_convert.h"

#ifdef commented_out
 
int read_precip_obs_ts(char *lid, time_t begin_time, time_t end_time,
		       Report **r_ts, int *r_count)
{
     
     /*
     
     Retrieves the time series from the database
     for the location lid that is within the 
     time window specified by begin_time and end_time
     (including both begin_time and end_time).
     Allocates memory for the data to be returned.
     Records are returned in r_ts.  The count
     of returned records is in r_count.
     
     
     */
     
     
     char 		where[BUFSIZ];
     char		ansiBeginTime[ANSI_TIME_LEN+1];
     char		ansiEndTime[ANSI_TIME_LEN+1];
     
     Precip	 	*pHead;
     Precip	        *pPtr;
     
     int 	  i = 0;
     int	  cnt;
     
     Report 	  *new_ts=NULL;
     
     time_t	rep_time;
     
     
     
     /*
     	   Convert timet to ANSI string
     */
     timet_to_yearsec_ansi(begin_time, ansiBeginTime);
     timet_to_yearsec_ansi(end_time, ansiEndTime);
     
     
     /*
     	  Create where clause
     */
     sprintf(where," WHERE lid = '%s' where obstime >= %s AND "
	     " obstime <= '%s' ORDER BY obstime ", 
	     lid, ansiBeginTime, ansiEndTime);
     
     
     if ( ( pHead = GetPrecip(where) ) )
     {
	  cnt = ListCount(&pHead->list);
	  
	  new_ts = (Report *) malloc(sizeof(Report)*cnt);
	  if (!new_ts)
	  {
	       fprintf(stderr,
		       "Error allocating memory in read_obs_precip_ts\n");
	       return 0;
	       
	  }
	  
	  
	  /* search until out of data, or the report time is too late */
	  
	  i = 0;
	  pPtr = (Precip *) ListFirst(&pHead->list);
	  
	  
	  while ( pPtr )	
	  {
	       
	       /* 
	       if the report is within time window
	       copy it to the malloc'ed array
	       */
	       yearsec_dt_to_timet(pPtr->obstime, &rep_time);
	       
	       new_ts[i].validtime = rep_time;
	       new_ts[i].value = pPtr->value;
	       
	       i++;
	    
	       
	       pPtr = (Precip *) ListNext(&pPtr->node);
	  }
	  
	  FreePrecip(pHead);
     }
     
     /* assign values to be returned */
     
     *r_count = i;
     
     *r_ts = (Report *) realloc((void *) new_ts, (*r_count)*sizeof(Report));
     
     return 1;
     
}

#endif

/*************************************************************/

#ifdef OLDCODE

int read_precip_fcst_ts(char *lid, time_t begin_time, time_t end_time,
		       Report **r_ts, int *r_count)
{

	/*
	
		Retrieves the time series from the database
		for the location lid that is within the 
		time window specified by begin_time and end_time
		(including both begin_time and end_time).
		Allocates memory for the data to be returned.
		Records are returned in r_ts.  The count
		of returned records is in r_count.
	
	
	*/


	char 		where[MAX_WHERE_LEN];
	
	PrecFcstTs 	*sHead,
			*sPtr;
	
	int 		i,
			cnt;
			
	time_t		rep_time,
	   		c_time;
	
	Report 		*new_ts,
			*temp_ts;
	

	sprintf(where," WHERE lid = '%s' ORDER by date, time ",lid);
	sHead = GetStageFcstTs(where);
	cnt = ListCount(&sHead->list);

	new_ts = (Report *) malloc(sizeof(Report)*cnt);
	if (!new_ts)
	{
		fprintf(stderr,
			"Error allocating memory in read_obs_stage_ts\n");
		return 0;
		
	}
	
	
	/* search until out of data, or the report time is too late */
	
	rep_time = 0;
	i = 0;
	sPtr = (StageFcstTs *) ListFirst(&sHead->list);
	
	
	while ( sPtr && (rep_time <= end_time))	
	{
		rep_time = sheftime_to_time_t(sPtr->date, sPtr->time);
		c_time = sheftime_to_time_t(sPtr->cdate, sPtr->ctime);

		/* 
			if the report is within time window
			copy it to the malloc'ed array
		*/
				
		if ((rep_time >= begin_time) && (rep_time <= end_time))
		{
			
			new_ts[i].validtime = rep_time;
			new_ts[i].value = sPtr->value;
			new_ts[i].basistime = c_time;

			i++;
		}
		
		sPtr = (StageFcstTs *) ListNext(&sPtr->node);
	}
	
	FreeStageFcstTs(sHead);
	 
	 
	/* assign values to be returned */
	
	*r_count = i;
	
	*r_ts = (Report *) realloc((void *) new_ts, (*r_count)*sizeof(Report));
	
	return 1;

}

#endif

