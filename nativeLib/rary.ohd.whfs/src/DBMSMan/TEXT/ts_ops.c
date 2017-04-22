/***************************************************************
ts_ops.c

****************************************************************/

#include "time_series.h"

#define RMIS -999.



/***************************************************************

ts_malloc()

****************************************************************/


Report* ts_malloc (time_t 	begin_time,
		   time_t 	end_time,
		   time_t 	interval,
		   int 		*elements)
{
   
   Report *ts;
   
   *elements = (((end_time - begin_time) + 1) / interval) + 1;
   
   ts = (Report *) malloc ((*elements)*sizeof(Report));
   
   return ts;
}


/***************************************************************

ts_normalize()

This function takes the input time series report_ts[]
and fills norm_ts[], with regular intervals and
interpolated values when necessary. It does NOT change
an accumulated series to an incremental one.  It returns
the number of values in the normalized time series.

****************************************************************/

int ts_normalize(Report 	report_ts[],
		 Report 	norm_ts[],
		 int 		num_reports,
                 int 		norm_elements,
		 time_t 	begin_time,
		 time_t 	end_time,
                 time_t 	interval,
		 int 		interp_mode)
{
   int 	i, j;
   
   /* set time stamp on norm_ts and interpolate missing values */
      
   for (i=0, j=0; ( (begin_time + (interval*i) <= end_time)  &&
	(j < norm_elements)); i++ )      
   {
      norm_ts[j].validtime = begin_time + (interval*i);
      
      norm_ts[j].value = ts_assign_value(report_ts, num_reports, 
					 norm_ts[j].validtime, interval,
					 interp_mode);
      
      if (norm_ts[j].value == -1 )
      {
	 norm_ts[j].value = MISSING;
      }
      j++;
   }
   return j;
}


/***************************************************************

ts_assign_value()


****************************************************************/

double ts_assign_value(Report 	report_ts[],
		       int 	num_reports,
		       time_t 	norm_time,
		       time_t 	interval,
		       int 	interp_mode)
{
   int 	i;
   int	done = 0;
   
   long	start_time, end_time, time_diff;
   
   double	assigned_value = -1;
   double	change,	fraction;
   
   
   
   for (i = 0; i < num_reports && !done; i++)
   {
      if ( report_ts[i].validtime == norm_time)
      {
	 assigned_value = report_ts[i].value;
	 done = 1;
      }
      
      
      /* allow interpolation */
      
      else if ( (interp_mode) && (report_ts[i].validtime > norm_time) && (i > 0))
      {
	 start_time = report_ts[i-1].validtime;
	 end_time = report_ts[i].validtime;
	 time_diff = end_time - start_time; 
	 
	 change = (report_ts[i].value - report_ts[i-1].value);
	 fraction = (norm_time - start_time) / (double)time_diff;
	 
	 assigned_value = change*fraction + report_ts[i-1].value;
	 
	 done = 1;
      }
      
      
      
      /* don't interpolate, just use report closest to hour.  */
      
      else if ( (! interp_mode) &&
	       ( abs(report_ts[i].validtime - norm_time) < (SECONDS_PER_HOUR / 2)) &&
	       (i > 0))
      {
	 assigned_value = report_ts[i].value;
	 done = 1;
      }
      
      
      
      else if ( (i == 0) && (report_ts[i].validtime > norm_time))
      {	 
	 /*
	 fprintf(stderr,"Error, first time slice comes after t\n");
	 fprintf(stderr," report  = %s", asctime(gmtime(&report_ts[i].validtime)));
	 fprintf(stderr," time t = %s", asctime(gmtime(norm_time)));	
	 */
	 
	 assigned_value = -1;
	 done = 1;
      }
      
      
      /* if the last observation is before the last time period,
	 return just the last observation.  */
      
      else if ((i == num_reports - 1) &&
	       (report_ts[i].validtime < norm_time))
	 assigned_value = -1;	
   }
   
   return assigned_value;
}


/***************************************************************

ts_accum_to_inc()

		This function takes the time series input_ts[] and creates
		output_ts[] which consists of the differences between
		successive elements of input_ts[].

****************************************************************/

int ts_accum_to_inc(Report 	input_ts[],
		    Report 	output_ts[],
		    int 	num_reports)
{
   
   
   int i;
   
   /* i starts at 1, to avoid indexing i = -1 */
   
   for ( i = 1 ; i < num_reports; i++)
   {
      output_ts[i-1].validtime   = input_ts[i].validtime;
      
      if ((input_ts[i].value == MISSING) || 
	  (input_ts[i-1].value == MISSING))
      {
	 output_ts[i-1].value = MISSING;
      }
      
      else
      {
	 output_ts[i-1].value  = input_ts[i].value - input_ts[i-1].value;
	 
      }
      
      if (output_ts[i-1].value < 0)
	 output_ts[i-1].value = MISSING;
      
   }
   return 1;
}


int ts_accum_to_inc2(Report 	input_ts[],
		     Report 	output_ts[],
		     int 	num_reports)
{
   
   
   int i;
   double last_valid = MISSING;
   
   /* i starts at 1, to avoid indexing i = -1 */
   
   for ( i = 1 ; i < num_reports; i++)
   {
       
      
      output_ts[i-1].validtime   = input_ts[i].validtime;
      
      if ((input_ts[i].value == MISSING) || 
	  (last_valid == MISSING))
      {
	 output_ts[i-1].value = MISSING;
      }
      
      else
      {
	 output_ts[i-1].value  = input_ts[i].value - last_valid;
	 
      }
      
      if (output_ts[i-1].value < 0)
	 output_ts[i-1].value = MISSING;
      
      
      /*
      	 Set the last valid data element
      */
      if (input_ts[i].value >= 0)
         last_valid = input_ts[i].value;

   }
   return 1;
}

/***************************************************************

ts_fill()

Takes the input time series report_ts[]
and fills norm_ts[], with regular intervals and
interpolated values when necessary.  It also returns
the number of values in the normalized time series.

****************************************************************/

void ts_fill(const 	Report 	report_ts[],
	     const 	int 	num_reports,
	     const 	time_t 	begin_time,
	     const	time_t 	end_time,
	     const	time_t 	interval,
	     const	int 	norm_mode,
		       	Report 	norm_ts[],
			int	*num_norm)
{
   int 	i;
   
   
   /* set the number of variables to be determined */
   
   *num_norm = (((end_time - begin_time) + 1) / interval) + 1;;
   
   
   /* loop on the number of values */
   
   for (i = 0; i < *num_norm; i++ )	   
   {
      norm_ts[i].validtime  = begin_time + (interval*i);
      
      
      /* if no input values, then assign missing data flag */
      
      if (num_reports > 0)
      {
	 norm_ts[i].value = ts_assignval(report_ts, num_reports,	  
					 norm_ts[i].validtime, interval, norm_mode);
	 if (norm_ts[i].value == -1) norm_ts[i].value = RMIS;
      }
      
      else 
	 norm_ts[i].value = RMIS;
      
   }
   
   return;
}


/***************************************************************** 

ts_assignval()

Assign the value depending upon the interp_mode:
  0 = no interpolation, exact match required
  1 = interpolate, no extrapolation
  2 = closest within 1/2 interval
  3 = closest, regardless of time; use with caution.


****************************************************************/

double ts_assignval(const 	Report 	report_ts[],
		    const	int 	num_reports,
		    const	time_t 	norm_time,
		    const	time_t 	interval,
			    	int 	norm_mode)
{
   int 	i, done;   
   long	start_time, end_time, time_diff;   
   double normval;
   double change, fraction;
   double diff, closediff;

   
   /* initialize and verify norm mode */
   
   closediff = +9999999;
   normval = RMIS;
   done = 0;
   
   if (norm_mode < 0 || norm_mode > 3) 
   {
      fprintf(stderr, "stage_ts_normalize: invalid mode specified %d\n",
      	      norm_mode);
      norm_mode = 1;
   }
   
   
   /* if using a mode which does not use the closest value,
      don't consider any value that is outside the range of input times */
   
   if (norm_mode <= 1 &&
       ((norm_time < report_ts[0].validtime) ||
	(norm_time > report_ts[num_reports - 1].validtime))) return(normval);
   
      
   /* loop on the number of input reports in chronological order */
   
   for (i = 0; i < num_reports && !done; i++)
   {
      
      /* for any of the modes, if times match exactly, use the value */
      
      if (report_ts[i].validtime == norm_time)
      {
	 normval = report_ts[i].value;
	 done = 1;
      }
      
      
      /* perform interpolation */
      
      else if (norm_mode == 1)
      {
         if ((report_ts[i].validtime > norm_time) && (i > 0))
         {
	    start_time = report_ts[i-1].validtime;
	    end_time = report_ts[i].validtime;
	    time_diff = end_time - start_time; 
	 
	    change = (report_ts[i].value - report_ts[i-1].value);
	    fraction = (norm_time - start_time) / (double)time_diff;
	 
	    normval = (change*fraction) + report_ts[i-1].value;	 
	    done = 1;
	 }
      }
           
      
      /* use closest value that is still within half the interval. */
      
      else if (norm_mode == 2)
      {
	 diff = report_ts[i].validtime - norm_time;
	 if ((abs(diff) < (interval / 2)) &&
	     (abs(diff) < closediff))
	 {
	    closediff = abs(diff);
	    normval = report_ts[i].value;
	 }
	 else if (diff > (interval / 2))
	    done = 1;
      }
      
      /* use closest value. */
      
      else if (norm_mode == 3)
      {
	 diff = report_ts[i].validtime - norm_time;
	 if (abs(diff) < closediff)
	 {
	    closediff = abs(diff);
	    normval = report_ts[i].value;
	 }
      }
      
      /* to avoid useless checking, if the current time is past
         the time for which to normalize, exit loop */
         
      if (report_ts[i].validtime > norm_time) done = 1;
      
   }
   
   
   return(normval);
}

