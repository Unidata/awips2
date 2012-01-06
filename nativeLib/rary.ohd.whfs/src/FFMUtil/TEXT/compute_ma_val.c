#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#include "FfmSummary.h"

#include "check_within230.h"    /* For the incircle_check routine prototype. */
#include "convert_hrap.h"  	/* for LOCAL_HRAP_ROWS, COLS */

#include "read_radargrids.h"	/* for MISSING_STAGE2 */
#include "ToolDefs.h"           /* for MISSING */



/*********************************************************************
   compute_ma_val()
   
   This computes the mean areal value for a single grid and returns
   a single value.
   
   It is the responsibility of the calling program to check 
   that an acceptable percent of valid data were found!!! 
   
   ********************************************************************/

void compute_ma_val(int		logall_flag,
		    double	south_row,
		    double	west_col,
		    int		numbins_230,
		    float	*grid_vals,
		    int		zero_flag,
		    char	*area_id,
		    int		numrows,
		    long	rows[],
		    long	beg_cols[],
		    long	end_cols[],
		    float	*avg_val,
		    float	*max_val,
		    float	*min_val,
		    float	*percent_valid,
		    int		*status)
{
   int 		i, jcol;
   int 		row, col;
   int 		in_cnt, out_cnt;
   int		val_cnt, miss_cnt, total_cnt;
   float 	raw_val, sum;
   int 		base_row, base_col;
   int		array_index;
   int		within_circle;
   double	cur_max, cur_min;
   
   
   /* initialize */
      
   in_cnt = out_cnt = miss_cnt = total_cnt = val_cnt = 0;
   cur_max = -88888.;
   cur_min = +88888.;
   sum = 0.0;
   
   *status = -1;
   
   
   /* no need to round off the hrap location of the southwest
      corner of the radar grid, since should already be at xxx.00 */
   
   base_row = south_row;
   base_col = west_col;
   
   
   /* loop on the number of rows for this basin */
   
   for (i = 0; i < numrows; i++)
   {     
      total_cnt += end_cols[i] - beg_cols[i] + 1;
      
      if (logall_flag)
	 printf("row, begin -> end col: %ld,  %ld -> %ld\n", 
		rows[i], beg_cols[i], end_cols[i]);
      
      
      /* loop on the number of columns in each row */
      
      for (jcol = beg_cols[i]; jcol <= end_cols[i]; jcol++)
      {	 
	 /* adjust the row, column to match the local grid */
	 
	 row = rows[i] - base_row;
	 col = jcol - base_col;
	 
	 
	 /* check if the bin being considered is inside not
	    just the radar grid, but within that circular portion
	    of the grid covered by the radar */
	 
	 within_circle = incircle_check(numbins_230, row, col);
	 
	 
	 /* check that the grid cell being specified is within
	    the radar umbrella portion of the grid being read
	    and get the radar value */
	 
	 if (within_circle)
	 {
	    in_cnt++;
	    
	    
	    /* sum the value and increment the cnts.
	       note that the array index method must match the
	       method by which the grid was originally loaded*/
	    
	    array_index = ((row * LOCAL_HRAP_ROWS) + col);
	    raw_val = grid_vals[array_index];
	    
	    if (raw_val != MISSING_STAGE2 && raw_val != MISSING)
	    {
	       sum += raw_val;	       
	       if (raw_val > cur_max) cur_max = raw_val;
	       if (raw_val < cur_min) cur_min = raw_val;
	       
	       val_cnt++;
	    }
	    
	    else
	       miss_cnt++;
	 }
	 
	 else
	    out_cnt++;
	 
      }
   }
   
   
   /* compute the map value as the average of all the 
      bins within the area that have valid area_id data. */
   
   if (logall_flag)
      printf("%s bincnts: total %d= in %d (= valid %d + msg %d) + out %d\n",
	     area_id, total_cnt, in_cnt, val_cnt, miss_cnt, out_cnt);
   
   if (total_cnt <= 0)
      *percent_valid = 0.0;
   else
      *percent_valid = ((float)val_cnt / (float )total_cnt);
   
     
   /* if the zero flag is set, then the actual value returned is
      0, the values are not used */
   
   if (zero_flag)
   {
      *avg_val = 0.0;
      *max_val = 0.0;
      *min_val = 0.0;
   }
      
   else
   {
      if (val_cnt > 0)
      {
	 *avg_val = sum / val_cnt;
         *max_val = cur_max;
         *min_val = cur_min;
      }
      else
      {
	 *avg_val = 0.0;
         *max_val = 0.0;
         *min_val = 0.0;
      }
   }
   
   *status = 0;
   
   
   if (logall_flag)
   {
      if (*avg_val > 0.0)
	 printf( "%s: Sum/cnt=unadjstd avg => %f/%d = %f; max,min= %f %f \n", 
		area_id, sum, val_cnt, *avg_val, *max_val, *min_val);
   }
   
   
   /* adjust the returned value if it is less than some minimal number;
      this is due to the nature of the precip data, especially the
      radar data which contains super-tiny values */
   
   if (*avg_val < .00001)
      *avg_val = 0.0;
      
   if (*max_val < .00001)
      *max_val = 0.0;
      
   if (*min_val < .00001)
      *min_val = 0.0;
      
   
   return;
}
