#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gen_areal_ffg.h"


/*********************************************************************
   compute_avg_ffg()
   
   This routine computes the mean areal FFG value for a single basin and
   returns a single value.  The value is computed in mm and changed to
   inches in this routine.
   
   calling routine:  process_areas

   ********************************************************************/

void compute_avg_ffg(int	logall_flag,
                    int         xor,
                    int         yor,
                    int         xsize,
                    int         ysize,
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
   int 		i, jcol, row, col;
   int		val_cnt, miss_cnt, total_cnt;
   float 	raw_val, sum;
   double	cur_max, cur_min;
   
   
   /* initialize */
      
   miss_cnt = total_cnt = val_cnt = 0;
   cur_max = -88888.;
   cur_min = +88888.;
   sum = 0.0;
   *status = 0;

   if (logall_flag)
      printf("yor = %d  xor = %d\n",yor,xor);
   
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
	 
	    /* sum the value and increment the cnts.
	       note that the array index method must match the
	       method by which the grid was originally loaded*/
	    
            row = (rows[i] - yor);
            col = (jcol - xor);

            /*-----------------------------------------------*/
            /*  check that box is within site's area         */
            /*  if not, return with status set to -1         */
            /*-----------------------------------------------*/

            if(row > (ysize - 1) || col > (xsize - 1) || row < 0 || col < 0)
            {
               *status = -1;
               return;
            }

            raw_val = mosaic_ffg_float[row][col];
            if (logall_flag)
               printf("%.1f  %d  %d  row = %d  col = %d\n",raw_val,i,jcol,row,col);
	    
	    if (raw_val != miss_value_float)
	    {
	       sum += raw_val;	       
	       if (raw_val > cur_max) cur_max = raw_val;
	       if (raw_val < cur_min) cur_min = raw_val;
	       
	       val_cnt++;
	    }
	    
	    else
	       miss_cnt++;
      }
   }
   
   
   /* compute the avg ffg value as the average of all the 
      bins within the area that have valid area_id data. */
   
   if (logall_flag)
      printf("%s bincnts: total %d (= valid %d + msg %d)\n",
	     area_id, total_cnt, val_cnt, miss_cnt);
   
   if (total_cnt <= 0)
      *percent_valid = 0.0;
   else
      *percent_valid = ((float)val_cnt / (float )total_cnt);
   
   if (val_cnt > 0)
   {
      *avg_val = (sum / val_cnt) / 25.4;
      *max_val = cur_max;
      *min_val = cur_min;
   }
   else
   {
      *avg_val = 0.0;
      *max_val = 0.0;
      *min_val = 0.0;
   }
   
   if (logall_flag)
   {
      if (*avg_val > 0.0)
	 printf( "%s: Sum/cnt=unadjstd avg => %.1f/%d = %.1f; max,min= %.1f %.1f \n", 
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
