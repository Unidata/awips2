#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#include "check_within230.h"


/*********************************************************************
   compute_numbins230()
   
   This computes the radius in HRAP bins for a 230 km distance
   for the given location.
   
   ********************************************************************/
int compute_numbins230(double lat,
		       int *numbins_230)
{
   double	z1, z2;
   int		status;
   
   /* calculate the number of HRAP bins in 230 km 
      based on the latitude of the radar */
   
   z1 = 1. + sin((double )(PI * lat / 180.));
   z2 = 4.7625 / ((1. + sin((double )(PI * 60. / 180.))) / z1);
   
   *numbins_230 = (int)(230/z2 + 1);
   
   /* do basic error check */
   
   if ((*numbins_230 < 0) || (*numbins_230 > 131)) 
      status = -1;
   
   else
      status = 0;
   
   return(status);
}


/*********************************************************************
   incircle_check()
   
   This determines whether the given row-col is within the
   230 km distance for a 131x131 grid.
   
   ********************************************************************/

int incircle_check(int		numbins_230,
		   int		row,
		   int		col)
{
   double 	bin_radius, row_dist, col_dist;
   int		array_index;
   int 		within_circle;
   
   
   /* determine if MAP bin being considered is within the 
      radar umbrella within the grid area.  */
   
   if ((row >= 0) && (row < LOCAL_HRAP_ROWS) &&
       (col >= 0) && (col < LOCAL_HRAP_COLS))
   {
      row_dist = abs((double )row - ((double )LOCAL_HRAP_ROWS/2.));
      col_dist = abs((double )col - ((double )LOCAL_HRAP_COLS/2.));
      bin_radius = pow( (pow(row_dist, 2.) + pow(col_dist, 2.)) , 0.5);
      
      array_index = ((row * LOCAL_HRAP_ROWS) + col);
      
      if (bin_radius <= numbins_230)
	 within_circle = 1;	 	 
      
      else
	 within_circle = 0;	 	 

   }
   else
      within_circle = 0;
   
   
   
   return(within_circle);
}

