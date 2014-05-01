#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "LineSegs.h"
#include "FfmSummary.h"
#include "FfmUtils.h"

#include "time_defs.h"

/*********************************************************************
   process_map_area()
   
   PURPOSE
   This performs all the MAP processing for a given area,
   given the single HRAP grid.
   
   Currently, the missing bin info is not used.
   
   ********************************************************************/

int process_map_area(int	logall_flag,
		     double	south_row,
		     double	west_col,
		     int	numbins_230,
		     char	*area_id,
		     time_t	gridtime,
		     int	zero_flag,
		     float	bias,
		     float	*grid_vals,
		     float	*avg_val,
		     float	*area_covered)
{
   long		*rows = NULL ;
   long 	*beg_cols = NULL , *end_cols = NULL ;
   int		numrows;
   int 		status;
   char 	where[80];
   LineSegs	*linesegPtr;
   float 	max_val, min_val;
   
   /* initialize */
   
   *area_covered = 0.;
   *avg_val = -1;
   
   
   /* read the HRAP bin coords for the area and
      extract the information from the blob fields. */
   
   sprintf(where, " WHERE area_id = '%s' ", area_id);
   linesegPtr = GetLineSegs(where);
   
   numrows   = ListCount(&linesegPtr->list);
   
   if (linesegPtr == NULL)
   {
      printf("LineSeg information not available for %s\n",
	     area_id);
      return(-1);
   }

   rows      = (long *)linesegPtr->hrap_row;
   beg_cols  = (long *)linesegPtr->hrap_beg_col;
   end_cols  = (long *)linesegPtr->hrap_end_col;

   if (numrows > 3000 || numrows <= 0)
   {
      printf("Invalid number of HRAP rows (%d) for %s.\n", numrows, area_id);
      status = -1;
   }

   else if (rows[0]     > 10000 || rows[0]     < 0 ||
            beg_cols[0] > 10000 || beg_cols[0] < 0 ||
            end_cols[0] > 10000 || end_cols[0] < 0)
   {
      printf("Invalid HRAP info for %s. Check LineSegs blobspace!\n",
             area_id);
      status = -1;
   }
   
     
   /* with the precip grid data and hrap area covered information
      in hand, now determine the map value. this needs to be done
      even if the entire grid is zeroes, since it is possible that
      the MAP area my be outside of the grid area and this precent
      of area covered needs to be known */
   
   else
   {
      compute_ma_val(logall_flag,
		     south_row, west_col, numbins_230, 
		     grid_vals, zero_flag,
		     area_id, numrows, rows, beg_cols, end_cols,
		     avg_val, &max_val, &min_val, area_covered,
		     &status);
   }
   
   /* free the memory */
   
   FreeLineSegs(linesegPtr);
   
   
   return(status);
}



