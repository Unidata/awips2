#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "FfmUtils.h"

#include "convert_hrap.h"
#include "check_within230.h"

#include "RadarLoc.h"
#include "DbmsDefs.h"


/*******************************************************************
   find_radar_coords()
   
   Given a radar id, it finds the HRAP coordinates for the
   southwest corner of the Stage1/2 sized grid, and the 
   number of bins for a 230 km distance for this grid location.
   
   The row and column returned are integer values even though they
   are determined as fractional units of a grid bin.
   
   *****************************************************************/
int find_radar_coords(char	*radid, 
		      double 	*south_row,
		      double 	*west_col,
		      int	*numbins_230)
{
   char		where[80];
   RadarLoc	*radarlocPtr;
   int		status;
   
   
   /* determine the HRAP south-west corner for the grid; this is done
      by subtracting the HRAP radius from the HRAP coords converted from
      the radar location, which is at the center of the grid */

   sprintf(where, " WHERE radid = '%s' ", radid);
   radarlocPtr = GetRadarLoc(where);
   if (radarlocPtr == NULL)
   {
      printf("Error getting lat-lon info from RadarLoc for: %s\n",
             radid);
      return(-1);
   }

   else
   {
      LatLongToHrapByReference(radarlocPtr->lat, radarlocPtr->lon,
                    south_row, west_col);
      /*
      *south_row -= (double )(LOCAL_HRAP_ROWS / 2.);
      *west_col  -= (double )(LOCAL_HRAP_COLS / 2.);
      */
      
      *south_row = (long )(*south_row) - (long )(LOCAL_HRAP_ROWS / 2);
      *west_col  = (long )(*west_col) - (long )(LOCAL_HRAP_COLS / 2.);
      
      
      /*  determine the number of bins in the 230 km radius */
      
      status = compute_numbins230(radarlocPtr->lat, numbins_230);
      
      
      /* free the info */

      FreeRadarLoc(radarlocPtr);
   }
   
   return(0);
}
