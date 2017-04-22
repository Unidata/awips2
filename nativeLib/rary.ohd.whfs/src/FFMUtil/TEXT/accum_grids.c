#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "ArealProductSettings.h"
#include "FfmUtils.h"

#include "time_convert.h"
#include "time_defs.h"

#include "ToolDefs.h"

#include "RadarLoc.h"
#include "DbmsDefs.h"

#include "check_within230.h"
#include "read_radargrids.h"


/*******************************************************************
   accum_grids.c	
   
   PURPOSE
   Accumulates grid values for a radar for one to many hours for the
   given grid types.  It is the responsibility of the calling
   program to check that an acceptable number of hours were found.
   
   *****************************************************************/

float * accum_grids(ArealProductTypeDescriptor	prod_descr,
		    ArealProductSpecifier	prod_spec,
		    int				*numhrs_found)
{
   float	*curgrid_vals;
   float	*outgrid_vals;
   char		where[160];
   char		radid[RADAR_ID_LEN + 1];
   time_t 	starttime, gridtime;
   int		status;
   int		i;
   DPARadar	*dpaHead = NULL, *dpaPtr = NULL;
   char		ansi_starttime[ANSI_YEARSEC_TIME_LEN + 1];
   char		ansi_endtime[ANSI_YEARSEC_TIME_LEN + 1];
   int		minute_window;
   
   
   /* initialize */
   
   *numhrs_found = 0;
   
   
   /* make a convenient local copy */
   
   strcpy(radid, prod_spec.sourceId);
      
   
   /* malloc memory for the local grid */
   
   curgrid_vals = (float *)
      malloc(sizeof(float) * (LOCAL_HRAP_ROWS * LOCAL_HRAP_COLS));
   if (curgrid_vals == NULL)
   {
      printf("Error in malloc of curgrid_vals in accum_grids().\n");
      return(NULL);
   }
   
      
   /* malloc memory for the returned grid and initialize it. */
   
   outgrid_vals = (float *)
      malloc(sizeof(float) * (LOCAL_HRAP_ROWS * LOCAL_HRAP_COLS));
   if (outgrid_vals == NULL)
   {
      printf("Error in malloc of outgrid_vals in accum_grids().\n");
      return(NULL);
   }
   
   for (i = 0; i < (LOCAL_HRAP_ROWS * LOCAL_HRAP_COLS); i++)
      outgrid_vals[i] = MISSING_STAGE2;
     
   
   /* get the data depending on the data type. */
   
   if (prod_descr.precipType == STAGE1_PRECIP)
   {
      /* get the minutes window for use in determining
	 the near or top of the hour products */
      
      get_stage1_window(&minute_window);
      
      
      /* for stage1 data, use a slightly larger time window than
	 normal to account for data that may not be on the top of the hour. */
      
      starttime = prod_spec.endTime - prod_spec.duration;  
      status = timet_to_yearsec_ansi(starttime, ansi_starttime);
      status = timet_to_yearsec_ansi(prod_spec.endTime + SECONDS_PER_HOUR,
                                     ansi_endtime);
      sprintf(where,
              " WHERE radid = '%s' AND obstime >= '%s' "
              " AND obstime <= '%s' AND supplmess IN (0,4) ",
              radid, ansi_starttime, ansi_endtime);
      dpaHead = GetDPARadar(where);
      
      if (dpaHead == (DPARadar *)NULL)
      {
         printf("No data found in DPARadar by accum_grids() for: %s\n", radid);
      }
      
      else
      {      
         /* now loop on each hour. */
         
         starttime = prod_spec.endTime - (prod_spec.duration - SECONDS_PER_HOUR);
	 
         for (gridtime = starttime; gridtime <= prod_spec.endTime;
	  gridtime += SECONDS_PER_HOUR)
         {
	    
	    /* find the grid, if there is one, which is either an exact time
	       match of the requested time (this covers the case of 
	       non-top-of-the-hour grids for 1 hour stage1 accums)
	       or find the grid which is closest to the top of the desired
	       hour and which is within a time window to that desired hour */
	    
	    dpaPtr = find_best_dpatime(minute_window, dpaHead, gridtime);
	    
	    if (dpaPtr)
	    {	       
	       /* if non-zero precip indicated, then get values from file
		  and add them.  otherwise, set any undefined values to zero */
	       
	       if (dpaPtr->supplmess == 4 && dpaPtr->maxvald > 0.0)
	       {
		  status = read_stage1file(dpaPtr, curgrid_vals);
		  if (status >= 0)
		  {
		     (*numhrs_found)++;
		     
		     for (i = 0; i < (LOCAL_HRAP_ROWS * LOCAL_HRAP_COLS); i++)
		     {
			if (curgrid_vals[i] != MISSING_STAGE2 &&
			    curgrid_vals[i] >= 0.0)
			{			   
			   if (outgrid_vals[i] != MISSING_STAGE2)
			      outgrid_vals[i] += curgrid_vals[i];
			   
			   else
			      outgrid_vals[i]  = curgrid_vals[i];
			}
		     }
		  }
	       }
	       
	       else
	       {
		  (*numhrs_found)++;
		  
		  for (i = 0; i < (LOCAL_HRAP_ROWS * LOCAL_HRAP_COLS); i++)
		  {
		     if (outgrid_vals[i] == MISSING_STAGE2)
			outgrid_vals[i] = 0.0;
		  }
		  
	       }
	    }
	 }
	 
	 FreeDPARadar(dpaHead);
      }
   }
   
   
   /* assign the grid vals special values depending on whether
      the radar covers the bin. */
   
   assign_special_bins(radid, outgrid_vals);
      
   
   /* free the allocated memory and return with the data */
   
   free(curgrid_vals);
   
   
   return(outgrid_vals);
}


/*******************************************************************
   
   *****************************************************************/

void assign_special_bins(char 	*radid,
			 float	*outgrid_vals)
{
   
   RadarLoc	*radarlocPtr = NULL;
   int		array_index;
   double	south_row, west_col;
   int		numbins_230;
   int		within_circle;
   char		where[140];
   int		status;
   int		row, col;
   
   /* determine the HRAP south-west corner for the grid; this is
      done by subtracting the HRAP radius from the HRAP coords
      converted from the radar location, which is at the center
      of the grid */
   
   sprintf(where, " WHERE radid = '%s' ", radid);
   radarlocPtr = GetRadarLoc(where);
   if (radarlocPtr == NULL)
   {
      printf("Error in assign_special_bins() getting RadarLoc for: %s\n",
	     radid);
   }
   
   else
   {
      LatLongToHrapByReference(radarlocPtr->lat, radarlocPtr->lon, 
		    &south_row, &west_col); 
      south_row -= (double )(LOCAL_HRAP_ROWS / 2.);
      west_col  -= (double )(LOCAL_HRAP_COLS / 2.);
      
      
      /* calculate the number of HRAP bins in 230 km 
	 based on the latitude of the radar */
      
      status = compute_numbins230(radarlocPtr->lat, &numbins_230);
      
      if (status < 0)
      {
	 printf("Error %d computing numbins in 230 km for %s\n",
		status, radid);
      }
      
      else
      {
	 /* convert the missing values to match the missing value expected 
	    by the calling function.  also consider whether the value is
	    missing or is expected to be missing because it is outside of the
	    range of the radar */
	 
	 for (row = 0; row < LOCAL_HRAP_ROWS; row++)
	 {
	    for (col = 0; col < LOCAL_HRAP_COLS; col++)
	    {
	       array_index = (row * LOCAL_HRAP_ROWS) + col;
	       
	       within_circle = incircle_check(numbins_230, row, col);
	       
	       if (within_circle)
	       {
		  if (outgrid_vals[array_index] == MISSING_STAGE2)
		     outgrid_vals[array_index] = MISSING;
	       }
	       
	       else
		  outgrid_vals[array_index] = OUTSIDE_RADAR_RANGE;
	    }
	 }
	 
	 /*
	 printf("accum_grids: for %s, Srow = %.3f, Wcol = %.3f, numbins230= %d\n",
	 radid, south_row, west_col, numbins_230);
	 */
      }
      
      FreeRadarLoc(radarlocPtr);
   }
   
   return;
}
