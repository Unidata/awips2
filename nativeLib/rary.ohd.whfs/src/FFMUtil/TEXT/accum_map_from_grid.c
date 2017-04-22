/*******************************************************************
   
   accum_map_from_grid.c
   
   Determines accumulated MAP data for those areas matching
   the specified area type, and for the specified time period,
   and using the specified data source.
   
   *****************************************************************/   

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <sqltypes.h>

#include "GeoArea.h"

#include "DbmsAccess.h"
#include "DbmsDefs.h"

#include "time_convert.h"
#include "time_defs.h"

#include "check_within230.h"
#include "FfmUtils.h"   /* for find_radar_coords proto */

#include "ArealProductSettings.h"
#include "FfmUtils.h"


/**********************************************************************
   
   pass in the number of hours to keep data, and the number of
   hours ago to serve as the obs, or ending time.
   
   *******************************************************************/
ArealData * accum_map_from_grid(ArealProductTypeDescriptor	prod_descr,
				ArealProductSpecifier		prod_spec,
				float				*grid_vals,
				int				*status,
				long				*precip_cnt)
{
   time_t	endtime;
   GeoArea	*gaHead = NULL, *gaPtr = NULL;
   char 	where[140];
   int		area_cnt;
   int 		ret;
   float 	area_covered;
   float	min_area_covered;
   float 	avg_val;
   char		boundary_type[40];
   ArealData	*areal_data;
   double	south_row, west_col;
   int		numbins_230;
   int		logall_flag;
   int		zero_flag;
   float 	bias;
   
   
   /* initialize */
   
   *status     = -1;
   *precip_cnt = 0;
   logall_flag = 0;
   zero_flag   = 0;
   bias	       = 1.0;
   
   
   /* get the minimum proportion of area covered.
      this is given as a ratio from 0.0-1.0 */
   
   get_min_area(&min_area_covered);
      
   
   /* determine the HRAP south-west corner for the grid; this is done
      by subtracting the HRAP radius from the HRAP coords converted from
      the radar location, which is at the center of the grid */
   
   ret = find_radar_coords(prod_spec.sourceId,
			      &south_row, &west_col, &numbins_230);   
   if (ret < 0)
   {
      printf("Error finding radar coord info for %s\n", prod_spec.sourceId);
      return NULL ;
   }
   
   
   /* get the list of map areas that match the boundary type */
   
   set_boundary_string(prod_descr.resolutionLevel, boundary_type);   
   sprintf(where, " WHERE boundary_type = '%s' ORDER BY area_id ",
	   boundary_type);       
   gaHead = GetGeoArea(where);
   
   
   /* malloc memory for the returned array */
   
   if (gaHead == NULL)
      return (NULL);
   
   else
   {
      area_cnt = ListCount(&gaHead->list);   
      *status = 1;
      
      areal_data = (ArealData *)malloc(sizeof(ArealData) * area_cnt);
      if (areal_data == NULL)
      {
	 printf("Error in malloc of areal_data in accum_map_from_grid().\n");
	 return NULL ;
      }
   }
   
   
   /* define a convenient local variable */
   
   endtime  = prod_spec.endTime;   
   
   
   /* loop on the areas and process each one */
   
   area_cnt = 0;
   
   gaPtr = (GeoArea *) ListFirst(&gaHead->list); 
   while(gaPtr)
   {      
      /* get the value for the area. this function takes the area id and
	 gets its geo information, then calls the function to
	 get the average value. */
      
      ret = process_map_area(logall_flag, south_row, west_col, numbins_230,
			     gaPtr->area_id, endtime,
			     zero_flag, bias, grid_vals,
			     &avg_val, &area_covered);
      
      
      /* determine if enough valid data were found to use the
	 total value. if so then load the array to return */
      
      /*
      printf("%s: accumval %f, hrs_filled %d, percent %f\n", 
	     gaPtr->area_id, avg_val, hrs_filled, 100. * area_covered);
      */
      
      if (area_covered >= min_area_covered)
      {
	 strcpy(areal_data[area_cnt].lid, gaPtr->area_id); 
	 areal_data[area_cnt].value     = avg_val;
	 areal_data[area_cnt].validtime = endtime;
	 areal_data[area_cnt].isOk      = 1;
	 
      }
      
      else
      {
	 strcpy(areal_data[area_cnt].lid, gaPtr->area_id); 
	 areal_data[area_cnt].value     = -1.0;
	 areal_data[area_cnt].validtime = 0;	 
	 areal_data[area_cnt].isOk      = 0;
      }
           
      
      /* process the next area in the list */
      
      area_cnt++;
      gaPtr = (GeoArea *) ListNext(&gaPtr->node);
   }
   
   FreeGeoArea(gaHead);
   
   
   /* return with values */
   
   *precip_cnt = area_cnt;
   return(areal_data);   
}   



