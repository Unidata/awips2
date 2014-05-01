#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "FfmUtils.h"
#include "ArealProductSettings.h"

#include "FcstPrecip.h"
#include "GeoArea.h"
#include "Location.h"

#include "time_defs.h"
#include "time_convert.h"
#include "convert_hrap.h"

#include "ToolDefs.h"



/***********************************************************************

   bld_areal_product()
   
   PURPOSE
   This function takes a user request for a single product, and returns
   the product data.  This may be just precip data, just ffg data, or 
   both, in the case of a comparison product.  When returning areal
   or point data (i.e. not gridded), this function returns an entry
   (i.e. element in array of structure) for every area/point, even if data
   are missing.  This is needed for the comparison products, where the
   precip and ffg data arrays must be sequenced the same to
   maintain proper correspondence between values for areas/points.
   
   *********************************************************************/

void bld_areal_product(ArealProductTypeDescriptor	prod_descr,
		       ArealProductSpecifier		prod_spec,
		       ArealProduct			*product)
{
   
   /* initialize data. make copy of the product descriptor
      and specifier information */
   
   product->descr.mode            = prod_descr.mode;
   product->descr.precipType      = prod_descr.precipType;
   product->descr.resolutionLevel = prod_descr.resolutionLevel;
   
   product->spec.endTime  =       prod_spec.endTime;
   product->spec.duration =       prod_spec.duration;
   strcpy(product->spec.sourceId, prod_spec.sourceId);
   
   product->precip_status = product->ffg_status = -1;
   product->precip_cnt    = product->ffg_cnt = 0;
   
   product->ffg_grid = product->precip_grid = NULL;
   product->ffg_data = product->precip_data = NULL;
   
   
   /* load precip data if in either precip mode or in comparison mode.*/
   
   if (prod_descr.mode == PRECIP_MODE || prod_descr.mode == COMPARISON_MODE)
   {      
      bld_precip_data(prod_descr, prod_spec, product);      
   }
   
   
   /* load FFG data when in FFG mode or comparison mode. */
   
   if (prod_descr.mode == FFG_MODE || prod_descr.mode == COMPARISON_MODE)
   {           
      bld_ffg_data(prod_descr, prod_spec, product);
   } 
   
   
   return;
}


/*******************************************************************
   bld_precip_data()	
   
   PURPOSE
   Build the requested precip product.
   
   *****************************************************************/

void bld_precip_data(ArealProductTypeDescriptor	prod_descr,
		     ArealProductSpecifier	prod_spec,
		     ArealProduct		*product)
{
 
   int 		qpf_hours;
   float	min_duration_filled;
   
   
   /* get the minimum allowable proportion of the duration filled.
      number is a ratio ranging from 0.0-1.0 */
   
   get_min_duration(&min_duration_filled);
   
   
   
   if (prod_descr.precipType == STAGE1_PRECIP)
   {
      bld_stage1_data(prod_descr, prod_spec, min_duration_filled,
		      product);
   }
   
   
   else if (prod_descr.precipType == STAGE2_GAGE_ONLY_PRECIP ||
	    prod_descr.precipType == STAGE2_GAGE_RADAR_PRECIP)
   { 
      bld_stage2_data(prod_descr, prod_spec, min_duration_filled,
		      product);
   }
   
   
   /* get the set of point data knowing the ending time and duration.
      for point data, the resolution is not meaningful */

#ifdef ELVIS   
   else if (prod_descr.precipType == POINT_GAGE_PRECIP)
   {
      product->precip_data = bld_ptprecip(prod_descr, prod_spec,
					  min_duration_filled,
					  &product->precip_status, 
					  &product->precip_cnt);					  
   }
#endif
   
   /* get the qpf data */
   
   else if (prod_descr.precipType == QPF_PRECIP)
   {      
      if (prod_descr.resolutionLevel == GRID_RES)
      {
	 qpf_hours = prod_spec.duration / SECONDS_PER_HOUR;
	 product->precip_grid = bld_qpf_grid(prod_spec.sourceId, qpf_hours,
					     prod_spec.endTime,
					     &product->precip_status);
      }
      
      else
      {            
	 product->precip_data = bld_qpf_data(prod_descr, prod_spec,
					     &product->precip_status,
					     &product->precip_cnt);
      }
   }
   
   
   return;
}


/*******************************************************************
   bld_stage1_data()	
   
   PURPOSE
   Build the requested stage1 precip product.
   
   Note that bld_stage1_data code is the same as the bld_stage1_data 
   code.  Its calls to the processing functions pass arguments
   that uniquely identify the grid type.
   
   *****************************************************************/
void bld_stage1_data(ArealProductTypeDescriptor	prod_descr,
		     ArealProductSpecifier	prod_spec,
		     float			min_duration_filled,
		     ArealProduct		*product)
{
   int		numhrs_found;
   float	duration_filled;
   
   
   /* compute the accumulated data for the time period */
   
   product->precip_grid = accum_grids(prod_descr, prod_spec,
				      &numhrs_found);
   
   
   /* check if enough data found */
   
   duration_filled =
      (float )numhrs_found / (prod_spec.duration / SECONDS_PER_HOUR);   
   if (duration_filled < min_duration_filled)
   {
      fprintf(stderr,
	      "Not enough stage1 data.  Hrs found/requested = %d, %ld.\n",
	      numhrs_found, (prod_spec.duration / SECONDS_PER_HOUR));
      
      product->precip_status = -1;
      free(product->precip_grid);
      product->precip_grid = NULL;
      
      return;
   }
   
   else
   {
      product->precip_status = 1;
   }
   
   
   /* if deriving data from the grid, then do that now */ 
   
   if (prod_descr.resolutionLevel != GRID_RES)
   {      
      /* now determine the values for the individual areas
	 using the gridded data. the status and cnt are returned
	 along with the precip data itself in the function argument */
      
      product->precip_data = accum_map_from_grid(prod_descr, prod_spec,
						 product->precip_grid,
						 &product->precip_status, 
						 &product->precip_cnt);
      
      
      /* free the gridded data since it is not used anymore */
      
      free(product->precip_grid);
      product->precip_grid = NULL;
   }
   
   
   return;
}


/*******************************************************************
   bld_stage2_data()	
   
   PURPOSE
   Build the requested stage2 precip product.
   
   Note that bld_stage1_data code is the same as the bld_stage1_data 
   code.  Its calls to the processing functions pass arguments
   that uniquely identify the grid type.
   
   *****************************************************************/
void bld_stage2_data(ArealProductTypeDescriptor	prod_descr,
		     ArealProductSpecifier	prod_spec,
		     float			min_duration_filled,
		     ArealProduct		*product)
{
   int 		numhrs_found;
   float	duration_filled;
      
   
   /* compute the accumulated data for the time period */
   
   product->precip_grid = accum_grids(prod_descr, prod_spec,
				      &numhrs_found);
   
   
   /* check if enough data found */
   
   duration_filled =
      (float )numhrs_found / (prod_spec.duration / SECONDS_PER_HOUR);   
   if (duration_filled < min_duration_filled)
   {
      fprintf(stderr,
	      "Not enough stage2 data.  Hrs found/requested = %d, %ld.\n",
	      numhrs_found, (prod_spec.duration / SECONDS_PER_HOUR));
      
      product->precip_status = -1;
      free(product->precip_grid);
      product->precip_grid = NULL;
      
      return;
   }
   
   else
   {
      product->precip_status = 1;
   }
   
   
   /* if deriving data from the grid, then do that now */ 
   
   if (prod_descr.resolutionLevel != GRID_RES)
   {      
      /* now determine the values for the individual areas
	 using the gridded data. the status and cnt are returned
	 along with the precip data itself in the function argument */
      
      product->precip_data = accum_map_from_grid(prod_descr, prod_spec,
						 product->precip_grid,
						 &product->precip_status, 
						 &product->precip_cnt);
      
      
      /* free the gridded data since it is not used anymore */
      
      free(product->precip_grid);
      product->precip_grid = NULL;
   }   
   
   return;
}


/*******************************************************************
   bld_ptprecip()	
   
   PURPOSE
   Builds the point precip product.
   
   
   *****************************************************************/
ArealData * bld_ptprecip(ArealProductTypeDescriptor	prod_descr,
			 ArealProductSpecifier		prod_spec,
			 float				min_duration_filled,
			 int				*precip_status,
			 long				*precip_cnt)
{   
   ArealData * precip_data = NULL ;
   
   return(precip_data);   
}

/*******************************************************************
   bld_qpf_data()	
   
   PURPOSE
   Build the qpf product.
      
   
   *****************************************************************/

ArealData * bld_qpf_data(ArealProductTypeDescriptor	prod_descr,
			 ArealProductSpecifier		prod_spec,
			 int				*precip_status,
			 long				*precip_cnt)
{
   GeoArea	*gaHead = NULL, *gaPtr = NULL;
   FcstPrecip	*fpHead = NULL;
   char		where[240];
   char 	ansi_timestr[ANSI_YEARSEC_TIME_LEN + 1];
   int		shefdur;
   int		area_cnt;
   int		status;
   char		boundary_type[40];
   ArealData	*precip_data;
   
   
   /* initialize */
   
   *precip_cnt = 0;
   *precip_status = -1;
   
   
   /* set the boundary type string for the query */
   
   set_boundary_string(prod_descr.resolutionLevel, boundary_type);
   
   sprintf(where, " WHERE boundary_type = '%s' ORDER BY area_id ",
	   boundary_type);       
   gaHead = GetGeoArea(where);
   
   
   /* malloc memory for the returned array */
   
   area_cnt = ListCount(&gaHead->list);
   
   if (area_cnt == 0)
      return(NULL);
   
   else
   {
      *precip_status = 1;
      
      precip_data = (ArealData *)malloc(sizeof(ArealData) * area_cnt);
      if (precip_data == NULL)
      {
	 printf("Error in malloc of precip_data in bld_qpf_data().\n");
	 return(NULL);
      }
   }
   
   
   /* convert the times */
   
   status = timet_to_shefdur(prod_spec.duration, &shefdur);
   status = timet_to_yearsec_ansi(prod_spec.endTime, ansi_timestr);
   
   
   /* get the data for each of the areas */
   
   area_cnt = 0;
   
   gaPtr = (GeoArea *) ListFirst(&gaHead->list);
   while (gaPtr)
   {
      
      sprintf(where, "  WHERE lid = '%s' AND dur = %d AND validtime = '%s' "
	      " ORDER BY basistime DESC ",
	      gaPtr->area_id, shefdur, ansi_timestr);
      fpHead = GetFcstPrecip(where);
      
      if (fpHead != (FcstPrecip * )NULL) 
      {	 
	 strcpy(precip_data[area_cnt].lid, fpHead->lid);
	 precip_data[area_cnt].value     = fpHead->value;
	 precip_data[area_cnt].validtime = prod_spec.endTime;
	 precip_data[area_cnt].isOk      = 1;
	 	 
	 FreeFcstPrecip(fpHead);
      }
      
      else
      {
	 strcpy(precip_data[area_cnt].lid, gaPtr->area_id);
	 precip_data[area_cnt].value     = -1.0;
	 precip_data[area_cnt].validtime = 0;
	 precip_data[area_cnt].isOk      = 0;
      }
      
      area_cnt++;
      
      
      /* go and process the next area */
      
      gaPtr = (GeoArea *) ListNext(&gaPtr->node);     	          
   }
   
   if (gaHead) FreeGeoArea(gaHead);
   
   
   /* return with the data */
   
   *precip_cnt = area_cnt;   
   return(precip_data);
   
}


/*******************************************************************
   bld_ffg_data()
   
   PURPOSE
   Builds the requested ffg data product.  If in comparison mode,
   always return a gridded FFG product; this is in addition to the
   possible array data also returned.
   
   *****************************************************************/

void bld_ffg_data(ArealProductTypeDescriptor	prod_descr,
		  ArealProductSpecifier		prod_spec,
		  ArealProduct			*product)
{
   char		boundary_type[40];
   int		numhrs;
   time_t	ffgtime;
   int		since_flag;
   char		areatypes_usage[10];
   
   
   /* initialize */
   
   product->ffg_cnt    = 0;
   product->ffg_status = -1;
   
   
   /* set the arguments for the data retrieval */
   
   numhrs = prod_spec.duration / SECONDS_PER_HOUR;
   
   
   /* if getting ffg data as part of a comparison against point
      precip data, then treat as a special case */
   
   if (prod_descr.resolutionLevel == GRID_RES)
   {
	 	 
      /* for gridded data, use any ffg data since an allowable
	 number of hours before the specified time. unlike when getting
	 ffg data for areas, building a gridded ffg always takes the ffg
	 data since the given time, rather than using an exact match for
	 the ffg time */
      
      ffgtime = prod_spec.endTime - (MAXHRS_FFG_LOOKBACK * SECONDS_PER_HOUR);
      
      
      /* when building a gridded ffg data set, need to
	 define the precedence order to use when building the
	 ffg grid - ie. basin, zone, then county. */
      
      strcpy(areatypes_usage, "BZC");
      
      
      /* get the data grid */
      
      product->ffg_grid = bld_ffg_grid(prod_spec.sourceId, areatypes_usage,
				       numhrs, ffgtime,
				       &product->ffg_status);
      
 
#ifdef ELVIS      
      /* for point data that compare against the ffg grid, the ffg values
	 for the grid bins which contain the point data must be loaded,
	 this also converts the missing values */
      
      if (prod_descr.precipType == POINT_GAGE_PRECIP)
      {
	 map_ffg_grid_to_point(prod_spec.sourceId, prod_spec.endTime, product);
	 
	 
	 /* free the memory for the gridded data since it is no longer
	    needed, and the calling program */
	 
	 free(product->ffg_grid);
	 product->ffg_grid = NULL;
      }
#endif
            
   }
   
   
   /* areal ffg needed */
   
   else
   {      
      set_boundary_string(prod_descr.resolutionLevel, boundary_type);
      
      
      /* if getting this product for display in FFG mode, 
	 then set the time to be the exact time; if getting for 
	 use in comparison mode, then set the time to be within
	 the time window */
      
      if (prod_descr.mode == FFG_MODE)
      {
	 since_flag = 0;
	 ffgtime = prod_spec.endTime;
      }
      
      else
      {
	 since_flag = 1;
	 ffgtime = prod_spec.endTime - 
	    (MAXHRS_FFG_LOOKBACK * SECONDS_PER_HOUR);
      }
      
      product->ffg_data = bld_ffg_area(boundary_type, numhrs,
				       ffgtime, since_flag,
				       &product->ffg_status,
				       &product->ffg_cnt); 
   }
   
   
   return;
}


/*******************************************************************
   map_ffg_grid_to_point()
   
   PURPOSE
   This function gets the ffg value from a grid for a set of
   locations.
   
   
   *****************************************************************/
void map_ffg_grid_to_point(char		*radid,
			   time_t	endtime,
			   ArealProduct	*product)
{
   
   int 		i;
   Location 	*locHead;
   char		where[80];
   double	lat, lon;
   int		status;
   double	south_row, west_col;
   int		numbins_230;
   double	row, col;
   int 		local_row, local_col;
   int		array_index;
   
   
   /* loop on the precip locations already gathered  */
   
   for (i = 0; i < product->precip_cnt; i++)
   { 
      
      /* assign the lid in the returned array */
            
      strcpy(product->ffg_data[i].lid, product->precip_data[i].lid);
      
      
      /* get the lat-lon of the location */
      
      sprintf(where, " WHERE lid = '%s' ", product->precip_data[i].lid);
      locHead = GetLocation(where);
      
      /* make sure location defined */
      
      if (locHead == NULL)
      {
	 fprintf(stderr, "Error getting location data for %s\n",
		 product->precip_data[i].lid);
	 
	 
	 /* assign the data array even though data are missing */
	 
	 product->ffg_data[i].value     = -1.0;
	 product->ffg_data[i].validtime = 0;
	 product->ffg_data[i].isOk      = 0;
      }
      
      else
      {	 
	 /* get the row and column of the location;
	    note that the hrap coords are returned as doubles */
	 
	 lat = locHead->lat;
	 lon = locHead->lon;
	 
	 LatLongToHrapByReference(lat, lon, &row, &col);
	 
	 
	 /* get the coords for the radar */
	 
	 status = find_radar_coords(radid, &south_row, &west_col, &numbins_230);
	 
	 
	 /* check the local coords of the location. note the grid is 
	    rooted at the southwest corner */
	 
	 if ((row >= south_row) && (row < (south_row  + LOCAL_HRAP_ROWS)) &&
	     (col >= west_col) && (col < (west_col + LOCAL_HRAP_COLS)))
	 {
	    local_row = row - south_row;
	    local_col = col - west_col;
	    
	    
	    /* now get the ffg value for this bin and assign it.
	       if value missing, return the value expected by the
	       calling function */
	    
	    array_index = ((local_row * LOCAL_HRAP_ROWS) + local_col);
	    
	    if (product->ffg_grid[array_index] != MISSING)
	    {
	       product->ffg_data[i].value     = product->ffg_grid[array_index];
	       product->ffg_data[i].validtime = endtime;
	       product->ffg_data[i].isOk      = 1;
	    }
	    
	    else
	    {
	       product->ffg_data[i].value     = MISSING;
	       product->ffg_data[i].validtime = 0;
	       product->ffg_data[i].isOk      = 0;
	    }
	 }
	 
	 
	 /* don't load since these data are outside the grid */
	 
	 else 
	 {
	    product->ffg_data[i].value     = MISSING;
	    product->ffg_data[i].validtime = 0;
	    product->ffg_data[i].isOk      = 0;
	 }
	 
	 
	 /* free the data */
	 
	 FreeLocation(locHead);
      }
      
   }
   
   product->ffg_cnt = product->precip_cnt;
   
   return;
}
