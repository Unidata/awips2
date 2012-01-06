#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>

#include "FfmUtils.h"
#include "FfmSummary.h"
#include "ArealProductSettings.h"

#include "convert_hrap.h"
#include "read_radargrids.h"

#include "DbmsDefs.h"
#include "DbmsUtils.h"

#include "time_defs.h"
#include "ToolDefs.h"

#include "GeoArea.h"
#include "LineSegs.h"


/*********************************************************************
   
   ********************************************************************/

ArealProduct *  malloc_summaryprods(int num_durations)
{   
   int	i;
   ArealProduct *products;
   
   
   /* allocate the memory for the different durations.  the memory for the
      precip and ffg grids are malloced separately later */
   
   products = (ArealProduct *)
      malloc(sizeof(ArealProduct) * num_durations);
   if (products == NULL)
   {
      fprintf(stderr, 
	      "Error in malloc of products in malloc_summaryprods().\n");
      exit(0);
   }
   
   
   /* initialize the data */
   
   for (i = 0; i < num_durations; i++)
   {   
      products[i].precip_status = products[i].ffg_status = -1;
      products[i].precip_cnt    = products[i].ffg_cnt    = 0;
      
      products[i].ffg_grid = products[i].precip_grid = NULL;
      products[i].ffg_data = products[i].precip_data = NULL;
   } 
   
   return(products);
}


/*******************************************************************
   
   PURPOSE
   Collect the data for the summary display.  The data are collected
   for all durations, and for the data sets that matches the
   product descriptor's source/type (e.g. stage2 gageonly) and
   the product specifier's endingtime and radar/source id.
   
   The product descriptor resolution is not used as gridded data
   are retrieved for both ffg and precip data.
   
   The duration in the specifier is not used, as the data are
   retrieved for all the specified durations; and the ending time
   in the specifier are not used as the precip data retrieved is always
   the latest data and the ffg data is retrieved for a time window
   based on the system time.
   
   ***********************************************************************/

int get_summary_data(ArealProductTypeDescriptor		prod_descr,      
		     ArealProductSpecifier		prod_spec,
		     int				num_durations,
		     int				*hr_durations,
		     ArealProduct			*products,
		     time_t				precip_timet[],
		     time_t				ffg_timet[])
{   
   /* This routine has been "stubbed out" because the ffm summary 
          functionality is no longer used.  However, there are a number of 
          no longer used routines in the HvAreal and HvWindows which call 
          get_summary_data.  Instead of cleaning everything out right now,
          we are taking a short cut. BAL June 25, 2003 */
   return 0 ;
}

/*********************************************************************
   
   ********************************************************************/

FfmSummaryStats	* bld_summary_stats(ArealProductTypeDescriptor	prod_descr,
				    ArealProductSpecifier	prod_spec,
				    ArealProduct		*products,
				    int				*num_areas,
				    int				num_durations,
				    int				*hr_durations)      
{
   char 	where[120];  
   GeoArea 	*gaHead = NULL, *gaPtr = NULL;
   char		boundary_type[40];
   
   LineSegs	*lsHead = NULL ;
   int		numrows;
   long		*rows;
   long 	*beg_cols, *end_cols;
   int		logall_flag = 0, zero_flag = 0;
   int		status;
   int		j;
      
   double	south_row, west_col;
   int		numbins_230;
   float	avg_val, max_val, min_val;
   float	percent_valid;
   FfmSummaryStats	*ffm_stats = NULL;
   int		index;
   float	min_area_covered;
   
   
   /* get the minimum allowed areal coverage. this
      number ranges from 0.0-1.0 */
   
   get_min_area(&min_area_covered);
   
   
   /* get the list of areas being considered */
   
   set_boundary_string(prod_descr.resolutionLevel, boundary_type);   
   sprintf(where, " WHERE boundary_type = '%s' ORDER BY area_id ",
	   boundary_type);
   
   gaHead = GetGeoArea(where);
   
   
   /* get the info on the radar area being considered */
   
   find_radar_coords(prod_spec.sourceId, &south_row, &west_col, &numbins_230);
   
   
   /* from the count of the number of areas being considered,
      allocate memory for holding info on each area */
   
   if (gaHead == NULL)
   {
      *num_areas = 0;
      return(NULL);
   }
   
   else
   {
      *num_areas = ListCount(&gaHead->list);
      
      ffm_stats = (FfmSummaryStats *)
	 malloc(sizeof(FfmSummaryStats) * (*num_areas));
      if (ffm_stats == NULL)
      {
	 fprintf(stderr, "Error in malloc of ffm_stats in bld_summary_stats().\n");
	 exit(0);
      }
   }
   
   
   /* now loop on the areas and compute the data for the area */
   
   index = 0;
   gaPtr = (GeoArea *) ListFirst(&gaHead->list); 
   
   while (gaPtr)
   { 
      
      /* load in the data that is always available */ 
      
      strcpy(ffm_stats[index].area_id, gaPtr->area_id);
      strcpy(ffm_stats[index].name,    gaPtr->name);
      
      strcpy(ffm_stats[index].boundary_type, boundary_type);
      
      
      /* get the linesegs info for the area */
      
      sprintf(where, " WHERE area_id = '%s' ", gaPtr->area_id);      
      lsHead = GetLineSegs(where);

      numrows = ListCount(&lsHead->list);
     
      if (lsHead != NULL)
      {	 	 
         /* extract the grid bin info for this area */
	 
         rows      = (long *)lsHead->hrap_row;
	 beg_cols  = (long *)lsHead->hrap_beg_col;
	 end_cols  = (long *)lsHead->hrap_end_col;

	 /* load in the linesegs table info */
	 
	 ffm_stats[index].area = lsHead->area;
	 
	 
	 /* now compute the real data for this area */
	 
	 for (j = 0; j < num_durations; j++)
	 {	 
	    
	    /* compute the mean areal precip value from the grid */
	    
	    if (products[j].precip_status >= 0)
	    {
	       compute_ma_val(logall_flag, south_row, west_col, numbins_230,
			      products[j].precip_grid, zero_flag,
			      gaPtr->area_id, numrows, rows, beg_cols, end_cols,
			      &avg_val, &max_val, &min_val, &percent_valid,
			      &status);
	       
	       if (status >= 0 && percent_valid > min_area_covered)
	       {
		  ffm_stats[index].dur_stats[j].precip     = avg_val;
		  ffm_stats[index].dur_stats[j].precip_max = max_val;
		  ffm_stats[index].dur_stats[j].precip_min = min_val;
	       }
	       else
	       {
		  ffm_stats[index].dur_stats[j].precip     = MISSING;
		  ffm_stats[index].dur_stats[j].precip_max = MISSING;
		  ffm_stats[index].dur_stats[j].precip_min = MISSING;
	       }
	    }
	    
	    else
	    {
	       ffm_stats[index].dur_stats[j].precip     = MISSING;
	       ffm_stats[index].dur_stats[j].precip_max = MISSING;
	       ffm_stats[index].dur_stats[j].precip_min = MISSING;
	    }
	    
	    
	    
	    /* compute the ffg value from the grid. for ffg data, don't
	       do anything with the max and min ffg returned */
	    
	    if (products[j].ffg_status >= 0)
	    {
	       compute_ma_val(logall_flag, south_row, west_col, numbins_230,
			      products[j].ffg_grid, zero_flag,
			      gaPtr->area_id, numrows, rows, beg_cols, end_cols,
			      &avg_val, &max_val, &min_val, &percent_valid,
			      &status);
	       
	       if (status >= 0 && percent_valid > min_area_covered)
		  ffm_stats[index].dur_stats[j].ffg = avg_val;
	       else
		  ffm_stats[index].dur_stats[j].ffg = MISSING;
	    }
	    
	    else
	       ffm_stats[index].dur_stats[j].ffg = MISSING;
	    	    
	    
	    /* compute the derived data */
	    	    
	    if (ffm_stats[index].dur_stats[j].precip != MISSING)
	       ffm_stats[index].dur_stats[j].rate =
		  ffm_stats[index].dur_stats[j].precip / hr_durations[j];
	    else
	       ffm_stats[index].dur_stats[j].rate = MISSING;
	    
	    if (ffm_stats[index].dur_stats[j].precip != MISSING &&
		ffm_stats[index].dur_stats[j].ffg != MISSING)
	    {
	       ffm_stats[index].dur_stats[j].diff    =
		  ffm_stats[index].dur_stats[j].precip -
		  ffm_stats[index].dur_stats[j].ffg;
	       
	       if (ffm_stats[index].dur_stats[j].ffg == 0.0)
		  ffm_stats[index].dur_stats[j].percent = 0.0;
	       else
		  ffm_stats[index].dur_stats[j].percent = 
		     100. * (ffm_stats[index].dur_stats[j].precip /
		     ffm_stats[index].dur_stats[j].ffg);
	       
	    }
	    
	    else
	    {
	       ffm_stats[index].dur_stats[j].diff    = MISSING;
	       ffm_stats[index].dur_stats[j].percent = MISSING;
	    }
	    	    
	 }  /* end of loop on durations */ 
	 
	 
	 /* find the index to most critical durations for this area */
	 
	 find_critical_durs(ffm_stats, index, num_durations);
	 	 
	 
	 /* increment the index to the current area */
	 
	 index++;
	 
	 /* free the line segs info */
	 
	 if (lsHead != NULL)
	 {
	    FreeLineSegs(lsHead);
	    lsHead = NULL;
	 }
      }  /* end of if check on LineSegs info */
      
      
      /* for areas that do not have any linesegs info, need to set
	 all the derived values to missing */
      
      else
      {
	 set_stats_missing(ffm_stats, index, num_durations);
      }
       
      
      /* process the next area in the list */
      
      gaPtr = (GeoArea *) ListNext(&gaPtr->node);
   }
   
   
   /* now reset the number of areas to the index since
      some areas may not have lineseg info defined for them;
      although this shouldn't happen */
   
   *num_areas = index;
      
   if (gaHead != NULL)
   {
      FreeGeoArea(gaHead);
      gaHead = NULL;
   }
   
   
   return(ffm_stats);   
}


/*********************************************************************
   
   ********************************************************************/

void find_critical_durs(FfmSummaryStats		*ffm_stats,
			int			area_index,
			int			num_durations)
{
   int		j;
   int		rate_index, percent_index, diff_index;
   float	rate_max, percent_max, diff_max;
   float 	value;
   
   
   /* initialize */
   
   rate_max   = percent_max   = diff_max   = -88888.;
   rate_index = percent_index = diff_index = 0;
   
   
   /* loop on the durations and check the values of 
      each type */
   
   
   for (j = 0; j < num_durations; j++)
   {
      value = ffm_stats[area_index].dur_stats[j].rate;      
      
      
      if (value != MISSING && value > rate_max)
      {
	 rate_index = j;
	 rate_max   = value;
      }
      
      value = ffm_stats[area_index].dur_stats[j].percent;
      if (value != MISSING && value > percent_max)
      {
	 percent_index = j;
	 percent_max   = value;
      }
      
      value = ffm_stats[area_index].dur_stats[j].diff;
      if (value != MISSING && value > diff_max)
      {
	 diff_index = j;
	 diff_max   = value;
      }      
   }
   
   
   /* now load the determined values */
   
   ffm_stats[area_index].rate_index    = rate_index;
   ffm_stats[area_index].percent_index = percent_index;
   ffm_stats[area_index].diff_index    = diff_index;
   
   return;
}


/*********************************************************************
   
   ********************************************************************/

void set_stats_missing(FfmSummaryStats		*ffm_stats,
		       int			area_index,
		       int			num_durations)
{
   int j;

   
   ffm_stats[area_index].area = 0.00;
   
   ffm_stats[area_index].percent_index = MISSING;
   ffm_stats[area_index].diff_index    = MISSING;
   ffm_stats[area_index].rate_index    = MISSING;
   
   for (j = 0; j < num_durations; j++)
   {	 
      ffm_stats[area_index].dur_stats[j].precip = MISSING;
      ffm_stats[area_index].dur_stats[j].ffg    = MISSING;
      
      ffm_stats[area_index].dur_stats[j].rate    = MISSING;
      ffm_stats[area_index].dur_stats[j].diff    = MISSING;
      ffm_stats[area_index].dur_stats[j].percent = MISSING;
   }
   
   return;
}
