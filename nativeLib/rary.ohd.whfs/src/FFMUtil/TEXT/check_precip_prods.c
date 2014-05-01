#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "ArealProductSettings.h"
#include "FfmUtils.h"

#include "time_convert.h"
#include "time_defs.h"

#include "DbmsDefs.h"
#include "DPARadar.h"


/*******************************************************************
   
   This function checks for the availability of gridded precip data
   sets matching a specified product type - where a product class is
   defined in terms of the ArealProductTypeDescriptor, which gives the
   1) mode (i.e. precip, ffg, comparison),
   2) type (stg1, stg2, etc.), and 
   3) resolution (grid, county, basin, etc.).
   
   ONLY sypports the modes: precip and comparison, 
   and the types: stg1, stg2gage, stg2gageradar
   
   The specifiers for each particular data set is given in 
   ArealProductSpecifier; for each product requested, it provides the 
   1) time ranges (ending time and duration), and
   2) sources (e.g. radar id)   
   A count of the number of grids which correspond to this set
   of products is also provided.  This count must correspond to the
   number of elements in the array of specifiers passed in.
   
   For each call to this function, it is ASSUMED that there is
   only ONE source identifier for all the specifiers.
   
   This function then uses this information to determine which
   products are available and returns a boolean array with count
   elements that indicate whether the requested products are
   available.
   
   The returned array MUST be allocated by the calling program.
   
   *****************************************************************/
int check_precipgrid_prods(ArealProductTypeDescriptor	class_descr,
			   ArealProductSpecifier	*prod_spec,
			   int				num_prodspecs,
			   ArealDataStatus		*product_status)
{
   int			i;
   time_t		endTime, startTime;
   ArealGridStatus	*gridstatus_list;
   int			num_grids;
   float		min_percent_duration;
   int			minutes_window;
   
   
   /* just in case */
   
   if (num_prodspecs <= 0)
      return(-1);
   
   
   /* initialize */
   
   for (i = 0; i < num_prodspecs; i++)
      product_status[i] = MISSING_STATUS;
   
   
   /* get the minimum percent duration allowed.
      units are actually not percent; it ranges from 0.0-1.0 */
   
   get_min_duration(&min_percent_duration);
   
   
   /* get the minutes window for stage1 products */
   
   get_stage1_window(&minutes_window);
   
   
   /* get the time range spanned by the requested data */
   
   scan_spec_timerange(prod_spec, num_prodspecs, &startTime, &endTime);
   
   
   /* if in precip mode */
   
   if (class_descr.mode == PRECIP_MODE || class_descr.mode == COMPARISON_MODE)
   {
      
      if (class_descr.precipType == STAGE1_PRECIP ||
	  class_descr.precipType == STAGE2_GAGE_ONLY_PRECIP ||
	  class_descr.precipType == STAGE2_GAGE_RADAR_PRECIP)
      {
	 
	 /* get an array of structures with the available grids
	    and their times and whether the grid has non-zero values.
	    this function also returns the number of grids found */
	 
	 gridstatus_list = load_precip_gridlist(class_descr.precipType,
						prod_spec[0].sourceId,
						startTime, endTime, 
						&num_grids);
	 
	 
	 /* now we know what data are available. now loop 
	    thru the specific products requests and check if the 
	    data are available for that requested time range.
	    need to free the list data that was previously allocated */
	 
	 if (num_grids > 0)
	 {
	    check_precip_avail(class_descr,
			       prod_spec, num_prodspecs,
			       prod_spec[0].sourceId,
			       min_percent_duration, minutes_window,
			       gridstatus_list, num_grids,
			       product_status);
	    
	    free(gridstatus_list);
	 }
	 
      }
      
#ifdef ELVIS      
      else if (class_descr.precipType == POINT_GAGE_PRECIP)
	 return(-1);
#endif    
	 
	 
      /* no support for qpf precip data */
	 
      else if (class_descr.precipType == QPF_PRECIP)
	 return(-1);
   }
   
   
   /* no support for ffg mode */
   
   else 
      return(-2);
   
  
   return(0);
}


/*******************************************************************
   scan_spec_timerange()	
   
   PURPOSE
   
   *****************************************************************/

void scan_spec_timerange(ArealProductSpecifier	*prod_spec,
			 int			num_prodspecs,
			 time_t			*startTime,
			 time_t			*endTime)
{
   int	i;
   
   
   /* initialize the time range using the first specifier */
   
   *endTime   = prod_spec[0].endTime;
   *startTime = (prod_spec[0].endTime - prod_spec[0].duration);
   
   
   /* go through all the other products and adjust the time 
      range so it fits all the product's time range. */
   
   for (i = 1; i < num_prodspecs; i++)
   {     
      if (prod_spec[i].endTime > *endTime)
	 *endTime = prod_spec[i].endTime;
      
      if ((prod_spec[i].endTime - prod_spec[i].duration) < *startTime)
	 *startTime = (prod_spec[i].endTime - prod_spec[i].duration);
   }
   
   
   /* now add an hour to each end of the time range.  this is done to
      support the stage1 non top-of-the-hour searches.  for the other
      stage2 products, having a slightly larger time range is not a problem. */
   
   *startTime = *startTime + SECONDS_PER_HOUR;
   *endTime   = *endTime   + SECONDS_PER_HOUR;
   
   
   return;
}


/*******************************************************************
   load_precip_gridlist()	
   
   PURPOSE
   Loads a list of available precip grids matching request.
   
   *****************************************************************/

ArealGridStatus * load_precip_gridlist(PrecipType	precip_type,
				       char 		*sourceId,
				       time_t		startTime,
				       time_t		endTime,
				       int		*num_grids)
{
   char		where[220];
   time_t       gridtime;   
   char		ansi_starttime[ANSI_YEARSEC_TIME_LEN + 1];
   char		ansi_endtime[ANSI_YEARSEC_TIME_LEN + 1];
   int		status;
   int		cnt;
   DPARadar	*dpaHead=NULL, *dpaPtr;
   ArealGridStatus	*gridstatus_list;
   
   
   /* initialize */
   
   *num_grids = 0;
   cnt = 0;
   
   
   /* convert the times */
   
   status = timet_to_yearsec_ansi(startTime, ansi_starttime);     
   status = timet_to_yearsec_ansi(endTime,   ansi_endtime);     
   
   
   /* get the database records with the gridded data */
   
   /* the stage1/dparadar supplmess flag values are 0= no precipitation
      detected; 1 = bad rate scan; 2 = not enough data in hour;
      3 = disk error; 4 = precipitation */
   
   if (precip_type == STAGE1_PRECIP)
   {
      sprintf(where,
	      " WHERE radid = '%s' AND obstime >= '%s' AND obstime <= '%s'  " 
	      " AND supplmess IN (0,4) ORDER BY obstime DESC ",
	      sourceId, ansi_starttime, ansi_endtime);
      dpaHead = GetDPARadar(where);
      
      if (dpaHead == (DPARadar *)NULL)
	 return(NULL);
      else
	 *num_grids = ListCount(&dpaHead->list);
   }
   
   
   /* the z indicates grid with all values of 0.0, the c indicates that
      the grid values were calculated, as opposed to not being calculated
      for various reasons, such as not enough input data */
      
   else
      return(NULL);
      
   
   /* allocate memory for the grids found */
   
   gridstatus_list = (ArealGridStatus *) malloc(sizeof(ArealGridStatus) * (*num_grids));
   if (gridstatus_list == NULL)
   {
      printf("Error in malloc of gridstatus_list in check_precip_grids.\n");
      *num_grids = 0;
      return(NULL);
   }   
   
   
   /* now load up the array of grids found, set the zero-nonzero status
      and free memory */
   
   if (precip_type == STAGE1_PRECIP)
   {
      dpaPtr = (DPARadar *) ListFirst(&dpaHead->list);
      while (dpaPtr)
      {
	 status = yearsec_dt_to_timet(dpaPtr->obstime, &gridtime);
	 gridstatus_list[cnt].grid_timet = gridtime;
	 
	 if (dpaPtr->supplmess == 0 || dpaPtr->maxvald == 0.0)
	    gridstatus_list[cnt].dataStatus = ALL_ZERO;
	 else
	    gridstatus_list[cnt].dataStatus = NON_ZERO;
	 
	 cnt++;
	 
	 dpaPtr = (DPARadar *) ListNext(&dpaPtr->node);
      }
      
      FreeDPARadar(dpaHead);
   }
   
   /* return */
   
   return(gridstatus_list);
}


/*******************************************************************
   check_precip_avail()
   
   PURPOSE
   For each of the requested products defined in prod_spec; the number
   of them are defined in num_prodspecs, see if data for the product exists
   by checking the status of each of the hourly products as defined
   in  hrs_status array.  Return the boolean on the products 
   availability in the product_status array elements.
   
   *****************************************************************/

void check_precip_avail(ArealProductTypeDescriptor 	class_descr,
			ArealProductSpecifier		*prod_spec,
			int				num_prodspecs,
			char				*sourceId,
			float				min_percent_duration,
			int				minutes_window,
			ArealGridStatus			*gridstatus_list,
			int				num_grids,
			ArealDataStatus			*product_status)
{
   int		i, j, k;
   time_t	prod_startTime, prod_endTime;
   time_t	prod_time;
   int		prod_numhrs;
   int		numhours_found;
   int		nonzero_found;
   int		hour_found;
   time_t	nearest_hour;
   onhour_flagtype	hr_flag;
   float	percent_found;
   
   
   /* loop on each of the specific products requested */
      
   for (i = 0; i < num_prodspecs; i++)
   {
      
      /* in this pass through this function, only consider
	 those requested products whose source id matches
	 the source id being processed */
      
      if (strcmp(prod_spec[i].sourceId, sourceId) == 0)
      {
	 
	 /* define the start and end times for this particular
	    requested product. use a local copy of the end time 
	    for convenience */
	 
	 prod_startTime = prod_spec[i].endTime - prod_spec[i].duration;
	 prod_endTime   = prod_spec[i].endTime;
	 prod_numhrs    = prod_spec[i].duration / SECONDS_PER_HOUR;
	 
	 nonzero_found = 0;
	 
	 
	 /* loop on each of the hours and check for the existence
	    of ALL hours */
	 
	 numhours_found = 0;
	 
	 for (j = 0; j < prod_numhrs; j++)
	 {
	    /* mjg - corrected this equation 8/12/98 */
	    prod_time = prod_startTime + ((j + 1) * SECONDS_PER_HOUR);
	    hour_found = 0;
	    
	    for (k = 0; k < num_grids; k++)
	    {
	       /* since here we are only building a LIST of available
		  products, as opposed to building the actual data in
		  the product, we are NOT worried here about WHICH
		  grid is the closest/best one, but only that one exists
		  which is close enough for the hour. */
	       
	       if (class_descr.mode == STAGE1_PRECIP)
	       {
		  check_onhour(gridstatus_list[k].grid_timet, minutes_window,
			       &hr_flag, &nearest_hour);
		  
		  if ((hr_flag == ON_HOUR || hr_flag == NEAR_HOUR) &&
		      prod_time == nearest_hour)
		     hour_found = 1;
	       }
	       
	       else
	       {
		  if (prod_time == gridstatus_list[k].grid_timet)
		     hour_found = 1;
	       }
	       
	       
	       /* break out of the inner loop not just to save time, 
		  but to avoid double-counting multiple stage1 grids
		  that may be within the allowable time window. */
	       
	       if (hour_found)
	       {
		  numhours_found++;		     
		  if (gridstatus_list[k].dataStatus == NON_ZERO)
		     nonzero_found++;
		  break;
	       }
	    }
	 }
	 
	 
	 /* based on the number of hours found, and on whether any
	    non-zero grids found, set the status accordingly */
	 
	 percent_found = ((float )numhours_found / (float )prod_numhrs);
	 
	 if (percent_found >= min_percent_duration)
	 {
	    if (nonzero_found == 0)
	       product_status[i] = ALL_ZERO;
	    else
	       product_status[i] = NON_ZERO;
	 }
	 
	 else
	    product_status[i] = MISSING_STATUS;
      }
      
      
   }
   
   return;
}


