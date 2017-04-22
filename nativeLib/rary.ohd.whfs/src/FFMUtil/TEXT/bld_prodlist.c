#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>

#include "GeneralUtil.h"
#include "LoadUnique.h"
#include "DPARadar.h"
#include "DbmsDefs.h"

#include "time_defs.h"
#include "time_convert.h"

#include "ArealProductSettings.h"
#include "FfmUtils.h"


/***********************************************************************

   bld_prodlist()
   
   PURPOSE
   This function takes a user rquest for products, and return a list
   of products.
   
   Specifically, the request is in the form of a product descriptor, 
   which describes the request in terms of the mode (precip, ffg, or
   comparison), type (stg1radar, etc.), and resolution 
   (grid, basin, etc.).  An option exists to only list the products
   of one-hour duration.  Also allowed as a part of the request is an
   optional list of user specified (i.e. preset) specifiers instructing
   this function to look for products for a specified time range.
   The preset time ranges are then checked to see if data exist, and
   if so the requested range is incorporated into the list.
   This feature applies only to stage 1 and stage 2 products.  
   Normally, the list includes what is actually in the database.
   Also as part of the list is the incorporation of a single slot
   in the list for a user-customizable time range, controllable through
   the gui interface.
   
   *********************************************************************/

void bld_prodlist(ArealProductTypeDescriptor	prod_descr,
		  int				one_hour_only,
		  ArealProductSpecifier		*preset_specs,
		  int				num_preset_specs,
		  int				max_specs,
		  ArealProductSpecifier 	*specs,
		  int				*num_specs)
{
     
   /* initialize */
   *num_specs = 0;
   
   /* process the request */
   if (prod_descr.mode == PRECIP_MODE)
   {       
      if (prod_descr.precipType == STAGE1_PRECIP)
      {	 	 
	 bld_stg1list(prod_descr, one_hour_only, preset_specs, num_preset_specs,
		      max_specs, specs, num_specs);	 
      }
      
      /* currently, won't support point gage source when in
	 precip mode, only in comparison mode */

#ifdef ELVIS     
      else if (prod_descr.precipType == POINT_GAGE_PRECIP)
      {
	 /* just in case */
	 
	 if (prod_descr.resolutionLevel == GRID_RES)
	 {
	    fprintf(stderr,
		    "bld_prodlist: PtGages NOT supported for grids.\n");
	    return
	 };
	 
	 
	 bld_ptpreciplist(max_specs, specs, num_specs);
      }
#endif  
      
      /* build a list based on the values in the FcstPrecip table.
	 note that the sourceId will be dependent upon whether
	 in grid resolution or not. */
      
      else if (prod_descr.precipType == QPF_PRECIP)
      {	 
	 bld_qpflist(prod_descr, max_specs, specs, num_specs); 
      }
   }
   
   
   /* when in FFG mode, the precip source is not involved.
      grid resolution is allowed - it uses the latest ffg found.
      note that only one entry is shown per duration for the gridded 
      ffg data; no attempt is made to see if the ffg data exist under
      multiple radar umbrellas.  this means that ffg data that may exist
      for a given duration is listed, but when the product is selected, no
      data may be found since the ffg areas may be outside the radar
      umbrella being considered. */
   
   else if (prod_descr.mode == FFG_MODE)
   {
      if (prod_descr.resolutionLevel != GRID_RES)
	 bld_ffglist(prod_descr, max_specs, specs, num_specs);
      
      else
	 bld_ffglist_hrs_avail(prod_descr, max_specs, specs, num_specs);
   }
   
   
   /* if in comparison mode, then build the list of precip data
      to compare against, then filter out those that don't have
      any ffg for that duration */
   
   else if (prod_descr.mode == COMPARISON_MODE)
   {     
      bld_comparisonlist(prod_descr, one_hour_only,
			 preset_specs, num_preset_specs,
			 max_specs, specs, num_specs);      
   }
      
   
   return;
}


/*******************************************************************
   bld_stg1list()	
   
   PURPOSE
   Builds a list of stage 1 products.
   
   *****************************************************************/

void bld_stg1list(ArealProductTypeDescriptor	prod_descr,
		  int				one_hour_only,
		  ArealProductSpecifier		*preset_specs,
		  int				num_preset_specs,
		  int				max_specs,
		  ArealProductSpecifier 	*specs,
		  int				*num_specs)
   
{
   DPARadar	*dpaHead, *dpaPtr;
   UniqueList	*ulHead, *ulPtr;
   int		ul_count;   
   char 	where[200];
   int		num_grids;
   int		cnt, i;
   int		status, dpa_ok;
   time_t	temp_timet;
   ArealDataStatus	*preset_dataStatus;
   int		minute_window;
   int		hr_lookback = DEFAULT_STAGE1LIST_LOOKBACK;
   
   
   /* initialize */
   
   cnt = *num_specs = 0;
   
   
   /* get the value for the minute window to use in determining
      whether the stage 1 grid is near the top of the hour */
   
   get_stage1_window(&minute_window);
   
   
   /* get a list of unique radar ids for which operational
      data does exist */
   
   strcpy(where, "  WHERE supplmess IN (0,4) ");
   ulHead = LoadUnique("radid", "DPARadar", where, &ul_count);	 
   if (ul_count == 0) return;
   
   
   /* loop on the number of radars with data */
   
   ulPtr = (UniqueList *) ListFirst(&ulHead->list);
   while (ulPtr)
   {
      
      /* get all the radar grids in the DPARadar table for the current radar */
      
      sprintf(where,
	      " WHERE radid = '%s' AND supplmess IN (0,4) ORDER BY obstime DESC ",
	      ulPtr->uchar);
      dpaHead = GetDPARadar(where);
      
      if (dpaHead == NULL)
	 num_grids = 0;
      else
	 num_grids = ListCount(&dpaHead->list);
      
      if (num_grids > 0)
      {
	 /* first load the default/custom user specifier as a
	    choice in the list */
	 /*
	 set_default_prodtime(&user_time_default, &user_duration_default);
	 
	 specs[cnt].endTime  = user_time_default;
	 specs[cnt].duration = user_duration_default;
	 
	 memset(specs[cnt].sourceId, 0, SOURCE_ID_LEN);
	 strncpy(specs[cnt].sourceId, ulPtr->uchar, RADAR_ID_LEN);
	 
	 cnt++;
	 */
	 
	 /* now load the grids that are indeed in the database */
	 
	 dpaPtr = (DPARadar *) ListFirst(&dpaHead->list);
	 while (dpaPtr)
	 {
	    /* only include grids that are either around the top of the
	       hour, or are recent grids - within the last X hours */
	    
	    status = yearsec_dt_to_timet(dpaPtr->obstime, &temp_timet);
	    dpa_ok = check_recent_onhr(temp_timet, minute_window, hr_lookback);
	    
	    
	    /* if grid is to be considered, load it in list now */
	    
	    if (dpa_ok && cnt < max_specs)
	    {	       
	       specs[cnt].endTime  = temp_timet;
	       specs[cnt].duration = SECONDS_PER_HOUR;
	       
	       memset(specs[cnt].sourceId, 0, SOURCE_ID_LEN + 1);
	       strncpy(specs[cnt].sourceId, ulPtr->uchar, RADAR_ID_LEN);
	       
	       if (dpaPtr->supplmess == 0 || dpaPtr->maxvald == 0.0)
		  specs[cnt].dataStatus  = ALL_ZERO;
	       else
		  specs[cnt].dataStatus  = NON_ZERO;
	       
	       cnt++;
	    }
	    
	    dpaPtr = (DPARadar *) ListNext(&dpaPtr->node);     
	 }
	 
	 FreeDPARadar(dpaHead);	       
	 
	 
	 /* now load in the preset times if needed. need to check that data for
	    each does exist.  */
	 
	 if (num_preset_specs > 0)
	 {
	    preset_dataStatus =
	       ( ArealDataStatus * ) 
               malloc (sizeof ( ArealDataStatus ) * ( num_preset_specs ) ) ;
	    if (preset_dataStatus == NULL)
	    {
	       printf("Error in malloc of preset_dataStatus in bld_stg1list.\n");
	       return;
	    }
	    
	    
	    /* before checking the availability of the presets, the 
	       source id needs to be set accordingly for this pass. */
	    
	    for (i = 0; i < num_preset_specs; i++)
	    {
	       memset(preset_specs[i].sourceId, 0, SOURCE_ID_LEN + 1);
	       strncpy(preset_specs[i].sourceId, ulPtr->uchar, RADAR_ID_LEN);
	    }
	    
	    
	    /* this function only supports checking for grids from one
	       source/radar per call */
	    
	    check_precipgrid_prods(prod_descr, preset_specs, num_preset_specs, 
				   preset_dataStatus);
	    
	    
	    /* load in those stage1 grids that are available to the list */
	    
	    for (i = 0; i < num_preset_specs; i++)
	    {
	       
	       if (preset_dataStatus[i] != MISSING_STATUS &&
		   cnt < max_specs)
	       {		     
		  specs[cnt].endTime  = preset_specs[i].endTime;
		  specs[cnt].duration = preset_specs[i].duration;
		  
		  memset(specs[cnt].sourceId, 0, SOURCE_ID_LEN + 1);
		  strncpy(specs[cnt].sourceId, preset_specs[i].sourceId,
			  RADAR_ID_LEN);
		  
		  
		  /* indicates whether data grid is all zero values
		     or has non-zero values */
		  
		  specs[cnt].dataStatus = preset_dataStatus[i];
		  
		  cnt++;
	       }
	    }
	    
	    
	    /* free the preset status array */
	    
	    if (preset_dataStatus != NULL) free(preset_dataStatus);
	 }
	 	  
      }  /* end of if on whether data found for this unique radar */
      
      
      /* go and process next radar source id */
      
      ulPtr = (UniqueList *) ListNext(&ulPtr->node); 
      
   }
   
   
   /* load the returned variable */
   
   *num_specs = cnt;
   
   FreeUnique(ulHead);	 
   
   return;
}


/*******************************************************************
   bld_qpflist()	
   
   PURPOSE
   Build a list for forecast precip products based on the
   unique(validtime, dur) values from the FcstPrecip table.
   This function should not be called if handling gridded
   qpf request.
      
   
   *****************************************************************/

void bld_qpflist(ArealProductTypeDescriptor	prod_descr,
		 int				max_specs,
		 ArealProductSpecifier		*specs,
		 int				*num_specs)
{
   UniqueList	*ulHead, *ulPtr;
   int		ul_count;
   char		where[220];
   int 		cnt;
   char 	ansi_timestr[ANSI_YEARSEC_TIME_LEN + 1];
   int		shefdur;
   int		status1, status2;
   time_t	valid_timet, dur_timet;
   char		boundary_type[40];

   int          len=0, rlen=0, istatus=0;
   char        ** values = NULL;
   char         rdir_path[128];

   
   
   /* initialize */
   cnt = *num_specs = 0;
   
   
   /* get the primary radar for use later. 
      also set the boundary type */
   
   if (prod_descr.resolutionLevel == GRID_RES)
   {
      len = strlen("whfs_primary_radar");
      istatus = get_apps_defaults("whfs_primary_radar", &len, rdir_path, &rlen);

      if (istatus != 0)
      {
	 fprintf(stderr,
		 "whfs_primary_radar undefined. QPF durs check aborted.\n");
	 return;
      }
      
      set_boundary_string(BASIN_RES, boundary_type);     
   }
   
   else
   {     
      set_boundary_string(prod_descr.resolutionLevel, boundary_type);     
   }
   
   
   /* get a list of unique qpf data sets; note the sort order
      and how it iteracts with the two fields being queried  */
   
   sprintf(where, " WHERE lid IN (SELECT area_id FROM geoarea "
	   " WHERE boundary_type = '%s')  ORDER BY 1 DESC ",
	   boundary_type);
   ulHead = LoadUnique("validtime||dur", "FcstPrecip", where, &ul_count);
   if (ul_count == 0) return;
   
   
   /* loop on the number of unique entries */
   
   ulPtr = (UniqueList *) ListFirst(&ulHead->list);
   while(ulPtr)
   {
      /* extract the fields from the unique string returned */
      values = ParseUnique ( ulPtr, &ul_count );

      if ( ( values == NULL ) || ( ul_count < 2 ) )
      {
         /* Failed to parse the unique string. */
         break;
      }

      memset ( ansi_timestr, '\0', ANSI_YEARSEC_TIME_LEN + 1 );
      strncpy ( ansi_timestr, values[0], ANSI_YEARSEC_TIME_LEN);
      shefdur = atoi ( values [1] ); 
      FreeParseUnique ( values );

      /* now convert the times */
      status1 = yearsec_ansi_to_timet(ansi_timestr, &valid_timet);
      if (status1 < 0) 
	 printf("Error %d converting time in bld_qpflist.\n", status1);
      
      status2 = shefdur_to_timet(shefdur, valid_timet, &dur_timet);
      if (status2 < 0) 
	 printf("Error %d converting duration in bld_qpflist.\n", status2);
      
      if (status1 >= 0 && status2 >= 0 && cnt < max_specs)
      {
	 specs[cnt].endTime  = valid_timet;
	 specs[cnt].duration = dur_timet;
	 
	 
	 if (prod_descr.resolutionLevel == GRID_RES)
	    strcpy(specs[cnt].sourceId, rdir_path);
	 
	 else
	    strcpy(specs[cnt].sourceId, " - ");
	 
	 
	 specs[cnt].dataStatus = NOT_APPLICABLE;
	 
	 cnt++;
      }
      
      
      /* go and process next unique qpf data set */
      
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);     	          
   }
   
   FreeUnique(ulHead);
   
   
   *num_specs = cnt;
   
   return;

}


/*******************************************************************
   bld_ptpreciplist()	
   
   PURPOSE
   Build a list for observed precip products based on the
   unique(validtime, dur) values from the PrecipAccum table.
      
   
   *****************************************************************/

void bld_ptpreciplist(int			max_specs,
		      ArealProductSpecifier	*specs,
		      int			*num_specs)
{
   UniqueList	*ulHead, *ulPtr;
   int		ul_count;
   char		where[120];
   int 		cnt;
   int		status;
   time_t	valid_timet;
   int		ptprecip_durs[] = {1, 3, 6, 12, 24, 48, 72};
   int		i;
   
   
   /* initialize */
   
   cnt = *num_specs = 0;
         
   
   /* get a list of unique data sets; note the sort order
      and how it interacts with the two fields being queried  */
   
   sprintf(where, "  ORDER BY 1 DESC ");
   ulHead = LoadUnique("basistime", "PrecipAccum", where, &ul_count);	 
   if (ul_count == 0) return;
   
   
   /* loop on the number of unique entries */
   
   ulPtr = (UniqueList *) ListFirst(&ulHead->list);
   while(ulPtr)
   {            
      /* now convert the times */
      
      status = yearsec_ansi_to_timet(ulPtr->uchar, &valid_timet);
      if (status < 0) 
	 printf("Error %d converting time in bld_ptpreciplist.\n", status);
      
      
      if (status >= 0)
      {
	 for (i = 0; i < NUM_PTPRECIP_DURS; i++)
	 {
	    
	    if (cnt < max_specs)
	    {
	       specs[cnt].endTime  = valid_timet;
	       specs[cnt].duration = ptprecip_durs[i] * SECONDS_PER_HOUR;
	       
	       /* note: pt precip list/display does not consider
		  Gridded resolution */
	       
	       strcpy(specs[cnt].sourceId, " - ");
	       specs[cnt].dataStatus = NOT_APPLICABLE;
	       
	       cnt++;
	    }
	 }
      }
      
      
      /* go and process next unique qpf data set */
      
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);     	          
   }
   
   FreeUnique(ulHead);
   
   
   *num_specs = cnt;
   
   return;

}


/*******************************************************************
   bld_ffglist()
   
   PURPOSE
   Builds a list of available ffg products for the requested
   boundary_type (basin, county, zone).  Do not use this 
   function for handling gridded FFG requests.
   
   *****************************************************************/

int bld_ffglist(ArealProductTypeDescriptor	prod_descr,
		int				max_specs,
		ArealProductSpecifier		*specs,
		int				*num_specs)
{
   UniqueList	*ulHead, *ulPtr;
   int		ul_count;
   char		where[220];
   int 		cnt;
   char 	ansi_timestr[ANSI_YEARSEC_TIME_LEN + 1];
   int		shefdur;
   int		status1, status2;
   time_t	valid_timet, dur_timet;
   char		boundary_type[40];
   char         ** values = NULL;
   
   /* initialize */
   cnt = *num_specs = 0;
  
   /* set the boundary type string for the database query */
   set_boundary_string(prod_descr.resolutionLevel, boundary_type);
   
   sprintf(where, 
           " WHERE pe='PP' AND ts='CF' AND lid IN (SELECT area_id FROM"
           " GeoArea where boundary_type = '%s') ORDER BY 1 DESC", 
           boundary_type ) ;
   
   ulHead = LoadUnique("validtime||dur", "ContingencyValue", where, 
                       &ul_count);	 
   if (ul_count == 0)
      return(-1);
   
   /* loop on the number of unique entries */
   
   ulPtr = (UniqueList *) ListFirst(&ulHead->list);
   while (ulPtr)
   {
      /* extract the fields from the unique string returned */
      values = ParseUnique ( ulPtr, &ul_count );  

      if ( ( values == NULL ) || ( ul_count < 2 ) )
      {
          /* Failed to parse the unique string. */
          break;
      }

      memset(ansi_timestr, '\0', ANSI_YEARSEC_TIME_LEN + 1);
      strncpy(ansi_timestr, values[0], ANSI_YEARSEC_TIME_LEN);
      shefdur = atoi ( values[1] ); 
      FreeParseUnique ( values );
      
      /* now convert the times */
      
      status1 = yearsec_ansi_to_timet(ansi_timestr, &valid_timet);
      if (status1 < 0) 
	 printf("Error %d converting time in bld_ffglist.\n", status1);
      
      status2 = shefdur_to_timet(shefdur, valid_timet, &dur_timet);
      if (status1 < 0) 
	 printf("Error %d converting duration in bld_ffglist.\n", status1);
      
      if (status1 >= 0 && status2 >= 0 && cnt < max_specs)
      {
	 specs[cnt].endTime  = valid_timet;
	 specs[cnt].duration = dur_timet;
	 
	 
	 /* for now, don't assign the source id since it
	    is not meaningful for non-gridded areas. */
	 	 
	 strcpy(specs[cnt].sourceId, " - ");
	 
	 specs[cnt].dataStatus = NOT_APPLICABLE;
	 
	 cnt++;
      }
      
      
      /* go and process next unique ffg data set */
      
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);     	          
   }
   
   FreeUnique(ulHead);
   
   *num_specs = cnt;
   
   
   return(0);
   
}


/*******************************************************************
   bld_ffglist_hrsavail()
   
   PURPOSE
   Return a set of specifiers that contain ffg data for the
   internal set of ff durations
   
   *****************************************************************/

int bld_ffglist_hrs_avail(ArealProductTypeDescriptor	prod_descr,
		  	  int				max_specs,
			  ArealProductSpecifier		*specs,
			  int				*num_specs)
{
   int		found;
   int		i, status;
   int		cnt;
   int		num_ffgdurs = NUM_FFG_DURS;
   int		ffgdurs[] = {1, 3, 6, 12, 24};
   time_t	cur_timet, lookback_timet;
   time_t	valid_timet;

   int          len=0, rlen=0, istatus=0;
   char         rdir_path[128];

   
   
   /* initialize */
   
   cnt = *num_specs = 0;
   
   
   /* get the primary radar for use below */
   

   len = strlen("whfs_primary_radar");
   istatus = get_apps_defaults("whfs_primary_radar", &len, rdir_path, &rlen);

   if (istatus != 0)
   {
      fprintf(stderr,
	      "whfs_primary_radar undefined. FFG durs check aborted.\n");
      return(-1);
   }
   
   
   /* set the lookback time for allowing FFG data */
   
   time(&cur_timet);
   lookback_timet = cur_timet - (MAXHRS_FFG_LOOKBACK * SECONDS_PER_HOUR); 
      
   
   /* loop on the FFG durations to check for */
   
   for (i = 0; i < num_ffgdurs; i++)
   {
      
      /* now get the latest time for the ffg so that there is at 
	 least some valid time shown with the nebulus data set */
      
      found = check_for_ffg(prod_descr.resolutionLevel,
			    lookback_timet, ffgdurs[i]);
      if (found)
      {
	 status = get_latest_ffgtime(lookback_timet, ffgdurs[i], &valid_timet);
	 if (status >= 0  && cnt < max_specs)
	 {
	    specs[cnt].endTime    = valid_timet;
	    specs[cnt].duration   = ffgdurs[i] * SECONDS_PER_HOUR;
	    
	    strcpy(specs[cnt].sourceId, rdir_path);
	    
	    specs[cnt].dataStatus = NOT_APPLICABLE;
	    
	    cnt++;
	 }
      }
   }
        
   *num_specs = cnt;
      
   return(0);  
}


/*******************************************************************
   bld_comparisonlist()	
   
   PURPOSE
   Build list of available precip products that can be compared to 
   FFG data.  Only precip products whose durations match available
   FFG data durations are accepted.  Note that this is somewhat
   inprecise since the times of the FFG are not compared
   against the times of the precip data to ensure that ffg data
   around the time of the precip time are available.  Therefore,
   there may be product entries in the comparison list that will
   not have valid ffg data to allow the comparison to occur.
       
   *****************************************************************/

void bld_comparisonlist(ArealProductTypeDescriptor	prod_descr,
			int				one_hour_only,
			ArealProductSpecifier		*preset_specs,
			int				num_preset_specs,  
		        int				max_specs,
			ArealProductSpecifier 		*specs,
			int				*num_specs)   
{   
   ArealProductSpecifier	*ffg_specs;
   int				num_ffg_specs;
   int				i, j;
   int				dur_found;
   int				spec_index;
   int				new_cnt;
   
   
   /* initialize */
   
   *num_specs = num_ffg_specs = 0;
   
    
   /* check and see which hour durations there are recent
      enough FFG data.  */
   
   ffg_specs = (ArealProductSpecifier *) 
      malloc(sizeof(ArealProductSpecifier) * NUM_FFG_DURS);
   if (ffg_specs == NULL)
   {
      printf("Error in malloc of ffg_specs in bld_comparisonlist\n");
      return;
   }
   
   /* get the list of ffg durations available. 
      NEED to check for data under the radar umbrella !!! */
   
   bld_ffglist_hrs_avail(prod_descr, NUM_FFG_DURS, ffg_specs, &num_ffg_specs);
   
   
   if (num_ffg_specs == 0)
   {
      printf("No FFG data found in bld_comparisonlist.\n");
      free(ffg_specs);
      return;
   }
   
   
   /* now go and get the list of precip data sets that are
      considered, using the exact same means as when in precip mode */  
   
   if (prod_descr.precipType == STAGE1_PRECIP)
   {
	 bld_stg1list(prod_descr, one_hour_only, 
		      preset_specs, num_preset_specs,
		      max_specs, specs, num_specs);	 
   }
   
   /* for point gage data, use only the available point gage
      data accumulated in the precipaccum data sets */

#ifdef ELVIS 
   else if (prod_descr.precipType == POINT_GAGE_PRECIP)
   {
      bld_ptpreciplist(max_specs, specs, num_specs);
   }
#endif 
   
   /* for fcst precip, use the same exact method as when building
      the list.  later, possible add methods to filter
      out in-the-past qpf data */
   
   else if (prod_descr.precipType == QPF_PRECIP)
   {
      bld_qpflist(prod_descr, max_specs, specs, num_specs);      
   }

      
   
   /* filter out those precip data sets which have durations 
      for which there is no similar duration FFG */
   
   new_cnt    = 0;
   spec_index = 0;
   
   for (i = 0; i < *num_specs; i++)
   {
      
      /* first see if there is ffg data for the duration of the data */
      
      dur_found = 0;
      for (j = 0; j < num_ffg_specs; j++)
      {
	 if (specs[i].duration == ffg_specs[j].duration)
	 {
	    dur_found = 1;
	    break;
	 }
      }
      
      
      /* if ffg data exists for the duration that the precip is for, then
	 keep the data in the list.  rather than building a new list, simply
	 load the same list with data from later rows of the list */
      
      if (dur_found) 
      {
	 specs[spec_index].endTime    = specs[i].endTime;
	 specs[spec_index].duration   = specs[i].duration;
	 strcpy(specs[spec_index].sourceId, specs[i].sourceId);
	 specs[spec_index].dataStatus = specs[i].dataStatus;
	 
	 spec_index++;
	 new_cnt++;
      }      
   }
   
   
   /* update the count before returning, and free memory */
   
   *num_specs = new_cnt;
   free(ffg_specs);
   
   return;
}


/*******************************************************************
   set_boundary_string()	
   
   PURPOSE
   Sets the boundary_type string for the database query of 
   the GeoArea table.
   
   *****************************************************************/
void set_boundary_string(ResolutionLevel	res,
			 char			*boundary_type)
{
   
   if (res == COUNTY_RES)
      strcpy(boundary_type, "COUNTY");
   
   else if (res == ZONE_RES)
      strcpy(boundary_type, "ZONE");
   
   else if (res == BASIN_RES)
      strcpy(boundary_type, "BASIN");
   
   else if (res == GRID_RES)
      strcpy(boundary_type, "INVALID");
   
   else 
      strcpy(boundary_type, "INVALID");
   
   return;
}


/*******************************************************************
   set_default_prodtime()	
   
   PURPOSE
   Sets the defaults for the user controlled time.
       
   *****************************************************************/

void set_default_prodtime(time_t	*endTime,
			  time_t	*duration)
{
   time_t	curtime;
  
   
   /* round down to the top of the nearest hour */
   
   time(&curtime);  
   *endTime = (long )(curtime / SECONDS_PER_HOUR) * SECONDS_PER_HOUR;
   
   
   /* use a 6 hour duration as the default */
   
   *duration = 6 * SECONDS_PER_HOUR;
   
   return;
}
