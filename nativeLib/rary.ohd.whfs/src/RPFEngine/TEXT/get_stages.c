/*********************************************************************
   get_stages.c
   
   compute_fp_mofo_info()
   compute_fp_prev_info()
   compute_grp_county_mofo()
   
   get_fpmofo_and_ts_ifneeded()
   
   compute_stage_cat()
   check_stage_data()
     
   -----------------
   Short description:
      
   compute_fp_mofo_info() - computes combined obs fcst

   compute_fp_prev_info() - computes current vs previous product fields
   
   compute_grp_county_mofo() - computes grp and county omf values based
   on the forecast points data loaded via get?_mofo. This is called after
   get_fp_mofo_and_ts_ifneeded() and loads the omf data and compute the
   derived info.         
   
   get_fpmofo_and_ts_ifneeded()
      -get obs info by using get_rpf_curobs()      
      -uses bldts_obsriv() to load obs timeseries
      -load obs info when necessary
      - load_rpf_fcstdata_lidpe() to load forecast time series and find 
        the maximum forecast data. Includes load_rpffcst_item(), bldts_rpffcstriv()	
      - always load forecast time series	
      -uses compute_stage_info() - determine the derived fp info 
   
   
   ********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "get_stages.h"
#include "Forecast.h"
#include "QualityCode.h"
   
/*********************************************************************
   compute_fp_mofo_info()
   
   PURPOSE
   Determines various values that use both observed and forecast
   stage data for each forecast point.  The information is defined
   in terms of index values that refer to the specific item in the
   stage data structure.

   NOTES
   Fields computed for fp structure by this function include:
   omfc, omfs, rise_and_fall
      
   ********************************************************************/

void compute_fp_mofo_info(int		fpindex,
			  fp_struct	*fp)
{
   /* initialize */
   
   fp[fpindex].omf     	= (float )MISSINGVAL;
   fp[fpindex].omf_cat	= NULLCAT; 
   fp[fpindex].omf_time = (time_t)MISSINGVAL;
   
   
   /* get the omf value and category values; this check
      works even if one of the values are missing */
   
   if (fp[fpindex].curobs.value  != MISSINGVAL  ||
       fp[fpindex].maxfcst.value != MISSINGVAL)
   {	 
      if (fp[fpindex].curobs.value > fp[fpindex].maxfcst.value)
      {
	 fp[fpindex].omf      = fp[fpindex].curobs.value;
	 fp[fpindex].omf_cat  = fp[fpindex].curobs_cat;
	 fp[fpindex].omf_time = fp[fpindex].curobs.validtime;
      }      
      else
      {
	 fp[fpindex].omf      = fp[fpindex].maxfcst.value;
	 fp[fpindex].omf_cat  = fp[fpindex].maxfcst_cat;
	 fp[fpindex].omf_time = fp[fpindex].maxfcst.validtime;
      }      
   }  
   
   
   return;
}


/*********************************************************************
   compute_fp_prev_info()
   
   PURPOSE
   Determines the values of certain variables that are based on the 
   previous product info of the stage data for each forecast point.

   NOTES
   The rise_or_fall flags for each forecast group are determined elsewhere,
   even though since they are from the rise_or_fall flags for each
   forecast point, they too rely on previous product data.
   If previous data are missing, the flag is set to 
   unchanged, which is "safe" but not really valid.
   
   This function is broken out in this manner since the previous product
   information may be updated during a given execution of the RiverPro
   program (e.g. if a product is issued via the gui...)
   
   ********************************************************************/
void compute_fp_prev_info(int		fpindex,
		  	  fp_struct	*fp)
{
   int catval;
   
   
   /* set the rise fall flags to unchanged, not missing */
   
   fp[fpindex].obs_rise_or_fall  = UNCHANGED;
   fp[fpindex].fcst_rise_or_fall = UNCHANGED;
   fp[fpindex].rise_or_fall      = UNCHANGED;
   
   
   if (fp[fpindex].prev_avail == TRUE)
   {	 
      if (fp[fpindex].curobs.value    != MISSINGVAL && 
	  fp[fpindex].prev_curobs_val != MISSINGVAL)
      {	    
	 /* if data are available and if the category for the current
	    stage is greater than the category for the previous product,
	    then a rise occurred */	    
	 
	 catval = compute_stage_cat(fp[fpindex].cat, 
				    fp[fpindex].prev_curobs_val);
	 
	 if (fp[fpindex].curobs_cat > catval)
	    fp[fpindex].obs_rise_or_fall = RISE;
	 
	 else if (fp[fpindex].curobs_cat == catval)
	    fp[fpindex].obs_rise_or_fall = UNCHANGED;
	 
	 else
	    fp[fpindex].obs_rise_or_fall = FALL;
      }
      
      
      if (fp[fpindex].maxfcst.value    != MISSINGVAL &&
	  fp[fpindex].prev_maxfcst_val != MISSINGVAL)
      {	 
	 /* if data are available and if the category for the stage
	    is greater than the category for the previous product,
	    then a rise occurred */
	 
	 catval = compute_stage_cat(fp[fpindex].cat, 
				    fp[fpindex].prev_maxfcst_val);
	 
	 if (fp[fpindex].maxfcst_cat > catval)
	    fp[fpindex].fcst_rise_or_fall = RISE;
	 
	 else if (fp[fpindex].maxfcst_cat == catval)
	    fp[fpindex].fcst_rise_or_fall = UNCHANGED;
	 
	 else
	    fp[fpindex].fcst_rise_or_fall = FALL;
      }
      
      
      /* get the previous obs and max fcst category if
	 previous data available for CAT threshold*/
      
      if (fp[fpindex].omf_cat > NULLCAT && fp[fpindex].prev_omf_cat > NULLCAT)
      {	    
	 if (fp[fpindex].omf_cat > fp[fpindex].prev_omf_cat)
	    fp[fpindex].rise_or_fall = RISE;
	 
	 else if (fp[fpindex].omf_cat == fp[fpindex].prev_omf_cat)
	    fp[fpindex].rise_or_fall = UNCHANGED;
	 
	 else
	    fp[fpindex].rise_or_fall = FALL;
      }
             	 
      
   }  /* end of if block on previous info available */
   
   
   return;
}


/*********************************************************************
   compute_grp_county_mofo()
   
   PURPOSE
   Computes info related to the stage for each of the groups and counties.
         
   NOTES
   Fields computed by this function for the grp/county structure include:
   omfc, rise_or_fall, max_curobscat_fpindex, maxfcstcat_fpindex
  
   When determining the rise_or_fall value for collections of points,
   the variable name is misleading.  For collections, the value is
   RISE if at least one forecast point rose, otherwise the
   value is set to UNCHANGED.
   
   Modification: change compute_grp_info to compute_grp_county_mofo,
   added fields computation for cnty structure including max_curobs_cat,
   max_maxfcst_cat and max_omf_cat
   
   *******************************************************************/

void compute_grp_county_mofo(fp_struct		*fp,
		             int		numgrps,
		             grp_struct	        *grp,
			     int                numcnty,
			     county_struct      *cnty)
{
   int 		i, j;
   int 		fpindex;
   int 		catval;
   time_t	timeval;
   int 		max_curobs_cat, max_maxfcst_cat;
   int 		max_omf_cat;
   time_t	max_curobs_time, max_maxfcst_time;
   time_t	max_omf_time;
      
   
   /* get the info for each of the forecast groups */
   
   for (i = 0; i < numgrps; ++i)
   { 
      /* initialize the group data */
      
      max_curobs_cat  = max_maxfcst_cat  = max_omf_cat  = NULLCAT;
      max_curobs_time = max_maxfcst_time = max_omf_time = MISSINGVAL;
      
      
      /* loop on the number of forecast points in the group */
      
      for (j = 0; j < grp[i].numfps; ++j)
      {
	 fpindex = grp[i].fpindex[j];
	 	 
	 
	 /* check the max current observed category value and omf category.
	    always use the earliest cur observed. */
	 
	 if (fp[fpindex].curobs_cat != NULLCAT)
	 {
	    catval  = fp[fpindex].curobs_cat;
	    timeval = fp[fpindex].curobs.validtime;
	    
	    if (catval > max_curobs_cat)
	    {
	       max_curobs_cat  = catval;
	       max_curobs_time = timeval;
	    }
	    else if (catval == max_curobs_cat)
	    {   
	       if (timeval < max_curobs_time || 
	           max_curobs_time == MISSINGVAL)
	              max_curobs_time = timeval;
	    }
	 }
	 
	 
	 /* check the max forecast category and omf category.
	    always use the earliest maxfcst */
	 
	 if (fp[fpindex].maxfcst_cat != NULLCAT)
	 {
	    catval  = fp[fpindex].maxfcst_cat;
	    timeval = fp[fpindex].maxfcst.validtime;
	    
	    if (catval > max_maxfcst_cat) 
	    {
	       max_maxfcst_cat  = catval;
	       max_maxfcst_time = timeval;
	    }
	    else if (catval == max_maxfcst_cat)
	    {   
	       if (timeval < max_maxfcst_time || 
	           max_maxfcst_time == MISSINGVAL)
	       max_maxfcst_time = timeval;
	    }
	 }	 	 
      }   /* end of loop of fps in group */
      
      
      /* load the local variables into the structure */
            
      grp[i].max_curobs_cat   = max_curobs_cat;
      grp[i].max_curobs_time  = max_curobs_time;
      
      grp[i].max_maxfcst_cat  = max_maxfcst_cat;
      grp[i].max_maxfcst_time = max_maxfcst_time;
      
      
      /* if the cats are equal, use the observed since it is earlier
         in time. */
      
      if (grp[i].max_curobs_cat >= grp[i].max_maxfcst_cat)
      {
         grp[i].max_omf_cat  = grp[i].max_curobs_cat;
	 grp[i].max_omf_time = grp[i].max_curobs_time;
      }
      else
      {    	 
         grp[i].max_omf_cat  = grp[i].max_maxfcst_cat;
         grp[i].max_omf_time = grp[i].max_maxfcst_time;
      } 	 	 
   }
   
     
   
   /* get the info for each of the forecast point in countys */
   
   for (i=0; i < numcnty; ++i)
   {
      /* initialize the data */
      
      max_curobs_cat  = max_maxfcst_cat  = max_omf_cat  = NULLCAT;
      max_curobs_time = max_maxfcst_time = max_omf_time = MISSINGVAL;
      
      /* loop on the number of forecast points in the county */
      
      for (j = 0; j< cnty[i].numfps; ++j)
      {
          fpindex = cnty[i].fpindex[j];
	  
	  /* check the max current observed category value and omf category */
	  
	  if (fp[fpindex].curobs_cat != NULLCAT)
	  {
	     catval  = fp[fpindex].curobs_cat;
	     timeval = fp[fpindex].curobs.validtime;
	     	     
	     if (catval > max_curobs_cat)
	     {
		max_curobs_cat  = catval;
		max_curobs_time = timeval;
	     }
	     else if (catval == max_curobs_cat)
	     {    	
		if (timeval < max_curobs_time ||
		    max_curobs_time == MISSINGVAL)
		max_curobs_time = timeval;
	     }   	
	 
	  }
	  
	  
	  /* check the max forecast category and omf category */
	  
	  if (fp[fpindex].maxfcst_cat != NULLCAT)
	  {
	     catval = fp[fpindex].maxfcst_cat;
	     timeval = fp[fpindex].maxfcst.validtime;
	     
	     if (catval > max_maxfcst_cat)
	     {
		max_maxfcst_cat  = catval;
		max_maxfcst_time =  timeval;
	     }
	     else if (catval == max_maxfcst_cat)
	     { 	
		if (timeval < max_maxfcst_time ||
		    max_maxfcst_time == MISSINGVAL)
		    
		max_maxfcst_time = timeval;
	     }
	     	   
	  }
       }  /* end of loop on fps in county */
       
      
       /* load the local variables into the county structure */
      
       cnty[i].max_curobs_cat   = max_curobs_cat;
       cnty[i].max_curobs_time  = max_curobs_time;
       
       cnty[i].max_maxfcst_cat  = max_maxfcst_cat;
       cnty[i].max_maxfcst_time = max_maxfcst_time;
       
       
       if (cnty[i].max_curobs_cat >= cnty[i].max_maxfcst_cat)
       {
          cnty[i].max_omf_cat = cnty[i].max_curobs_cat;
	  cnty[i].max_omf_time = cnty[i].max_curobs_time;
       }
       else
       {   	  
          cnty[i].max_omf_cat = cnty[i].max_maxfcst_cat;
          cnty[i].max_omf_time = cnty[i].max_maxfcst_time;
       }
       
       /* printf("countyname=%s, statename=%s, max_curobs_cat=%d,
               max_maxfcst_cat=%d, max_omf_cat=%d\n", cnty[i].county,
	       cnty[i].state, cnty[i].max_curobs_cat, cnty[i].max_maxfcst_cat,
	       cnty[i].max_omf_cat); */
	             
    }
       	         
   return;
}
 

/*********************************************************************
   get_fpmofo_and_ts_ifneeded()
   
   PURPOSE
   Always loads the mofo (current observed and maximum forecast) stages and
   the observed and forecast time series for a given forecast point for the 
   first time. Then check if there is new current obs data that came in, if there is 
   then reload the obs timeseries. If there is no obs data, reload obs
   timeseries. Always load forecast timeseries.
   
   Consider the situation that previous riseabove/crest/fallbelow event is in
   the past and a forecast riseabove/crest/fallbelow event exist within the
   timeseriese window, if the previous event's ending time is in the past,
   RiverPro just load the obs/fcst data starting from the previous event's
   ending time (lookback time may different from the obshrs from RpfParams
   table). This is only for VTEC significance as "W" and this is not a
   very often case.
   
   ********************************************************************/

void get_fpmofo_and_ts_ifneeded(int		fpindex,
	                        fp_struct	*fp,
			        vtecinfo_struct	*vtecinfo,
			        pcc_struct      *pcc)
{   
   struct Report obsReport;
   int		obsFound;
   char		use_pe[SHEF_PE_LEN + 1];
   char		lid[LOC_ID_LEN + 1];
   time_t	obshrs, fcsthrs, basishrs;
   int          reload_obs_timeseries;
   int		filter_qc = 1;
   time_t 	system_time;
   time_t 	obs_btime, obs_etime;
   time_t	fcst_etime, basis_btime;       
   char         obsload_type[10], fcstload_type[10];
   char		ansi_btime[ANSI_TIME_LEN + 1], ansi_etime[ANSI_TIME_LEN + 1];
   char         ansi_basisbtime[ANSI_TIME_LEN + 1]; 
   int		status;
     
   
  /* load local strings for convenience */
   
      strcpy(lid,    fp[fpindex].id);
      strcpy(use_pe, fp[fpindex].pe);
           
      time(&system_time);
      
   /* get some default time window values for observed look back hours, forecast
      look forward hours and basis time look back hours*/
            	     
      get_hrvals(&obshrs, &fcsthrs, &basishrs);      
     
   /* use point specific look back hours if exist, they should
      not be able to be missing, but check just in case. */
    
      if (fp[fpindex].backhrs != MISSINGVAL)
         obshrs = fp[fpindex].backhrs;        
     	 
   /* use point specific look foward hours if exist, they should
      not be able to be missing, but check just in case. */
    
      if (fp[fpindex].forwardhrs != MISSINGVAL)
         fcsthrs = fp[fpindex].forwardhrs;    
	    
   /* get the obs/fcst time window limits based on points. The look back
      hours/look forward hours should not be missing*/     
      
      obs_btime = system_time - (3600 * (long)obshrs);      	       
      fcst_etime = system_time + (3600 * (long)fcsthrs);        
      basis_btime = system_time - (3600 * (long )basishrs);	 

      obs_etime = system_time;	 
	 
   /* get the current observed data from RiverStatus table.*/
   
      get_rpf_curobs(lid, use_pe, (int )obshrs, &obsFound, &obsReport);      
           
   /* initialize reload options.  these are used to dictate whether 
      to reload the observed full time series data. Only consider the
      observed data since always load forecast time series */
    
      reload_obs_timeseries  = FALSE;
              
   /* if the full ts has never been loaded, load in everything.
      this load_time may also be intentionally reset by the calling
      function in order to force a full reload. */   
     
      if (fp[fpindex].fullts_loaded_time == MISSINGVAL)
      {    
	 reload_obs_timeseries  = TRUE;               

	 /* load in the obs info into the structure */

	    if (obsFound) 
	    {	 	                 
	       fp[fpindex].curobs.value      = obsReport.value;      
	       fp[fpindex].curobs.validtime  = obsReport.validtime;
	       strcpy(fp[fpindex].curobs.ts, obsReport.ts);           
	       fp[fpindex].curobs_cat = compute_stage_cat(fp[fpindex].cat, 
					      fp[fpindex].curobs.value);
	    }
	    else
	    {
               fp[fpindex].curobs.value      = MISSINGVAL;      
               fp[fpindex].curobs.validtime  = 0;      
               strcpy(fp[fpindex].curobs.ts, "");     
               fp[fpindex].curobs_cat        = NULLCAT;
	    }           
      }   
     
   /* if it is not the first time loading */ 
   
      else 
      {
	  /* check if there is new current observed data.  if no data were
             found, then still try and load the full time series, if only
	     to have it set to missing.  this strange condition could occur if 
	     the latest obs was old and just barely in the time window at the time
	     of the previous retrieval but now is outside the window. */

	     if (obsFound)
	     {
        	if ((fp[fpindex].curobs.value     != obsReport.value) ||
        	    (fp[fpindex].curobs.validtime != obsReport.validtime))
		{
        	   reload_obs_timeseries = TRUE;

		   fp[fpindex].curobs.value      = obsReport.value;      
		   fp[fpindex].curobs.validtime  = obsReport.validtime;
		   strcpy(fp[fpindex].curobs.ts, obsReport.ts);           
		   fp[fpindex].curobs_cat = compute_stage_cat(fp[fpindex].cat, 
				        	    fp[fpindex].curobs.value);
		} 
	     }

	     else
	     {
        	reload_obs_timeseries = TRUE;

        	fp[fpindex].curobs.value      = MISSINGVAL;      
        	fp[fpindex].curobs.validtime  = 0;      
        	strcpy(fp[fpindex].curobs.ts, "");     
        	fp[fpindex].curobs_cat        = NULLCAT;
	     }	                 

      }  /* end of check if new current obs data came in*/
     
    
   /* if loading any new time series data */
          	 
      if (reload_obs_timeseries == TRUE )
      {       			           		                 	 
	 /* free any memory that may be allocated for the data */

	    if (fp[fpindex].numobsH > 0 && reload_obs_timeseries == TRUE)
	    {
	       if (fp[fpindex].obsH != NULL)
	       {
        	  free(fp[fpindex].obsH);
		  fp[fpindex].obsH     = NULL;
        	  fp[fpindex].numobsH  = 0;
        	  fp[fpindex].use_obsH = 0;
	       }	 
	    }
                             
	 /* load the obs data with the ts as current obs ts, and the data within
            the window between obs_btime and obs_etime */

	    if (reload_obs_timeseries == TRUE) 
	    {        
        	bldts_obsriv(fp[fpindex].id, fp[fpindex].pe, fp[fpindex].curobs.ts,
			     filter_qc, obs_btime, obs_etime, 	
	 		     &fp[fpindex].obsH, &fp[fpindex].numobsH); 

		fp[fpindex].use_obsH = fp[fpindex].numobsH;
	    }	  	                     

	 /* set the load time for the next possible pass into this function */

	    fp[fpindex].fullts_loaded_time = system_time; 
      }
           
    
   /* Load forecast timeseries in RiverPro and find the 
      maximum forecast data within this forecast timeseries. The
      fcst timeseries is limited from current time to the current
      time plus look forward hours. */
      
   /* initialize maximum forecast data fields */

      fp[fpindex].maxfcst.value = MISSINGVAL;
      fp[fpindex].maxfcst.validtime = 0;
      fp[fpindex].maxfcst.basistime = 0;
      strcpy(fp[fpindex].maxfcst.ts, "");
      fp[fpindex].maxfcst_cat = NULLCAT;    

      
   /* free space if exists */

      if (fp[fpindex].fcstH != NULL)
      {
         free(fp[fpindex].fcstH);
	 fp[fpindex].fcstH     = NULL;
         fp[fpindex].numfcstH  = 0;         
      }	

   /* start to load forecast time series */

      if (use_pe[0] == 'H')
         load_rpf_fcstdata_lidpe("FcstHeight", lid, use_pe, fcst_etime, basis_btime, 
	                          fpindex, fp);
      else if (use_pe[0] == 'Q')
         load_rpf_fcstdata_lidpe("FcstDischarge", lid, use_pe, fcst_etime, basis_btime,
	                          fpindex, fp);	 

      						     
   /* recompute the obs and forecast point mofo info.
      always recompute the prev info in the event that the previous 
      info changes independent of new time series data loaded in. */
                  
      compute_fp_mofo_info(fpindex, fp);      
      compute_fp_prev_info(fpindex, fp);        						     
   
   /* set up info for a message */

      if (reload_obs_timeseries == TRUE) 
	 sprintf(obsload_type, "loaded");
      else
	 sprintf(obsload_type, "retained");

   /* always load forecast data */
   
      sprintf(fcstload_type, "loaded");
      
      status = timet_to_yearsec_ansi(obs_btime, ansi_btime);
      status = timet_to_yearsec_ansi(fcst_etime, ansi_etime);
      status = timet_to_yearsec_ansi(basis_btime, ansi_basisbtime);
      
      printf("%s:%s: %s %d %s obs > %s; %s %d %s fcst < %s; basis > %s\n", 
	     fp[fpindex].id, fp[fpindex].pe,
	     obsload_type, fp[fpindex].numobsH,
	     fp[fpindex].curobs.ts, ansi_btime, 
	     fcstload_type, fp[fpindex].numfcstH, fp[fpindex].maxfcst.ts,
	     ansi_etime, ansi_basisbtime); 

   /* store the lookback load-since time for info purposes */

      fp[fpindex].obs_load_time = obs_btime;

   /* filter out old data if vtec enabled for any VTEC significance
      and data found.  otherwise, always use all obs data. */

      if (fp[fpindex].numobsH > 0 && pcc->product.vtec_flag == TRUE)
      {
	 apply_obsdata_filter(fpindex, fp, vtecinfo, pcc);   
      }

      else
      {
	 fp[fpindex].use_obsH        = fp[fpindex].numobsH;
	 fp[fpindex].obs_cutoff_time = obs_btime;
      }

   	   
   /* always recompute the the derived stage values */

      compute_stage_info(fpindex, fp);  
      
   
    return;   
}

/****************************************************************
   
   load_rpf_fcstdata_lidpe()
   
   PURPOSE
   Load forecast time series with lid|pe and higher rank ts from table
   fcstHeight or fcstDischarge, the timeseries is within current
   time and look foreward time specified based on point. 
   Find the maximum forecast data from that loaded forecast time series.
   
   **************************************************************/
void load_rpf_fcstdata_lidpe(char 	      *tablename,                               
		             char	      *lid,
			     char	      *pe,
			     time_t           end_validtimet,	
			     time_t           basis_btime,
			     int              fpindex,		
			     fp_struct        *fp)
{
   static UniqueList *ulistPtr = NULL;
   UniqueList        *ulPtr = NULL;
   char 	      ults[SHEF_TS_LEN + 1];
   char        	      ullid[LOC_ID_LEN + 1];
   char		      ulpe[SHEF_PE_LEN + 1];
   char		      where[160];
   int 		      cnt;      
   static int         first = TRUE;               
   char               ** values = NULL;
   int                max_fcst_found = 0;
   
   /* load type source which pe starts as 'H' or 'Q' and ts starts as 'F'.
      only load once */  
      
      if (first)
      {              
	  sprintf(where,
        	   " WHERE ts LIKE 'F%%' AND ( pe LIKE 'H%%' OR pe LIKE 'Q%%') "
		   " AND ingest= 'T'");  

	  ulistPtr = LoadUnique("ts_rank||ts||lid||pe", "IngestFilter", where, &cnt); 

	  first = FALSE;     

	  if ( ulistPtr == NULL )
	  {
	     return ;
	  }                    
      }
   
      ulPtr = (UniqueList *) ListFirst(&ulistPtr->list); 
       
   /* starting from the highest rank type source, load the timeseries and
      find the maximum value */
   
      while (ulPtr)
      {     
	 /* extract the lid, pe, ts code from the unique string. */

	    values = ParseUnique ( ulPtr, &cnt );

	    if ( ( values == NULL ) || ( cnt < 4 ) )
	    {
        	/* An error was ecountered parsing the unique string. */

        	break;
	    }                

	    memset(ults, 0, SHEF_TS_LEN + 1);
	    strncpy(ults, values[1], SHEF_TS_LEN);

	    memset(ullid, 0, LOC_ID_LEN + 1);
	    strncpy(ullid, values[2], LOC_ID_LEN);

	    memset(ulpe, 0, SHEF_PE_LEN + 1);
	    strncpy(ulpe, values[3], SHEF_PE_LEN);            

	    FreeParseUnique ( values );      

	 /* perform the load_maxfcst operations for specifed
	    lid, pe, ts */

	    if ((strcmp(ullid, lid) == 0) && (strcmp(ulpe, pe) == 0))
               load_rpffcst_item(lid, pe, ults, end_validtimet, basis_btime,
	                	    fpindex, fp, &max_fcst_found);

	 /* if the maximum fcst data is found, break */

	    if (max_fcst_found)
               break;

	 /* loop to the next ts entry if there is one. */

	    ulPtr = (UniqueList *) ListNext(&ulPtr->node);
      }
              
      return;
}

/****************************************************************
   
   load_rpffcst_item()
   
   PURPOSE
   Loads forecast timeseries for lid|pe and starting from highest
   type source. If forecast data is found, find the maximum
   data.
   
**************************************************************/
void load_rpffcst_item(char 	    *lid, 
		       char	    *pe,
		       char	    *ts,
		       time_t       end_validtimet,
		       time_t       basis_btime,
		       int          fpindex,
		       fp_struct    *fp,		
		       int          *max_fcst_found)
{   
   Report	*fcst_ts;
   Report	max_fcst_record;
   int		fcst_ts_cnt;
   int		use_latest, qc_filter;
 
       
   /* get the setting for the use_latest_fcst field
      for the current location */        
      
      if (fp[fpindex].use_latest_fcst == TRUE)
         use_latest = 1;
      else
         use_latest = 0;	 
      
   /* get the forecast time series for this location, pe, and ts
      using any instructions on any type-source to screen and
      whether to use only the latest basis time  */
   
      qc_filter = 1;
         
      bldts_rpffcstriv(lid, pe, ts, qc_filter, use_latest, end_validtimet,
                       basis_btime, &fcst_ts, &fcst_ts_cnt);
     
   
   /* find the data for this location, pe, and ts given the
      forecast time-series and the count of values in it.	  
      if data found, determine its max and load the value */
   
      if (fcst_ts_cnt > 0)
      {
	 find_maxfcst(fcst_ts, fcst_ts_cnt, &max_fcst_record);                

	 *max_fcst_found = 1;

	 /* assign the forecast data structure to fp, 
            the number of forecast data to fp structure */

	    fp[fpindex].fcstH = fcst_ts;
	    fp[fpindex].numfcstH = fcst_ts_cnt;                  

      }
      else
         *max_fcst_found = 0;
	 
   /* load the maximum forecast data into fp structure */

      if (*max_fcst_found)
      {
         fp[fpindex].maxfcst.value = max_fcst_record.value;
	 fp[fpindex].maxfcst.validtime = max_fcst_record.validtime;
	 fp[fpindex].maxfcst.basistime = max_fcst_record.basistime;
	 strcpy(fp[fpindex].maxfcst.ts, max_fcst_record.ts);
	 fp[fpindex].maxfcst_cat = compute_stage_cat(fp[fpindex].cat,
	                                             fp[fpindex].maxfcst.value);
      }
      else
      {      
         fp[fpindex].maxfcst.value = MISSINGVAL;
	 fp[fpindex].maxfcst.validtime = 0;
	 fp[fpindex].maxfcst.basistime = 0;
	 strcpy(fp[fpindex].maxfcst.ts, "");
	 fp[fpindex].maxfcst_cat = NULLCAT;      
      } 	
           
   return;
}


 /**************************************************************
 bldts_rpffcstriv()
 
 PURPOSE
 Load forecast time series with lid|pe|ts,and within current to
 look forward time  frames. 
 ***************************************************************/
void bldts_rpffcstriv(char		*lid,
		      char		*pe,
		      char 	        *ts_filter,
		      int		use_qcfilter,     /* not used */
		      int		use_latest,
		      time_t	        end_validtimet,
		      time_t            basis_btime,
		      Report     	**fcst,
		      int 		*num_fcst)
{
   char		use_ts[SHEF_TS_LEN + 1];
   Forecast	*fcstHead, *fcstPtr;
   UniqueList	*ulHead, *ulPtr;
   int		fcst_count, ul_count;
   int 		status;
   char		where[450], qcwhere[MAXLEN_QCWHERE];
   int		*doKeep;
   int 		i, j;
   int		keepcnt;
   int		ordinal;
   char		tablename[40];
   char		basis_btime_ansi[ANSI_TIME_LEN];
   char		end_validtime_ansi[ANSI_TIME_LEN];
   double       msgval = -9999.0;
   
   /* initialize */
   
      *num_fcst = 0;
     
   /* define which typesource code to use, whether it is passed in
      whether it is determined by the ingestfilter ranks */
   
      if (strlen(ts_filter) == 0)
      {
	 ordinal = 0;
	 status = get_best_ts(lid, pe, "F%", ordinal, use_ts);
	 if (status < 0) return;
      }   
      else
	 strcpy(use_ts, ts_filter);
   
      
   /* define the qc filter to apply */
   
      build_qc_where(QC_NOT_FAILED, qcwhere);
   
    
   /* set the tablename to use */
   
      getTableName(pe, use_ts, tablename);
    
   /* convert the times for the where clause */
   
      timet_to_yearsec_ansi(basis_btime, basis_btime_ansi);
      timet_to_yearsec_ansi(end_validtimet, end_validtime_ansi);

   /* retrieve a list of unique basis times; use descending sort. 
      only consider forecast data before some ending time,
      and with some limited basis time ago */
   
      sprintf(where,
	   " WHERE lid= '%s' AND pe= '%s' AND ts= '%s' AND "
	   " probability < 0.0 AND "
	   " validtime >= CURRENT_TIMESTAMP AND "
	   " validtime <= '%s' AND "
	   " basistime >= '%s' AND value != %f AND %s" 
	   " ORDER BY basistime DESC ",
	   lid, pe, use_ts, end_validtime_ansi, basis_btime_ansi, 
	   msgval, qcwhere);
	         
      ulHead = LoadUnique("basistime", tablename, where, &ul_count);
      if (ul_count <= 0) return;
   
   
   /* retrieve the data; the ordering by validtime is important.
      as before, limit the forecast time valid time window
      and as needed, the age of the forecast (basistime). */ 
      
      if (use_latest || ul_count == 1)
      {
	 ulPtr  = (UniqueList *)ListFirst(&ulHead->list);        
	 sprintf(where,
		 " WHERE lid = '%s' AND pe = '%s' AND ts = '%s' AND "
		 " probability < 0.0 AND "
		 " validtime >= CURRENT_TIMESTAMP AND "
		 " validtime <= '%s' AND basistime = '%s' AND "
		 " value != %f AND %s "
		 " ORDER BY validtime ASC",
		 lid, pe, use_ts, end_validtime_ansi, ulPtr->uchar, 
		 msgval, qcwhere);  
      }
   
      else
      {
	 sprintf(where,
		 " WHERE lid = '%s' AND pe = '%s' AND ts = '%s' AND "
		 " probability < 0.0 AND "
		 " validtime >= CURRENT_TIMESTAMP AND "
		 " validtime <= '%s' AND basistime >= '%s' AND "
		 " value != %f AND %s"
		 " ORDER BY validtime ASC",
		 lid, pe, use_ts, end_validtime_ansi, basis_btime_ansi, 
		 msgval, qcwhere);        
      }
      
      fcstHead = GetForecast(where, tablename);
      fcst_count = ListCount(&fcstHead->list);

     
   /* define a local array to determine which items in the time
      series to keep and return */
   
      doKeep = (int *)malloc(sizeof(int) * (fcst_count));
      if (doKeep == NULL)
      {
	 fprintf(stderr, "Error in malloc for doKeep()");
	 return;
      }

   
   /* if only getting the latest basis time's data
      or only one basis time was found, then consider all;
      otherwise, need to adjoin/butt the time series together
      for the multiple basis times. */
   
      if (use_latest || ul_count <= 1)
      {
	 for (i = 0; i < fcst_count; i++)
	    doKeep[i] = 1;
      }
      else
	 set_fcst_keep(ulHead, fcstHead, doKeep);

   
   /* now load the values and info to return, knowing which items
      to keep since all the values have been tagged.  first get
      the count of the number of values to keep and allocate the data */
     
      keepcnt = 0;
      for (i = 0; i < fcst_count; i++)
	 if (doKeep[i]) keepcnt++;
   
      *num_fcst = keepcnt;
      *fcst = (Report *)malloc(sizeof(Report) * (keepcnt));
      if (*fcst == NULL)
      {
	 fprintf(stderr, "Error in malloc for *fcst in bldts_rpffcstriv()");
	 return;
      }
  
      j = 0;
      fcstPtr = (Forecast *) ListFirst(&fcstHead->list);
      for (i = 0; i < fcst_count; i++)
      {
	 if (doKeep[i])
	 {
	    strcpy((*fcst)[j].pe,       fcstPtr->pe);
	    (*fcst)[j].dur            = fcstPtr->dur;
	    strcpy((*fcst)[j].ts,       fcstPtr->ts);
	    strcpy((*fcst)[j].extremum, fcstPtr->extremum);
	    (*fcst)[j].probability    = fcstPtr->probability;
	    yearsec_dt_to_timet(fcstPtr->validtime, &((*fcst)[j].validtime));
	    yearsec_dt_to_timet(fcstPtr->basistime, &((*fcst)[j].basistime));
	    (*fcst)[j].value          = fcstPtr->value;
	    (*fcst)[j].quality_code   = fcstPtr->quality_code; 
	    strcpy((*fcst)[j].shef_qual_code, fcstPtr->shef_qual_code);      

            j++;
	 }

	 fcstPtr = (Forecast *) ListNext(&fcstPtr->node);     
      }
   
   
   /* free the data */
   
      FreeForecast(fcstHead);
      FreeUnique(ulHead);
      free(doKeep);
      
      return;
}


/****************************************************************
get_rpf_curobs()

PURPOSE
Get current obs data with lid, pe and ts which reads from RiverStatus
table. The retrieved type source should between look back time and
current time frame.
*****************************************************************/
void get_rpf_curobs(char	   *lid,
		    char 	   *pe,
		    int		   obs_hours_ago,					 
		    int		   *obs_found,
		    struct Report  *obsReport)
{
   char 		where[350];
   int			cnt;
   UniqueList		*ulistHead = NULL, *ulPtr = NULL;
   RiverStatus		*rsHead = NULL, *rsPtr = NULL;
   char			use_ts[SHEF_TS_LEN + 1];
   time_t		curtimet;
   time_t		beg_validtimet;
   time_t		validtime;
   char			ansi_validtime[ANSI_TIME_LEN];
   char                 ** values = NULL;
   time_t               best_obsvalidtime = -1;
   int                  use_tsrank_int,prev_tsrank_int = +99;
   
   /* initialize arguments */
   
      *obs_found = 0;          
   
   /* define the beginning of the window for looking for the observed data. */
	       
      time(&curtimet);

      beg_validtimet = curtimet - (obs_hours_ago * 3600);
      timet_to_yearsec_ansi(beg_validtimet, ansi_validtime);      
   
   /* in an effort to minimize reads of the database, get the RiverStatus
      info all at once, for all ts's and for observed Data.
      There is validtime limit for observed data */
   
      sprintf(where, " WHERE lid='%s' AND pe='%s' AND "
	          " (validtime >= '%s' AND validtime <= CURRENT_TIMESTAMP) "
		  " AND basistime IS NULL ",
	          lid, pe, ansi_validtime);   	      
   
      rsHead = GetRiverStatus(where);
      
   /* get a unique list of entries for ts's that match the given lid and pe. 
      the ingestfilter entries are needed because they contain the type-source
      rank information.   insert a comma in between the
      two fields to make them easier to parse. note that the ts rank
      sort method will not handle numbers greater than 9 (i.e. 12 is ranked
      higher than 3)! also note that the query does not filter out
      extremums that are not "Z". only bother getting info if riverstat entries
      exist. we try and read riverstatus first since that table is smaller */
   
      if (rsHead != NULL)
      {
	 sprintf(where,
		 " WHERE lid= '%s' AND pe= '%s' AND ingest= 'T' "
		 " ORDER BY 1 ", lid, pe);       
	 ulistHead = LoadUnique("ts_rank||ts", "IngestFilter", where, &cnt);   
      }
   
   /* ---------------------------------------------------------------------- */
   /* process the data if there are possible entries to check */
   
      if (ulistHead != NULL && rsHead != NULL)
      {
	 /* loop on the observed entries and try to get the observed data.
	 note that processed data is grouped in with observed data searches. */
      
         ulPtr = (UniqueList *) ListFirst(&ulistHead->list);
                  
	 while (ulPtr)
	 { 
            /* extract the type source and rank currently being considered */

               values = ParseUnique ( ulPtr, &cnt );

               if ( ( values == NULL ) || ( cnt < 2 ) )
               {
        	  /* Could not parse the unique string. */

        	  FreeUnique(ulistHead);
        	  FreeRiverStatus(rsHead);
        	  return;
               }
	 
	       use_tsrank_int = atoi(values[0]);
	       memset(use_ts, 0, SHEF_TS_LEN + 1);	 
	       strncpy(use_ts, values[1], SHEF_TS_LEN);

               FreeParseUnique ( values );
               values = NULL;
	 
	    /* only process the observed type-sources entries, and
	       only if the ts rank is the same as the previously checked ts
	       or no obs data has been found at all yet */	 	 
	 
	       if ((use_ts[0] == 'R' || use_ts[0] == 'P') &&
		   ((use_tsrank_int == prev_tsrank_int) || !(*obs_found)))
	       {		           	    	    
		  /* loop on the riverstatus entries for the observed 
	             value that matches the type-source being considered */
	    	   	    
                  rsPtr = (RiverStatus *) ListFirst(&rsHead->list);	       
	    
		  while (rsPtr)
		  {	    
	             /* only use this riverstatus entry if it is for the current 
	        	ts and the the validtime for the current ts is more recent
			than the previous validtime for ts with a possible matching
			rank */

	        	yearsec_dt_to_timet(rsPtr->validtime, &validtime);

	        	if ((strcmp(rsPtr->ts, use_ts) == 0 ) &&
	        	   (validtime > best_obsvalidtime)) 
	        	{
	             
			   strcpy(obsReport->pe,       rsPtr->pe);
			   obsReport->dur            = rsPtr->dur;
			   strcpy(obsReport->ts,       rsPtr->ts);
			   strcpy(obsReport->extremum, rsPtr->extremum);
			   obsReport->value          = rsPtr->value;
			   obsReport->validtime     = validtime;
			   obsReport->basistime      = 0;

			   best_obsvalidtime = validtime;

			   *obs_found = 1;
		     
		           break;		
		      
	                }
	       
	                rsPtr = (RiverStatus *) ListNext(&rsPtr->node);
	          }
	       
	          prev_tsrank_int = use_tsrank_int;	       	    
	    
	       }  /* end of if check on observed type-source */	 
	 
	       /* get the next observed ts */
	 
	          ulPtr = (UniqueList *) ListNext(&ulPtr->node);
	 
         }  /* end of while loop on ulPtr and if obs not found */                                
      
         /* finally free the lists */
      
	    FreeUnique(ulistHead);
	    FreeRiverStatus(rsHead);
      
      }  /* end of if check on observed data to process */
   
   
   return;
}


/*********************************************************************
   apply_obsdata_filter()
   
   PURPOSE
   This function sets a variable associated with each forecast point's
   stage data (use_obsH) that defines how many of the observed data 
   values it should use.  Because the obs data are ordered such that 
   array [0] is the oldest, this "used" values which pass this filter
   are at the "end" of the array, from [numobsH-use_obsH] to the 
   [numobsH-1} last element.
   
   The filter is applied for VTEC purposes and reflects the fact that
   no data since the end of the previous event's ending time should be
   used, with a few special exceptions.
   
   This function should only be called if in VTEC mode.
   
   ********************************************************************/
void apply_obsdata_filter(int		        fpindex,
	                  fp_struct	        *fp,
			  vtecinfo_struct	*vtecinfo,
			  pcc_struct            *pcc)
{
   time_t	begin_time;
   int		i, j;
   int    	cnt, status;
   char         prev_action[VTEC_ACTION_LEN + 1];          
   time_t	prev_endtime, prev_producttime; 
   char		msgstr[MAXLEN_STRING]; 
   char		ansi_time[ANSI_TIME_LEN + 1]; 
   
   
   /* define a local convenience variable */
   
   i = fpindex;
   
   
   /* load the previous action and endtime based on the 
      vtec significance code in effect, for use below. */
      
   if (strcmp(pcc->product.vtec_default_signif, SIGNIF_WATCH) == 0)
   {
      strcpy(prev_action, vtecinfo[i].inactive_fla.action);
      prev_endtime =      vtecinfo[i].inactive_fla.endtime;
      prev_producttime =  vtecinfo[i].inactive_fla.producttime;
   }
   
   else if (strcmp(pcc->product.vtec_default_signif, SIGNIF_ADVISORY) == 0)
   {
      strcpy(prev_action, vtecinfo[i].inactive_fly.action);
      prev_endtime =      vtecinfo[i].inactive_fly.endtime;
      prev_producttime =  vtecinfo[i].inactive_fly.producttime;
   } 
   
   /* this else is just in case, but should always be when 
      (strcmp(pcc->product.vtec_default_signif, SIGNIF_WARNING) == 0) */
  
   else
   {
      strcpy(prev_action, vtecinfo[i].inactive_flw.action);
      prev_endtime =      vtecinfo[i].inactive_flw.endtime;
      prev_producttime =  vtecinfo[i].inactive_flw.producttime;
   }
     
     
   /* determine the time before which no obs data should be used */
      
   begin_time = MISSINGVAL;
   
   
   /* if the previous action is CANcel, take the earlier of the 
      previous event's end time and product time */
      
   if (strcmp(prev_action, "CAN") == 0)
   {
      if (prev_endtime != MISSINGVAL && prev_producttime != MISSINGVAL)
      {
         if (prev_producttime < prev_endtime)
	    begin_time = prev_producttime;
	 else 
	    begin_time = prev_endtime;
      }
      
      else if (prev_endtime != MISSINGVAL)
         begin_time = prev_endtime;
	 
      else if (prev_producttime != MISSINGVAL) 
         begin_time = prev_producttime;
   }
   
   else
   {
      if (prev_endtime != MISSINGVAL)
         begin_time = prev_endtime;
   }
   
   
   /* now loop thru the observed time series and see where the determined
      begin time falls */
   
   if (begin_time != MISSINGVAL && fp[i].numobsH > 0)
   {
      cnt = 0;
      for (j = (fp[i].numobsH - 1); j >= 0; j--)
      {
         if (fp[i].obsH[j].validtime > begin_time)
	    cnt++;
      }
      

      /* make sure that at least one observed value, i.e. the latest value,
         is used.  btw, this value should agree with the omf obs value 
	 maintained for the location. */
       
      if (cnt > 0)
        fp[i].use_obsH = cnt;	
      else
        fp[i].use_obsH = 1;
       
      
      /* log some info about the adjustment */
      
      status = timet_to_yearsec_ansi(begin_time, ansi_time);
      
      printf("%s: ignore < %s, using %d of %d vals\n", 
             fp[fpindex].id, ansi_time,  fp[i].use_obsH, fp[i].numobsH);
      sprintf(msgstr, "%s: ignore < %s, using %d of %d vals", 
              fp[fpindex].id, ansi_time,  fp[i].use_obsH, fp[i].numobsH); 
      log_msg("", msgstr);  
      
      
      /* store the cutoff time for info purposes */
      /* ther resulting begin_time may actually be earlier than the time
      for which data were retrieved, as noted by obs_load_time. If so, adjust
      the begin time so that the desplayed obs_cutoff_time represents the
      actual time used in the filters */
      
      if (begin_time < fp[i].obs_load_time)
         fp[i].obs_cutoff_time = fp[i].obs_load_time;
      else  
         fp[i].obs_cutoff_time = begin_time;
	 
   }
   
   
   /* reset the cutoff time info to indicate the full time 
      period covered in the loading */
      
   else   
   {
      fp[i].obs_cutoff_time = fp[i].obs_load_time;
   }
          

   return;
}


/*********************************************************************
   compute_stage_cat()
   
   PURPOSE
   Determine the stage category for a given stage value for a single
   forecast point for general CAT threshold mode. because of the
   system, sometimes the float number is stored not exactly as it is shown.
   For example 23.1 is stored as 23.1000004. If the absolute
   difference between cat_vals[i] and dataval is very small, consider
   them equal.
   
   ********************************************************************/

int compute_stage_cat(float	cat_vals[],
		      double	dataval)
{
   int cat;
   int i;
    
      
   cat = 0;
   
   if (dataval == MISSINGVAL)
      cat = NULLCAT;
   
   else
   {      
      for (i = 1; i < MAX_CAT; ++i)
      {
	if (((dataval >= cat_vals[i]) ||
	     (fabs(dataval - cat_vals[i]) < 0.0001))
	      && cat_vals[i] != MISSINGVAL)	      
	  cat = i;
      }    		 	     
   }
   
   return(cat);
}


/*********************************************************************
   check_stage_data()
   
   PURPOSE
   Provides notification of any forecast points that are missing data. 
   
   ********************************************************************/
void check_stagedata(int		fpindex,
		     fp_struct		*fp)
{   
   /* check the forecast point data status */
   
   if (fp[fpindex].numobsH  == 0 && 
       fp[fpindex].numfcstH == 0)
      log_msg(MISSING_STAGEFLOW_DATA, fp[fpindex].id);
      
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

