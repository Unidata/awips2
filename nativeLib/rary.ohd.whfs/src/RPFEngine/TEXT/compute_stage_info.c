/*********************************************************************

   compute_stage_info.c

   
   The functions in this file are for those stage/discharge 
   related variables that require the full time series info,
   and not just the curobs/maxfcst data.
   
   compute_stage_info() - called after get time series via get_obsfcst_data()   
   -->compute_obs_info()
   -->compute_fcst_info()   
   -->compute_fp_risefall()
   -->load_trend_info()
      
   ---->load_stage_in_interval()  
   ---->load_special_stages() 
   
   
   compute_grp_county_full_info()   
   compute_fp_risefall() 
   
   compute_detailed_fcst_info()
   load_detailed_trend_info()
   rise_hydrograph()
   fall_hydrograph()
   twopoints_flat_hydrograph()
   threepoints_crestflat_hydrograph()
   threepoints_valleyflat_hydrograph()	 
   fourpoints_flat_hydrograph()   
   load_tidal_trendinfo()   
   round_float() 
   
   ********************************************************************/

#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>

#include "compute_stage_info.h"

int tidal_cnt = 0; 

/*********************************************************************
   compute_stage_info()
   
   PURPOSE
   This function makes the calls to the other functions which 
   determine various information about the stage data.
   
   NOTES
   The derived info for the detailed forecast trend information is 
   uses functions contained in this file, but called directly 
   as needed by the load_variable_value operations.
      
   ********************************************************************/
void compute_stage_info(int		fpindex,
			fp_struct	*fp)
{
          
   /* get the info on the observed stage data for forecast points */
   
   compute_obs_info(fpindex, fp);
   
   
   /* get the info on the forecast stage data for forecast points */
   
   compute_fcst_info(fpindex, fp); 
   
   
   /* assign fp[].riseabove_time and fp[].fallbelow_time */
   
   compute_fp_risefall(fpindex, fp);
   
   
   /* get the info on the trend data, which uses observed
      and forecast data */
   
   load_trend_info(fpindex, fp);
   
            
   return;
}


/*********************************************************************
   compute_obs_info()
   
   PURPOSE
   Determines the derived values for the observed stage data for each
   forecast point.  The information is defined in terms of index
   values that refer to the specific item in the stage data structure.
   
   NOTES
   Computed fields for fp[fpindex].fp structure by this function include:
   obs_cur_index, 
   obs_min06_index, obs_max06_index, obs_min24_index, obs_max24_index,
   obs_FSdeparture, obs_fallbelow_time, obs_riseabove_time, 
   obs_crest_value/time
   
   The fields based on the previous product info are computed elsewhere.
   
   ********************************************************************/

void compute_obs_info(int		fpindex,
		      fp_struct		*fp)
{
   int		k;
   float 	curstage;
   double 	maxstage;
   time_t	system_time;
   int		start_index, end_index;
   
   
   /* initialize all the data */
   
   fp[fpindex].obs_cur_index   = MISSINGVAL;
   fp[fpindex].obs_max_index   = MISSINGVAL;
   fp[fpindex].obs_max24_index = MISSINGVAL;
   fp[fpindex].obs_max06_index = MISSINGVAL;
   fp[fpindex].obs_FSdeparture = MISSINGVAL;
   fp[fpindex].obs_riseabove_time = (time_t )MISSINGVAL;
   fp[fpindex].obs_fallbelow_time = (time_t )MISSINGVAL;
   fp[fpindex].obs_crest_value = (double )MISSINGVAL;
   fp[fpindex].obs_crest_time  = (time_t )MISSINGVAL;
   
   
   /* for the current pe and type-source for this station,
      get the time series of data */
   
   
   /* if observed values available for this point, process the data */
   
   if (fp[fpindex].use_obsH > 0)
   {
      
      /* get the max stage. if sustained max exists, get the most recent. */
      
      maxstage = -9999.0;
      start_index = fp[fpindex].numobsH - fp[fpindex].use_obsH;
      end_index = fp[fpindex].numobsH;
      
      for (k = start_index; k < end_index; ++k)
      {
	 if (fp[fpindex].obsH[k].value >= maxstage &&
	     fp[fpindex].obsH[k].value != MISSINGVAL)
	 {
	    maxstage = fp[fpindex].obsH[k].value;
	    fp[fpindex].obs_max_index = k;
	 }	    
      }
      
      
      /* load index to the current stage value, checking for missing vals.
	 then get the flood departure. look in reverse order, accounting
	 for data usage */
      
      start_index = fp[fpindex].numobsH - 1;
      end_index   = fp[fpindex].numobsH - fp[fpindex].use_obsH;
      
      for (k = start_index; k >= end_index; --k)
      {
	 if (fp[fpindex].obsH[k].value != MISSINGVAL)
	 {
	    fp[fpindex].obs_cur_index = k;
	    
	    curstage =
	       fp[fpindex].obsH[fp[fpindex].obs_cur_index].value;
	    
	    if (fp[fpindex].pe[0] == 'Q')
	    {
	       if (fp[fpindex].fq != MISSINGVAL)
		  fp[fpindex].obs_FSdeparture = curstage - fp[fpindex].fq;
	    }
	    
	    else
	    {
	       if (fp[fpindex].fs != MISSINGVAL)
		  fp[fpindex].obs_FSdeparture = curstage - fp[fpindex].fs;
	    }
	    
	    break;
	 }	    
      }
      
      
      /* get the max value for 6 and 24 hour periods */
      
      time(&system_time);
      
      load_stage_in_interval(fpindex, 06, system_time, fp);
      load_stage_in_interval(fpindex, 24, system_time, fp);
      
      
      /* find the times that the stage crested or passed thru flood stage */
      
      load_special_stages(fpindex, fp, OBS_DATA);      
   }  
   
   
   return;
}


/*********************************************************************
   compute_fcst_info()
   
   PURPOSE
   Determines the various aspects of the forecast stage data for each
   forecast point.  The information is defined in terms of index
   values that refer to the specific item in the stage data structure.

   NOTES
   Computed fields for fp[fpindex].fp structure by this function include:
   fcst_max_index,  fcst_Fpdeparture,   
   fcst_fallbelow_time, fcst_riseabove_time, fcst_crest_value/time
   
   The fields based on the previous product info are computed elsewhere.
   
   ********************************************************************/

void compute_fcst_info(int		fpindex,
		       fp_struct	*fp)   
{
   int k;
   double maxstage;
   time_t maxbasistime, minvalidtime;
   
   
   /* initialize all the forecast point stage data */
   
   fp[fpindex].fcst_max_index      = MISSINGVAL;
   fp[fpindex].fcst_FSdeparture    = MISSINGVAL;
   fp[fpindex].fcst_xfcrest_index   = (int) MISSINGVAL;
   fp[fpindex].fcst_riseabove_time = (time_t )MISSINGVAL;
   fp[fpindex].fcst_fallbelow_time = (time_t )MISSINGVAL;
   fp[fpindex].fcst_crest_value    = (double )MISSINGVAL;
   fp[fpindex].fcst_crest_time     = (time_t )MISSINGVAL;
   
   
   /* if forecast values available for this point, process the data */
   
   if (fp[fpindex].numfcstH > 0)
   {
      /* loop on the number of forecasts for the current forecast 
	 point. if the stage being checked exceeds the previous 
	 maximum, then reset the maximum. */
      
      maxstage = -9999.0;
      maxbasistime = (time_t)MISSINGVAL;
      minvalidtime = LONG_MAX;
      
      for (k = 0; k < fp[fpindex].numfcstH; ++k)
      {
	 if (fp[fpindex].fcstH[k].value > maxstage && 
	     fp[fpindex].fcstH[k].value != MISSINGVAL)
	 {
	    maxstage = fp[fpindex].fcstH[k].value;
	    fp[fpindex].fcst_max_index = k;
	 }	    
      }
      
      if (fp[fpindex].pe[0] == 'Q')
      {	 
	 if (fp[fpindex].fq != MISSINGVAL && maxstage != -9999.)
	    fp[fpindex].fcst_FSdeparture = maxstage - fp[fpindex].fq;
      }
      else
      {	 
	 if (fp[fpindex].fs != MISSINGVAL && maxstage != -9999.)
	    fp[fpindex].fcst_FSdeparture = maxstage - fp[fpindex].fs;
      }
      
      
      /* find the times that the stage crested or passed thru
	 a flood stage */
      
      load_special_stages(fpindex, fp, FCST_DATA);   
      
      /* find index fcst_xrcrest_index which is the data of either
         FFX data in the most recent basis time if exist or same
	 as fcst_crest_index.  */
      
      for (k = 0; k < fp[fpindex].numfcstH; ++k)
      {
	 if (fp[fpindex].fcstH[k].basistime > maxbasistime &&
	     fp[fpindex].fcstH[k].basistime != MISSINGVAL)
	 {
	    maxbasistime = fp[fpindex].fcstH[k].basistime;
	   
	 }	    
      }
      
      for (k = 0; k < fp[fpindex].numfcstH; ++k)
      {
          if ((fp[fpindex].fcstH[k].basistime == maxbasistime )&&
	      (maxbasistime != MISSINGVAL))
	  {
	    /* look for the earliest forecast data with extremum
	       as X */
	           
	      if ((fp[fpindex].fcstH[k].validtime < minvalidtime)  &&
	          (fp[fpindex].fcstH[k].validtime != MISSINGVAL)   &&
		  (strcmp(fp[fpindex].fcstH[k].extremum, "X")== 0) &&
		  (fp[fpindex].fcstH[k].value != MISSINGVAL))
	      {
	          minvalidtime = fp[fpindex].fcstH[k].validtime;
	          fp[fpindex].fcst_xfcrest_index = k;
	          
	      }	    
          } 
      }  	           
   }  
   
   return;
}


/*********************************************************************
   load_trend_info()
   
   PURPOSE
   Loads the trend values using observed and forecast data.
   This function also load the riseabove and fallbelow times 
   or the observed and forecast combination.
   
   ********************************************************************/
void load_trend_info(int 		fpindex,
		     fp_struct		*fp)
{   
   static double STAGE_WINDOW;
   
   int		min_set, max_set;
   float	minstage, maxstage;
   time_t	minstage_time, maxstage_time;
   float	checkstage, checkstage_time;
   float	refstage;
   float	compstage;
   int 		i, start_index, end_index;
   int		num_fcst;
   static int   first = TRUE;
   char         token_string[60];
   int          token_len, string_len;
   char 	msgstr[100];
   
   
   /* initialize */
   
   fp[fpindex].obs_trend = MISSINGVAL;
   fp[fpindex].trend     = MISSINGVAL;
        
   minstage = maxstage  = MISSINGVAL;
   refstage = compstage = MISSINGVAL;
   minstage_time = maxstage_time = 0;
      
   
   /* set STAGE_WINDOW as fp[].chg_threshold, if fp[].chg_threshold is
      missing, then use the value from token stage_window */      
   
   if (fp[fpindex].chg_threshold != MISSINGVAL)
      STAGE_WINDOW = fp[fpindex].chg_threshold;
      
   else
   {
     if (first) 
     {
       token_len = strlen("rpf_stage_window");
       get_apps_defaults("rpf_stage_window", &token_len, token_string, 
                	 &string_len);
       if (string_len > 0)
       {
	 STAGE_WINDOW = atof(token_string);
	 
       }        
       else      
       {
          STAGE_WINDOW = DEFAULT_STAGE_WINDOW;
	  sprintf(msgstr, "Error in value for token rpf_stage_window: %s.\n"
	                  "Using default value of %f.\n",
			  token_string, STAGE_WINDOW);
	  log_msg("", msgstr);
       }
         
       first = FALSE;
     }  
   } 
   
     
   /* first compute the observed trend. to do this, then we need to make
      sure there are at least two observed values.  use the current observed
      value as the reference stage and compare it to the most recent max or
      min value that is outside the stage window around the reference stage */
   
   if (fp[fpindex].use_obsH > 1)
   {
      /* find the latest non-missing obs value for use as the reference stage.
         search the data based on the order and the usage of the data. */    
      
      start_index = fp[fpindex].numobsH - 1;
      end_index   = fp[fpindex].numobsH - fp[fpindex].use_obsH;
      
      for (i = start_index; i >= end_index; i--)
      {
         if (fp[fpindex].obsH[i].value != MISSINGVAL)
	 {
	    refstage = fp[fpindex].obsH[i].value;
	    break;
	 }
      }
      
      /* loop on all the stages and find the min or max that is
	 outside the stage window relative to the reference stage. */
	 
      min_set = max_set = FALSE;
      
      start_index = fp[fpindex].numobsH - fp[fpindex].use_obsH;
      end_index =   fp[fpindex].numobsH; 
      
      for (i = start_index; i < end_index; i++)
      {
	 checkstage      = fp[fpindex].obsH[i].value;
	 checkstage_time = fp[fpindex].obsH[i].validtime;
	 
	 
	 /* if the min is not set yet, then initialize it. 
	    if it is set already, then check if there is a later min
	    value. only consider values that are not close in value 
	    to the reference stage. note that in the event of duplicate
	    mins/maxes, it uses the MOST RECENT min/max because the 
	    data is sorted from earliest to latest */ 
	 
	 if (checkstage != MISSINGVAL && refstage != MISSINGVAL)
	 {
	    if (checkstage < (refstage - STAGE_WINDOW) &&
	        (!min_set || checkstage <= minstage))
	    {
	       minstage      = checkstage;
	       minstage_time = checkstage_time;
	       min_set = TRUE;
	    }
	 
	    if (checkstage > (refstage + STAGE_WINDOW) &&
	        (!max_set || checkstage >= maxstage))
	    {
	       maxstage      = checkstage;
	       maxstage_time = checkstage_time;
	       max_set = TRUE;
	    }
	 }
      }
      
      
      /* now that we have info on the min and max stages, find which is the
	 MOST RECENT and will be used to compare against the reference stage. */
      
      if (min_set && max_set)
      {
	 if (minstage_time > maxstage_time)
	    compstage = minstage;
	 else
	    compstage = maxstage;
      }
      
      else if (min_set)
	 compstage = minstage;
      
      else if (max_set)
	 compstage = maxstage;
      
      
      /* now determined the trend. if no mins or maxes were found
	 outside the stage window, then the trend is considered unchanged */
      
      if (min_set || max_set)
      {
	 if (compstage > refstage)
	    fp[fpindex].obs_trend = FALL;
	 else if (compstage < refstage)
	    fp[fpindex].obs_trend = RISE;
	 else
	    fp[fpindex].obs_trend = UNCHANGED;
      }
      
      else
	 fp[fpindex].obs_trend = UNCHANGED;
      
   } /* end of check if numobs used > 1 */
   
   
   /* if only one obs value, set the reference stage of use in the 
      overall trend check below.  the case of one obs value can occur
      because of the vtec obs filter.  note that this check does not 
      account for the possibility of a missing value indicator... */
      
   else if (fp[fpindex].use_obsH == 1)
   {
      start_index = fp[fpindex].numobsH - 1;
      refstage = fp[fpindex].obsH[start_index].value;
   }
   
     
   /* -------------------------------------------------------------- */
   /* set a convenient local variable */
   
   num_fcst = fp[fpindex].numfcstH;
   
   
   /* now compute the general trend.  this uses forecast data
      to determine the expected overall trend. if no forecast
      data exist, then the general trend is set to be the same
      as the observed trend. */
   
   if (fp[fpindex].use_obsH > 1 && num_fcst == 0)
   {
      fp[fpindex].trend = fp[fpindex].obs_trend;
   }
    
   
   /* the reference stage is the latest observed which is compared
      to the first max or min which is found in the forecast data,
      that is outside the stage window around the reference stage.
      if no observed stage is available use the first forecast
      value as the reference stage. */
   
   else if ((fp[fpindex].use_obsH > 0  && num_fcst > 0) ||
	    (fp[fpindex].use_obsH == 0 && num_fcst > 1))
   {
   
      /* use the already determined refstage if obs data available.
         if no obs available, use the first forecast value, assuming
	 that it is not missing, and set the start_index to bypass 
	 the first forecast value. */
      
      if (fp[fpindex].use_obsH > 0)
      {
	 start_index = 0;
      }
      
      else
      {
	 refstage = fp[fpindex].fcstH[0].value;
	 start_index = 1;
      }
      
      min_set = max_set = FALSE;   
      
      for (i = start_index; i < fp[fpindex].numfcstH; i++)
      {
	 checkstage      = fp[fpindex].fcstH[i].value;
	 checkstage_time = fp[fpindex].fcstH[i].validtime;
	 
	 
	 /* if the min is not set yet, then initialize it. 
	    if it is set already, then check if there is a later min
	    value. only consider values that are not close in value 
	    to the reference stage. note that in the event of duplicate
	    mins/maxes, it uses the EARLIEST min/max because the 
	    data is sorted from earliest to latest */ 
	 
	 if (checkstage != MISSINGVAL && refstage != MISSINGVAL)
	 {
	    if (checkstage < (refstage - STAGE_WINDOW) &&
	        (!min_set || checkstage < minstage))
	    {
	       minstage      = checkstage;
	       minstage_time = checkstage_time;
	       min_set = TRUE;
	    }
	 
	    if (checkstage > (refstage + STAGE_WINDOW) &&
	        (!max_set || checkstage > maxstage))
	    {
	       maxstage      = checkstage;
	       maxstage_time = checkstage_time;
	       max_set = TRUE;
	    }
	 }
      }
      
      
      /* now that we have the info on the min and max stages, find
	 which is the EARLIEST and will be used to compare
	 against the reference stage. */
      
      if (min_set && max_set)
      {
	 if (minstage_time < maxstage_time)
	    compstage = minstage;
	 else
	    compstage = maxstage;
      }
      
      else if (min_set)
	 compstage = minstage;
      
      else if (max_set)	 
	 compstage = maxstage;
      
      
      /* now determined the trend. if no mins or maxes were found
	 outside the stage window, then the trend is considered
	 unchanged */
      
      if (min_set || max_set)
      {
	 if (compstage < refstage)
	    fp[fpindex].trend = FALL;
	 
	 else if (compstage > refstage)
	    fp[fpindex].trend = RISE;
	 
	 else
	    fp[fpindex].trend = UNCHANGED;
      }
      
      else
	 fp[fpindex].trend = UNCHANGED;
      
   }
   
      
   return;
}


/*********************************************************************
   load_stage_in_interval()
   
   PURPOSE
   Gets a max observed stage value within a given number of hours.

     
   ********************************************************************/
void load_stage_in_interval(int		fpindex,
			    int		interval,
			    time_t	system_time,
			    fp_struct	*fp)
{
   int 		max_index;
   int 		num_secs, i;
   time_t 	begin_time;
   float 	maxval;
   int		start_index;
   
   
   /* initialize */
      
   max_index = MISSINGVAL;
   maxval    = MISSINGVAL;
   
   
   /* define the beginning of the time window based on the current
      time and the hour interval to look back */

   num_secs = interval * 3600;
   begin_time = system_time - (time_t )num_secs;
   
   
   /* loop thru all the stage values in the time series. 
      set the start index, recognizing the order and usage specs
      of the data */
   
   start_index = fp[fpindex].numobsH - fp[fpindex].use_obsH;
   
   for (i = start_index; i < fp[fpindex].numobsH; i++)
   {
      
      if (fp[fpindex].obsH[i].value != (float )MISSINGVAL)
      {
         if (difftime(fp[fpindex].obsH[i].validtime, begin_time) > 0)
	 {
	    if (max_index == MISSINGVAL)
	    {
	       maxval = fp[fpindex].obsH[i].value;
	       max_index = i;
	    }
	    
	    else if (fp[fpindex].obsH[i].value >= maxval)
	    {
	       maxval = fp[fpindex].obsH[i].value;
	       max_index = i;
	    }
	 }
      }
   }

   
   /* load the values in depending upon the time interval being considered */
   
   if (interval == 06)
      fp[fpindex].obs_max06_index = max_index;
   
   else if (interval == 24)
      fp[fpindex].obs_max24_index = max_index;
   
   return;
}


/*********************************************************************
   load_special_stages()
   
   PURPOSE
   Determine special stage values for a time series, this includes the
   rise-above-flood, fall-below-flood, and crest.  This function needs
   to know whether it is dealing with observed or forecast values.   

   NOTES
   This code assumes that the stages are stored in chronological
   order, where 0 index is the earliest. This is based on an
   ASCending retrieval of the data.   
   
   ********************************************************************/
void load_special_stages(int 		fpindex,
			 fp_struct	*fp,
			 int		obs_or_fcst)
{
   int 		numvals, numobs, numfcst, i;
   double 	stage, prev_stage, fld_level;
   time_t 	riseabove_time, fallbelow_time;
   int 		start_index, crest_index, sustained_crest_index;
   int		prev_trend, cur_trend;
   int 		crest_found, rise_found, fall_found;
   time_t 	stagetime, prev_stagetime;
   double       *temp_value;
   time_t	*temp_timet;

     
   /* initialize ------------------------------------------------ */
   
   rise_found = fall_found = FALSE;
   riseabove_time = fallbelow_time = (time_t )MISSINGVAL;
   
   prev_trend = cur_trend = MISSINGVAL;
   crest_found = FALSE;
   crest_index = sustained_crest_index = MISSINGVAL;
   
   numobs = numfcst = 0;
   
         	 
   /* can't do anything if not enough data */	 
	 
   if ((obs_or_fcst == OBS_DATA  && fp[fpindex].use_obsH == 0) ||
       (obs_or_fcst == FCST_DATA && fp[fpindex].numfcstH == 0))
	 return;

    
   /* load the flood stage/discharge into convenient variable. */
   
   if (fp[fpindex].pe[0] == 'Q')
      fld_level = fp[fpindex].fq;
   else   
      fld_level = fp[fpindex].fs;
      
      
   /* build a convenient time series -------------------------------- */
   
   /* for obs, it only includes the portion of the time series to use, 
      which is constrained possibly by any preceding VTEC event,
      plus the first of any forecast time series as the last value.  
      for fcst, it includes the full fcst time series, plus the latest obs 
      value, if it exists, as the first value.
      numvals is the total number of values in the temporary time series.
      numobs is the number of additional obs values in the fcst time series,
      which at most is one more, representing the latest obs.
      numfcst is the number of additional fcst value in the obs time series,
      which at most is one more, representing the first forecast. */
      
   if (obs_or_fcst == OBS_DATA)
   {
      if (fp[fpindex].numfcstH > 0) 
         numfcst = 1;
      else
         numfcst = 0;
	       
      numvals = fp[fpindex].use_obsH + numfcst;
   }
   
   else
   {
      if (fp[fpindex].use_obsH > 0) 
         numobs = 1;
      else
         numobs = 0;
	 
      numvals = fp[fpindex].numfcstH + numobs; 
   }
   
   
   /* malloc arrays for value and time */
   
   temp_value = (double *) malloc(sizeof(double) * numvals);
   if (temp_value == NULL)
   {
      log_msg(FAILED_MALLOC, "of temp_value in load_special_stages");
      return;
   }
   
   temp_timet = (time_t *) malloc(sizeof(time_t) * numvals);
   if (temp_timet == NULL)
   {
      log_msg(FAILED_MALLOC, "of temp_time_t in load_special_stages");
      return;
   }
   
   
   /* load the temp data into the convenient arrays. */
   
   if (obs_or_fcst == OBS_DATA)
   {
      start_index = fp[fpindex].numobsH - fp[fpindex].use_obsH;
      
      for (i = 0; i < fp[fpindex].use_obsH; i++)
      {
         temp_value[i] = fp[fpindex].obsH[start_index + i].value;
         temp_timet[i] = fp[fpindex].obsH[start_index + i].validtime;
      }
      
      if (numfcst > 0)
      {
         temp_value[numvals - 1] = fp[fpindex].fcstH[0].value;
         temp_timet[numvals - 1] = fp[fpindex].fcstH[0].validtime;
      }
   }
   
   else
   {     
      if (numobs > 0) 
      {
         start_index = fp[fpindex].numobsH - 1;
	 
	 temp_value[0] = fp[fpindex].obsH[start_index].value;
	 temp_timet[0] = fp[fpindex].obsH[start_index].validtime;
      }
      
      for (i = 0; i < fp[fpindex].numfcstH; i++)
      {
	 temp_value[numobs + i] = fp[fpindex].fcstH[i].value;
	 temp_timet[numobs + i] = fp[fpindex].fcstH[i].validtime;
      }
   }     
   
   
   /* compute the crest value ------------------------------------ */   
     
   /* loop on the number of stage values in chronological order.
      note that the order of the looping and how the if checks affect
      multiple crests. also note that in the event of a sustained stage
      at flood stage, the pass thru stage time is the last in the series
      of sustained values, not the first. */
   
   for (i = 1; i < numvals; i++)
   {
      prev_stage     = temp_value[i-1];
      prev_stagetime = temp_timet[i-1];
	 
      stage          = temp_value[i];
      stagetime      = temp_timet[i];
            
      
      /* perform the trend check on the very first pair of values. */
	 
      if (i == 1) 
      {
         if (stage > prev_stage)
            prev_trend = RISE;
         else if (stage == prev_stage)
            prev_trend = UNCHANGED;
         else
            prev_trend = FALL;  
	        
      }   
      
      /* check if the value has crested; this method defines a crest
	 as a maximum surrounded by values that are either equal to or below.
	 use the idea of tracking the trend of the stage to find a crest;
	 the method allows for detection of sustained crests. */
      
      else
      {
	 /* this check ensures that the crests, whether forecast or observed,
	    are the crests that occur closest to the current time. */
	 
	 if (obs_or_fcst == OBS_DATA || (crest_found == FALSE))
	 {
	    /* determine the current trend for later use */
	    
	    if (stage > prev_stage)
	       cur_trend = RISE;	    
	    else if (stage == prev_stage)
	       cur_trend = UNCHANGED;	    
	    else
	       cur_trend = FALL;
	    
	    
	    /* adjust the current trend value from unchanged to rise if the 
	       previous trend was a rise; this allows for the detection of
	       sustained crests. also define the beginning of the sustained
	       crest in the event that it undefined */
	    
	    if (cur_trend == UNCHANGED && prev_trend == RISE)
	    {
	       cur_trend = RISE;
	       if (sustained_crest_index == MISSINGVAL) 
		  sustained_crest_index = i - 1;
	    }
	    
	    else if (cur_trend == RISE)
	       sustained_crest_index = MISSINGVAL;
	    
	    
	    /* assign the crest index where a crest occurs if the
	       previous trend was a rise and the current trend is a fall */
	    
	    if (prev_trend == RISE && cur_trend == FALL)
	    {
	       if (sustained_crest_index == MISSINGVAL)
		  crest_index = i - 1;
	       else
		  crest_index = sustained_crest_index;
	       
	       crest_found = TRUE;
	    }
	    
	    
	    /* set the previous trend to be the current trend in 
	       preparation for the next time thru the loop */
	    
	    prev_trend = cur_trend;
	    
	 } /* end of if check on whether observed or crest_found */
      }    /* working on second pair */          
   }       /* end of for loop */
   
    
   /* load the crest value and time */
      
   if (obs_or_fcst == OBS_DATA)
   {
      if (crest_index != MISSINGVAL)
      {
         fp[fpindex].obs_crest_value = temp_value[crest_index];
         fp[fpindex].obs_crest_time  = temp_timet[crest_index];
      }
      else
      {
         fp[fpindex].obs_crest_value = (double )MISSINGVAL;
         fp[fpindex].obs_crest_time  = (time_t )MISSINGVAL;
      }
   }
   
   else
   {
      if (crest_index != MISSINGVAL)
      { 
         fp[fpindex].fcst_crest_value = temp_value[crest_index];
         fp[fpindex].fcst_crest_time  = temp_timet[crest_index];
      }
      else
      {
         fp[fpindex].fcst_crest_value = (double )MISSINGVAL;
         fp[fpindex].fcst_crest_time  = (time_t )MISSINGVAL;
      }
   }
    
   
   /* compute the pass thru flood times ------------------------------ */
   
   /* if flood stage not defined, force it to skip over the section
      below, and return with the initialized missing values. */
   
   if (fld_level == MISSINGVAL)
   {      
      numvals = 0;  
   }
   
      
   /* loop on the values in chronological order.
      note that the order of the looping and the if checks affect how
      multiple crests or pass thru events are handled.
      also note that in the event of a sustained stage at flood stage,
      the pass thru stage time is the last in the series of sustained
      values, not the first */
   
   for (i = 1; i < numvals; i++)
   {
      prev_stage     = temp_value[i-1];
      prev_stagetime = temp_timet[i-1];
	 
      stage          = temp_value[i];
      stagetime      = temp_timet[i];
            
      
      /* check if the stage value has risen above the flood stage; 
	 compute the time of stage via interpolation.
	 if multiple rises, set the rise to be the one nearest the 
	 current time, for both observed and forecast data.
	 for the rise above check, it is considered a
	 rise above even if it only hits the flood stage precisely.
	 for obs data, do not assign a rise above if it is based on the 
	 appended last forecast value, since this type of rise should be
	 associated with the forecast rise above. */      
	 
      if (prev_stage < fld_level && fld_level <= stage)
      {
	 if ((obs_or_fcst == OBS_DATA  && (numfcst == 0 || i != (numvals - 1))) ||
	     (obs_or_fcst == FCST_DATA && rise_found == FALSE))
	 {
	    rise_found = TRUE;
	    
	    if (prev_stage == stage)
	       riseabove_time = prev_stagetime;
	    else
	       riseabove_time = stagetime - 
		  ((stage - fld_level) / (stage - prev_stage)) *
		   (stagetime - prev_stagetime);
	 }
      }
      
      
      /* check if the stage value has passed below the flood stage.
         for both obs and fcst, always use the latest fall below.
	 for the fall below check, it is considered a fall
	 below only if it truly falls below the flood level.
	 for obs data, do not assign a fall below if it is based on the 
	 appended last forecast value, since this type of fall should be
	 associated with the forecast rise above.  */
      
      if (prev_stage >= fld_level && fld_level > stage)
      {
	 if ((obs_or_fcst == OBS_DATA  && (numfcst == 0 || i != (numvals - 1))) ||
	     (obs_or_fcst == FCST_DATA))
	 {
	    fall_found = TRUE;
	 
	    if (prev_stage == stage)
	       fallbelow_time = prev_stagetime;
	    else
	       fallbelow_time = stagetime - 
	         ((stage - fld_level) / (stage - prev_stage)) *
	          (stagetime - prev_stagetime);
	 }      
      }
   }
            
   
   /* have special check for forecast time series ending above flood stage
      with there being a fall below time, because of a prior period
      above flood stage.  In this case, make the fall below undefined
      to reflect the uncertainty. */
      
   if (numvals >= 1 && obs_or_fcst == FCST_DATA)
   {     
      stage = temp_value[numvals - 1];
      if (stage > fld_level)
      {
          fallbelow_time = MISSINGVAL;
      }      
   }

    
   /* load the pass-thru time values */
      
   if (obs_or_fcst == OBS_DATA)
   {
      fp[fpindex].obs_fallbelow_time = fallbelow_time;
      fp[fpindex].obs_riseabove_time = riseabove_time;
      
      if (fallbelow_time < riseabove_time)
         fp[fpindex].obs_fallbelow_time = MISSINGVAL;
   }
   
   else
   {
      fp[fpindex].fcst_fallbelow_time = fallbelow_time;
      fp[fpindex].fcst_riseabove_time = riseabove_time;
      
      if (fallbelow_time < riseabove_time)
         fp[fpindex].fcst_fallbelow_time = MISSINGVAL;
   }
   
   
   /* free memory ---------------- */
   
   if (temp_value != NULL)
      free(temp_value);
      
   if (temp_timet != NULL)
      free(temp_timet);
      
   
   return;
}


/*********************************************************************
   compute_grp_county_full_info()
   
   PURPOSE
   Compute the grp and county info that is dependent on the full
   time series for the forecast points in the group or county.
   Because the forecast point full time series retrieval takes a 
   moment, it is only done as needed, and this function is only called
   when the full time series are loaded.
   
   ********************************************************************/
void compute_grp_county_full_info(fp_struct		*fp,
				  int			numgrps,
				  grp_struct	        *grp,
				  int                	numcnty,
				  county_struct      	*cnty)
{
   time_t	obs_risetime,  obs_falltime;
   time_t	fcst_risetime, fcst_falltime;
   int 		i, j, fpindex;
   
      
   /* process the group information */
   
   for (i = 0; i < numgrps; ++i)
   { 
      /* initialize the local variables */
      
      obs_risetime  = obs_falltime  = MISSINGVAL;
      fcst_risetime = fcst_falltime = MISSINGVAL;
      
      
      /* loop on the number of forecast points in the group */
      
      for (j = 0; j < grp[i].numfps; ++j)
      {
	 fpindex = grp[i].fpindex[j];
	 
	 /* only process the data if the full time series
	    has been loaded, which would mean that the
	    riseabove and fallbelow values are also loaded,
	    or the values are uninitialized */
	 
	 if (fp[fpindex].use_obsH > 0)
	 {
	    if ((fp[fpindex].obs_riseabove_time != MISSINGVAL &&
	         fp[fpindex].obs_riseabove_time < obs_risetime) ||
	        (fp[fpindex].obs_riseabove_time != MISSINGVAL &&
		 obs_risetime == MISSINGVAL))	
	       obs_risetime = fp[fpindex].obs_riseabove_time;
	 
   	   if ((fp[fpindex].obs_fallbelow_time != MISSINGVAL &&
	        fp[fpindex].obs_fallbelow_time > obs_falltime) ||
		(fp[fpindex].obs_fallbelow_time != MISSINGVAL &&
		 obs_falltime == MISSINGVAL))
	       obs_falltime = fp[fpindex].obs_fallbelow_time;
	 }
	 
	 if (fp[fpindex].numfcstH > 0)
	 {
	    if ((fp[fpindex].fcst_riseabove_time != MISSINGVAL &&
	         fp[fpindex].fcst_riseabove_time < fcst_risetime) ||
		(fp[fpindex].fcst_riseabove_time != MISSINGVAL &&
		 fcst_risetime == MISSINGVAL))
	       fcst_risetime = fp[fpindex].fcst_riseabove_time;
	 
	    if ((fp[fpindex].fcst_fallbelow_time != MISSINGVAL &&
	         fp[fpindex].fcst_fallbelow_time > fcst_falltime) ||
		(fp[fpindex].fcst_fallbelow_time != MISSINGVAL &&
		 fcst_falltime == MISSINGVAL))
	       fcst_falltime = fp[fpindex].fcst_fallbelow_time;
	 }
      }
      
      
      /* load in the data values */
      
      grp[i].obs_riseabove_time = obs_risetime;
      grp[i].obs_fallbelow_time = obs_falltime;
      
      grp[i].fcst_riseabove_time = fcst_risetime;
      grp[i].fcst_fallbelow_time = fcst_falltime;
      
      
      /* use the earliest of the rise and latest of the fall variables,
	 to be conservative. */
      
      if (obs_risetime != MISSINGVAL)
	 grp[i].riseabove_time = obs_risetime;
      else
	 grp[i].riseabove_time = fcst_risetime;
	 
      if (fcst_falltime != MISSINGVAL)
	 grp[i].fallbelow_time = fcst_falltime;
      else
	 grp[i].fallbelow_time = obs_falltime;
   }
   
   
   /* process the county information in a similar fashion */
   
   for (i = 0; i < numcnty; ++i)
   { 
      /* initialize the local variables */
      
      obs_risetime  = obs_falltime  = MISSINGVAL;
      fcst_risetime = fcst_falltime = MISSINGVAL;
      
      
      /* loop on the number of forecast points in the group */
      
      for (j = 0; j < cnty[i].numfps; ++j)
      {
	 fpindex = cnty[i].fpindex[j];
	 
	 
	 if (fp[fpindex].use_obsH > 0)
	 {
	    if ((fp[fpindex].obs_riseabove_time != MISSINGVAL &&
	        fp[fpindex].obs_riseabove_time < obs_risetime) ||
		(fp[fpindex].obs_riseabove_time != MISSINGVAL &&
		 obs_risetime == MISSINGVAL))
	       obs_risetime = fp[fpindex].obs_riseabove_time;
	 
	    if ((fp[fpindex].obs_fallbelow_time != MISSINGVAL &&
	        fp[fpindex].obs_fallbelow_time > obs_falltime) ||
		(fp[fpindex].obs_fallbelow_time != MISSINGVAL &&
		 obs_falltime == MISSINGVAL))
	       obs_falltime = fp[fpindex].obs_fallbelow_time;
	 }
	 
	 
	 if (fp[fpindex].numfcstH > 0)
	 {
	    if ((fp[fpindex].fcst_riseabove_time != MISSINGVAL &&
	        fp[fpindex].fcst_riseabove_time < fcst_risetime) ||
		(fp[fpindex].fcst_riseabove_time != MISSINGVAL &&
		fcst_risetime == MISSINGVAL))
	       fcst_risetime = fp[fpindex].fcst_riseabove_time;
	 
	    if ((fp[fpindex].fcst_fallbelow_time != MISSINGVAL &&
	        fp[fpindex].fcst_fallbelow_time > fcst_falltime) ||
		(fp[fpindex].fcst_fallbelow_time != MISSINGVAL &&
		fcst_falltime == MISSINGVAL))
	       fcst_falltime = fp[fpindex].fcst_fallbelow_time;
	 }
      }
      
      
      /* load in the data values */
      
      cnty[i].obs_riseabove_time = obs_risetime;
      cnty[i].obs_fallbelow_time = obs_falltime;
      
      cnty[i].fcst_riseabove_time = fcst_risetime;
      cnty[i].fcst_fallbelow_time = fcst_falltime;
      
      
      /* use the earliest of the rise and latest of the fall variables. */
      
      if (obs_risetime != MISSINGVAL)
	 cnty[i].riseabove_time = obs_risetime;
      else
	 cnty[i].riseabove_time = fcst_risetime;
	 
      if (fcst_falltime != MISSINGVAL)
	 cnty[i].fallbelow_time = fcst_falltime;
      else
	 cnty[i].fallbelow_time = obs_falltime;
   }
   
   return;
}

   
/***********************************************************
   compute_fp_risefall()
   
   PURPOSE
   Determine the overall riseabove_time and fallbelow_time for
   the forecast point.
   **********************************************************/

void compute_fp_risefall(int		fpindex,
		        fp_struct	*fp)   
{         
   /* determine the rise above time */
   /* if there are both obs and fcst times, use the obs rise above, 
      and the fcst fall below */
   
   if (fp[fpindex].obs_riseabove_time != MISSINGVAL)
   {
      fp[fpindex].riseabove_time = fp[fpindex].obs_riseabove_time;
      strcpy(fp[fpindex].riseabove_ts, "R*");
   }   
   else if (fp[fpindex].fcst_riseabove_time != MISSINGVAL)
   {
      fp[fpindex].riseabove_time = fp[fpindex].fcst_riseabove_time;
      strcpy(fp[fpindex].riseabove_ts, "F*");
   }   
   else
   {
      fp[fpindex].riseabove_time = MISSINGVAL;
      strcpy(fp[fpindex].riseabove_ts, "");
   } 
   
     
   /* determine the fall below info.  */             
 	          
   if (fp[fpindex].fcst_fallbelow_time != MISSINGVAL)
   {
      fp[fpindex].fallbelow_time = fp[fpindex].fcst_fallbelow_time; 
      strcpy(fp[fpindex].fallbelow_ts, "F*");
   }
   
   else if (fp[fpindex].obs_fallbelow_time != MISSINGVAL)
   {
      fp[fpindex].fallbelow_time = fp[fpindex].obs_fallbelow_time;
      strcpy(fp[fpindex].fallbelow_ts, "R*");
   }     
   else 
   {
      fp[fpindex].fallbelow_time = MISSINGVAL;
      strcpy(fp[fpindex].fallbelow_ts, "");
   }
      
   
   /* decide the crest_ts if both obs and fcst times exit*/
   
    if (fp[fpindex].obs_max_index  != MISSINGVAL &&
	fp[fpindex].fcst_max_index != MISSINGVAL)
    {
       if (fp[fpindex].obsH[fp[fpindex].obs_max_index].value >= 
	   fp[fpindex].fcstH[fp[fpindex].fcst_max_index].value)       
	  strcpy(fp[fpindex].crest_ts, "R*");
       
       else       
	  strcpy(fp[fpindex].crest_ts, "F*");       
    }
    
    else if (fp[fpindex].obs_max_index != MISSINGVAL) 
       strcpy(fp[fpindex].crest_ts, "R*");    

    else if (fp[fpindex].fcst_max_index != MISSINGVAL)       
       strcpy(fp[fpindex].crest_ts, "F*");

    else      
       strcpy(fp[fpindex].crest_ts, "");

      
   return;       
} 

   
/*********************************************************************
   compute_detailed_fcst_info()
   
   PURPOSE
   Determines the various aspects of the forecast stage data for each
   forecast point. Including the first, last, min and max inflection stage/flow
   and the time from the last observed data and the full time series
   for a forecast point. 
   
   ********************************************************************/

void compute_detailed_fcst_info(int		fpindex,
				fp_struct	*fp)   
{
   int    k, inflection_cnt;
   double *inflection_value, *temp_value, flat_index;  
   int    *inflection_validtime, *temp_validtime;  
   char   msgstr[300];
   int    hydrograph_points_cnt;
   int    obs_cur_index = MISSINGVAL;
   
   /* initialize all the forecast point stage data */
   
   fp[fpindex].fcst_max_value    = MISSINGVAL;
   fp[fpindex].fcst_min_value    = MISSINGVAL;
   fp[fpindex].fcst_first_value  = MISSINGVAL;
   fp[fpindex].fcst_last_value   = MISSINGVAL;
   fp[fpindex].fcst_maxvalue_time = (time_t )MISSINGVAL;
   fp[fpindex].fcst_minvalue_time = (time_t )MISSINGVAL;
   fp[fpindex].fcst_firstvalue_time = (time_t )MISSINGVAL;
   fp[fpindex].fcst_lastvalue_time  = (time_t )MISSINGVAL; 
   
   
  /* flat_index = BE_FLAT_THRESHOLD;*/
  flat_index = 0.09;
   //base_cnt = NUM_OF_BASEPOINTS;
        
   if (fp[fpindex].numfcstH == 0)
   {
      sprintf(msgstr, "Warning - No forecast data found for %s in" 
	      " compute_detailed_fcst_info", fp[fpindex].id);    
      log_msg("", msgstr);
      return;
   }        
   
   /* use the last observed data as the first point in the trend.If missing,
      use the first forecast data*/

   obs_cur_index = fp[fpindex].obs_cur_index;
   if ( obs_cur_index != MISSINGVAL &&
        fp[fpindex].obsH[obs_cur_index].value != MISSINGVAL)
       
   {
      fp[fpindex].fcst_first_value = fp[fpindex].obsH[obs_cur_index].value;
      fp[fpindex].fcst_firstvalue_time = fp[fpindex].obsH[obs_cur_index].validtime;
      hydrograph_points_cnt = fp[fpindex].numfcstH + 1;
   }
   
   else
   {
      /* loop on the number of forecasts for the forecast 
	 point to find the first stage/flow and the time */
            	 
      for (k = 0; k < fp[fpindex].numfcstH; k++)
      {	   
	 fp[fpindex].fcst_first_value = fp[fpindex].fcstH[k].value;
	 fp[fpindex].fcst_firstvalue_time = fp[fpindex].fcstH[k].validtime;

	 break;	   	    
      }
      	      
      hydrograph_points_cnt = fp[fpindex].numfcstH;
   }
   
   /* no need to output trend phrase for less than two forecast data */
   
   if (hydrograph_points_cnt <= 1)
   {
      sprintf(msgstr, "Warning - less than 2 timeseries data for %s in" 
	              " compute_detailed_fcst_info", fp[fpindex].id);    
      log_msg("", msgstr);
      return;
   }
     
   
   /* get the last point data in the full time series */
        
   /* loop on the number of forecasts for the current forecast 
      point to find the last stage/flow and the time */

   for (k = fp[fpindex].numfcstH - 1; k >= 0; k--)
   {	 
      fp[fpindex].fcst_last_value = fp[fpindex].fcstH[k].value;
      fp[fpindex].fcst_lastvalue_time = fp[fpindex].fcstH[k].validtime;

      break;	 	    
   }   
         
   /* find the inflection points in the time series */
   
   /* malloc and initialize array */
   
   temp_value = (double *) malloc(sizeof(double) * (hydrograph_points_cnt));
   if (temp_value == NULL)
   {
      log_msg(FAILED_MALLOC, "of temp_value in compute_detailed_fcst_info for <FcstTrend> variable");
      return;
   }
   
   temp_validtime = (int *) malloc(sizeof(int) * (hydrograph_points_cnt));
   if (temp_validtime == NULL)
   {
      log_msg(FAILED_MALLOC, "of temp_validtime in compute_detailed_fcst_info for <FcstTrend> variable");
      return;
   }
   
   inflection_value = (double *) malloc(sizeof(double) * (  2 * hydrograph_points_cnt));
   if (inflection_value == NULL)
   {
      log_msg(FAILED_MALLOC, "of inflection_value in compute_detailed_fcst_info for <FcstTrend> variable");
      return;
   }	  
   
   inflection_validtime = (int *) malloc(sizeof(int) * ( 2 * hydrograph_points_cnt));
   if (inflection_validtime == NULL)
   {
      log_msg(FAILED_MALLOC, "of inflection_validtime in compute_detailed_fcst_info for <FcstTrend> variable");
      return;
   }	 
   
   /* initialize temp_value and temp_validtime */
   
   for (k = 0; k < hydrograph_points_cnt; k++)
   {      
      temp_value[k] = MISSINGVAL;
      temp_validtime[k] = MISSINGVAL;
   }	  
   
   /* initialize inflection_value and inflection_validtime */
   
   for (k = 0; k < 2 *(hydrograph_points_cnt); k++)
   {
      inflection_value[k] = MISSINGVAL;
      inflection_validtime[k] = MISSINGVAL;
      
   }
   
   /* build temp_value and temp_validtime array to hold first data and
      the forecast data */       
   
   temp_value[0] = fp[fpindex].fcst_first_value;
   temp_validtime[0] = fp[fpindex].fcst_firstvalue_time;
   
   if (obs_cur_index != MISSINGVAL &&
       fp[fpindex].obsH[obs_cur_index].value != MISSINGVAL)
   {
      for (k = 0; k < (hydrograph_points_cnt - 1); k++)
      {
         temp_value[k+1] = fp[fpindex].fcstH[k].value;
         temp_validtime[k+1] = fp[fpindex].fcstH[k].validtime;
      }
   }
   
   else
   {
      for (k = 1; k < (hydrograph_points_cnt); k++)
      {
         temp_value[k] = fp[fpindex].fcstH[k].value;
	 temp_validtime[k] = fp[fpindex].fcstH[k].validtime;
      }
   }
   
   /* initialize the inflection points count*/
   
   inflection_cnt = 0;
   
   
   /* for hydrograph_points_cnt is 2, set inflection_cnt = 0 */
   
   if (hydrograph_points_cnt == 2)
   {
      inflection_cnt = 0;
   }
   else if (hydrograph_points_cnt == 3)
   {
      inflection_cnt = 1;
      inflection_value[0] = temp_value[1];
      inflection_validtime[0] = temp_validtime[1];
   }      
          
   /* for hydrograph_points_cnt data number greater than 3 */
   
   else
   { 
      for (k = 1; k < hydrograph_points_cnt -1; k++)
      {
       
          /* if the checed point is the 2nd data in the hydrograph */
	  
	  if (k == 1)
	  {
              if (((temp_value[1]- temp_value[0]) >= flat_index) &&
	          (fabs(temp_value[1] - temp_value[2]) < 0.001) &&
	          (fabs(temp_value[1] - temp_value[3]) < 0.001))	  
	      {
		 inflection_value[inflection_cnt] = temp_value[1];
		 inflection_validtime[inflection_cnt] = temp_validtime[1];
		 inflection_cnt++;
	      }

	      else if (((temp_value[0] - temp_value[1]) >= flat_index)  &&		        
		       (fabs(temp_value[1] - temp_value[2]) < 0.001) &&
		       (fabs(temp_value[1] - temp_value[3]) < 0.001))	  
	      {
		 inflection_value[inflection_cnt] = temp_value[1];
		 inflection_validtime[inflection_cnt] = temp_validtime[1];
		 inflection_cnt++;
	      }  

	      else if (((temp_value[1]- temp_value[0]) >= flat_index) &&		        
		       ((temp_value[1] - temp_value[2]) >= flat_index) &&		        
		       ((temp_value[1] - temp_value[3]) >= flat_index) &&
		       ((temp_value[2] - temp_value[3]) > 0.001))	  
	      {
		 inflection_value[inflection_cnt] = temp_value[1];
		 inflection_validtime[inflection_cnt] = temp_validtime[1];
		 inflection_cnt++;
	      }

	      else if (((temp_value[0]- temp_value[1]) >= flat_index) &&		        
		       ((temp_value[2] - temp_value[1]) >= flat_index) &&		        
		       ((temp_value[3] - temp_value[1]) >= flat_index) &&
		       ((temp_value[3] - temp_value[2]) > 0.001))	  
	      {
		 inflection_value[inflection_cnt] = temp_value[1];
		 inflection_validtime[inflection_cnt] = temp_validtime[1];
		 inflection_cnt++;
	      }
	      
	      else if ((fabs(temp_value[1] - temp_value[0]) < 0.001) &&
	                ((temp_value[1] - temp_value[2]) >= flat_index) &&
			((temp_value[1] - temp_value[3]) >= flat_index) &&
			((temp_value[2] - temp_value[3]) > 0.001))
              {
	         inflection_value[inflection_cnt] = temp_value[1];
		 inflection_validtime[inflection_cnt] = temp_validtime[1];
		 inflection_cnt++;
	      
	      }
	      
	      else if ((fabs(temp_value[1] - temp_value[0]) < 0.001) &&
	               ((temp_value[2] - temp_value[1]) >= flat_index) &&
		       ((temp_value[3] - temp_value[1]) >= flat_index) &&
		       ((temp_value[3] - temp_value[2]) > 0.001))
              {
	         inflection_value[inflection_cnt] = temp_value[1];
		 inflection_validtime[inflection_cnt] = temp_validtime[1];
		 inflection_cnt++;
	      
	      }
	      			 
	  }
	  
	  /* if the checked point is the one before last point in the hydrgraph*/
	  else if ( k == hydrograph_points_cnt - 2)
	  {	       
	      if (((temp_value[k]- temp_value[k+1]) >= flat_index) &&
		  (fabs(temp_value[k] - temp_value[k-1]) < 0.001) &&
		  (fabs(temp_value[k] - temp_value[k-2]) < 0.001))	  
	      {
		 inflection_value[inflection_cnt] = temp_value[k];
		 inflection_validtime[inflection_cnt] = temp_validtime[k];
		 inflection_cnt++;
	      }

	      else if (((temp_value[k+1]- temp_value[k]) >= flat_index) &&
		       (fabs(temp_value[k] - temp_value[k-1]) < 0.001) &&
		       (fabs(temp_value[k] - temp_value[k-2]) < 0.001))	 
	      {
		 inflection_value[inflection_cnt] = temp_value[k];
		 inflection_validtime[inflection_cnt] = temp_validtime[k];
		 inflection_cnt++;
	      }  

	      else if (((temp_value[k] - temp_value[k+1]) >= flat_index) &&		  
		       ((temp_value[k] - temp_value[k-1]) >= flat_index) &&		  
		       ((temp_value[k] - temp_value[k-2]) >= flat_index) &&
		       ((temp_value[k-1] - temp_value[k-2]) > 0.001))	  
	      {
		 inflection_value[inflection_cnt] = temp_value[k];
		 inflection_validtime[inflection_cnt] = temp_validtime[k];
		 inflection_cnt++;
	      }

	      else if (((temp_value[k+1] - temp_value[k]) >= flat_index) &&		  
		       ((temp_value[k-1] - temp_value[k]) >= flat_index) &&		  
		       ((temp_value[k-2] - temp_value[k]) >= flat_index) &&
		       ((temp_value[k-2] - temp_value[k-1]) > 0.001))	  
	      {
		 inflection_value[inflection_cnt] = temp_value[k];
		 inflection_validtime[inflection_cnt] = temp_validtime[k];
		 inflection_cnt++;
	      }
	      
	      else if (((temp_value[k-1] - temp_value[k]) >= flat_index) &&
	               ((temp_value[k-2] - temp_value[k]) >= flat_index) &&
		       (( temp_value[k-2] - temp_value[k-1]) > 0.001) &&
		       (fabs(temp_value[k] - temp_value[k+1]) < 0.001))
	      {
	         inflection_value[inflection_cnt] = temp_value[k];
		 inflection_validtime[inflection_cnt] = temp_validtime[k];
		 inflection_cnt++;
	      
	      
	      }	 
	      
	      else if (((temp_value[k] - temp_value[k-1]) >= flat_index) &&
	               ((temp_value[k] - temp_value[k-2]) >= flat_index) &&
		       (( temp_value[k-1] - temp_value[k-2]) > 0.001) &&
		       (fabs(temp_value[k] - temp_value[k+1]) < 0.001))
	      {
	         inflection_value[inflection_cnt] = temp_value[k];
		 inflection_validtime[inflection_cnt] = temp_validtime[k];
		 inflection_cnt++;	      	      
	      }	        
	  	 	  
	  }    
	  else 
	  {	 		 	
	      /* check one side has 2 points, another side has 1 points */

	     if (((temp_value[k] - temp_value[k-1]) >= flat_index) &&		 
		  ((temp_value[k] - temp_value[k-2]) >= flat_index) &&	     		 
		  ((temp_value[k] - temp_value[k+1]) >= flat_index) &&
		  ((temp_value[k-1] - temp_value[k-2]) > 0.001))	     
	     {	     
		inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;
	     }

	     else if (((temp_value[k] - temp_value[k-1]) >= flat_index) &&	     		 
		      ((temp_value[k] - temp_value[k+1]) >= flat_index) &&		 
		      ((temp_value[k] - temp_value[k+2]) >= flat_index) &&
		      ((temp_value[k+1] - temp_value[k+2]) > 0.001))	     
	     {	     
		inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;
	     }

             
	     else if (((temp_value[k-1] - temp_value[k]) >= flat_index) &&		  
		      ((temp_value[k-2] - temp_value[k]) >= flat_index) &&	     		      
		      ((temp_value[k+1] - temp_value[k]) >= flat_index) &&
		      ((temp_value[k-2] - temp_value[k-1]) > 0.001))	    
	     {	     
		inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;
	     }

	     else if (((temp_value[k-1] - temp_value[k]) >= flat_index) &&	     		  
		      ((temp_value[k+1] - temp_value[k]) >= flat_index) &&		  
		      ((temp_value[k+2] - temp_value[k]) >= flat_index) &&
		      ((temp_value[k+2] - temp_value[k+1]) > 0.001))	     
	     {	     
		inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;
	     }

	     else if ((fabs(temp_value[k] - temp_value[k-1]) < 0.001) &&	    		  
		      ((temp_value[k] - temp_value[k+1]) >= flat_index) &&		     
		      ((temp_value[k] - temp_value[k+2]) >= flat_index) &&
		      ((temp_value[k+1] - temp_value[k+2]) > 0.001)) 
	     {	     
		inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;
	     }	
	      
	     else if ((fabs(temp_value[k] - temp_value[k-1]) < 0.001) &&	    		  
		      ((temp_value[k+1] - temp_value[k]) >= flat_index) &&		      
		      ((temp_value[k+2] - temp_value[k]) >= flat_index) &&
		      ((temp_value[k+2] - temp_value[k+1]) > 0.001)) 
	     {	     
		inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;
	     }

	     else if ((fabs(temp_value[k] - temp_value[k-1]) < 0.001) &&
		      (fabs(temp_value[k] - temp_value[k-2]) < 0.001) &&	     		 
		      (fabs(temp_value[k] - temp_value[k+1]) >= flat_index))	     	     
	     {	     
		inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;
	     }	    	     
	     
	     else if ((fabs(temp_value[k] - temp_value[k+1]) < 0.001 ) &&
	              (fabs(temp_value[k] - temp_value[k+2]) < 0.001) &&
		      (fabs(temp_value[k] - temp_value[k-1]) >= flat_index))
             {
	        inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;
	     
	     }
	     else if ((fabs(temp_value[k] - temp_value[k+1]) < 0.001 ) &&
	              ((temp_value[k] - temp_value[k-1]) >= flat_index) &&			      
		      ((temp_value[k] - temp_value[k-2]) >= flat_index) &&
		      ((temp_value[k-1] - temp_value[k-2]) > 0.001))
             {
	        inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;
	     
	     }
	     else if ((fabs(temp_value[k] - temp_value[k+1]) < 0.001 ) &&
	              ((temp_value[k-1] - temp_value[k]) >= flat_index) &&			      	      
		      ((temp_value[k-2] - temp_value[k]) >= flat_index) &&
		      ((temp_value[k-2] - temp_value[k-1]) > 0.001))
             {
	        inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;
	     
	     }
	     
	     else if ((fabs(temp_value[k] - temp_value[k-1]) < 0.001) &&
	              (fabs(temp_value[k] - temp_value[k-2]) < 0.001) &&
		      ((temp_value[k+1] - temp_value[k]) >= flat_index))
             {
	        inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;	     
	     }		      	
	    
	     else if ((fabs(temp_value[k] - temp_value[k+1]) < 0.001) &&
	              (fabs(temp_value[k] - temp_value[k+2]) < 0.001) &&
		      ((temp_value[k-1] - temp_value[k]) >= flat_index))
             {
	        inflection_value[inflection_cnt] = temp_value[k];
		inflection_validtime[inflection_cnt] = temp_validtime[k];
		inflection_cnt++;	     
	     }		     	     
          }	  
      } 	
      
   } /* end of finding inflection points where hydrograph_points_cnt>3 */ 
          
       
    sprintf(msgstr, "<FcstTrend> variable: the number of inflection for the hydrograph "
                    "(excluding first and last points) is %d", inflection_cnt);
    log_msg("", msgstr);                                        

    /* determine fp[].fcst_max_value, fp[].fcst_maxvalue_validitme, 
       fp[].fcst_min_value and fp[].fcst_minvalue_validitme*/

    if (inflection_cnt == 0)
    {
       fp[fpindex].fcst_max_value = MISSINGVAL;
       fp[fpindex].fcst_maxvalue_time = MISSINGVAL;
       fp[fpindex].fcst_min_value= MISSINGVAL;
       fp[fpindex].fcst_minvalue_time = MISSINGVAL;
    }

    else if (inflection_cnt == 1)
    {     
       fp[fpindex].fcst_max_value = inflection_value[0];
       fp[fpindex].fcst_maxvalue_time = inflection_validtime[0];
       fp[fpindex].fcst_min_value = MISSINGVAL;
       fp[fpindex].fcst_minvalue_time = MISSINGVAL; 

    }
    else 
    {              
       /* find the inflection point with the maximum stage/flow 
          If has same inflection_value, set the
	  earliest one as maximum*/

       for (k=0; k < inflection_cnt; k++)
       {
	  if (inflection_value[k] > fp[fpindex].fcst_max_value)
	  {
             fp[fpindex].fcst_max_value = inflection_value[k];
             fp[fpindex].fcst_maxvalue_time = inflection_validtime[k];
	  }
       }
       
       /* find the inflection point with the minimum stage/flow. If 
          has same inflection_value, set the lastest one
	  as the minimum, it is possible that the point has
	  the same value as the above point with maximum stage/flow */

       for (k=0; k < inflection_cnt; k++)
       {
	  if (inflection_value[k] <= fp[fpindex].fcst_min_value ||
	      fp[fpindex].fcst_min_value == MISSINGVAL)
	  {
	     fp[fpindex].fcst_min_value = inflection_value[k];
	     fp[fpindex].fcst_minvalue_time= inflection_validtime[k];
	  }     
       }
    }   
      
   if (obs_cur_index != MISSINGVAL &&
       fp[fpindex].obsH[obs_cur_index].value != MISSINGVAL)
   {    
      sprintf(msgstr, "<FcstTrend> variable: the last observed value for location %s is %f", 
                   fp[fpindex].id, fp[fpindex].obsH[obs_cur_index].value);
      log_msg("", msgstr);     
   }   
   sprintf(msgstr, "<FcstTrend> variable: the first value for location %s is %f", 
                   fp[fpindex].id, fp[fpindex].fcst_first_value);
   log_msg("", msgstr);
   sprintf(msgstr, "<FcstTrend> variable: the last forecast value for location %s is %f", 
                    fp[fpindex].id, fp[fpindex].fcst_last_value);
   log_msg("", msgstr);
   
   if (fp[fpindex].fcst_max_value != MISSINGVAL)
   {
      sprintf(msgstr, "<FcstTrend> variable: the first inflection point forecast value "
                   "(excluding first and last values) for location %s is %f",
                    fp[fpindex].id, fp[fpindex].fcst_max_value);
      log_msg("", msgstr);
   }
   
   if (fp[fpindex].fcst_min_value != MISSINGVAL)
   {   
      sprintf(msgstr, "<FcstTrend> variable: the second inflection point forecast value "
                   "(excluding first and last values) for location %s is %f",
                    fp[fpindex].id, fp[fpindex].fcst_min_value);
      log_msg("", msgstr);		    
   }
   		    
   /* free space */
   
   if (inflection_value != NULL)
      free(inflection_value);
   if (inflection_validtime != NULL)
      free(inflection_validtime);
   
   if (temp_value != NULL)
      free(temp_value);
   if (temp_validtime != NULL)
      free(temp_validtime);      
   
   return;
}


/*********************************************************************
   load_detailed_trend_info()
   
   PURPOSE
   Loads the detailed trend phrases by using forecast data (stage/flow 
   and time) on the four    points (first, last, min and max) in the 
   full timeseries. These phrases can be shown in the roundup section
   in the generated product. For example: Forecast expect to rise near
   5 feet on Saturday noon.
   
   ********************************************************************/
void load_detailed_trend_info(int		fpindex,
			      fp_struct		*fp)			      
{
   int 		cnt = 0;
   double 	pt1_value,pt2_value,pt3_value,early_point_value,late_point_value;
   time_t 	pt1_time, pt2_time, pt3_time,early_point_time,late_point_time;   
   int		case_index;
   double 	chg_threshold;
   char 	*reach_str = NULL;
   double 	wstage_or_flow;   
   int          k;
   double       tidal_max_value;
   char         msg_str[300]="";
   int          second_rise_fourpoints = FALSE;
   
   /* initialize*/
   
   pt1_value = pt2_value = pt3_value = MISSINGVAL;
   early_point_value = late_point_value = MISSINGVAL;
   pt1_time  = pt2_time  = pt3_time  = MISSINGVAL;
   early_point_time = late_point_time = MISSINGVAL;
   fp[fpindex].fcstpoint_type = MISSINGVAL;   
   
   
   /* initialize fp[].action_index[], fp[].action_value[] and 
      fp[].action_time[] */
   

   for (k=0; k < MAXNUM_OF_TRENDPHRASE; k++)
   {
      fp[fpindex].action_index[k] = MISSINGVAL;
      fp[fpindex].action_value[k] = MISSINGVAL;
      fp[fpindex].action_time[k]  = MISSINGVAL;
   }
      
   
   /* initialize case_index and wstage_or_flow */
   
   wstage_or_flow = case_index = MISSINGVAL;
   
   reach_str = (char *) malloc(sizeof(char) * (SHORT_LEN + 1));
   if (reach_str == NULL)
   {
      log_msg(FAILED_MALLOC, " of reach_str in load_detailed_trend_info");
      exit(-1);
   }   
   
   chg_threshold = fp[fpindex].chg_threshold;
   
   
   /* define wstage_or_flow, fstage_or_flow according to primary_pe */
   
   if (fp[fpindex].pe[0] == 'Q' && fp[fpindex].aq != MISSINGVAL)
      wstage_or_flow = fp[fpindex].aq;
   else if (fp[fpindex].pe[0] != 'Q' && fp[fpindex].wstg != MISSINGVAL)
      wstage_or_flow = fp[fpindex].wstg;
   
   
   /* check if this point is normal one or weir or fremont or tidal */
   
   if (fp[fpindex].reach != NULL)
   {
      strcpy(reach_str, fp[fpindex].reach);
      convert_str_to_upcase(reach_str);
      
      if (strstr(reach_str, "SPECIAL WEIR") != NULL)
         fp[fpindex].fcstpoint_type = FREMONT;
      
      else if (strstr(reach_str,"WEIR") != NULL)
         fp[fpindex].fcstpoint_type = WEIR;
      
      else if (strstr(reach_str, "TIDAL") != NULL)
      {                 
	  /* for tidal points, find the maximum data in the forecast time series*/
                          
	  tidal_max_value = (double)MISSINGVAL;
	 
	  if (fp[fpindex].numfcstH > 0)
	  {
	     for (k = 0; k < fp[fpindex].numfcstH; ++k)
	     {
                if (fp[fpindex].fcstH[k].value > tidal_max_value &&
	            fp[fpindex].fcstH[k].value != MISSINGVAL)
	        {
	           tidal_max_value = fp[fpindex].fcstH[k].value;
	       
	        }
	     }
	 
          }
	  else
	  {
	     sprintf(msg_str, "No forecast data for point %s", fp[fpindex].id);
	     log_msg("", msg_str);
	     return;
	  }    
	 
	  /* consider the point as tidal point if the max of the forecast
	    stage is less or equal to the TIDAL_THRESHOLD, otherwise, consider
	    it as normal point. Note this is only apply to I Street point in
	    STO wfo!! */
	 
	  if (tidal_max_value < TIDAL_THRESHOLD)
	     fp[fpindex].fcstpoint_type = TIDAL;
	  else
	     fp[fpindex].fcstpoint_type = NORMAL;   	 	 
      }	 
      
      else
         fp[fpindex].fcstpoint_type = NORMAL;	 	 
   }
        
   /* write log message */
   
   if (fp[fpindex].fcstpoint_type == FREMONT)
      sprintf(msg_str, "<FcstTrend> variable: the location type is %s for %s", 
              "SPECIAL WEIR", fp[fpindex].id);
   else if (fp[fpindex].fcstpoint_type == WEIR)
      sprintf(msg_str, "<FcstTrend> variable: the location type is %s for %s", 
              "WEIR", fp[fpindex].id);	   
   else if (fp[fpindex].fcstpoint_type == TIDAL)
      sprintf(msg_str, "<FcstTrend> variable: the location type is %s for %s", 
              "TIDAL", fp[fpindex].id);	 
   else
      sprintf(msg_str, "<FcstTrend> variable: the location type is %s for %s", 
              "NORMAL", fp[fpindex].id);	
	      
   log_msg("", msg_str);	       	           
	                 
   /* for tidal points */
   
   if (fp[fpindex].fcstpoint_type == TIDAL)
      load_tidal_trendinfo(fp, fpindex);
   
   
   /* for normal points, weir points and fremont points */
   
   else 
   {		      	     	
      /* Different hydrograph for forecast time series data */
      /*no time series data*/
      
      if (fp[fpindex].fcst_first_value  == MISSINGVAL &&
	  fp[fpindex].fcst_last_value   == MISSINGVAL &&
	  fp[fpindex].fcst_max_value    == MISSINGVAL &&
	  fp[fpindex].fcst_min_value    == MISSINGVAL)
      {   
         log_msg("", "Warning - No time series data in load_detailed_trend_info");
	 return;
      }
      else
      {         	 
	 /* for weir and fremont points only */
	 
	 if (fp[fpindex].fcstpoint_type == WEIR || 
	     fp[fpindex].fcstpoint_type == FREMONT)
	 {
	    if (fp[fpindex].fcst_first_value > wstage_or_flow && 
		wstage_or_flow != MISSINGVAL)
	    {
	       fp[fpindex].action_value[cnt] = 
		  fp[fpindex].fcst_first_value - wstage_or_flow;
	       
	       if (fp[fpindex].action_value[cnt] < 0.1)
		  fp[fpindex].action_value[cnt] = 0.1;
	       
	       fp[fpindex].action_index[cnt] = WEIR_PRESENT_OVERFLOW;
	       
	       cnt +=1;
	    }
	 }
	 
	 
	 /* For hydrograph just has 2 points, which means no peak, bottom 
	    in the full time series. there are three cases: "--"; "/";"\" */
	 
	 if ((fp[fpindex].fcst_lastvalue_time > fp[fpindex].fcst_firstvalue_time) &&
	     (fp[fpindex].fcst_max_value == MISSINGVAL) &&
	     (fp[fpindex].fcst_min_value == MISSINGVAL )) 	      
	 {
	    case_index = TWOPOINTS_CASE;      
	    pt1_value  = fp[fpindex].fcst_first_value;
	    pt1_time   = fp[fpindex].fcst_firstvalue_time;
	    pt2_value  = fp[fpindex].fcst_last_value;
	    pt2_time   = fp[fpindex].fcst_lastvalue_time;
	    
	    
	    /* flat hydrograph case "--", means no change of stage/flow in the
	       full time series */
	    
	    if (fabs(fp[fpindex].fcst_first_value - fp[fpindex].fcst_last_value)
	        <= chg_threshold) 
	       
	       twopoints_flat_hydrograph(pt1_value, pt1_time, pt2_value, 
					 pt2_time, &cnt, fpindex,fp); 
	    
	    
	    /* rising case- "/", check if the rising is a significant one or not, if
	       cross the warning stage or flood stage */
	    
	    else if ((fp[fpindex].fcst_last_value- fp[fpindex].fcst_first_value)
	              > chg_threshold)
	       
	       rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time, &cnt,
			       fpindex,fp, case_index, FALSE);
	    
	    
	    /* falling case - "\", check if the falling is a significant one or not, if
	       cross the warning stage or flood stage */
	    
	    else if ((fp[fpindex].fcst_first_value - fp[fpindex].fcst_last_value)
	              > chg_threshold)
	       
	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time, &cnt,
			       fpindex, fp, case_index);     
	    
	 }  /* end of 2 points hydrograph */
	 
	 
	 /* For 3 points hydrograph */
	 
	 else if ((fp[fpindex].fcst_lastvalue_time > fp[fpindex].fcst_firstvalue_time)
	          && (fp[fpindex].fcst_max_value != MISSINGVAL ) && 
		     (fp[fpindex].fcst_min_value == MISSINGVAL))	 	  
	    
	 {  
	 	    
	    case_index = THREEPOINTS_CASE;
	    
	    /* fluctuate over the whole period with peak in the timeseries */
	    
	    if ((fp[fpindex].fcst_max_value >= fp[fpindex].fcst_first_value) &&
	        (fp[fpindex].fcst_max_value >= fp[fpindex].fcst_last_value) &&
		(fabs(fp[fpindex].fcst_max_value - fp[fpindex].fcst_first_value) <=
		 chg_threshold) &&
		(fabs(fp[fpindex].fcst_max_value - fp[fpindex].fcst_last_value) <= 
		 chg_threshold))
	    {  
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;	      
	       pt2_value = fp[fpindex].fcst_max_value;
	       pt2_time  = fp[fpindex].fcst_maxvalue_time;
	       pt3_value = fp[fpindex].fcst_last_value;
	       pt3_time  = fp[fpindex].fcst_lastvalue_time;
	       
	       threepoints_crestflat_hydrograph(pt1_value, pt1_time, pt2_value,
						pt2_time, pt3_value, pt3_time,
						&cnt, fpindex, fp);
	       
	    } /* end of fluctuate over the whole period */
	    
	    /* fluctate with trough in the time series */
	    
	    else if ((fp[fpindex].fcst_max_value <= fp[fpindex].fcst_first_value) &&
		     (fp[fpindex].fcst_max_value <= fp[fpindex].fcst_last_value) &&
		     (fabs(fp[fpindex].fcst_max_value - fp[fpindex].fcst_first_value) <=
		      chg_threshold) &&
		     (fabs(fp[fpindex].fcst_max_value - fp[fpindex].fcst_last_value) <= 
		      chg_threshold)) 	 	 	   		  
	       
	    {  		       
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = fp[fpindex].fcst_max_value;
	       pt2_time  = fp[fpindex].fcst_maxvalue_time;
	       pt3_value = fp[fpindex].fcst_last_value;
	       pt3_time  = fp[fpindex].fcst_lastvalue_time;
	       
	       threepoints_valleyflat_hydrograph(pt1_value, pt1_time, pt2_value,
						 pt2_time, pt3_value, pt3_time,
						 &cnt, fpindex, fp);
	       
	    } /* end of fluctuate over the whole period */  
	    
	    /*for rise first then rise again, consider as one rise */
	    
	    else if ((fp[fpindex].fcst_max_value > fp[fpindex].fcst_first_value) &&
	             (fp[fpindex].fcst_max_value < fp[fpindex].fcst_last_value))			     
            {
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;
	       
	       rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt, fpindex, fp, case_index, FALSE);		     
	    }
	    
	    /* for fall first then fall again, consider as one fall */
	    
	    else if ((fp[fpindex].fcst_max_value < fp[fpindex].fcst_first_value) &&
	             (fp[fpindex].fcst_max_value > fp[fpindex].fcst_last_value))
		     	
	       
            {
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;
	       
	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt, fpindex, fp, case_index);		     
	    }	
	    
	    /* rise first then fall "/\" */
	    
	    else if (((fp[fpindex].fcst_max_value - fp[fpindex].fcst_first_value)
	                > chg_threshold) &&
	             ((fp[fpindex].fcst_max_value - fp[fpindex].fcst_last_value) 
		        > chg_threshold))
		    	     
	    {		   
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = fp[fpindex].fcst_max_value;
	       pt2_time  = fp[fpindex].fcst_maxvalue_time;
	       
	       rise_hydrograph(pt1_value,pt1_time,pt2_value,pt2_time,&cnt,fpindex,fp,
			       case_index, FALSE);	  	  
	       
	       /* falling part */
	       
	       pt1_value = fp[fpindex].fcst_max_value;
	       pt1_time  = fp[fpindex].fcst_maxvalue_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;
	       
	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt, fpindex, fp, case_index);
	    }/*end of "/\"*/	 	   
	 
	    /*rise first then flat "/-"*/
	 
	    else if (((fp[fpindex].fcst_max_value - fp[fpindex].fcst_first_value)
	                > chg_threshold) &&	              
		      (fabs(fp[fpindex].fcst_max_value - fp[fpindex].fcst_last_value)
		        <= chg_threshold))	
	    {  	     
	    	    	       	  	       
	       /*rising part*/
	       
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = fp[fpindex].fcst_max_value;
	       pt2_time  = fp[fpindex].fcst_maxvalue_time;
	       
	       rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt, fpindex, fp, case_index, FALSE);
			       
               /* flat part */		   
	      
	       pt1_value = fp[fpindex].fcst_max_value;
	       pt1_time  = fp[fpindex].fcst_maxvalue_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;
	       
	       twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,pt2_time,&cnt,fpindex,fp);				       
	    
	    } /* end of "/-" */ 
	 
	    
	    /* flat first then fall "-\" */
	 
	    else if (((fp[fpindex].fcst_max_value - fp[fpindex].fcst_last_value)
	                > chg_threshold) &&
	              (fabs(fp[fpindex].fcst_max_value - fp[fpindex].fcst_first_value)
		        <= chg_threshold))
		     
	    {  	      
	    	    	       	  	       
	       /* flat part */
	       
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = fp[fpindex].fcst_max_value;
	       pt2_time  = fp[fpindex].fcst_maxvalue_time;
	       
	       twopoints_flat_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt, fpindex, fp);
			       
               /* fall part */		   
	      
	       pt1_value = fp[fpindex].fcst_max_value;
	       pt1_time  = fp[fpindex].fcst_maxvalue_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;
	       
	       fall_hydrograph(pt1_value,pt1_time,pt2_value,pt2_time,&cnt,fpindex,fp, case_index);				       
	    
	    }/*end of "-\"*/	    	   	    	      
	    
	    /* for fall first then rise "\/"*/
	    
	    else if (((fp[fpindex].fcst_first_value - fp[fpindex].fcst_max_value)
	               > chg_threshold) &&
	             ((fp[fpindex].fcst_last_value - fp[fpindex].fcst_max_value)
		       > chg_threshold))	             	
	    {
	       /* falling part */
	       
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = fp[fpindex].fcst_max_value;
	       pt2_time  = fp[fpindex].fcst_maxvalue_time;
	       
	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time, &cnt, 
			       fpindex, fp, case_index);
	       
	       /* rising part */
	       
	       pt1_value = fp[fpindex].fcst_max_value;
	       pt1_time  = fp[fpindex].fcst_maxvalue_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;
	       
	       rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time, &cnt,
			       fpindex, fp, case_index, FALSE);
	    } /*end of "\/" */		     
	    
	    
	    /* for flat first then rise "_/"*/
	 
	    else if (((fabs(fp[fpindex].fcst_max_value - fp[fpindex].fcst_first_value))
	               <= chg_threshold) &&
		      ((fp[fpindex].fcst_last_value - fp[fpindex].fcst_max_value) 
		       > chg_threshold))	
	    {  	       
	    	    
	      /* flat part */		   
	      
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = fp[fpindex].fcst_max_value;
	       pt2_time  = fp[fpindex].fcst_maxvalue_time;
	       
	       twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,pt2_time,&cnt,fpindex,fp);	  	  
	       
	       /* rising part */
	       
	       pt1_value = fp[fpindex].fcst_max_value;
	       pt1_time  = fp[fpindex].fcst_maxvalue_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;
	       
	       rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt, fpindex, fp, case_index, FALSE);
	    
	    } 
	    	    
	    /* fall first then flat "\_" */
	    
	    else if (((fp[fpindex].fcst_first_value - fp[fpindex].fcst_max_value)
	                > chg_threshold) &&
		     (fabs(fp[fpindex].fcst_max_value - fp[fpindex].fcst_last_value)
		        <= chg_threshold)) 	
	    { 	    	       	  	       
	       /* fall part */
	       
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = fp[fpindex].fcst_max_value;
	       pt2_time  = fp[fpindex].fcst_maxvalue_time;
	       
	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt, fpindex, fp, case_index);
	       
               /* flat part */		   
	       
	       pt1_value = fp[fpindex].fcst_max_value;
	       pt1_time  = fp[fpindex].fcst_maxvalue_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;
	       
	       twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,pt2_time,&cnt,fpindex,fp);				       
	       
	    }	    	   	       
	    
	 } /* end of 3 points hydrograph */
	 
	 /* For 4 points hydrograph */
	 
	 else if ((fp[fpindex].fcst_maxvalue_time != fp[fpindex].fcst_firstvalue_time) &&
		  (fp[fpindex].fcst_maxvalue_time != fp[fpindex].fcst_lastvalue_time) &&
		  (fp[fpindex].fcst_minvalue_time != fp[fpindex].fcst_firstvalue_time) &&
		  (fp[fpindex].fcst_minvalue_time != fp[fpindex].fcst_firstvalue_time) &&
		  (fp[fpindex].fcst_max_value != MISSINGVAL ) &&
		  (fp[fpindex].fcst_min_value != MISSINGVAL))
	 {
	    case_index = FOURPOINTS_CASE;
	    
	    if (fp[fpindex].fcst_maxvalue_time < fp[fpindex].fcst_minvalue_time)
	    {
	       early_point_value = fp[fpindex].fcst_max_value;
	       early_point_time = fp[fpindex].fcst_maxvalue_time;
	       late_point_value = fp[fpindex].fcst_min_value;
	       late_point_time = fp[fpindex].fcst_minvalue_time;
	    }
	    else
	    {
	       early_point_value = fp[fpindex].fcst_min_value;
	       early_point_time = fp[fpindex].fcst_minvalue_time;
	       late_point_value = fp[fpindex].fcst_max_value;
	       late_point_time = fp[fpindex].fcst_maxvalue_time; 
	    }      
	    
	    
	    /* flat */
	    
	    if ((fabs(fp[fpindex].fcst_first_value - early_point_value)
	           <= chg_threshold) &&   
		(fabs(early_point_value - late_point_value)
	           <= chg_threshold) &&   
		(fabs(late_point_value - fp[fpindex].fcst_last_value)
	           <= chg_threshold) &&
		(fabs(early_point_value - fp[fpindex].fcst_last_value)
		   <= chg_threshold) &&
		(fabs(late_point_value - fp[fpindex].fcst_first_value)
		   <= chg_threshold))     
            {
	         pt1_value = fp[fpindex].fcst_first_value;
		 pt1_time = fp[fpindex].fcst_firstvalue_time;	      
		 pt2_value = fp[fpindex].fcst_last_value;
		 pt2_time = fp[fpindex].fcst_lastvalue_time;
		 
	         if (fabs(fp[fpindex].fcst_first_value - fp[fpindex].fcst_last_value)
	             <= chg_threshold)
	         {	     		  
		     twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
					    pt2_time,&cnt,fpindex,fp);	    	    
                 }					 
	       
	         else
	         {
	             if ((fp[fpindex].fcst_last_value - fp[fpindex].fcst_first_value) 
		           > chg_threshold)
		     {
		         rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index, FALSE);		  		  
		     }	
		     else
		     {		     
		        fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index);
		     } 
		  }   	    
	    }		    
	   	   	   
	    /* rise */
	    else if ((early_point_value > fp[fpindex].fcst_first_value) &&
		    (late_point_value > early_point_value) &&
		    (fp[fpindex].fcst_last_value > late_point_value)) 
            {
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;	       	       
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;	      

	       rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index, FALSE);		  

	    } 
	       			  	   
	    /* fall */

	    else if ((late_point_value > fp[fpindex].fcst_last_value) &&
		     (early_point_value > late_point_value) &&
		     (fp[fpindex].fcst_first_value > early_point_value))
            {
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;

	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index);	       
	    }
	    
	     /* //-*/
	    else if((early_point_value > fp[fpindex].fcst_first_value) &&
	            (late_point_value > early_point_value) &&
		    ((late_point_value - fp[fpindex].fcst_first_value) 
		      > chg_threshold) &&
		    (fabs(late_point_value - fp[fpindex].fcst_last_value)
		      <= chg_threshold))
            {	    
	        /* rise */
		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = late_point_value;
		pt2_time  = late_point_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt,fpindex, fp, case_index, FALSE);		
                
		/* flat */ 
		pt1_value = late_point_value;
		pt1_time = late_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time = fp[fpindex].fcst_lastvalue_time;

		twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
				pt2_time,&cnt,fpindex,fp); 	    	    
	    
	    } 
	    
	    /*/\\*/
	    else if (((early_point_value - fp[fpindex].fcst_first_value)
	                > chg_threshold)  &&
		     (early_point_value > late_point_value) &&
		     (late_point_value > fp[fpindex].fcst_last_value) &&
		     ((early_point_value - fp[fpindex].fcst_last_value)
		        > chg_threshold))
            {	    	    
	        /* first rising part */

		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			    &cnt,fpindex, fp, case_index, FALSE);		
		
		pt1_value = early_point_value;
		pt1_time  = early_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time  = fp[fpindex].fcst_lastvalue_time;

		fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			        &cnt,fpindex, fp, case_index);
	    
	    }			
	 
	    /*\//*/
	    else if  (((fp[fpindex].fcst_first_value - early_point_value)
	                > chg_threshold) &&
		      (late_point_value > early_point_value ) &&
		      (fp[fpindex].fcst_last_value > late_point_value) &&
		      ((fp[fpindex].fcst_last_value - early_point_value)
		        > chg_threshold))
	    {	    
	        /*fall*/		
		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt,fpindex, fp, case_index);			    

		/* rise part */

		pt1_value = early_point_value;
		pt1_time = early_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time = fp[fpindex].fcst_lastvalue_time;

                rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				  &cnt,fpindex, fp, case_index, FALSE);
			    
	    }
	    
	    /* \\_ */
	    else if ((fp[fpindex].fcst_first_value > early_point_value) &&
	            (early_point_value > late_point_value) &&
		    ((fp[fpindex].fcst_first_value - late_point_value) 
		        > chg_threshold) &&
		    (fabs(late_point_value - fp[fpindex].fcst_last_value)
		        <= chg_threshold))
	    {
	       /*fall*/
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = late_point_value;
	       pt2_time  = late_point_time;

	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index);			    

	       /* second flat part */

	       pt1_value = late_point_value;
	       pt1_time = late_point_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time = fp[fpindex].fcst_lastvalue_time;

	       twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
					 pt2_time,&cnt,fpindex,fp);				  	    
	    
	    }
	    
	    /*\\/*/
	    else if ((fp[fpindex].fcst_first_value > early_point_value) &&
	            (early_point_value > late_point_value) &&
		    ((fp[fpindex].fcst_first_value - late_point_value) 
		        > chg_threshold) &&
		    ((fp[fpindex].fcst_last_value - late_point_value)
		        > chg_threshold))
	    {
	       /*fall*/
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = late_point_value;
	       pt2_time  = late_point_time;

	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index);	
			       
               /*rise*/
	       
	       pt1_value = late_point_value;
	       pt1_time  = late_point_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;

	       rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt, fpindex, fp, case_index, FALSE);			       	    
	    
	    }
	    
	    /*-\\*/
	    else if ((fabs(fp[fpindex].fcst_first_value - early_point_value)
	                 <= chg_threshold) &&
		      (early_point_value > late_point_value) &&
		      (late_point_value > fp[fpindex].fcst_last_value) &&
		      ((early_point_value - fp[fpindex].fcst_last_value)
		         > chg_threshold))
            {
	        pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
				pt2_time,&cnt,fpindex,fp);


		pt1_value = early_point_value;
		pt1_time = early_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time = fp[fpindex].fcst_lastvalue_time;

		fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index);	
	    
	    }
	    
	    /*_//*/
	    else if ((fabs(fp[fpindex].fcst_first_value - early_point_value)
	                <= chg_threshold) &&
		     (late_point_value > early_point_value) &&
		     (fp[fpindex].fcst_last_value > late_point_value) &&
		     ((fp[fpindex].fcst_last_value - early_point_value)
		         > chg_threshold))
            {
	        pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
					  pt2_time,&cnt,fpindex,fp);

		/*second rise part*/

		pt1_value = early_point_value;
		pt1_time = early_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time = fp[fpindex].fcst_lastvalue_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt,fpindex, fp, case_index, FALSE);
	    
	    } 
	    
	    /*//\*/
	    else if ((early_point_value > fp[fpindex].fcst_first_value) &&
	            (late_point_value > early_point_value ) &&
		    ((late_point_value - fp[fpindex].fcst_first_value)
		       > chg_threshold) &&
		    ((late_point_value - fp[fpindex].fcst_last_value)
		       > chg_threshold))
            {
	        /*rise */
	        pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = late_point_value;
		pt2_time  = late_point_time;

                rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			        &cnt,fpindex, fp, case_index, FALSE);
				    
	        /* second fall part */
		pt1_value = late_point_value;
		pt1_time  = late_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time  = fp[fpindex].fcst_lastvalue_time;

		fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time, &cnt,
			        fpindex, fp, case_index);

	    }	
	    		
	    /* /-- */
	    else if (((early_point_value - fp[fpindex].fcst_first_value)
	               > chg_threshold) &&
		     (fabs(late_point_value - early_point_value)
		       <= chg_threshold) &&
		     (fabs(late_point_value - fp[fpindex].fcst_last_value)
		       <= chg_threshold) &&
		     (fabs(fp[fpindex].fcst_last_value - early_point_value)
		       <= chg_threshold))
            {	       
		/* rise part */

		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt,fpindex, fp, case_index, FALSE);			    

		/* second flat part */

		pt1_value = early_point_value;
		pt1_time = early_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time = fp[fpindex].fcst_lastvalue_time;

		twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
				pt2_time,&cnt,fpindex,fp);
	    	    	    
	    } 		             
	    
	   
	    /*\_ _*/
	    else if (((fp[fpindex].fcst_first_value - early_point_value)
	               > chg_threshold) &&
		     (fabs(late_point_value - early_point_value)
		       <= chg_threshold) &&
		     (fabs(late_point_value - fp[fpindex].fcst_last_value)
		       <= chg_threshold) &&
		     (fabs(fp[fpindex].fcst_last_value - early_point_value)
		       <= chg_threshold))
	    {
	        /*fall*/
		
		 pt1_value = fp[fpindex].fcst_first_value;
		 pt1_time  = fp[fpindex].fcst_firstvalue_time;
		 pt2_value = early_point_value;
		 pt2_time  = early_point_time;

		 fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				 &cnt,fpindex, fp, case_index);			    

		 /* second flat part */

		 pt1_value = early_point_value;
		 pt1_time = early_point_time;
		 pt2_value = fp[fpindex].fcst_last_value;
		 pt2_time = fp[fpindex].fcst_lastvalue_time;

		 twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
					   pt2_time,&cnt,fpindex,fp);					    
	    
	    }	             
	    
	    
	    /*--\*/
	    else if (((late_point_value - fp[fpindex].fcst_last_value)
	                > chg_threshold) &&
		     (fabs(fp[fpindex].fcst_first_value - early_point_value)
			<= chg_threshold) &&
		     (fabs(early_point_value - late_point_value)
			<= chg_threshold) &&
		     (fabs(fp[fpindex].fcst_first_value - late_point_value)
			<= chg_threshold))
            {
	    			         	   	       	      
	       /* flat part */

	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = late_point_value;
	       pt2_time  = late_point_time;

	       twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
			       pt2_time,&cnt,fpindex,fp);

	       /* second fall part */

	       pt1_value = late_point_value;
	       pt1_time = late_point_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time = fp[fpindex].fcst_lastvalue_time;

	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			      &cnt,fpindex, fp, case_index);
            }
	    	   	 	 	    
	    /* _ _/*/
	    else if (((fp[fpindex].fcst_last_value - late_point_value)
	                > chg_threshold) &&
		     (fabs(fp[fpindex].fcst_first_value - early_point_value)
		        <= chg_threshold) &&
		     (fabs(early_point_value - late_point_value)
		        <= chg_threshold) &&
		     (fabs(fp[fpindex].fcst_first_value - late_point_value)
		        <= chg_threshold))
            {
	        /*flat part*/

		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = late_point_value;
		pt2_time  = late_point_time;

		twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
					  pt2_time,&cnt,fpindex,fp);

		/*second rise part*/

		pt1_value = late_point_value;
		pt1_time = late_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time = fp[fpindex].fcst_lastvalue_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt,fpindex, fp, case_index, FALSE);	
	    
	    }					 	
	    
	    	          
	    			 				      									
	    /* for rise first, flat second then fall "/-\" */
	    else if (((early_point_value - fp[fpindex].fcst_first_value)
	                > chg_threshold &&
		     (late_point_value - fp[fpindex].fcst_last_value)
		        > chg_threshold) && 
		     (fabs(early_point_value - late_point_value)
		        <= chg_threshold))  		    
	    {

	       /* rise part */

	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;	       	       
	       pt2_value = early_point_value;
	       pt2_time  = early_point_time;	      

	       rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index, FALSE);

	       /* second flat part */

	       pt1_value = early_point_value;
	       pt1_time = early_point_time;
	       pt2_value = late_point_value;
	       pt2_time = late_point_time;

	       twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
					 pt2_time,&cnt,fpindex,fp);

	       /* third falling part */

	       pt1_value = late_point_value;
	       pt1_time  = late_point_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;

	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index);

	    }
	       	    
	    /* for /-/ */
	    else if (((early_point_value - fp[fpindex].fcst_first_value)
	                > chg_threshold) &&
		     (fabs(early_point_value - late_point_value)
		        <= chg_threshold) &&		      
		     ((fp[fpindex].fcst_last_value - late_point_value)
		        > chg_threshold))		     	    
	    {
	       /* rise part */

	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = early_point_value;
	       pt2_time  = early_point_time;

	       rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index, FALSE);

	       /* second flat part */

	       pt1_value = early_point_value;
	       pt1_time = early_point_time;
	       pt2_value = late_point_value;
	       pt2_time = late_point_time;

	       twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
					 pt2_time,&cnt,fpindex,fp);

	       /* third rising part */

	       pt1_value = late_point_value;
	       pt1_time  = late_point_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;

	       rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index, FALSE);

	    }
	       
	    /* for \_/ */  

	    else if (((fp[fpindex].fcst_first_value - early_point_value)
	                > chg_threshold) &&
		     ((fp[fpindex].fcst_last_value - late_point_value)
		        > chg_threshold) &&
		     (fabs(early_point_value - late_point_value)
		        <= chg_threshold))  			      		     			    
	    {
	       /* fall part */

	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = early_point_value;
	       pt2_time  = early_point_time;

	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index);

	       /* second flat part */

	       pt1_value = early_point_value;
	       pt1_time = early_point_time;
	       pt2_value = late_point_value;
	       pt2_time = late_point_time;

	       twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
					 pt2_time,&cnt,fpindex,fp);

	       /* third rise part */

	       pt1_value = late_point_value;
	       pt1_time  = late_point_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;

	       rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index, FALSE);

	    }
	       	       
	    /* for \_\ */	          

	    else if (((fp[fpindex].fcst_first_value - early_point_value)
	                > chg_threshold) &&
		     (fabs(early_point_value - late_point_value)
		        <= chg_threshold) &&		      
		     ((late_point_value - fp[fpindex].fcst_last_value)
		        > chg_threshold)) 		    
	    {
	       /* fall part */

	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = early_point_value;
	       pt2_time  = early_point_time;

	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index);

	       /* second flat part */

	      pt1_value = early_point_value;
	      pt1_time = early_point_time;
	      pt2_value = late_point_value;
	      pt2_time = late_point_time;

	      twopoints_flat_hydrograph(pt1_value,pt1_time,pt2_value,
			      pt2_time,&cnt,fpindex,fp);

	      /* third fall part */

	      pt1_value = late_point_value;
	      pt1_time  = late_point_time;
	      pt2_value = fp[fpindex].fcst_last_value;
	      pt2_time  = fp[fpindex].fcst_lastvalue_time;

	      fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			     &cnt,fpindex, fp, case_index);

	    }

	    /*-\_*/
	    else if ((fabs(fp[fpindex].fcst_first_value - early_point_value)
	                <= chg_threshold) &&
		     ((early_point_value - late_point_value) 
		        > chg_threshold) &&
		     (fabs(late_point_value - fp[fpindex].fcst_last_value)
		        <= chg_threshold))
            {
	       /* first flat part */
		  
	       pt1_value = fp[fpindex].fcst_first_value;
	       pt1_time  = fp[fpindex].fcst_firstvalue_time;
	       pt2_value = early_point_value;
	       pt2_time  = early_point_time;

	       twopoints_flat_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
					 &cnt,fpindex, fp);             					    
	       /* second falling part */

	       pt1_value = early_point_value;
	       pt1_time  = early_point_time;
	       pt2_value = late_point_value;
	       pt2_time  = late_point_time;

	       fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index);

               /* third flat part */

	       pt1_value = late_point_value;
	       pt1_time  = late_point_time;
	       pt2_value = fp[fpindex].fcst_last_value;
	       pt2_time  = fp[fpindex].fcst_lastvalue_time;

	       twopoints_flat_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
					 &cnt,fpindex, fp);				     
	    
	    }					
	    
	    /*-\/*/
	    else if ((fabs(fp[fpindex].fcst_first_value - early_point_value)
	                <= chg_threshold) &&
		     ((early_point_value - late_point_value) 
		        > chg_threshold) &&
		     ((fp[fpindex].fcst_last_value - late_point_value)
		        > chg_threshold))
	    {
	        /* first flat part */
		  
		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		twopoints_flat_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
					  &cnt,fpindex, fp);
	        /* second falling part */

		pt1_value = early_point_value;
		pt1_time  = early_point_time;
		pt2_value = late_point_value;
		pt2_time  = late_point_time;

		fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			        &cnt,fpindex, fp, case_index);
				
                /*rise*/
						
		pt1_value = late_point_value;
		pt1_time  = late_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time  = fp[fpindex].fcst_lastvalue_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt,fpindex, fp, case_index, FALSE);

	    }
	         	                                    	       	       	       
	    /*\/-*/
	    else if (((fp[fpindex].fcst_first_value - early_point_value)
	               > chg_threshold) &&
		     ((late_point_value - early_point_value) 
		       > chg_threshold) &&
		     (fabs(fp[fpindex].fcst_last_value - late_point_value)
		       <= chg_threshold))
            {
	        /* first falling part */

		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time, &cnt,
			     fpindex, fp, case_index);
                /*rise*/
                  
		pt1_value = early_point_value;
		pt1_time  = early_point_time;
		pt2_value = late_point_value;
		pt2_time  = late_point_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt, fpindex, fp, case_index, FALSE);
		
		/* flat*/
				
		pt1_value = late_point_value;
		pt1_time  = late_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time  = fp[fpindex].fcst_lastvalue_time;

		twopoints_flat_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			                  &cnt,fpindex, fp);		  		 	    
	    }		           
	      
	    /*_/-*/
	    else if ((fabs(fp[fpindex].fcst_first_value - early_point_value)
	                 <= chg_threshold) &&
		      ((late_point_value - early_point_value)
		      	 > chg_threshold) &&
		      (fabs(late_point_value - fp[fpindex].fcst_last_value)
		         <= chg_threshold))
	    {
	        /*first flat part*/

		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		twopoints_flat_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			     &cnt,fpindex, fp);
			       
	        /* second rising part */

		pt1_value = early_point_value;
		pt1_time  = early_point_time;
		pt2_value = late_point_value;
		pt2_time  = late_point_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index, FALSE);
	             
		/*third flat*/
					                         			    			
		pt1_value = late_point_value;
		pt1_time  = late_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time  = fp[fpindex].fcst_lastvalue_time;

		twopoints_flat_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			     &cnt,fpindex, fp);
	   
	    }
	   
	    /*_/\*/
	    else if ((fabs(fp[fpindex].fcst_first_value - early_point_value)
	                  <= chg_threshold) &&
		       ((late_point_value - early_point_value)
		      	  > chg_threshold) &&
		       ((late_point_value - fp[fpindex].fcst_last_value)
		          > chg_threshold))
            {
		/*first flat part*/

		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		twopoints_flat_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			     &cnt,fpindex, fp);

		/* second rising part */

		pt1_value = early_point_value;
		pt1_time  = early_point_time;
		pt2_value = late_point_value;
		pt2_time  = late_point_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			       &cnt,fpindex, fp, case_index, FALSE);

		/* third fall part */

		pt1_value = late_point_value;
		pt1_time  = late_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time  = fp[fpindex].fcst_lastvalue_time;

		fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt,fpindex, fp, case_index);

	    }
	   
	    /*/\_*/
	    else if (((early_point_value - fp[fpindex].fcst_first_value)
	               > chg_threshold) &&
		     ((early_point_value - late_point_value)
		       > chg_threshold)  &&
		      (fabs(late_point_value - fp[fpindex].fcst_last_value)
		       <= chg_threshold))
            {
		/* first rising part */

		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			    &cnt,fpindex, fp, case_index, FALSE);

		/* second falling part */

		pt1_value = early_point_value;
		pt1_time  = early_point_time;
		pt2_value = late_point_value;
		pt2_time  = late_point_time;

		fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt,fpindex, fp, case_index);

		/* third flat*/

		pt1_value = late_point_value;
		pt1_time  = late_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time  = fp[fpindex].fcst_lastvalue_time;

		twopoints_flat_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				          &cnt,fpindex, fp);

	    }
	  
	   /*/\/*/
	    else if (((early_point_value - fp[fpindex].fcst_first_value)
	               > chg_threshold) &&
		     ((early_point_value - late_point_value)
		       > chg_threshold)  &&
		      ((fp[fpindex].fcst_last_value - late_point_value)
		       > chg_threshold))
            {
		/* first rising part */

		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			    &cnt,fpindex, fp, case_index, FALSE);

		/* second falling part */

		pt1_value = early_point_value;
		pt1_time  = early_point_time;
		pt2_value = late_point_value;
		pt2_time  = late_point_time;

		fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt,fpindex, fp, case_index);

		/* third rising part */

		pt1_value = late_point_value;
		pt1_time  = late_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time  = fp[fpindex].fcst_lastvalue_time;

		second_rise_fourpoints = TRUE;
		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt,fpindex, fp, case_index, second_rise_fourpoints);      

	    }		       
	  
	    /*\/\*/
	    else if (((fp[fpindex].fcst_first_value - early_point_value)
	        	> chg_threshold) &&
		      ((late_point_value - early_point_value)
			> chg_threshold)  &&
		       ((late_point_value - fp[fpindex].fcst_last_value)
			> chg_threshold))
            {
		/* first falling part */

		pt1_value = fp[fpindex].fcst_first_value;
		pt1_time  = fp[fpindex].fcst_firstvalue_time;
		pt2_value = early_point_value;
		pt2_time  = early_point_time;

		fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time, &cnt,
			     fpindex, fp, case_index);

        	/* rise part*/

		pt1_value = early_point_value;
		pt1_time  = early_point_time;
		pt2_value = late_point_value;
		pt2_time  = late_point_time;

		rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
				&cnt, fpindex, fp, case_index, FALSE);

		/* third falling part */

		pt1_value = late_point_value;
		pt1_time  = late_point_time;
		pt2_value = fp[fpindex].fcst_last_value;
		pt2_time  = fp[fpindex].fcst_lastvalue_time;

		fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
			     &cnt,fpindex, fp, case_index);	      

	    }		      	     
	     	    	   	    	 
         } /*end of 4 points*/
      }	
    }
   
   /* free the memory */
   
   if (reach_str != NULL)
      free(reach_str);
   
   
   return;
}


/*******************************************************************************
   
   rise_hydrograph()
   
   PURPOSE 
   Determine the trend phrase index, stage/flow value and time for a rising
   hydrograph which might rise across warning stage or flood stage, the rising
   might be a significant one or not.
   *********************************************************************************/

void rise_hydrograph(double        pt1_value,
		     time_t        pt1_time,          	   	   
		     double        pt2_value,
		     time_t        pt2_time,
		     int           *cnt,
		     int           fpindex,
		     fp_struct     *fp,
		     int           case_index,
		     int           second_rise_fourpoints)
{
   int k;
   int exceed_fs = MISSINGVAL;
   int exceed_ms = MISSINGVAL;
   double wstage_or_flow = MISSINGVAL;
   double fstage_or_flow = MISSINGVAL;   
   double stage, prev_stage;
   double exceed_ms_stage = MISSINGVAL,exceed_fs_stage=MISSINGVAL;    
   time_t stagetime, prev_stagetime;
   time_t exceed_ms_time=MISSINGVAL,exceed_fs_time=MISSINGVAL;
   double chg_threshold;   
   int  obs_cur_index = MISSINGVAL;
   double obs_cur_value = MISSINGVAL;
   double  early_point_value = MISSINGVAL;
   double late_point_value = MISSINGVAL;
   time_t obs_cur_time = MISSINGVAL;
   time_t early_point_time = MISSINGVAL;
   time_t late_point_time = MISSINGVAL;
  
   
   /* define convenient variable */
   
   chg_threshold = fp[fpindex].chg_threshold;
   
   /* define early point and late point */
   
   if (fp[fpindex].fcst_min_value != MISSINGVAL)
   {
      if (fp[fpindex].fcst_maxvalue_time < fp[fpindex].fcst_minvalue_time)
      {
	 early_point_value = fp[fpindex].fcst_max_value;
	 early_point_time = fp[fpindex].fcst_maxvalue_time;
	 late_point_value = fp[fpindex].fcst_min_value;
	 late_point_time = fp[fpindex].fcst_minvalue_time;
      }
      else
      {
	 early_point_value = fp[fpindex].fcst_min_value;
	 early_point_time = fp[fpindex].fcst_minvalue_time;
	 late_point_value = fp[fpindex].fcst_max_value;
	 late_point_time = fp[fpindex].fcst_maxvalue_time; 
      } 
   }
   /* define wstage_or_flow, fstage_or_flow according to primary_pe */
   
   if (fp[fpindex].pe[0] == 'Q') 
   {
      if (fp[fpindex].fq != MISSINGVAL)
	 fstage_or_flow = fp[fpindex].fq;
      if (fp[fpindex].aq != MISSINGVAL)
	 wstage_or_flow = fp[fpindex].aq;
   }
   
   else 
   {
      if (fp[fpindex].fs != MISSINGVAL)
	 fstage_or_flow = fp[fpindex].fs;
      if (fp[fpindex].wstg != MISSINGVAL)
	 wstage_or_flow = fp[fpindex].wstg;
   }
   
   /* check only one time for each point to get the acrossing point */
   
   if (pt2_time > pt1_time)
   {
      
      //if (first)
      //{
	 /* check for exceedance of monitor stage or flood stage*/

	 if ((pt1_value > fstage_or_flow && fstage_or_flow != MISSINGVAL) ||
             (pt2_value < fstage_or_flow && fstage_or_flow != MISSINGVAL)) 
	    exceed_fs = -1; 
	 if ((pt1_value > wstage_or_flow && wstage_or_flow != MISSINGVAL) ||
             (pt2_value < wstage_or_flow && wstage_or_flow != MISSINGVAL))
	    exceed_ms = -1; 


	 /* look for the position which passes the monitor stage */

	 if (exceed_ms != -1)
	 {
	    for (k = 0; k < fp[fpindex].numfcstH; k++)       
	    {	  
	       if (fp[fpindex].fcstH[k].validtime >= pt1_time)
	       {
		  if (fp[fpindex].fcstH[k].value >= wstage_or_flow && 
		      wstage_or_flow != MISSINGVAL )
		  {
		     exceed_ms = k;
		     break;
		  }   		
	       }   
	    }
	 }


	 /* look for the position which passes the flood stage */

	 if (exceed_fs != -1)
	 {	
	    for (k=0; k< fp[fpindex].numfcstH; k++)
	    {	   
	       if(fp[fpindex].fcstH[k].validtime >= pt1_time)
	       {
		  if (fp[fpindex].fcstH[k].value >= fstage_or_flow &&
		      fstage_or_flow != MISSINGVAL)
		  {
		     exceed_fs = k;
		     break;
		  }      	        
	       }
	    }  
	 }

	 /* Determine the stage/flow value and time when the forecast rise above
	    warning stage */
         
	 obs_cur_index = fp[fpindex].obs_cur_index;
	 if (obs_cur_index != MISSINGVAL &&
	     fp[fpindex].obsH[obs_cur_index].value != MISSINGVAL )
	 {
	    obs_cur_value = fp[fpindex].obsH[obs_cur_index].value;
	    obs_cur_time = fp[fpindex].obsH[obs_cur_index].validtime;
	 }   
	  
	 if (exceed_ms >= 0)  	           
	 {   	     	
            /* if the exceed_ms is the first forecast point 
	       interpolate the time between the non-missing latest
	       observed data and fisrt forecast point data*/

	    if (exceed_ms == 0 && obs_cur_value != MISSINGVAL)
	    {
	       exceed_ms_stage = wstage_or_flow;	     	    
	       prev_stage      = obs_cur_value;
	       prev_stagetime  = obs_cur_time;
	       stage           = fp[fpindex].fcstH[0].value;
	       stagetime       = fp[fpindex].fcstH[0].validtime;

	       exceed_ms_time = stagetime - 
		  ((stage - wstage_or_flow) / (stage-prev_stage)) *
		  (stagetime - prev_stagetime);
	    }      
	    /* interpolate the time from the validtime at exceed_ms represented
	       point and its its previous point, if the value on previous point
	       is missing, just use the validtime at exceed_ms point. */

	    else if (exceed_ms >= 1 &&
		fp[fpindex].fcstH[exceed_ms - 1].value != MISSINGVAL)
	    {   
	       exceed_ms_stage = wstage_or_flow;	     	    
	       prev_stage      = fp[fpindex].fcstH[exceed_ms - 1].value;
	       prev_stagetime  = fp[fpindex].fcstH[exceed_ms - 1].validtime;
	       stage           = fp[fpindex].fcstH[exceed_ms].value;
	       stagetime       = fp[fpindex].fcstH[exceed_ms].validtime;

	       exceed_ms_time = stagetime - 
		  ((stage - wstage_or_flow) / (stage-prev_stage)) *
		  (stagetime - prev_stagetime);
	    }

	    else
	    {
	       exceed_ms_stage = fp[fpindex].fcstH[exceed_ms].value;
	       exceed_ms_time  = fp[fpindex].fcstH[exceed_ms].validtime;
	    }


	 }


	 /* forecast to rise above flood stage, apply to NORMAL points only */

	 if (exceed_fs >= 0 && fp[fpindex].fcstpoint_type == NORMAL)
	 {	   	   
	    if (exceed_fs == 0 && obs_cur_value != MISSINGVAL)
	    {
	       exceed_fs_stage = fstage_or_flow;	     	    
	       prev_stage      = obs_cur_value;
	       prev_stagetime  = obs_cur_time;
	       stage           = fp[fpindex].fcstH[0].value;
	       stagetime       = fp[fpindex].fcstH[0].validtime;

	       exceed_fs_time = stagetime - 
		  ((stage - fstage_or_flow) / (stage-prev_stage)) *
		  (stagetime - prev_stagetime);
	    }    
	    /* interpolate the time from the validtime at exceed_fs represented 
	       point and its previous point, if the value on previous point is
	       missing, just use the validtime at exceed_fs point. */

	    else if (exceed_fs >= 1 &&
	             fp[fpindex].fcstH[exceed_fs - 1].value != MISSINGVAL)		   
	    {   	       
	       exceed_fs_stage = fstage_or_flow;
	       prev_stage      = fp[fpindex].fcstH[exceed_fs - 1].value;
	       prev_stagetime  = fp[fpindex].fcstH[exceed_fs - 1].validtime;
	       stage           = fp[fpindex].fcstH[exceed_fs].value;
	       stagetime       = fp[fpindex].fcstH[exceed_fs].validtime;

	       exceed_fs_time = stagetime - 
		  ((stage - fstage_or_flow) / (stage - prev_stage)) *
		  (stagetime - prev_stagetime);	
	    }
	    else
	    {
	       exceed_fs_stage = fp[fpindex].fcstH[exceed_fs].value;
	       exceed_fs_time  = fp[fpindex].fcstH[exceed_fs].validtime;
	    }         
	 }

      //first = FALSE;
      
      //}
      
      
      /* determine trend index, stage/flow and time for WEIR, FREMONT and 
	 NORMAL points */
      
      /* for WEIR points */
      
      if (fp[fpindex].fcstpoint_type == WEIR && wstage_or_flow != MISSINGVAL)
      {
	 /* if the rising is minor for the time series*/
	 
	 if (pt2_value - pt1_value <= chg_threshold)
	 {
	    if (pt1_value > wstage_or_flow) /*initial stage/flow aready exceed warning stage*/
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_REMAIN_OVERFLOW;
	       fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow; 
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	       
	       /* assign 0.1 to action_value if it is less than 0.1 */
	       
	       if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
		  fp[fpindex].action_value[*cnt] = 0.1;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;
	    }
	    
	    else if (pt1_value <= wstage_or_flow && pt2_value > wstage_or_flow) 
	    {
	       /*begin overflow*/
	       
	       fp[fpindex].action_index[*cnt] = WEIR_BEGIN_OVERFLOW;
	       fp[fpindex].action_time[*cnt] = exceed_ms_time;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;
	       
	       
	       /* because rise cross warning stage, so max stage higher than
		  warning stage,  means increase */
	       
	       fp[fpindex].action_index[*cnt] = WEIR_OVERFLOW_INC;
	       fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow;
	       fp[fpindex].action_time[*cnt] = pt2_time;
	       
	       
	       /* assign 0.1 to action_value if it is less than 0.1 */
	       
	       if (fp[fpindex].action_value[*cnt] < 0.1 &&
		   fp[fpindex].action_value[*cnt] != MISSINGVAL)
		  fp[fpindex].action_value[*cnt] = 0.1;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;      
	       
	    }
	    
	    
	    /* means max stage below or equal to warning stage */
	    
	    else if (pt2_value <= wstage_or_flow)	       
	    {
	       /* do not output WEIR_NO_OVERFLOW if it is the first phrase */
	       if (*cnt != 0)
	       {	       
		  fp[fpindex].action_index[*cnt] = WEIR_NO_OVERFLOW;
		  fp[fpindex].action_time[*cnt]  = pt2_time;

		  if (*cnt > 0)
		     fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;

		  *cnt +=1;
	       }
	    }
	 }  /* end of minor rising*/
	 
	 
	 else /* significant rising */
	 {
	    /* initial stage/flow aready exceed warning stage */
	    
	    if (pt1_value > wstage_or_flow) 
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_OVERFLOW_INC; /* diffrent from minor rise*/
	       fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow; 
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	       
	       /* assign 0.1 to action_value if it is less than 0.1*/
	       
	       if (fp[fpindex].action_value[*cnt] < 0.1 && 
		   fp[fpindex].action_value[*cnt] != MISSINGVAL)
		  fp[fpindex].action_value[*cnt] = 0.1;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;
	    }
	    
	    else if (pt1_value <= wstage_or_flow && pt2_value > wstage_or_flow)
	    {
	       /* begin overflow */
	       
	       fp[fpindex].action_index[*cnt] = WEIR_BEGIN_OVERFLOW;
	       fp[fpindex].action_time[*cnt]  = exceed_ms_time;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;
	       
	       
	       /* because rise cross warning stage, so max stage higher than
		  warning stage, means increase */
	       
	       fp[fpindex].action_index[*cnt] = WEIR_OVERFLOW_INC;
	       fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow;
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	       
	       
	       /* assign 0.1 to action_value if it is less than 0.1*/
	       
	       if (fp[fpindex].action_value[*cnt] < 0.1 && 
		   fp[fpindex].action_value[*cnt] != MISSINGVAL)
		  fp[fpindex].action_value[*cnt] = 0.1;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] +=TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;      
	       
	    }
	    
	    
	    /* means max stage below or equal to warning stage */
	    
	    else if (pt2_value <= wstage_or_flow) 
	    {
	       /* do not output WEIR_NO_OVERFLOW if it is the first phrase */
	       if (*cnt != 0)
	       {
		  fp[fpindex].action_index[*cnt] = WEIR_NO_OVERFLOW;
		  fp[fpindex].action_time[*cnt]  = pt2_time;

		  if (*cnt > 0)
		     fp[fpindex].action_index[*cnt] +=TRENDPHRASE_BASE_INDEX;

		  *cnt +=1;
	       }
	    }
	 } /* end of significant rising */
	 
      } /* end of WEIR point*/
      
      
      
      /* for FREMONT point*/  
      
      if (fp[fpindex].fcstpoint_type == FREMONT && wstage_or_flow != MISSINGVAL)  
      {
	 /* if the rising is minor for the time series */
	 
	 if (pt2_value - pt1_value <= chg_threshold)
	 {
	    if (pt1_value > wstage_or_flow) /*initial stage/flow aready exceed warning stage*/
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_REMAIN_OVERFLOW;
	       fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow; 
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	       
	       /* assign 0.1 to action_value if it is less than 0.1*/
	       
	       if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
		  fp[fpindex].action_value[*cnt] = 0.1;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;
	    }
	    
	    else if (pt1_value <= wstage_or_flow && pt2_value > wstage_or_flow) /*rise cross warning stage*/
	    {
	       
	       /*fluctuate near*/

	       fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	       fp[fpindex].action_time[*cnt] = exceed_ms_time;
	       fp[fpindex].action_value[*cnt] = wstage_or_flow;    	       	         	
	         
	       if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] +=TRENDPHRASE_BASE_INDEX;

	       *cnt +=1;
	          		 
	       /* begin overflow */

	       fp[fpindex].action_index[*cnt] = WEIR_BEGIN_OVERFLOW;
	       fp[fpindex].action_time[*cnt]  = exceed_ms_time;

	       if (*cnt > 0)
		 fp[fpindex].action_index[*cnt] +=TRENDPHRASE_BASE_INDEX;

	       *cnt +=1;
	       
	       
	       /* assign 0.1 to action_value if it is less than 0.1*/
	       
	       if (fp[fpindex].action_value[*cnt] < 0.1 && 
		   fp[fpindex].action_value[*cnt] != MISSINGVAL)
		  fp[fpindex].action_value[*cnt] = 0.1;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;      
	       
	    }
	    
	    else if (pt2_value <= wstage_or_flow) /* means max below or equal to warning stage */
	    {
	       fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	       fp[fpindex].action_value[*cnt] = round_float(pt2_value);
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	       
	       /* assign 0.1 to action_value if it is less than 0.1*/
	       
	       if (fp[fpindex].action_value[*cnt] < 0.1 && 
		   fp[fpindex].action_value[*cnt] != MISSINGVAL)
		  fp[fpindex].action_value[*cnt] = 0.1;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;
	    }
	 } /*end of minor rising */
	 
	 
	 else /*significant rising */
	 {
	    /* initial stage/flow aready exceed warning stage */
	    
	    if (pt1_value > wstage_or_flow) 
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_OVERFLOW_INC; 
	       fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow; 
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	       
	       /* assign 0.1 to action_value if it is less than 0.1*/
	       
	       if (fp[fpindex].action_value[*cnt] < 0.1 && 
		   fp[fpindex].action_value[*cnt] != MISSINGVAL)
		  fp[fpindex].action_value[*cnt] = 0.1;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;
	    }
	    
	    else if (pt1_value <= wstage_or_flow && pt2_value > wstage_or_flow)
	    {		       
	    	/*rise steady*/
		       	       
	       fp[fpindex].action_index[*cnt] = RISE_STEADY;
	       fp[fpindex].action_value[*cnt] = wstage_or_flow;
	       fp[fpindex].action_time[*cnt]  = exceed_ms_time;
	       
	       if (*cnt > 0)
	          fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	  
	       *cnt +=1;
	       	        
	       /* begin overflow */
	       
	       fp[fpindex].action_index[*cnt] = WEIR_BEGIN_OVERFLOW;
	       fp[fpindex].action_time[*cnt]  = exceed_ms_time;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] +=TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;
	       
	       
	       /* because rise cross warning stage, so max stage higher than 
		  warning stage, means increase*/
	       
	       fp[fpindex].action_index[*cnt] = WEIR_OVERFLOW_INC;
	       fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow;
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	       
	       /* assign 0.1 to action_value if it is less than 0.1*/
	       
	       if (fp[fpindex].action_value[*cnt] < 0.1 && 
		   fp[fpindex].action_value[*cnt] != MISSINGVAL)
		  fp[fpindex].action_value[*cnt] = 0.1;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;      	       
	    }
	    
	    else if (pt2_value <= wstage_or_flow) /* max stage below or equal to the warning stage*/
	    {
	       
	       if (case_index == THREEPOINTS_CASE)
	       {
	          /* if the difference between the peak with the initiaal stage > 3 ft,
		     output crest phrase */
		  if ((fp[fpindex].fcst_max_value - fp[fpindex].fcst_first_value) > 3.0 )
		  {
		     fp[fpindex].action_index[*cnt] = RISE_CREST;
		     fp[fpindex].action_value[*cnt] = round_float(pt2_value);
		     fp[fpindex].action_time[*cnt]  = pt2_time;
		     
		     /* assign 0.1 to action_value if it is less than 0.1 */
		     
		     if (fp[fpindex].action_value[*cnt] < 0.1 &&
			 fp[fpindex].action_value[*cnt] != MISSINGVAL)
			fp[fpindex].action_value[*cnt] = 0.1;
		     
		     if (*cnt > 0)
			fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
		     
		     *cnt +=1;
		  }
		  
		  else
		  {		       		       
		     fp[fpindex].action_index[*cnt] = RISE_STEADY;
		     fp[fpindex].action_value[*cnt] = round_float(pt2_value);
		     fp[fpindex].action_time[*cnt] = pt2_time;
		     
		     /* assign 0.1 to action_value if it is less than 0.1*/
		     
		     if (fp[fpindex].action_value[*cnt] < 0.1 &&
			 fp[fpindex].action_value[*cnt] != MISSINGVAL)
			fp[fpindex].action_value[*cnt] = 0.1;
		     
		     if (*cnt > 0)
			fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
		     
		     *cnt +=1;  
		     
		  }
	       }
	       
	       else if (case_index == FOURPOINTS_CASE)
	       {	    	      
		  if ((second_rise_fourpoints == FALSE) &&
		      (((early_point_value - fp[fpindex].fcst_first_value) > 3.0 ) ||
		      ((late_point_value - early_point_value) > 3.0)))
		  {
		     fp[fpindex].action_index[*cnt] = RISE_CREST;
		     fp[fpindex].action_value[*cnt] = round_float(pt2_value);
		     fp[fpindex].action_time[*cnt]  = pt2_time;

		     /* assign 0.1 to action_value if it is less than 0.1*/

		     if (fp[fpindex].action_value[*cnt] < 0.1 &&
			 fp[fpindex].action_value[*cnt] != MISSINGVAL)
			fp[fpindex].action_value[*cnt] = 0.1;

		     if (*cnt > 0)
			fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;

		     *cnt +=1;
		     //first_4points_FREMONT_crest = FALSE;
		  }

		  else
		  {  
		     if (pt2_value != wstage_or_flow && pt2_value != fstage_or_flow)
		     {  
			fp[fpindex].action_index[*cnt] = RISE_STEADY;
			fp[fpindex].action_value[*cnt] = round_float(pt2_value);
			fp[fpindex].action_time[*cnt] = pt2_time;

			/* assign 0.1 to action_value if it is less than 0.1*/

                	if (fp[fpindex].action_value[*cnt] < 0.1 && 
			    fp[fpindex].action_value[*cnt] != MISSINGVAL)
                           fp[fpindex].action_value[*cnt] = 0.1;

			if (*cnt > 0)
	        	   fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;

			*cnt +=1;  
		     }  
		  }	    
	       }
	       
	       else
	       {		    		    
		  fp[fpindex].action_index[*cnt] = RISE_STEADY;
		  fp[fpindex].action_value[*cnt] = round_float(pt2_value);
		  fp[fpindex].action_time[*cnt] = pt2_time;
		  
		  /* assign 0.1 to action_value if it is less than 0.1*/
		  
		  if (fp[fpindex].action_value[*cnt] < 0.1 && 
		      fp[fpindex].action_value[*cnt] != MISSINGVAL)
		     fp[fpindex].action_value[*cnt] = 0.1;
		  
		  if (*cnt > 0)
		     fp[fpindex].action_index[*cnt] +=TRENDPHRASE_BASE_INDEX;
		  
		  *cnt +=1;  
		  
	       }
	    }      	     	  
	    
	 } /* end of significant rising */         	   	 
      } /* end of FREMONT point*/  
      
      
      /* for NORMAL points */
      
      if (fp[fpindex].fcstpoint_type == NORMAL)  
      {
	 if (exceed_ms >= 0) /* rise across warning stage*/
	 {
	    fp[fpindex].action_index[*cnt] = RISE_ABOVEWS;
	    fp[fpindex].action_value[*cnt] = exceed_ms_stage;
	    fp[fpindex].action_time[*cnt]  = exceed_ms_time;
	    
	    /* assign 0.1 to action_value if it is less than 0.1*/
	    
	    if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
	       fp[fpindex].action_value[*cnt] = 0.1;
	    
	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	    
	    *cnt +=1;
	 }
	 
	 if (exceed_fs >= 0)  /* rise across flood stage*/
	 {
	    fp[fpindex].action_index[*cnt] = RISE_ABOVEFS;
	    fp[fpindex].action_value[*cnt] = exceed_fs_stage;
	    fp[fpindex].action_time[*cnt]  = exceed_fs_time;
	    
	    /* assign 0.1 to action_value if it is less than 0.1 */
	    
	    if (fp[fpindex].action_value[*cnt] < 0.1 &&
		fp[fpindex].action_value[*cnt] != MISSINGVAL)
	       fp[fpindex].action_value[*cnt] = 0.1;
	    
	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	    
	    *cnt +=1;
	 }
	 
	 /* if the rising is minor for the time series */
	 
	 if (pt2_value - pt1_value <= chg_threshold)
	 {	      	      
	    /* fluctuate over the time series */
	    
	    fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	    fp[fpindex].action_value[*cnt] = round_float(pt2_value);
	    fp[fpindex].action_time[*cnt]  = pt2_time;
	    
	    /* assign 0.1 to action_value if it is less than 0.1 */
	    
	    if (fp[fpindex].action_value[*cnt] < 0.1 && 
		fp[fpindex].action_value[*cnt] != MISSINGVAL)
	       fp[fpindex].action_value[*cnt] = 0.1;
	    
	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	    
	    *cnt +=1;
	    
	 } /*end of minor change*/    	     	    
	 
	 else /*significant change*/
	 {	      
	    if (case_index == THREEPOINTS_CASE)
	    {
	       if ((fp[fpindex].fcst_max_value - fp[fpindex].fcst_first_value) > 3.0)
	       {
		  fp[fpindex].action_index[*cnt] = RISE_CREST ;
		  fp[fpindex].action_value[*cnt] = round_float(pt2_value);
		  fp[fpindex].action_time[*cnt]  = pt2_time;
		  
		  /* assign 0.1 to action_value if it is less than 0.1*/
		  
		  if (fp[fpindex].action_value[*cnt] < 0.1 &&
		      fp[fpindex].action_value[*cnt] != MISSINGVAL)
		     fp[fpindex].action_value[*cnt] = 0.1;
		  
		  if (*cnt > 0)
		     fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
		  
		  *cnt +=1;
	       }
	       
	       else
	       {  
		  if (pt2_value != wstage_or_flow && pt2_value != fstage_or_flow)
		  {  
		     fp[fpindex].action_index[*cnt] = RISE_STEADY;
		     fp[fpindex].action_value[*cnt] = round_float(pt2_value);
		     fp[fpindex].action_time[*cnt] = pt2_time;
		     
		     /* assign 0.1 to action_value if it is less than 0.1*/
		     
                     if (fp[fpindex].action_value[*cnt] < 0.1 && 
			 fp[fpindex].action_value[*cnt] != MISSINGVAL)
                        fp[fpindex].action_value[*cnt] = 0.1;
		     
		     if (*cnt > 0)
	        	fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
		     
		     *cnt +=1;  
		  }  
	       }
	    }
	    
	    else if (case_index == FOURPOINTS_CASE)
	    {	    	      
	    
	       if ((second_rise_fourpoints == FALSE) &&
	           (((early_point_value - fp[fpindex].fcst_first_value) > 3.0) ||
		   ((late_point_value - early_point_value) > 3.0)))
	       {
		  fp[fpindex].action_index[*cnt] = RISE_CREST;
		  fp[fpindex].action_value[*cnt] = round_float(pt2_value);
		  fp[fpindex].action_time[*cnt]  = pt2_time;
		  
		  /* assign 0.1 to action_value if it is less than 0.1*/
		  
		  if (fp[fpindex].action_value[*cnt] < 0.1 &&
		      fp[fpindex].action_value[*cnt] != MISSINGVAL)
		     fp[fpindex].action_value[*cnt] = 0.1;
		  
		  if (*cnt > 0)
		     fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
		  
		  *cnt +=1;
		  
		  //first_4points_NORMAL_crest = FALSE;
	       }
	       
	       else
	       {  
		  if (pt2_value != wstage_or_flow && pt2_value != fstage_or_flow)
		  {  
		     fp[fpindex].action_index[*cnt] = RISE_STEADY;
		     fp[fpindex].action_value[*cnt] = round_float(pt2_value);
		     fp[fpindex].action_time[*cnt] = pt2_time;
		     
		     /* assign 0.1 to action_value if it is less than 0.1*/
		     
                     if (fp[fpindex].action_value[*cnt] < 0.1 && 
			 fp[fpindex].action_value[*cnt] != MISSINGVAL)
                        fp[fpindex].action_value[*cnt] = 0.1;
		     
		     if (*cnt > 0)
	        	fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
		     
		     *cnt +=1;  
		  }  
	       }	    
	    }
	    
	    else
	    {
	       if (pt2_value != wstage_or_flow && pt2_value != fstage_or_flow)
	       {
		  fp[fpindex].action_index[*cnt] = RISE_STEADY;
		  fp[fpindex].action_value[*cnt] = round_float(pt2_value);
		  fp[fpindex].action_time[*cnt] = pt2_time;
		  
		  
		  /* assign 0.1 to action_value if it is less than 0.1*/
		  
		  if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
		     fp[fpindex].action_value[*cnt] = 0.1;
		  
		  if (*cnt > 0)
		     fp[fpindex].action_index[*cnt] +=TRENDPHRASE_BASE_INDEX;
		  
		  *cnt +=1;  
	       } 
	    }    	 
	    
	 } /* end of the significant change for NORMAL point*/ 	  
	 
      } /* end of NORMAL points*/
      
   }  /* end of pt2_time > pt1_time */        
   
   return;
}


/***********************************************************************
   fall_hydrograph()
   
   PURPOSE
   Determine the trend phrase index, stage/flow value and the time
   in the falling part for forecast hydrograph. Check if the falling
   is a significant one or not, if cross the flood stage or warning stage. 
   ************************************************************************/ 
void fall_hydrograph(double    pt1_value,
		     time_t    pt1_time,
		     double    pt2_value,
		     time_t    pt2_time,
		     int       *cnt,
		     int       fpindex,
		     fp_struct *fp,
		     int       case_index)  
   
{
   int k;
   int exceed_fs = MISSINGVAL;
   int exceed_ms = MISSINGVAL;
   double wstage_or_flow = MISSINGVAL;
   double fstage_or_flow = MISSINGVAL;
   double stage, prev_stage;
   double exceed_ms_stage=MISSINGVAL,exceed_fs_stage=MISSINGVAL;    
   time_t stagetime, prev_stagetime;
   time_t exceed_ms_time=MISSINGVAL,exceed_fs_time=MISSINGVAL;     
   double chg_threshold;
   //int    first = TRUE;
   int   obs_cur_index = MISSINGVAL;
   double obs_cur_value = MISSINGVAL;
   time_t  obs_cur_time = MISSINGVAL;
   
   /* define convenient variable */
   
   chg_threshold = fp[fpindex].chg_threshold;
   
   
   /* define wstage_or_flow, fstage_or_flow according to primary_pe*/
   
   if (fp[fpindex].pe[0] == 'Q')
   {
      if (fp[fpindex].fq != MISSINGVAL)
	 fstage_or_flow = fp[fpindex].fq;
      if (fp[fpindex].aq != MISSINGVAL)
	 wstage_or_flow = fp[fpindex].aq;
   }
   
   else
   {
      if (fp[fpindex].fs != MISSINGVAL)
	 fstage_or_flow = fp[fpindex].fs;
      if (fp[fpindex].wstg != MISSINGVAL)
	 wstage_or_flow = fp[fpindex].wstg;
   }	  	
   
   if (pt2_time > pt1_time)
   {         
      /* get the crossing point for the first time */
      
      //if (first)
     // {
	 /* check for recession of monitor stage or flood stage*/

	 if ((pt1_value < fstage_or_flow && fstage_or_flow != MISSINGVAL) ||
             (pt2_value > fstage_or_flow && fstage_or_flow != MISSINGVAL)) 
	    exceed_fs = -1; 

	 if ((pt1_value < wstage_or_flow && wstage_or_flow != MISSINGVAL) ||
             (pt2_value > fstage_or_flow && fstage_or_flow != MISSINGVAL))
	    exceed_ms = -1; 


	 /* find the position which passes the flood stage */	      

	 if (exceed_fs != -1)
	 {
	    for (k = 0; k < fp[fpindex].numfcstH; k++)
	    {	 
	       if (fp[fpindex].fcstH[k].validtime >= pt1_time)
	       { 	   	     
		  if (fp[fpindex].fcstH[k].value <= fstage_or_flow &&
		      fstage_or_flow != MISSINGVAL)
		  {
		     exceed_fs = k;
		     break;
		  }	       
	       }     
	    }
	 }  


	 /* find the position which passes the monitor stage*/	

	 if (exceed_ms != -1)
	 {
	    for (k=0; k< fp[fpindex].numfcstH; k++)
	    {
	       if (fp[fpindex].fcstH[k].validtime >= pt1_time)
	       {   
		  if (fp[fpindex].fcstH[k].value <= wstage_or_flow &&
		      wstage_or_flow != MISSINGVAL)
		  {
		     exceed_ms = k;
		     break; 
		  }       	      
	       }      
	    }
	 }

         obs_cur_index = fp[fpindex].obs_cur_index;
	 if (obs_cur_index != MISSINGVAL && 
	     fp[fpindex].obsH[obs_cur_index].value != MISSINGVAL)
	 {
	    obs_cur_value = fp[fpindex].obsH[obs_cur_index].value;
	    obs_cur_time = fp[fpindex].obsH[obs_cur_index].validtime;
	 }   

	 /* forecast to fall below flood stage*/

	 if (exceed_fs >= 0 && fp[fpindex].fcstpoint_type == NORMAL)             
	 {	  	     
            /* interpolate the time between the latest observed data
	    and the first forecast data if exceed_fs is 0 */

	    if (exceed_fs == 0 && obs_cur_value != MISSINGVAL)
	    {
	       exceed_fs_stage = fstage_or_flow;	     	    
	       prev_stage      = obs_cur_value;
	       prev_stagetime  = obs_cur_time;
	       stage           = fp[fpindex].fcstH[0].value;
	       stagetime       = fp[fpindex].fcstH[0].validtime;

	       exceed_fs_time = stagetime - 
		  ((stage - fstage_or_flow) / (stage-prev_stage)) *
		  (stagetime - prev_stagetime);
	    } 

	    else if (exceed_fs >= 1 &&
	             fp[fpindex].fcstH[exceed_fs - 1].value != MISSINGVAL)
	    {	     	       
	       exceed_fs_stage = fstage_or_flow;
	       prev_stage      = fp[fpindex].fcstH[exceed_fs - 1].value;
	       prev_stagetime  = fp[fpindex].fcstH[exceed_fs - 1].validtime;
	       stage           = fp[fpindex].fcstH[exceed_fs].value;
	       stagetime       = fp[fpindex].fcstH[exceed_fs].validtime;

	       exceed_fs_time = stagetime-((stage-fstage_or_flow)/(stage-prev_stage))*
		  (stagetime - prev_stagetime);   
	    }
	    else
	    {								       
	       exceed_fs_stage = fp[fpindex].fcstH[exceed_fs].value;
	       exceed_fs_time = fp[fpindex].fcstH[exceed_fs].validtime;
	    }	 
	 }

	 /* forecast to fall below the monitor stage */

	 if (exceed_ms >= 0)
	 {	 
            /* interpolate the time between the latest observed data
	    and the first forecast data if exceed_fs is 0 */

	    if (exceed_ms == 0 && obs_cur_value != MISSINGVAL)
	    {
	       exceed_ms_stage = wstage_or_flow;	     	    
	       prev_stage      = obs_cur_value;
	       prev_stagetime  = obs_cur_time;
	       stage           = fp[fpindex].fcstH[0].value;
	       stagetime       = fp[fpindex].fcstH[0].validtime;

	       exceed_ms_time = stagetime - 
		  ((stage - wstage_or_flow) / (stage-prev_stage)) *
		  (stagetime - prev_stagetime);
	    }     	  	  

	    else if (exceed_ms >= 1 &&
	             fp[fpindex].fcstH[exceed_ms - 1].value != MISSINGVAL)
	    {	     	       
	       exceed_ms_stage = wstage_or_flow;
	       prev_stage      = fp[fpindex].fcstH[exceed_ms - 1].value;
	       prev_stagetime  = fp[fpindex].fcstH[exceed_ms - 1].validtime;
	       stage           = fp[fpindex].fcstH[exceed_ms].value;
	       stagetime       = fp[fpindex].fcstH[exceed_ms].validtime;

	       exceed_ms_time = stagetime-((stage-wstage_or_flow)/(stage-prev_stage))*
		  (stagetime - prev_stagetime);
	    }

	    else
	    {   
	       exceed_ms_stage = fp[fpindex].fcstH[exceed_ms].value;
	       exceed_ms_time  = fp[fpindex].fcstH[exceed_ms].validtime;
	    }
	 }

      //first = FALSE;
        
      //}
      
      /* determine the trend index, stage/flow and time for WEIR, 
	 FREMONT and NORMAL points*/
      
      /* for WEIR points */
      
      if (fp[fpindex].fcstpoint_type == WEIR && wstage_or_flow != MISSINGVAL)
      {
	 /* if the falling is minor for the time series*/
	 
	 if (pt1_value - pt2_value <= chg_threshold)
	 {	      	      
	    if (pt1_value > wstage_or_flow && pt2_value <= wstage_or_flow)  /*fall pass warning stage*/
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_END_OVERFLOW;
	       fp[fpindex].action_time[*cnt]  = exceed_ms_time;
	    }
	    else if (pt2_value > wstage_or_flow) /* min stage is above warning stage*/
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_REMAIN_OVERFLOW;
	       fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow;
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	    }
	    else if (pt1_value <= wstage_or_flow && *cnt != 0) /* max stage is below or equal to warn stage*/
	    {	       	       
	       fp[fpindex].action_index[*cnt] = WEIR_NO_OVERFLOW;		 
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	       	 	
	    }	   
	    
	    if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
	       fp[fpindex].action_value[*cnt] = 0.1;
	    
	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	    
	    *cnt +=1;
	 } /* end of minor recession*/
	 
	 
	 else /*significant recession */
	 {
	    if (pt1_value > wstage_or_flow && pt2_value <= wstage_or_flow)  /* fall pass warning stage*/
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_END_OVERFLOW;
	       fp[fpindex].action_time[*cnt]  = exceed_ms_time;
	    }
	    else if (pt2_value > wstage_or_flow) /* min stage is above warning stage*/
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_OVERFLOW_DEC;
	       fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow;
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	    }
	    else if (pt1_value <= wstage_or_flow && *cnt != 0) /* max stage is below or equal to warning stage*/
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_NO_OVERFLOW;		 
	       fp[fpindex].action_time[*cnt]  = pt2_time;	 	
	    }	   
	    
	    
	    /* assign 0.1 to action_value if it is less than 0.1*/
	    
	    if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
	       fp[fpindex].action_value[*cnt] = 0.1;
	    
	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] +=TRENDPHRASE_BASE_INDEX;
	    
	    *cnt +=1;
	    
	 } /* end of significant recession */
	 
      }  /* end of WEIR point */
      
	 
      /* for FREMONT point */
      
      if (fp[fpindex].fcstpoint_type == FREMONT && wstage_or_flow != MISSINGVAL)
      {
	 /* if the falling is minor for the time series*/
	 
	 if (pt1_value - pt2_value <= chg_threshold)
	 {	     
	    if (pt1_value > wstage_or_flow && pt2_value <= wstage_or_flow)  /* fall pass warning stage*/
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_END_OVERFLOW;
	       fp[fpindex].action_time[*cnt] = exceed_ms_time;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;
	       
	       fp[fpindex].action_index[*cnt] =  FALL_STEADY;
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	       fp[fpindex].action_value[*cnt] = round_float(pt2_value);   
	    }
	    
	    else if (pt2_value > wstage_or_flow) /* min stage is above warning stage */
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_REMAIN_OVERFLOW;
	       fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow;
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	    }
	    
	    else if (pt1_value <= wstage_or_flow) /* max stage is below or equal to wstage */
	    {
	       fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	       fp[fpindex].action_value[*cnt] = round_float(pt2_value);		 
	       fp[fpindex].action_time[*cnt]  = pt2_time;	 	
	    }	   
	    
	    /* assign 0.1 to action_value if it is less than 0.1*/
	    
	    if (fp[fpindex].action_value[*cnt] < 0.1 && 
		fp[fpindex].action_value[*cnt] != MISSINGVAL)
	       fp[fpindex].action_value[*cnt] = 0.1;
	    
	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	    
	    *cnt +=1;
	 }    /*end of minor recession*/
	 
	 else /*significant recession */
	 {
	    if (pt1_value > wstage_or_flow && pt2_value <= wstage_or_flow)  /* fall pass wstage */
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_END_OVERFLOW;
	       fp[fpindex].action_time[*cnt]  = exceed_ms_time;
	       
	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	       
	       *cnt +=1;
	       
	       fp[fpindex].action_index[*cnt] =  FALL_STEADY;
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	       fp[fpindex].action_value[*cnt] = round_float(pt2_value);   
	       
	    }
	    else if (pt2_value > wstage_or_flow) /*the min stage is above warning stage*/
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_OVERFLOW_DEC;
	       fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow;
	       fp[fpindex].action_time[*cnt]  = pt2_time;
	    }
	    else if (pt1_value <= wstage_or_flow) /*the max stage is below or equal to warning stage*/
	    {
	       fp[fpindex].action_index[*cnt] = FALL_STEADY;
	       fp[fpindex].action_value[*cnt] = round_float(pt2_value);		 
	       fp[fpindex].action_time[*cnt]  = pt2_time;	 	
	    }	   
	    
	    
	    /* assign 0.1 to action_value if it is less than 0.1*/
	    
	    if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
	       fp[fpindex].action_value[*cnt] = 0.1;
	    
	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	    
	    *cnt +=1;
	    
	 } /* end of significant recession*/
	 
      } /* end of FREMONT point*/
      
      /* for NORMAL point*/
      
      if (fp[fpindex].fcstpoint_type == NORMAL)  
      {
	 if (exceed_fs > 0) /* fall across flood stage */
	 {
	    fp[fpindex].action_index[*cnt] = FALL_BELOWFS;
	    fp[fpindex].action_value[*cnt] = exceed_fs_stage;
	    fp[fpindex].action_time[*cnt] = exceed_fs_time;
	    
	    /* assign 0.1 to action_value if it is less than 0.1 */
	    
	    if (fp[fpindex].action_value[*cnt] < 0.1 &&
		fp[fpindex].action_value[*cnt] != MISSINGVAL)
	       fp[fpindex].action_value[*cnt] = 0.1;
	    
	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;
	    
	    *cnt +=1;
	 }
	 
	 if (exceed_ms > 0) /*fall across warning stage*/
	 {
	    fp[fpindex].action_index[*cnt] = FALL_BELOWWS;
	    fp[fpindex].action_value[*cnt] = exceed_ms_stage;
	    fp[fpindex].action_time[*cnt] = exceed_ms_time;
	    
	    /* assign 0.1 to action_value if it is less than 0.1*/
	    
	    if (fp[fpindex].action_value[*cnt] < 0.1 &&
		fp[fpindex].action_value[*cnt] != MISSINGVAL)
	       fp[fpindex].action_value[*cnt] = 0.1;
	    
	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] +=TRENDPHRASE_BASE_INDEX;
	    
	    *cnt +=1;
	 }
	 
	 
	 /* if the falling is minor for the time series */
	 
	 if (pt1_value - pt2_value <= chg_threshold)
	 {	      	      	      	      
	    /* fluctuate over the time series */
	    
	    fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	    fp[fpindex].action_value[*cnt] = round_float(pt2_value);
	    fp[fpindex].action_time[*cnt] = pt2_time;
	    
	    
	    /* assign 0.1 to action_value if it is less than 0.1 */
	    
	    if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
	       fp[fpindex].action_value[*cnt] = 0.1;
	    
	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] +=TRENDPHRASE_BASE_INDEX;
	    
	    *cnt +=1;
	    
	 } /* end of minor change */    	     	    
	 
	 else /* significant change */
	 {	       	      	      	          
	    /* fall steady */	    
	   
	    if (pt2_value != wstage_or_flow && pt2_value != fstage_or_flow)
	    { 
	       fp[fpindex].action_index[*cnt] = FALL_STEADY; 
	       fp[fpindex].action_value[*cnt] = round_float(pt2_value);
	       fp[fpindex].action_time[*cnt]  = pt2_time;

	       /* assign 0.1 to action_value if it is less than 0.1*/

	       if (fp[fpindex].action_value[*cnt] < 0.1 && 
		   fp[fpindex].action_value[*cnt] != MISSINGVAL)
		  fp[fpindex].action_value[*cnt] = 0.1;

	       if (*cnt > 0)
		  fp[fpindex].action_index[*cnt] +=TRENDPHRASE_BASE_INDEX;

	       *cnt +=1;
	    }   
	     
	 } /* end of the significant change for NORMAL point*/ 	  
	 
      } /* end of NORMAL points*/       	
      
   } /* end of pt2_time > pt1_time */  
   
   return;
   
}


/**************************************************************************
   twopoints_flat_hydrograph()
   
   PURPOSE
   Determine the trend phrase index, stage/flow value and the time
   in the flat (with first and last points) forecast hydrograph for 
   WEIR, FREMONT and NORMAL points. 
   ************************************************************************/ 
void twopoints_flat_hydrograph(double      pt1_value,
		               time_t      pt1_time,
		               double      pt2_value,
		               time_t      pt2_time,
			       int         *cnt,
			       int         fpindex,
			       fp_struct   *fp)  
{
   double wstage_or_flow = MISSINGVAL;
  
   /* determine use flow or stage */
   
   if (fp[fpindex].pe[0] == 'Q')
   {      
      if (fp[fpindex].aq != MISSINGVAL)
         wstage_or_flow = fp[fpindex].aq;
   }
   
   else
   {      
      if (fp[fpindex].wstg != MISSINGVAL)
         wstage_or_flow = fp[fpindex].wstg;
   }	  	
   
   
   /* For WEIR points */ 
	 
   if (fp[fpindex].fcstpoint_type == WEIR && wstage_or_flow != MISSINGVAL)
   {
      
       /* flat */
       if (fabs(pt1_value - pt2_value) < 0.001)
       {
          if (pt1_value > wstage_or_flow) /*initial stage/flow aready exceed warning stage*/
	  {
	     fp[fpindex].action_index[*cnt] = WEIR_REMAIN_OVERFLOW;
	     fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow; 
	     fp[fpindex].action_time[*cnt]  = pt2_time;

	     /* assign 0.1 to action_value if it is less than 0.1 */

	     if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
		fp[fpindex].action_value[*cnt] = 0.1;

	     if (*cnt > 0)
		fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;

	     *cnt +=1;
	  }
	  else
	  {
	     /* do not output WEIR_NO_OVERFLOW if it is the first phrase */
	     if (*cnt != 0)
	     {
		fp[fpindex].action_index[*cnt] = WEIR_NO_OVERFLOW;
		fp[fpindex].action_time[*cnt]  = pt2_time;

		if (*cnt > 0)
		   fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;

		*cnt +=1;
	     }
	  }
       }
       /*if the change is rising*/
       
       else if (pt1_value < pt2_value )
       {
          
	  rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
	                  &(*cnt), fpindex, fp, TWOPOINTS_CASE, FALSE); 
	  
       }  /* end of minor rising*/
       
       /* minor falling */
       else 
       {
          
	  fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
	                  &(*cnt), fpindex, fp, TWOPOINTS_CASE);
       }	  
   }
   
   /* for FREMONT points */
   
   else if (fp[fpindex].fcstpoint_type == FREMONT && wstage_or_flow != MISSINGVAL) 
   {
      /* flat */
      
      if (fabs(pt1_value - pt2_value) < 0.001)
      {
         if (pt1_value > wstage_or_flow) /*initial stage/flow aready exceed warning stage*/
	 {
	    fp[fpindex].action_index[*cnt] = WEIR_REMAIN_OVERFLOW;
	    fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow; 
	    fp[fpindex].action_time[*cnt]  = pt2_time;

	    /* assign 0.1 to action_value if it is less than 0.1 */

	    if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
	       fp[fpindex].action_value[*cnt] = 0.1;

	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;

	    *cnt +=1;
	 }
	 else
	 {
	    fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	    fp[fpindex].action_value[*cnt] = round_float(pt2_value);
	    fp[fpindex].action_time[*cnt]  = pt2_time;
	    
	     /* assign 0.1 to action_value if it is less than 0.1 */

	    if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
               fp[fpindex].action_value[*cnt] = 0.1;
	       
	    if (*cnt > 0)
	       fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;

	    *cnt +=1;
	 }
      }
   
      /*  rising */
      
      else if (pt1_value < pt2_value)
      {
         
	 rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
	                 &(*cnt), fpindex, fp, TWOPOINTS_CASE, FALSE);                   

      }
   
      /*  falling */
      
      else
      {
         fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
                  &(*cnt), fpindex, fp, TWOPOINTS_CASE);
         
      }   
   
   }  	
   
   /* for NORMAL points */
   
   else if (fp[fpindex].fcstpoint_type == NORMAL)
   {   
      /* flat */
      
      if (fabs(pt1_value - pt2_value) < 0.001 )
      {
         fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	 fp[fpindex].action_value[*cnt] = round_float(pt2_value);
	 fp[fpindex].action_time[*cnt] = pt2_time;	    
         		                 
	 /* assign 0.1 to action_value if it is less than 0.1*/

	 if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
	    fp[fpindex].action_value[*cnt] = 0.1;

	 if (*cnt > 0)
	    fp[fpindex].action_index[*cnt] += TRENDPHRASE_BASE_INDEX;

	 *cnt +=1;
   
      }
   
      /* minor rising */
      
      else if (pt1_value < pt2_value)
      {
         rise_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
	                 &(*cnt), fpindex, fp, TWOPOINTS_CASE, FALSE); 
      
      }
    
      /* minor falling */
      
      else
      {
         fall_hydrograph(pt1_value, pt1_time, pt2_value, pt2_time,
                  &(*cnt), fpindex, fp, TWOPOINTS_CASE);
      
      }   
   
   }
	  
   return;
   
} 


/**************************************************************************
   threepoints_crestflat_hydrograph()
   
   PURPOSE
   Determine the trend phrase index, stage/flow value and the time
   in the flat (with first, last and max points) forecast hydrograph for 
   WEIR, FREMONT and NORMAL points. 
   ****************************************************************************/ 
void threepoints_crestflat_hydrograph(double      pt1_value,
		                      time_t      pt1_time,
		                      double      pt2_value,
		                      time_t      pt2_time,
				      double      pt3_value,
				      time_t      pt3_time,
			              int         *cnt,
			              int         fpindex,
			              fp_struct   *fp) 
{
   double wstage_or_flow = MISSINGVAL;
   double small_value;
   
   /* compare the first value and the last value to find the small one */
   
   small_value = pt1_value;
   if (pt3_value < small_value)
       small_value = pt3_value;	
       
   /* determine use flow or stage */
   
   if (fp[fpindex].pe[0] == 'Q') 
   {      
      if (fp[fpindex].aq != MISSINGVAL)
         wstage_or_flow = fp[fpindex].aq;
   }
   
   else
   {      
      if (fp[fpindex].wstg != MISSINGVAL)
         wstage_or_flow = fp[fpindex].wstg;
   }
   
           
   /* determine trend index, stage/flow value and time for WEIR, FREMONT
      and NORMAL points*/				  
   
   if (pt1_time < pt2_time && pt2_time < pt3_time)
   {
      /* for WEIR and FREMONT points */
      
      if ((fp[fpindex].fcstpoint_type == WEIR || fp[fpindex].fcstpoint_type == FREMONT)
          && wstage_or_flow != MISSINGVAL)
      {
	 if (pt2_value > wstage_or_flow) /* max stage is above warning stage */
	 {
	    if (small_value >= wstage_or_flow)
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_REMAIN_OVERFLOW;
	       fp[fpindex].action_value[*cnt] = 
		             (small_value + fp[fpindex].fcst_max_value) / 2.0
		             - wstage_or_flow;
	       fp[fpindex].action_time[*cnt] = pt3_time;
	    }
	    else /* rise across warning stage */
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_SLIGHT_OVERFLOW;
	       fp[fpindex].action_time[*cnt] = pt2_time;
	    }
	 }
	 else  /* max stage is below warning stage */
	 {
	    if (fp[fpindex].fcstpoint_type == WEIR && *cnt != 0)
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_NO_OVERFLOW;	 
	       fp[fpindex].action_time[*cnt] = pt3_time;   
	    }
	    else if (fp[fpindex].fcstpoint_type == FREMONT)
	    {
	       fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	       fp[fpindex].action_value[*cnt] = round_float((pt2_value +
							  small_value)/2.0);	 
	       fp[fpindex].action_time[*cnt] = pt3_time;  
	    
	    }
	 }
      }  /* end of WEIR and FREMONT point*/
            
      
      /* for NORMAL point*/
      
      if (fp[fpindex].fcstpoint_type == NORMAL)
      {	 
	 fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	 fp[fpindex].action_value[*cnt] = round_float((pt2_value + small_value)/2.0);	 
	 fp[fpindex].action_time[*cnt] = pt3_time;           
      } 
      
      
      /* assign 0.1 to action_value if it is less than 0.1*/
      
      if (fp[fpindex].action_value[*cnt] < 0.1 && fp[fpindex].action_value[*cnt] != MISSINGVAL)
	 fp[fpindex].action_value[*cnt] = 0.1;	           
      
   }  /* end of pt1_time < pt2_time && pt2_time < pt3_time*/
   
   
   return;
} 


/**************************************************************************
   threepoints_valleyflat_hydrograph()
   
   PURPOSE 
   Determine the trend phrase index, stage/flow value and the time
   in the flat (with first, last and min points) forecast hydrograph for 
   WEIR, FREMONT and NORMAL points. 
   ************************************************************************/ 
void threepoints_valleyflat_hydrograph(double      pt1_value,
		                       time_t      pt1_time,
		                       double      pt2_value,
		                       time_t      pt2_time,
				       double      pt3_value,
				       time_t      pt3_time,
			               int         *cnt,
			               int         fpindex,
			               fp_struct   *fp) 
{
   double wstage_or_flow = MISSINGVAL;
   double big_value;
   
   /* compare the first value and the last value to find the bigger one*/

   big_value = pt1_value;
   if (pt3_value > big_value)
      big_value = pt3_value;

   
   /* determine use flow or stage */
   
   if (fp[fpindex].pe[0] == 'Q')
   {      
      if (fp[fpindex].aq != MISSINGVAL)
         wstage_or_flow = fp[fpindex].aq;
   }
   
   else 
   {      
      if (fp[fpindex].wstg != MISSINGVAL)
         wstage_or_flow = fp[fpindex].wstg;
   }
     
   
   /* determine trend index, stage/flow value and time for WEIR, 
      FREMONT and NORMAL points */				  
   
   if (pt1_time < pt2_time && pt2_time < pt3_time)
   {
      /* for WEIR and FREMONT points */
      
      if ((fp[fpindex].fcstpoint_type == WEIR || fp[fpindex].fcstpoint_type == FREMONT)
          && wstage_or_flow != MISSINGVAL)
      {	        
	 if(pt2_value >= wstage_or_flow)
	 {
	    fp[fpindex].action_index[*cnt] = WEIR_REMAIN_OVERFLOW;
	    fp[fpindex].action_value[*cnt] = pt2_value - wstage_or_flow;
	    fp[fpindex].action_time[*cnt] = pt3_time;
	 }
	 else if (big_value <= wstage_or_flow)
	 {
	    if (fp[fpindex].fcstpoint_type == WEIR && *cnt != 0)
	    {
	       fp[fpindex].action_index[*cnt] = WEIR_NO_OVERFLOW;
	       fp[fpindex].action_time[*cnt] = pt3_time;
	    }
	    else if (fp[fpindex].fcstpoint_type == FREMONT)
	    {
	       fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	       fp[fpindex].action_value[*cnt] = round_float(pt3_value);
	       fp[fpindex].action_time[*cnt] = pt3_time;
	    }   
	 }   	 
      } 
         
      
      /* for NORMAL point */
      
      if (fp[fpindex].fcstpoint_type == NORMAL)
      {
	 fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	 fp[fpindex].action_value[*cnt] = round_float(pt3_value);
	 fp[fpindex].action_time[*cnt] = pt3_time;  	  	         	      	    
      } /* end of NORMAL point */
      
      
      /* assign 0.1 to action_value if it is less than 0.1*/
      
      if (fp[fpindex].action_value[*cnt] < 0.1 && 
	  fp[fpindex].action_value[*cnt] != MISSINGVAL)
	 fp[fpindex].action_value[*cnt] = 0.1;	      		 
      
   }  /* end of pt1_time < pt2_time && pt2_time < pt3_time*/	   
   
   return;	      	      
}  	  	      	  


/**************************************************************************
   fourpoints_flat_hydrograph()
   
   PURPOSE
   Determine the trend phrase index, stage/flow value and the time
   in the flat forecast hydrograph with first,last, min and max points for 
   WEIR, FREMONT and NORMAL points. 
   ************************************************************************/ 
void fourpoints_flat_hydrograph(double      pt1_value,
		                time_t      pt1_time,
		                double      pt2_value,
		                time_t      pt2_time,
			        int         *cnt,
			        int         fpindex,
			        fp_struct   *fp)  
{ 
   double wstage_or_flow, big_value, small_value;   
   
   
   /* initialize */
   
   wstage_or_flow = MISSINGVAL;
     
   /* determine the big_value and small value among the four points*/
      
   small_value = fp[fpindex].fcst_first_value;
   big_value = fp[fpindex].fcst_first_value;
   
   if (fp[fpindex].fcst_last_value < small_value )
      small_value = fp[fpindex].fcst_last_value;
   
   if (pt1_value < small_value)
      small_value = pt1_value;
   
   if (pt2_value < small_value)
      small_value = pt2_value;      
      
   if (fp[fpindex].fcst_last_value > big_value)   
      big_value = fp[fpindex].fcst_last_value;
   
   if (pt1_value >= big_value)   
      big_value = pt1_value;         
   
   if (pt2_value >= big_value)
      big_value = pt2_value;
   
   
   /* determine use flow or stage */
   
   if (fp[fpindex].pe[0] == 'Q') /*use flow*/
   {      
      if (fp[fpindex].aq != MISSINGVAL)
         wstage_or_flow = fp[fpindex].aq;
   }
   
   else
   {      
      if (fp[fpindex].wstg != MISSINGVAL)
         wstage_or_flow = fp[fpindex].wstg;
   }
   
      
   /* determine trend index, stage/flow value and time for WEIR, 
      FREMONT and NORMAL points*/
   
   /* for WEIR and Fremont point */
   
   if ((fp[fpindex].fcstpoint_type == WEIR || fp[fpindex].fcstpoint_type == FREMONT)
       && wstage_or_flow != MISSINGVAL)
   {      
      if (small_value > wstage_or_flow)
      {
	 fp[fpindex].action_index[*cnt] = WEIR_REMAIN_OVERFLOW;
	 fp[fpindex].action_value[*cnt] = (pt1_value + pt2_value)/2.0 - wstage_or_flow;
	 fp[fpindex].action_time[*cnt]  = fp[fpindex].fcst_lastvalue_time;
      }
      
      else if (big_value <= wstage_or_flow)
      {  
         if (fp[fpindex].fcstpoint_type == WEIR && *cnt != 0)
	 {
            fp[fpindex].action_index[*cnt] = WEIR_NO_OVERFLOW;
	    fp[fpindex].action_time[*cnt]  = fp[fpindex].fcst_lastvalue_time;
	 }
	 else if (fp[fpindex].fcstpoint_type == FREMONT)
	 {
	    fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
	    fp[fpindex].action_value[*cnt] = round_float(fp[fpindex].fcst_last_value);
	    fp[fpindex].action_time[*cnt]  = fp[fpindex].fcst_lastvalue_time; 
	 }   
      }
      
      else if ((small_value <= wstage_or_flow) && (big_value > wstage_or_flow)
               && ! ((fp[fpindex].fcst_first_value > wstage_or_flow) &&
	             (fp[fpindex].fcst_last_value <= wstage_or_flow) &&
		     (pt1_value <= wstage_or_flow) &&
		     (pt2_value <= wstage_or_flow)))
      {
         fp[fpindex].action_index[*cnt] = WEIR_SLIGHT_OVERFLOW;
	 
	 if (fp[fpindex].fcst_max_value > fp[fpindex].fcst_last_value)
	    fp[fpindex].action_time[*cnt]  = fp[fpindex].fcst_maxvalue_time;
	 else
	    fp[fpindex].action_time[*cnt] = fp[fpindex].fcst_lastvalue_time;   
      
      } 	 
   }  
        
   
   /* for NORMAL point */
   
   if (fp[fpindex].fcstpoint_type == NORMAL)
   {	    
      fp[fpindex].action_index[*cnt] = FLUCTUATE_NEAR;
      fp[fpindex].action_value[*cnt] = round_float(fp[fpindex].fcst_last_value);
      fp[fpindex].action_time[*cnt]  = fp[fpindex].fcst_lastvalue_time;   
   }
   
      
   /* assign 0.1 to action_value if it is less than 0.1 */
   
   if (fp[fpindex].action_value[*cnt] < 0.1 && 
       fp[fpindex].action_value[*cnt] != MISSINGVAL)
      fp[fpindex].action_value[*cnt] = 0.1;	
   
   return;
   
}


/*************************************************************************************
   round_float()
   
   PURPOSE
   The forecast stage value for Normal Forecast Point is rounded to .0 if the 
   decimal point is less than .4, will be .5 if the decimal point is .4, .5 or
   .6 and add 1 to the integral part and use .0 as the decimal part if the 
   decimal part is greater than .6
   
   ************************************************************************************/
double round_float(double input_num)
{   
   double decimal_part, output_num;
   double integral_part;            
   double threshold = 0.00001;
   
   
   /* perform the adjustment */
        
   decimal_part = modf(input_num, &integral_part);
                     
   if ((decimal_part <= 0.3 ) || 
       (decimal_part> 0.3 && (fabs(decimal_part - 0.3) < threshold)))
       output_num = integral_part;
   
   else if ((decimal_part >= 0.7) ||
       (decimal_part< 0.7 && (fabs(decimal_part - 0.7) <threshold)))
       output_num = integral_part + 1.0;
   
   else
       output_num = integral_part + 0.5;            
   
   
   return (output_num);
}      


/*************************************************************************************
   load_tidal_trendinfo()
   
   PURPOSE 
   Describe each max and min stage occured with the time.
   **************************************************************************************/
void load_tidal_trendinfo(fp_struct *fp,
			  int        fpindex)
{
   int k;
   
     
   /* initialize tidal_cnt*/
   
   tidal_cnt = 0;
   
           
   /* record the stage values in the forecast period if they are not missing*/
   
   if (fp[fpindex].numfcstH > 0)
   {   
      if (fp[fpindex].fcstH[0].value != MISSINGVAL)
      {   
        fp[fpindex].action_value[tidal_cnt] = fp[fpindex].fcstH[0].value;
        fp[fpindex].action_time[tidal_cnt] = fp[fpindex].fcstH[0].validtime;
      
        tidal_cnt++;      
      } 
      
      
      /* look for the maximum and minimum stage values during the forecast preiod */

      for (k = 1; k < fp[fpindex].numfcstH - 1; k++ )
      {    
         /* The array boundary is MAXNUM_OF_TRENDPHRASE for fp[].action_value and
	     fp[].action_time */
	 
	 if (tidal_cnt >= MAXNUM_OF_TRENDPHRASE - 1)
	 {
	    log_msg("", "The number of max/min forecast for tidal point exceeds" 
	                 " the allowed maxnumber.");  		  
	    break;
	 }   
	 
	 else
	 {       
	    if (fp[fpindex].fcstH[k-1].value != MISSINGVAL &&
		fp[fpindex].fcstH[k].value != MISSINGVAL &&
		fp[fpindex].fcstH[k+1].value != MISSINGVAL &&
		fp[fpindex].fcstH[k].validtime != MISSINGVAL)
	    {	 
	       if ((fp[fpindex].fcstH[k-1].value <= fp[fpindex].fcstH[k].value &&
		    fp[fpindex].fcstH[k+1].value <  fp[fpindex].fcstH[k].value) ||
		   (fp[fpindex].fcstH[k-1].value >= fp[fpindex].fcstH[k].value &&
		    fp[fpindex].fcstH[k+1].value >  fp[fpindex].fcstH[k].value) ||
		   (fp[fpindex].fcstH[k-1].value <  fp[fpindex].fcstH[k].value &&
		    fp[fpindex].fcstH[k+1].value <= fp[fpindex].fcstH[k].value) ||
		   (fp[fpindex].fcstH[k-1].value >  fp[fpindex].fcstH[k].value &&
		    fp[fpindex].fcstH[k+1].value >= fp[fpindex].fcstH[k].value)) 
	       {
		  
        	  fp[fpindex].action_value[tidal_cnt] = fp[fpindex].fcstH[k].value;	   
        	  fp[fpindex].action_time[tidal_cnt]  = fp[fpindex].fcstH[k].validtime;
		  
		  tidal_cnt ++;
	       }
	    } 
	 }       
      }
      
      
      /* record the stage values for the last point in the 
	 forecast period if they are not missing */

      if (fp[fpindex].fcstH[fp[fpindex].numfcstH - 1].value != MISSINGVAL)
      {         
       	 fp[fpindex].action_value[tidal_cnt] =
	    fp[fpindex].fcstH[fp[fpindex].numfcstH - 1].value;
	 fp[fpindex].action_time[tidal_cnt] = 
	    fp[fpindex].fcstH[fp[fpindex].numfcstH - 1].validtime;			    
      }

   }
   return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/whfs_lib/src/RPFEngine/RCS/compute_stage_info.c,v $";
 static char rcs_id2[] = "$Id: compute_stage_info.c,v 1.8 2007/06/25 18:08:50 deng Exp $";}
/*  ===================================================  */

}

