#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <datetime.h>
#include <string.h>

#include "DbmsUtils.h"
#include "Observation.h"
#include "Forecast.h"
#include "LoadUnique.h"

#include "Report.h" 
#include "List.h"
#include "time_convert.h"
#include "QualityCode.h"

#include "bldts_height.h"
#include "get_best_ts.h"

#define MISSINGVAL -9999.
 

/*********************************************************************
   
   bld_ts_obsriv()
   
   Given a location id, a physical element, and an optional 
   type-source code, return a time series of data.
   Only returns data within the given time window.
   
   The type source should be given as the full two-character
   type source or as an empty string.  
   
   ******************************************************************/

void bldts_obsriv(char 		*lid,
		  char		*pe,
		  char		*ts_filter,
		  int		use_qcfilter,   /* not used */
		  time_t	obs_btime,
		  time_t	obs_etime,
		  Report	**obs,
		  int		*num_obs)
{
   char		use_ts[SHEF_TS_LEN + 1];
   char 	where[240], qcwhere[MAXLEN_QCWHERE];
   Observation	*obsHead, *obsPtr;
   char		btime[ANSI_TIME_LEN], etime[ANSI_TIME_LEN];
   int		status, i;
   int		ordinal;
   char		tablename[40];
   
   
   /* initialize */
   
   *num_obs = 0;
   
   
   /* define which typesource code to use, whether it is passed in
      whether it is determined by the ingestfilter ranks */
   
   if (strlen(ts_filter) == 0)
   {
      ordinal = 0;
      status =  get_best_ts(lid, pe, "R%", ordinal, use_ts);
      if (status < 0) return;
   }   
   else
      strcpy(use_ts, ts_filter);
      
      
   /* define the qc filter to apply */
   
   build_qc_where(QC_NOT_FAILED, qcwhere);
      
   
   /* set the tablename to use */
   
   getTableName(pe, use_ts, tablename);
      
   
   /* get the data for the specified time window and for the
      determined PEDTSEP entry.  build the where clause depending
      upon whether considering only passed qc data.
      note that ListNth index numbers start at 1, not 0. */
     
   status = timet_to_yearsec_ansi(obs_btime, btime);
   status = timet_to_yearsec_ansi(obs_etime, etime);
   
   sprintf(where, " WHERE lid = '%s' AND pe = '%s' AND ts = '%s' AND "
	    " obstime >= '%s' AND obstime <= '%s' AND value != %f AND %s"
	    " ORDER BY obstime ASC ",
	    lid, pe, use_ts, btime, etime, MISSINGVAL, qcwhere);
   
   
   obsHead = GetObservation(where, tablename);
   if (obsHead == NULL)
      return;
   
   
   /* load the data into the local structure */
   
   *num_obs = ListCount(&obsHead->list);
   *obs = (Report *)malloc(sizeof(Report) * (*num_obs));
   if (*obs == NULL)
   {
      fprintf(stderr, "Error in malloc for *obs in bldts_obsht()");
      return;
   }
   
   obsPtr = (Observation *) ListFirst(&obsHead->list);
   for (i = 0; i < *num_obs; i++)
   {
      strcpy((*obs)[i].pe,       obsPtr->pe);
      (*obs)[i].dur            = obsPtr->dur;
      strcpy((*obs)[i].ts,       obsPtr->ts);
      strcpy((*obs)[i].extremum, obsPtr->extremum);
      (*obs)[i].probability    = -1.;
      yearsec_dt_to_timet(obsPtr->obstime, &((*obs)[i].validtime));
      (*obs)[i].basistime      = 0;
      (*obs)[i].value          = obsPtr->value;
      (*obs)[i].quality_code   = obsPtr->quality_code; 
      strcpy((*obs)[i].shef_qual_code, obsPtr->shef_qual_code);      
      
      obsPtr = (Observation *) ListNext(&obsPtr->node);     
   }
   
   
   /* free the allocated data memory */
   
   FreeObservation(obsHead);
   
   
   return;   
}


/****************************************************************
   
   bld_ts_fcstht()
   
   This function assembles a forecast time series for a given
   location and pe.  The data are retrieved for:
   1) either the specified type-source or for the type-source
   defined in the ingest filter as the one to use, based on its rank;
   and for
   2) either all forecast values regardless of basis time
   or only those forecasts with the latest basis time.
   3) for non-probabilistic values only.
   
   It returns a times series of values in an array of structures,
   and also returns the count of values.

   Presuming that the duration and extremum values in the forecast
   table never yield duplicates, then there can only be duplicates
   for the same validtime due to multiple basis times.
   
   There is a passed in limit regarding how far in the future 
   data is considered, and how old the forecast (basistime) can be.
   
   This function is needed since some locations have short-term
   forecasts and long-term forecasts, both of which are valid and
   do not prempt the other.  This avoids problems with the previous method
   where the software always used the forecast with the latest creation time
   and ignored all other forecasts, for certain purposes.
      
   The approach herein does NOT assume that the creation data corresponds
   to the valid time covered - i.e. it does NOT require that long-term
   forecast have the latest creation time.  The heart of the logic
   for this function is contained in the adjust_startend() function.
     
   **************************************************************/

void bldts_fcstriv(char		*lid,
		   char		*pe,
		   char 	*ts_filter,
		   int		use_qcfilter,     /* not used */
		   int		use_latest,
		   time_t	basis_btime,
		   Report 	**fcst,
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
   

   /* retrieve a list of unique basis times; use descending sort. 
      only consider forecast data before some ending time,
      and with some limited basis time ago */
   
   sprintf(where,
	   " WHERE lid= '%s' AND pe= '%s' AND ts= '%s' AND "
	   " probability < 0.0 AND "
	   " validtime >= CURRENT_TIMESTAMP AND "
	   " basistime >= '%s' AND value != %f AND %s" 
	   " ORDER BY basistime DESC ",
	   lid, pe, use_ts, basis_btime_ansi, 
	   MISSINGVAL, qcwhere);
  
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
	      " validtime >= CURRENT_TIMESTAMP AND basistime = '%s' AND "
	      " value != %f AND %s "
	      " ORDER BY validtime ASC",
	      lid, pe, use_ts, ulPtr->uchar, 
	      MISSINGVAL, qcwhere);  
   }
   
   else
   {
      sprintf(where,
	      " WHERE lid = '%s' AND pe = '%s' AND ts = '%s' AND "
	      " probability < 0.0 AND "
	      " validtime >= CURRENT_TIMESTAMP AND basistime >= '%s' AND "
	      " value != %f AND %s"
	      " ORDER BY validtime ASC",
	      lid, pe, use_ts, basis_btime_ansi, 
	      MISSINGVAL, qcwhere);        
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
      fprintf(stderr, "Error in malloc for *fcst in bldts_fcstriv()");
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
   
   set_fcst_keep()
   
   Determine which items in the forecast time series to keep,
   as there may be overlap due to multiple time_series.   
   
   **************************************************************/
void set_fcst_keep(UniqueList	*ulHead,
		   Forecast	*fcstHead,
		   int		*doKeep)
{
   int		ul_count, fcst_count;
   time_t	*ts_start_time, *ts_end_time;
   int		*ts_firstchk, *basis_index;
   time_t	*ts_basis_time;
   time_t	valid_timet, basis_timet;
   int		i, j, status;
   char		basistime_str[ANSI_TIME_LEN];
   Forecast	*fcstPtr;
   UniqueList	*ulPtr;
   
   
   /* get counts of linked lists, one for the forecast values themselves
      and one for the number of unique basis times */
   
   fcst_count = ListCount(&fcstHead->list);
   ul_count = ListCount(&ulHead->list);
   
   
   /* allocate space for identification of which values in the
      time series to keep, for use below */
      
   basis_index = (int *)malloc(sizeof(int) * (fcst_count));
   if (basis_index == NULL)
   {
      fprintf(stderr, "Error in malloc for basis_index in set_fcst_keep()");
      return;
   }
   
   
   /* allocate arrays for each basis time */
   
   ts_start_time = (time_t *)malloc(sizeof(time_t) * (ul_count));
   if (ts_start_time == NULL)
   {
      fprintf(stderr, "Error in malloc for ts_start_time in set_fcst_keep()");
      return;
   }
   
   ts_end_time = (time_t *)malloc(sizeof(time_t) * (ul_count));
   if (ts_end_time == NULL)
   {
      fprintf(stderr, "Error in malloc for ts_end_time in set_fcst_keep()");
      return;
   }
   
   ts_firstchk = (int *)malloc(sizeof(int) * (ul_count));
   if (ts_firstchk == NULL)
   {
      fprintf(stderr, "Error in malloc for ts_firstchk in set_fcst_keep()");
      return;
   }
   
   
   for (i = 0; i < ul_count; i++)
      ts_firstchk[i] = 0;
   
   
   ts_basis_time = (time_t *)malloc(sizeof(time_t) * (ul_count));
   if (ts_basis_time == NULL)
   {
      fprintf(stderr, "Error in malloc for ts_basis_time in set_fcst_keep()");
      return;
   }
   
   
   /* now loop thru the retrieved time series data values and get the
      start and end times for each of the basis times found. */
   
   fcstPtr = (Forecast *) ListFirst(&fcstHead->list);
   
   for (i = 0; i < fcst_count; i++)
   {	       
      
      /* find out which basis time's time series this value belongs to */
      
      status = yearsec_dt_to_ansi(fcstPtr->basistime,  basistime_str);
      
      basis_index[i] = MISSINGVAL;
      
      ulPtr = (UniqueList *) ListFirst(&ulHead->list);
      for (j = 0; ((j < ul_count) && (basis_index[i] == MISSINGVAL)); j++)
      {
	 if (strncmp(ulPtr->uchar, basistime_str, 19) == 0)
	    basis_index[i] = j;
	 
	 ulPtr = (UniqueList *) ListNext(&ulPtr->node);
      }
      
      if (basis_index[i] == MISSINGVAL)
	 fprintf(stderr, "Unexpected error assigning basis_index for %d\n", i);
      
      
      /* check if the values constitute the start or end times
	 for the time series and record these times if they do */
	 
      status = yearsec_dt_to_timet(fcstPtr->validtime, &valid_timet);
      
      if (ts_firstchk[basis_index[i]])
      {
	 if (valid_timet < ts_start_time[basis_index[i]])
	    ts_start_time[basis_index[i]] = valid_timet;
	    
	 else if (valid_timet > ts_end_time[basis_index[i]])
	    ts_end_time[basis_index[i]]   = valid_timet;
      }
      
      else
      {
	 ts_start_time[basis_index[i]] = valid_timet;
	 ts_end_time[basis_index[i]]   = valid_timet;
	 
	 ts_firstchk[basis_index[i]]   = 1;
      }
      
      fcstPtr = (Forecast *) ListNext(&fcstPtr->node);
   }
   
   
   /* for each of the unique basis times, assign the basis time
      in a convenient array for use in the adjust_startend 
      function. */
   
   ulPtr = (UniqueList *) ListFirst(&ulHead->list);
   for (j = 0; j < ul_count; j++)
   {
      status = yearsec_ansi_to_timet(ulPtr->uchar, &basis_timet);
      ts_basis_time[j] = basis_timet;
            
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);
   }
   
   
   /* knowing the actual start and end times for the multiple
      time series, loop thru the time series and adjust the start
      and end times so that they reflect the time span to use;
      i.e. there is no overlap.  THIS IS THE KEY STEP IN THE 
      PROCESS OF DEFINING AN AGGREGATE VIRTUAL TIME SERIES!!! */
   
   adjust_startend(ul_count, ts_basis_time, ts_start_time, ts_end_time);
   
   
   /* loop thru the complete retrieved time series and only
      keep the value if it lies between the start and end
      time for this basis time */
      
   fcstPtr = (Forecast *) ListFirst(&fcstHead->list);
   
   for (i = 0; i < fcst_count; i++)
   {	 
      status = yearsec_dt_to_timet(fcstPtr->validtime, &valid_timet);
      
      if (valid_timet >= ts_start_time[basis_index[i]] &&
	  valid_timet <= ts_end_time[basis_index[i]])
	 doKeep[i] = 1;
      else
	 doKeep[i] = 0;
      
      fcstPtr = (Forecast *) ListNext(&fcstPtr->node);
   }
   
   
   /* free memory */
      
   free(ts_start_time);
   free(ts_end_time);
   free(ts_firstchk);
   free(ts_basis_time);
   
   free(basis_index);
   
   return;
}


/****************************************************************
   
   NEW VERSION   
   adjust_startend()
   
   This method uses the time series with the latest basis time first,
   and uses it in its entirety.  Then the time series with the next
   latest basis time is used.  If it overlaps portions of the already
   saved time series, then only that portion which doesn't 
   overlap is used.  This process continues until all time series
   have been considered.  In essences, this method adjoins adjacent
   time series.
   
   **************************************************************/
void adjust_startend(int	ul_count,
		     time_t	*basis_time,
		     time_t	*start_valid_time,
		     time_t	*end_valid_time)
{
   int		*basis_order;
   int		cur_index;
   time_t	full_start_valid_time, full_end_valid_time;
   int		i, j, k, found;
   time_t	tmp_time;
   
   
   /* initialize array to keep track of order
      of the basis time series' */
        
   basis_order = (int *)malloc(sizeof(int) * (ul_count));
   if (basis_order == NULL)
   {
      fprintf(stderr, "Error in malloc for basis_order in adjust_startend()");
      return;
   }
   
   for (i = 0; i < ul_count; i++)
      basis_order[i] = -1;   
   
   
   /* find the order of the time series by their latest basis time.
      if two time series have the same basis time, use the one that
      has the earlier starting time. note that the order is such
      that the latest basis time is last in the resulting order array. */
   
   for (i = 0; i < ul_count; i++)
   {
      tmp_time = 0;
      cur_index = 0;
      
      for (j = 0; j < ul_count; j++)
      {	
      
         /* only consider the time series if it hasn't been accounted
            for in the order array */
            
         found = 0;
         for (k = 0; k < i; k++)
         {
            if (j == basis_order[k])
            {
               found = 1;
               break;
            }
         }
            
             
	 if (!found)
	 {
	    if (basis_time[j] > tmp_time)
	    {
	       cur_index = j;
	       tmp_time  = basis_time[j];
	    }
	    
	    else if (basis_time[j] == tmp_time)
	    {
	       if (start_valid_time[j] < start_valid_time[cur_index])
	       {
		  cur_index = j;
		  tmp_time  = basis_time[j];
	       }
	    }
	 }
      }
      
      basis_order[i]   = cur_index;
   }
     
   
   /* do NOT adjust the start and end time of the time series
      with the latest ending time.  loop through all the other
      time series and adjust their start and end times as necessary
      so that they do not overlap the time limits of the 
      being-built aggregate time series. */
   
   cur_index = basis_order[0];
   full_start_valid_time = start_valid_time[cur_index];
   full_end_valid_time   = end_valid_time[cur_index];
   
   for (i = 1; i < ul_count; i++)
   {
      cur_index = basis_order[i];
    
      
      /* each additional time series being considered is checked to 
	 see if it falls outside the time window already encompassed
	 by the assembled time series. there are four cases that can 
	 occur; each is handled below. */
      
      /* if the basis time series being considered is fully within the
	 time of the already existing time series, then 
	 ignore it completely, and reset its times. */
      
      if (start_valid_time[cur_index] >= full_start_valid_time &&
	  end_valid_time[cur_index]   <= full_end_valid_time)
      {
	 start_valid_time[cur_index] = 0;
	 end_valid_time[cur_index]   = 0;
      }
      
      
      /* if the basis time series being considered covers time both before
	 and after the existing time series, use the portion of it 
	 that is before the time series.  it is not desirable to use
	 both the before and after portion (this results in a 
	 non-contiguous time-series that is weird), and given a choice
	 it is better to use the forecast data early on than the 
	 later forecast data, so use the before portion */
      
      else if (start_valid_time[cur_index] <= full_start_valid_time &&
	       end_valid_time[cur_index]   >= full_end_valid_time)
      {
	 end_valid_time[cur_index]   = full_start_valid_time - 1;
	 full_start_valid_time = start_valid_time[cur_index];
      }
   
      
      /* if the basis time series being considered straddles the beginning
	 or is completely before the existing time series, then use the
	 portion of it that is before the time series. */
      
      else if (start_valid_time[cur_index] <= full_start_valid_time &&
	       end_valid_time[cur_index]   <= full_end_valid_time)
      {
	 end_valid_time[cur_index]   = full_start_valid_time - 1;
	 full_start_valid_time = start_valid_time[cur_index];
      }
      
      
      /* if the basis time series being considered straddles the end
	 or is completely after the existing time series, then use the
	 portion of it that is after the time series. */
      
      else if (start_valid_time[cur_index] >= full_start_valid_time &&
	       end_valid_time[cur_index]   >= full_end_valid_time)
      {
	 start_valid_time[cur_index]   = full_end_valid_time + 1;
	 full_end_valid_time = end_valid_time[cur_index];
      }
      
   }  /* end for loop on the unique ordered basis times */
     

   /* free the data */
      
   free(basis_order);
   
   return;
}


/****************************************************************
   
   find_maxfcst()
   
   This gets the max forecast value from a forecast
   time-series that has already been prepared.   
   This function returns the ts, value, basistime, and validtime
   for the maximum forecast value.   
   
   **************************************************************/

void find_maxfcst(Report	*fcst_ts,
		  int		fcst_ts_cnt,
		  Report	*max_record)
{  
   int 		i;
   double 	max_value = -8888.;
   int		max_index = MISSINGVAL;
      
   
   /* just in case */
   
   if (fcst_ts_cnt == 0)
   {
      fprintf(stderr, "ERROR - find_maxfcst called with no records!");
      return;
   }
   
   
   /* loop and get the max */
   
   for (i = 0 ; i < fcst_ts_cnt; i++)
   {
      if (fcst_ts[i].value > max_value)
      {
	 max_value = fcst_ts[i].value;
	 max_index = i;
      }      
   }
   
   
   /* if for some bizarre reason, load the first record */
   
   if (max_index == MISSINGVAL)
   {
      fprintf(stderr, "ERROR - find_maxfcst couldn't find max?!\n");
      max_index = 0;
   }
   
   
   /* load the record */
   
   i = max_index;
   
   strcpy(max_record->pe,       fcst_ts[i].pe);
   max_record->dur =            fcst_ts[i].dur; 		
   strcpy(max_record->ts,       fcst_ts[i].ts);
   strcpy(max_record->extremum, fcst_ts[i].extremum);
   max_record->probability =    fcst_ts[i].probability;
   
   strcpy(max_record->shef_qual_code, fcst_ts[i].shef_qual_code);
   max_record->quality_code =         fcst_ts[i].quality_code;
   
   max_record->value     = fcst_ts[i].value;
   max_record->validtime = fcst_ts[i].validtime;
   max_record->basistime = fcst_ts[i].basistime;
   
   
   return;
}


