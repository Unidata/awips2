/*********************************************************************
   I
   load_pe_value.c 
   
   
   load_pe_value() 
     
   load_pe_obsvalue()
   load_pe_fcstvalue()
   
   load_pe_obsdata()
   load_pe_fcstdata()
   
   build_pe_where()
   
   load_precip_value()
   
   get_nearest_obs()
   get_nearest_fcst()
   
   get_derived_obs()
   get_derived_fcst()
   
   get_converted_value()
   
   get_PPdur_match()
   check_if_translate()
   
   unit_conversion()
   *********************************************************************/

#include <string.h>           
#include <stdlib.h>          
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <math.h>

#include "CurPC.h"
#include "CurPP.h"
#include "RawPC.h"
#include "RawPP.h"
#include "HourlyPC.h"
#include "HourlyPP.h"
#include "PairedValue.h"

#include "GeneralUtil.h"
#include "load_pe_value.h"
#include "get_total_precip.h"
#include "load_PCPP_data.h"


extern template_variable_struct TEMPLATE_VARIABLES_TABLE[];


/*********************************************************************
   load_pe_value()
   
   Controls the loading of the pe variable data.
   
   *******************************************************************/

void load_pe_value(int			filter_qcrange,
		   char			*lid,
		   varinfo_struct	varinfo, 
		   values 		*rawvalue,
		   char			*dqcode,
		   char			longstring[])
{               
   char		local_lid[LOC_ID_LEN + 1];
   
   
   /* initialize the returned values. the value depends on the type. */
   
   memset(dqcode,     0, 1);
   memset(longstring, 0, 1);
   
   if (varinfo.time_flag)
      rawvalue->t = (time_t )MISSINGVAL;
   else
      rawvalue->f = (float  )MISSINGVAL;
   
   
   /* identify the location locally */
   
   if (strlen(varinfo.lid) != 0)   
      strcpy(local_lid, varinfo.lid);
   
   else
   {
      if (strlen(lid) <= 0)
      {
         log_msg(UNDEFINED_LID, "results in ignoring PE variable");
	 return;
      }
      else
	 strcpy(local_lid, lid);
   }
   
   
   /* call the appropriate function. */
   
   if (varinfo.ts[0] == 'R' || varinfo.ts[0] == 'P')
   {
      load_pe_obsvalue(filter_qcrange, local_lid, varinfo,
		       rawvalue, dqcode, longstring);
   }
   
   else if (varinfo.ts[0] == 'F' || varinfo.ts[0] == 'C')
   {
      load_pe_fcstvalue(filter_qcrange, local_lid, varinfo,
			rawvalue, dqcode, longstring);
   }
   
   else
      fprintf(stderr, 
	      "Invalid ts %s in call to load_pe_value!!!\n", varinfo.ts);
   
 
   
   /*load the value corresponding to the referece value which is after PV deriven*/
   
   if (varinfo.pv_flag )
       load_paired_value(rawvalue, varinfo);    
       
 
   /*convert english unit to metric unit if metric_flag=1*/
   
   if (varinfo.metric_flag && !varinfo.time_flag && rawvalue->f != MISSINGVAL)	      
       unit_conversion(rawvalue, varinfo);
          
       
   return;
}


/*********************************************************************
   load_pe_obsvalue()
   
   Handles extraction of observed and processed data.
   
   Loads the value of a single variable into a union variable.
   The returned value is either a float value or the time of the value.
   
   *******************************************************************/

void load_pe_obsvalue(int		filter_qcrange,
		      char		*lid,
		      varinfo_struct	varinfo, 
		      values 		*rawvalue,
		      char		*dqcode,
		      char		longstring[])
{         
   char 	msgstr[140];
   int		cnt, status;   
   Observation	*obsHead = NULL;
   time_t	obs_time;
  
   
   /* if reqesting precip data, need to process this in
      a special way to get the incremental accumulation data.  */
   
   if (varinfo.pe[0] == 'P')
   {
      if (varinfo.pe[1] != 'A' && varinfo.pe[1] != 'D' && 
	  varinfo.pe[1] != 'E' && varinfo.pe[1] != 'L')
      {
	 load_precip_value(lid, varinfo, rawvalue, dqcode);
	 
         /* if the data is precip data and the amount is a trace
	    amount, then show the letter T and load into the longstring
	    variable, which is not formatted in the normal fashion. 
	    this longstring trick is used elsewhere also. */
	 
	 if (!varinfo.time_flag)
	    check_if_translate("PP", rawvalue->f, longstring);
	 	 
	 return;
      }	  
   }
   
   
   /* load the observed data for either the explicit PEDTSE or 
      the chosen TS timeseries, and for all the other various
      instructions that might apply */
   
   obsHead = load_pe_obsdata(filter_qcrange, lid, varinfo);
   if (obsHead == NULL)
	cnt = 0;
   else
	cnt = ListCount(&obsHead->list);
   
   
   /* if no data and requested any type-source for the data, then
      see if the next best data are available. */
     
   
   /* now extract the appropriate value or time */
   
   if (obsHead)
   {      
      /* get the non-derived data, whether it be for an
	 exact time or for the latest time. */
      
      if (!varinfo.min_flag && !varinfo.max_flag && !varinfo.chg_flag)
      {
	 if (varinfo.latest_flag)
	 {
	    if (varinfo.time_flag)
	    {
	       status = yearsec_dt_to_timet(obsHead->obstime, &obs_time);
	       rawvalue->t = obs_time;
	    }
	    else
	       rawvalue->f = obsHead->value;
	    
	    strcpy(dqcode, obsHead->shef_qual_code);
	 }
	 
	 else
	 {
	    get_nearest_obs(obsHead, varinfo.datatime, 
			    (varinfo.hr_window * 3600), varinfo.time_flag,
			    rawvalue, dqcode);	    
	 }
	 	 
      }
      
      
      /* getting a derived value of either min, max, or change */
      
      else
      {
	 get_derived_obs(obsHead, varinfo, rawvalue, dqcode);
      }
             
      
      FreeObservation(obsHead);
      obsHead = NULL;
   }
   
   
   /* if presenting the retrieved value in a different
      form, then convert to the new value */
   
   if (!varinfo.time_flag && (int )rawvalue->f != MISSINGVAL &&
       (varinfo.gagezero_flag || varinfo.msl_flag ||
        varinfo.stage_flag    || varinfo.flow_flag))
   {
      get_converted_value(lid, varinfo, rawvalue);
   }
   
   
   /* check if the value is missing and log a message as needed */
   
   sprintf(msgstr, "%s,%s,%d,%s", lid, varinfo.pe, varinfo.dur, varinfo.ts);
   
   if (!varinfo.time_flag && (int )rawvalue->f == MISSINGVAL)
   {
      if (varinfo.derive_flag) strcat(msgstr, " derived");
      if (varinfo.latest_flag) strcat(msgstr, " latest");
      
      log_msg(MISSING_FLTVAL, msgstr);
   }
   
   else if (varinfo.time_flag && (int )rawvalue->t == MISSINGVAL)
   {
      if (varinfo.derive_flag) strcat(msgstr, " derived");
      if (varinfo.latest_flag) strcat(msgstr, " latest");
      
      log_msg(MISSING_TIMVAL, msgstr);
   }
   
   
   /* if the value is NOT missing, then do one last special check.
      see if the value should be represented as a character string
      for whatever reason.  this takes advantage of the fact that if
      the longstring variable is not null, then the normal formatting
      of numeric data is not performed.  this   */
   
   else
   {
      /* if the data being retrieved is a weather element code or
	 certain other physical elements, then see if there is a text
	 translation specified for the code. if so, then use it
	 and return the value as a string, not a number. */
      
      if (!varinfo.time_flag)
      {	 
	 check_if_translate(varinfo.pe, rawvalue->f, longstring);
      }
      
   }

   
   return;
}


/*********************************************************************
   
   load_pe_fcstvalue()
   
   Forecast precip data gets NO special treatment.
      
   *******************************************************************/

void load_pe_fcstvalue(int		filter_qcrange,
		       char		*lid,
		       varinfo_struct	varinfo, 
		       values 		*rawvalue,
		       char		*dqcode,
		       char		longstring[])
{
   char 	msgstr[140];
   int		cnt, status;   
   Forecast	*fcstHead = NULL;
   time_t	fcst_time;
   	      
   
   /* load the data for either the explicit PEDTSE or 
      the chosen TS timeseries, and for all the other various
      instructions that might apply */
   
   fcstHead = load_pe_fcstdata(filter_qcrange, lid, varinfo);
   if (fcstHead == NULL)
   	cnt = 0;
   else
   	cnt = ListCount(&fcstHead->list);
   
   
   /* now extract the appropriate value or time */
   
   if (fcstHead)
   {      
      /* get the non-derived data, whether it be for an exact
	 time or for the next time.  latest is left in the if
	 check even though it is not supported for fcst data. */
      
      if (!varinfo.min_flag && !varinfo.max_flag)
      {
	 if (varinfo.next_flag || varinfo.latest_flag)
	 {	    
	    /* need to get the value with the latest basis time.
	       since the data are sorted first by basis time, then
	       valid time, this will use the forecast from the latest
	       basis time, even if an earlier basis time */
	    
	    if (varinfo.time_flag)
	    {
	       status = yearsec_dt_to_timet(fcstHead->validtime, &fcst_time);
	       rawvalue->t = fcst_time;
	    }
	    else
	       rawvalue->f = fcstHead->value;
	    
	    strcpy(dqcode, fcstHead->shef_qual_code);
	 }
	 
	 else
	    get_nearest_fcst(fcstHead, varinfo.datatime, 
			     (varinfo.hr_window * 3600), varinfo.time_flag,
			     rawvalue, dqcode);
      }
      
      
      /* getting a min or max derived value */
      
      else
      {
	 get_derived_fcst(fcstHead, varinfo, rawvalue, dqcode);
      }
             
      
      FreeForecast(fcstHead);
      fcstHead = NULL;
   }
      
   
   /* if presenting the retrieved value in a different
      form, then derive the new value */
   
   if (!varinfo.time_flag && (int )rawvalue->f != MISSINGVAL &&
       (varinfo.gagezero_flag || varinfo.msl_flag ||
        varinfo.stage_flag    || varinfo.flow_flag))
   {
      get_converted_value(lid, varinfo, rawvalue);
   }
   
   
   /* check if the value is missing */
   
   sprintf(msgstr, "%s,%s,%d,%s",
	   lid, varinfo.pe, varinfo.dur, varinfo.ts);
   
   if (!varinfo.time_flag && (int )rawvalue->f == MISSINGVAL)
   {
      if (varinfo.derive_flag) strcat(msgstr, " derived");
      if (varinfo.next_flag)   strcat(msgstr, " next");
      
      log_msg(MISSING_FLTVAL, msgstr);
   }
   
   else if (varinfo.time_flag && (int )rawvalue->t == MISSINGVAL)
   {
      if (varinfo.derive_flag) strcat(msgstr, " derived");
      if (varinfo.next_flag)   strcat(msgstr, " next");
      
      log_msg(MISSING_TIMVAL, msgstr);
   }
  
     
   return;
}


/*********************************************************************
   load_pe_obsdata()
   
   PURPOSE
   Load the proper pe data based on the user instructions.
   
   
   *******************************************************************/
Observation * load_pe_obsdata(int		filter_qcrange,
			      char		*lid,
			      varinfo_struct	varinfo)
{
   Observation 	*obsHead = NULL;
   IngestFilter	*ingestHead = NULL, *ingestPtr = NULL;
   int		cnt;
   char		use_ts[SHEF_TS_LEN + 1];
   char 	where[600];
   char 	tablename[60];
   values	rawvalue;
   char		dqcode[SHEF_QC_LEN + 1];
   int		ts_data_found;
   
      
   /* initialize */
   
   obsHead = (Observation *)NULL;
   
   
   /* get the table name.  if an invalid pe is given, then the
      table name is returned as "INVALID" */
   
   getTableName(varinfo.pe, varinfo.ts, tablename);
   
   
   /* if requesting an explicit type-source then process 
      in the normal fashion */
   
   if (varinfo.ts[1] != '*')
   {      
     /* build the where clause.  the time window returned in the where clause
	 plays a role in the retrieval of the requested data, particulary
	 for certain derived data. */
      
      memset(use_ts, 0, SHEF_TS_LEN +1);
      build_pe_where(filter_qcrange, lid, varinfo, use_ts, tablename, where);
      
      
      /* now get the data finally */
      
      obsHead = GetObservation(where, tablename);    
   }
   
   
   else
   {
      /* get the possible stage time-series PEDTSE info
	 from the IngestFilter.  note the sorted order. */
      
      sprintf(where, " WHERE lid = '%s' AND pe = '%s' AND "
	      " ts LIKE '%c%%' AND extremum='%s' AND ingest = 'T' "
	      " ORDER BY ts_rank, ts ",
	      lid, varinfo.pe, varinfo.ts[0], varinfo.extremum);
      
      ingestHead = GetIngestFilter(where);
      
      if (ingestHead == (IngestFilter *) NULL)
	 return(obsHead);
      
      
      /* now loop on each possible one and check if the necessary data
	 are available. if so, then exit */
      
      ingestPtr = (IngestFilter *) ListFirst(&ingestHead->list);
      while (ingestPtr)
      {
	 /* try and read in the data for this type-source
	    for this lid, pe, and time range */
	 
	 strcpy(use_ts, ingestPtr->ts);
	 
	 
	 /* build the where clause based on the requested info
	    after setting the type-source being considered. */
	 
	 build_pe_where(filter_qcrange, lid, varinfo, use_ts, tablename, where);
	 
	 
	 /* now try and get the data */
	 
	 ts_data_found = FALSE;
	 obsHead = GetObservation(where, tablename);    
	 if (obsHead == NULL)
		cnt = 0;
	 else
		cnt = ListCount(&obsHead->list);
	 
	 
	 /* for almost all the different instructions, if any data are found, 
	    then we can exit with confidence knowing that the specific request
	    can be fulfilled - this includes min,max,gagezero,msl,flow,stage
	    derivations.  however, for x-hour change requests, just having data
	    does not guarantee that we have the necessary data. so for this one
	    case, see if we can derive the necessary value.  */
	 
	 if (cnt > 0)
	 {
	    if (varinfo.chg_flag)
	    {
	       get_derived_obs(obsHead, varinfo, &rawvalue, dqcode);
	       
	       if (varinfo.time_flag)
	       {
		  if (rawvalue.t != (time_t )MISSINGVAL)
		     ts_data_found = TRUE;
		     
	       }
	       else
	       {
		  if (rawvalue.f != (float )MISSINGVAL)
		     ts_data_found = FALSE;
	       }
	    }
	    	       
	    else
	       ts_data_found = TRUE;
	 }
	 
	 
	 /* if no data found, then look in next TS, if there is one */
	 
	 if (ts_data_found == FALSE)	   
	    ingestPtr = (IngestFilter *) ListNext(&ingestPtr->node);
	    
	 else
	    break;
	    
      }  /* end of while on ingestfilter */
      
      FreeIngestFilter(ingestHead);
      
   }     /* end of else block on R*,P* */
   
   
   return(obsHead);
}


/*********************************************************************
   load_pe_fcstdata()
   
   PURPOSE
   Load the proper pe data based on the user instructions.
   
   TS rank-stepping for ContingencyValue is not functional
   as long as the IngestFilter does not track contingency entries.
   
   
   *******************************************************************/
Forecast * load_pe_fcstdata(int			filter_qcrange,
			    char		*lid,
			    varinfo_struct	varinfo)
{
   Forecast 	*fcstHead = NULL;
   IngestFilter	*ingestHead = NULL, *ingestPtr = NULL;
   int		cnt;
   char		use_ts[SHEF_TS_LEN + 1];
   char 	where[600];
   char 	tablename[60];
   
   
   /* initialize */
   
   fcstHead = (Forecast *)NULL;
   
   
   /* get the table name. if an invalid pe is given, then the
      table name is returned as "INVALID" */
   
   getTableName(varinfo.pe, varinfo.ts, tablename);
   
   
   /* if requesting an explicit type-source, then process 
      in the normal fashion */
   
   if (varinfo.ts[1] != '*')
   {      
     /* build the where clause.  the time window returned in the where clause
	plays a role in the retrieval of the requested data, particulary
	for certain derived data. */
      
      memset(use_ts, 0, SHEF_TS_LEN +1);
      build_pe_where(filter_qcrange, lid, varinfo, use_ts, tablename, where);
      
      
      /* now get the data finally */
      
      fcstHead = GetForecast(where, tablename);    
   }
   
   
   else
   {
      /* get the possible stage time-series PEDTSE info
	 from the IngestFilter. note the sorted order. */
      
      sprintf(where, " WHERE lid = '%s' AND pe = '%s' AND "
	      " ts LIKE '%c%%' AND ingest = 'T' "
	      " ORDER BY ts_rank, ts ",
	      lid, varinfo.pe, varinfo.ts[0]);
      
      ingestHead = GetIngestFilter(where);
      
      if (ingestHead == (IngestFilter *) NULL)
	 return(fcstHead);
      
      
      /* now loop on each possible one and check if the necessary data
	 are available. if so, then exit */
      
      ingestPtr = (IngestFilter *) ListFirst(&ingestHead->list);
      while (ingestPtr)
      {
	 /* try and read in the data for this type-source
	    for this lid, pe, and time range */
	 
	 strcpy(use_ts, ingestPtr->ts);
	 
	 
	 /* build the where clause based on the requested info
	    after setting the type-source being considered. */
	 
	 build_pe_where(filter_qcrange, lid, varinfo, use_ts, tablename, where);
	 
	 
	 /* now try and get the data */
	 
	 fcstHead = GetForecast(where, tablename);
	 if (fcstHead == NULL)
		cnt = 0;
	 else
		cnt = ListCount(&fcstHead->list);
	 
	 
	 /* for almost all the different instructions, if any data are found, 
	    then we can exit with confidence knowing that the specific request
	    can be fulfilled - this includes min,max,gagezero,msl,flow,stage
	    derivations.  however, for x-hour change requests, just having data
	    does not guarantee that we have the necessary data.  
	    but since the change instruction is not supported for 
	    forecast/contingency data, don't worry about it after all */
	 
	 if (cnt > 0)
	    break;	 
	 else
	    ingestPtr = (IngestFilter *) ListNext(&ingestPtr->node);
	 
      }  /* end of while on ingestfilter */
      
      FreeIngestFilter(ingestHead);
      
   }  /* end of else block on F*,C* */
   
   
   return(fcstHead);
}


/*********************************************************************
   build_pe_where()
   
   PURPOSE
   Build the where clause for the data retrieval of pe variables.
   The time window plays a key role in the proper later retrieval
   of the requested data.
   
   The tablename is passed in the event that it is needed for 
   a subquery within the main query that this function assembles.  
   
   *******************************************************************/
void build_pe_where(int			filter_qcrange,   /* not used */
		    char		*lid,
		    varinfo_struct	varinfo,
		    char		*ts,
		    char		*tablename,
		    char		*where)
{
   char		ansi_btime[ANSI_TIME_LEN], ansi_etime[ANSI_TIME_LEN];
   time_t	tnow, obs_time, fcst_time, basis_time;
   int		status, i, probability_match = FALSE;
   char		clause[460], qcwhere[MAXLEN_QCWHERE], where1[100];
   char		use_ts[SHEF_TS_LEN + 1];
   
   static ShefProb *shefprobHead = NULL;
   float           shef_probability;
   static int       first = TRUE;
   ShefProb       *shefprobPtr = NULL;
   
   /* use either the explicit type-source in the user request
      or the type-source code being tested against
      based on the user wildcard request */
   
   if (strlen(ts) == 2)
      strcpy(use_ts, ts);
   else
      strcpy(use_ts, varinfo.ts);     
   
   /* define the where clause */
   
   build_qc_where(QC_NOT_FAILED, qcwhere);
   
      
   /* first insert the general where clause items */
   
   sprintf(where, " WHERE lid= '%s' AND pe= '%s' AND dur= %d"
	   " AND ts= '%s' AND extremum= '%s' "
	   " AND value != %.0f AND %s",
	   lid, varinfo.pe, varinfo.dur, use_ts,
	   varinfo.extremum, (float )MISSINGVAL, qcwhere);
   
   
   /* get the current system time for possible later use */
   
   time(&tnow);
   
   
   /* OBSERVED or PROCESSED ------------------- */
   
   if (varinfo.ts[0] == 'R' || varinfo.ts[0] == 'P')
   {
      
      /* if getting the latest min or max data then limit the
	 lookback window to the exact window requested.  if getting
	 the change value, then limit the retrieval based in part 
	 on the maximum lookback time considered by riverpro to avoid
	 treating very old data as latest */
      
      if (varinfo.latest_flag)
      {	 
	 /* combining min max requests with latest implies 
	    a reference time of now. */
	 
	 if (varinfo.min_flag || varinfo.max_flag)
	 {
	    obs_time = tnow - (varinfo.derive_numhrs * 3600);
	 }
	 
	 
	 /* if a change value requested */
	 
	 else if (varinfo.chg_flag)
	 {
	    set_timevals(tnow, &obs_time, &fcst_time, &basis_time);
	    obs_time = obs_time - (varinfo.derive_numhrs * 3600);
	 }
	 
	 else
	 {
	    set_timevals(tnow, &obs_time, &fcst_time, &basis_time);
	 }
	 
	 
	 status = timet_to_yearsec_ansi(obs_time, ansi_btime);      
	 sprintf(clause, " AND obstime >= '%s' ORDER BY obstime DESC ",
		 ansi_btime);
      }
      
      
      /* if getting a value within a specific time window.
	 then use an earlier, larger window for certain derived data */
      
      else
      {
	 if (varinfo.min_flag || varinfo.max_flag || varinfo.chg_flag)
	 {
	    obs_time = varinfo.datatime - (varinfo.hr_window * 3600) -
	       (varinfo.derive_numhrs * 3600);
	    status = timet_to_yearsec_ansi(obs_time, ansi_btime);
	    
	    obs_time = varinfo.datatime + (varinfo.hr_window * 3600);
	    status = timet_to_yearsec_ansi(obs_time, ansi_etime);
	    
	 } 
	 
	 else	 
	 {
	    obs_time = varinfo.datatime - (varinfo.hr_window * 3600);
	    status = timet_to_yearsec_ansi(obs_time, ansi_btime);
	    
	    obs_time = varinfo.datatime + (varinfo.hr_window * 3600);
	    status = timet_to_yearsec_ansi(obs_time, ansi_etime);	 
	 }
	 
	 sprintf(clause, " AND obstime >= '%s' AND obstime <= '%s' "
		 " ORDER BY obstime DESC ",
		 ansi_btime, ansi_etime);
      }
      
      strcat(where, clause);
   }
   
      
   /* FORECAST or CONTINGENCY --------------------- */
   
   /* set the time range for the retrieval.
      the next flag is supported for forecast data only.
      using the "NEXT" specified with MIN or MAX specifier forces
      the reference time to be now. 
      the change value is currently NOT supported for forecast data.
      NOTE THE DIFFERENT TIME SORT-ORDERs SPECIFIED.  */
      
   else
   {
      /*consider probablistic data */
      
      /*get probablistic number from ShefProb table based on varinfo.probcode*/
     
      if (first)
      {
        first = FALSE;
        sprintf(where1, " "); 
        shefprobHead = GetShefProb(where1);
        if (shefprobHead == NULL)  	   
           fprintf(stderr, "ERROR - No probability found for the character %s,"
	                   " use default probability Z", varinfo.probcode);				  
			 
      }			 
      
      if (shefprobHead != NULL)            
      {
         shefprobPtr = (ShefProb *) ListFirst(&shefprobHead->list);
	 for (i=0; shefprobPtr; i++)
	 {
	    if (strcmp(shefprobPtr->probcode, varinfo.probcode) == 0)
	    {
	       probability_match = TRUE;
	       shef_probability = shefprobPtr->probability;
               sprintf(clause, " AND ABS(ABS(probability) - ABS(%f)) < 0.0001 ",
	                  shef_probability); 
            }
	    shefprobPtr = (ShefProb *) ListNext(&shefprobPtr->node);
	 } /*end of for loop*/   
      } /*end of if statement*/	     			  
      else   			              	 	 
         sprintf(clause, " AND probability < 0.0 ");

      /*if no probability found for the specified probability code, use default code Z*/	 
      
      if (probability_match == FALSE)
         sprintf(clause, "AND probability < 0.0 ");	 
	 
      strcat(where, clause);
                  	 
      /* when the calling function processes the returned data, for most
         cases, the processing function only uses a value if it is for the 
	 latest basis time.  however, for "next" requests, this is not the
	 case.  to ensure that only values for the latest basis times
	 are used, use a subquery to get the data.  this was a problem
	 for the case where a extremum value was in a previous basis time
	 but not in the latest basis time.
	 later, could add this subclause to all forecast data queries,
	 and then remove the basis time check in the get_nearest_fcst()
	 and get_derived_fcst() functions !!! */
      
      if (varinfo.next_flag)
      { 
	 status = timet_to_yearsec_ansi(tnow, ansi_btime);      
	 
	 if (varinfo.min_flag || varinfo.max_flag)
	    fcst_time = tnow + (varinfo.derive_numhrs * 3600);      
	 else
	    set_timevals(tnow, &obs_time, &fcst_time, &basis_time);
	 
	 status = timet_to_yearsec_ansi(fcst_time, ansi_etime);
	 
	 
	 if (strcmp(varinfo.extremum, "Z") == 0)	 
	   sprintf(clause, " AND validtime >= '%s' AND validtime <= '%s' "
		   " ORDER BY basistime DESC, validtime ASC ",
		   ansi_btime, ansi_etime);
		   
	 else
	 {
	   sprintf(clause, " AND validtime >= '%s' AND validtime <= '%s' "
	           " AND basistime = (SELECT MAX(basistime) FROM %s "
		   " WHERE lid= '%s' AND pe= '%s' AND dur= %d"
	           " AND ts= '%s' AND %s) " 	   
		   " ORDER BY basistime DESC, validtime ASC ",
		   ansi_btime, ansi_etime, tablename,
		   lid, varinfo.pe, varinfo.dur, use_ts,  qcwhere);
	 }	 
      }
      
      
      /* the latest flag is allowed for contingency data.
	 its data can be in the past or the future so use a window
	 that straddles the current time, and order by times
	 in descending order so the newest value is used. */
      
      else if (varinfo.latest_flag)
      {
	 set_timevals(tnow, &obs_time, &fcst_time, &basis_time);      
	 
	 status = timet_to_yearsec_ansi(obs_time,  ansi_btime);
	 status = timet_to_yearsec_ansi(fcst_time, ansi_etime);
	 
	 sprintf(clause, " AND validtime >= '%s' AND validtime <= '%s' "
		 " ORDER BY basistime DESC, validtime DESC ",
		 ansi_btime, ansi_etime);   
      }
      
      
      /* getting a value within a specific time window.
	 use an earlier, larger window for derived data */
      
      else
      {	 
	 if (varinfo.min_flag || varinfo.max_flag)	 
	 {
	    fcst_time = varinfo.datatime - (varinfo.hr_window * 3600) -
	       (varinfo.derive_numhrs * 3600);
	    status = timet_to_yearsec_ansi(fcst_time, ansi_btime);
	    
	    fcst_time = varinfo.datatime + (varinfo.hr_window * 3600);
	    status = timet_to_yearsec_ansi(fcst_time, ansi_etime);	 
	 } 
	 
	 else	 
	 {
	    fcst_time = varinfo.datatime - (varinfo.hr_window * 3600);
	    status = timet_to_yearsec_ansi(fcst_time, ansi_btime);
	    
	    fcst_time = varinfo.datatime + (varinfo.hr_window * 3600);
	    status = timet_to_yearsec_ansi(fcst_time, ansi_etime);	 
	 }
	 
	 sprintf(clause, " AND validtime >= '%s' AND validtime <= '%s' "
		 " ORDER BY basistime DESC, validtime ASC ",
		 ansi_btime, ansi_etime);
      }
      
      strcat(where, clause);
   }
   
   
   return;
}


/*********************************************************************
   load_precip_value()
   
   PURPOSE
   Loads a specified precip value, either its value or its time.
   Use load_PC_raw() and load_PP_raw() to load pe PC data from CurPC table
   (instead of CurPrecip table as the past),
   load pe PP data from CurPP table (instead of CurPrecip table as the past). 
   Use get_total_raw_precip() to get the total structure for PC and PP.
   
   *******************************************************************/

void load_precip_value(char		*lid,
		       varinfo_struct	varinfo, 
		       values 		*rawvalue,
		       char		*dqcode)
{
   static float         min_percent;
   time_t	      valid_time, tnow, rounded_now;
            
   CurPC               *cpcHead = NULL;
   CurPP               *cppHead = NULL;
   time_t              query_begin_time, query_end_time;
   time_t              start_time;
   time_t	       obs_time, fcst_time, basis_time;
   int                  record_count, status;
   float              value;
   float               durfilled;
   char                 value_indicator;
   char                *ts;
   time_t 	       datadur;
   char			msgstr[120];
   char                 token_string[60];
   int                  token_len, string_len;
   static int            first = TRUE;
   float                 missing_hour, datadur_in_hour, addition_hour;
   struct total_precip   total;
   int                   ending_time_match;
   unsigned char         settings;
   short int             advance;
   int                   pc_records, pp_records;
   
   
   /* initialize value_indicator */
   
   value_indicator = 'F';
   value = MISSINGVAL;
   durfilled = MISSINGVAL;
   
   
   /* get min_percent from token whfs_min_dur_filled */
   
   if (first) 
   {
     token_len = strlen("rpf_min_dur_filled");
     get_apps_defaults("rpf_min_dur_filled", &token_len, token_string, 
                       &string_len);
     if (string_len > 0)
     {
       min_percent = atof(token_string);
       if ((min_percent <= 0.0 ) || (min_percent > 1.00) )
       {
           min_percent = DEFAULT_MIN_PERCENT;
	   sprintf(msgstr, "Error in specified value for token: %s.\n"
	                   "Using default value of %f percent.\n",
			   token_string, (min_percent * 100.));
	   log_msg("", msgstr);
       }
     }
     else
       min_percent = DEFAULT_MIN_PERCENT;  
    
     first = FALSE;
   }     
    
   
   /* set up convenience variable. ignore the extremum code */
   
    ts = varinfo.ts;

  
   /* determine the current time rounded to the top of the previous hour */
   
   time(&tnow); 
   rounded_now = tnow / SECONDS_PER_HOUR;
   rounded_now = rounded_now * SECONDS_PER_HOUR;
   
   
   /* processing PC data ---------------------------------------------------- */
   
   if (strcmp(varinfo.pe, "PC") == 0)
   {
       /*set the arguments for get_total_raw_precip()*/
      
       ending_time_match = 0;
       settings = REPORT_MISSING_BELOW_MIN_PERCENT;   
       advance = 0;
   
      /* for PC data, the Latest flag implies use the top of the hour. */
      
      if (varinfo.latest_flag)      
	 valid_time = rounded_now;   
      else
	 valid_time = varinfo.datatime;
      
      
      /* now set the duration.  the valid time is important for those
	 durations that are referenced to a base time (ie. dur 5004).
	 the set the other important time values. when setting the begin
	 time, add some hours for good measure.  */
      
      status = shefdur_to_timet(varinfo.dur, valid_time, &datadur);
      
      start_time       = valid_time - datadur;
      query_begin_time = valid_time - datadur - (varinfo.hr_window * 3600);
      query_end_time   = valid_time + (varinfo.hr_window * 3600);           
      
      /* now get the data, then process it */
      
           
      cpcHead = (CurPC *) load_PC_raw(query_begin_time, query_end_time, 
                                      lid, (const char **)&ts, 1,
                                      CurRawPrecip, &record_count); 
      
      if (cpcHead != ( CurPC *) NULL)
      {
          total = get_total_raw_precip( (RawPC **) & cpcHead, NULL, start_time,
                     valid_time, ending_time_match, min_percent, settings,
		     advance, & pc_records, & pp_records); 
            
          value_indicator = total.value_indicator;
          value = total.value;
          durfilled = total.hours_covered;		    
      }     		    
   }
   
   
   /* processing PP data ------------------------------------------------- */  
   
   else if (strcmp(varinfo.pe, "PP") == 0)
   {
      /* for PP data, first set the query time window, based on whether the
	 Latest flag is set. */
      
      if (varinfo.latest_flag)
      {
	 set_timevals(rounded_now, &obs_time, &fcst_time, &basis_time);
	 status = shefdur_to_timet(varinfo.dur, rounded_now, &datadur);
	 
	 query_begin_time = obs_time - datadur - (3600 * 12);	 
	 query_end_time   = tnow;
	 
      }
      
      else
      {
	 status = shefdur_to_timet(varinfo.dur, varinfo.datatime, &datadur);
	 
	 
	 /* if not using the auto accum option, then limit the retrieval
	    time window  so the time window specified by the user will
	    be adhered to when looking for an exact duration match later */
	 
	 if (varinfo.accum_flag)
	    query_begin_time  = varinfo.datatime - (varinfo.hr_window * 3600) -
	       datadur - (3600 *12);
	 else
	    query_begin_time  = varinfo.datatime - (varinfo.hr_window * 3600);
	 
	 query_end_time       = varinfo.datatime + (varinfo.hr_window * 3600);
      }
      
      
      /* get the data for the specified query time window. it is very	 
	 important to note that the returned data is ordered from
	 most recent to oldest and that since the lid and ts are
	 specified, only records for that lid and ts are returned.*/

      cppHead = (CurPP *)load_PP_raw(query_begin_time, query_end_time, 
                                     lid, (const char **)&ts, 1,
                                     CurRawPrecip, &record_count);           
                  
      
      if (varinfo.latest_flag)
	 valid_time = rounded_now;
      else
	 valid_time = varinfo.datatime;

      start_time = valid_time - datadur;
	
      /* if instructed to accumulate a value, then try to find the exact match
         duraction and requested time first, if can not find, then try to build
         accumulation*/
      
      if (varinfo.accum_flag)
      { 
         /* try to find the exact match first */
	 	 
	 /* set the arguments for get_total_raw_precip() */
      
         ending_time_match = EXACT_ENDINGTIME_MATCH;
         settings = REPORT_MISSING_BELOW_MIN_PERCENT;   
         advance  = 0;
	  		
	 if ( cppHead != (CurPP *) NULL)
	 {	 
	    total = get_total_raw_precip( NULL, (RawPP **) & cppHead,
	               start_time, valid_time, ending_time_match,
		       min_percent, settings, advance, & pc_records, 
		       & pp_records);
	 	       
            value_indicator = total.value_indicator;
            value           = total.value;
            durfilled       = total.hours_covered;
	  } 	  
      }
      
      
      /* if instructed to not accumulate a value, then check
	 for an exact duration match.  if the 'latest' flag is set,
	 return with the latest matching value.  if the 'latest' flag
	 is not set, then return with the matching value closest to the
	 requested time. */
      
      else
      {	 
         /* if the latest flag is set, find the latest data with matched
	    duration */
	 
	 if (varinfo.latest_flag)
	 {
	   /* set the arguments for get_total_raw_precip() */
      
           ending_time_match = LATEST_ENDINGTIME_MATCH;
           settings = REPORT_MISSING_BELOW_MIN_PERCENT | PRECIP_NO_ACCUM ;   
           advance  = 0;
	 	   	      
         }
	 else
	 {
	    /* if no "latest" flag, then find the report with a matching duration
	       whose obstime is closest to the ending time of the accumulation
	       interval */
	    
	    /* set the arguments for get_total_raw_precip() */
      
            ending_time_match = CLOSEST_ENDINGTIME_MATCH;
            settings = REPORT_MISSING_BELOW_MIN_PERCENT | PRECIP_NO_ACCUM ;   
            advance  = 0;	    
	 }
	 
	
	 if ( cppHead != (CurPP *) NULL)	 
	 {
	    total = get_total_raw_precip(NULL, (RawPP **) & cppHead,
	                                 start_time, valid_time, 
					 ending_time_match,
		                         min_percent, settings, advance, 
					 & pc_records, & pp_records);	 
	   
	    value_indicator = total.value_indicator;
            value           = total.value;	       
	    durfilled       = total.hours_covered;
	    valid_time      = total.match_time;
	 }
	 
	 if (value_indicator == OK_CHAR)
	    durfilled = datadur / SECONDS_PER_HOUR;   	 	    	      
      }
   }
   
   
   /* unsupported pe specified */
   
   else
      return;  
  
   
   /* assign the returned value */
   
   
   if (value_indicator == OK_CHAR)
   {
      if (varinfo.time_flag)
	 rawvalue->t = valid_time;
      else
	 rawvalue->f = value; 
	   
      datadur_in_hour = datadur/(double)SECONDS_PER_HOUR;
      if (durfilled < datadur_in_hour)
      {
          missing_hour = datadur_in_hour - durfilled; 
          sprintf(msgstr, 
	     "Missing %4.1f hours out of requested %4.1f hours for %s.", 
	      missing_hour, datadur_in_hour, lid);
          log_msg("", msgstr);
      }	
      
      else if (durfilled > datadur_in_hour)
      { 
          addition_hour = durfilled - datadur_in_hour;
	  sprintf(msgstr, 
	     "Found additional %4.1f hours beyond %4.1f hours for %s.",
	      addition_hour, datadur_in_hour, lid);
          log_msg("", msgstr);
      }  		 
   }
   
   else
   {
      sprintf(msgstr, "%s,%s,%d,%s", lid, varinfo.pe, varinfo.dur, ts);

      if (varinfo.time_flag)
	 log_msg(MISSING_TIMVAL, msgstr);
      else
	 log_msg(MISSING_FLTVAL, msgstr);
   }
   
   
   /* free any retrieved data */
   
   if (cpcHead)
      FreeCurPC(cpcHead);
      
   if (cppHead)
      FreeCurPP(cppHead);   
   
   return;   
}


/*********************************************************************

   get_nearest_obs()
   
   *******************************************************************/
void get_nearest_obs(Observation 	*obsHead,
		     time_t		datatime,
		     time_t		seconds_window,
		     int		time_flag,
		     values 		*rawvalue,
		     char		*dqcode)
{
   time_t	timediff;
   int          smallest_diff = MISSINGVAL;
   int		nearest_set = 0;
   Observation	*obsPtr = NULL;
   int		status;
   time_t	obs_time;
   
   
   /* initialize */
   
   nearest_set = 0;
   
   if (time_flag)
      rawvalue->t = (time_t )MISSINGVAL;
   else
      rawvalue->f = (float  )MISSINGVAL;
   
   
   /* get closest value */
   
   if (obsHead != NULL)
      obsPtr = (Observation *) ListFirst(&obsHead->list);

   while (obsPtr)	
   {
      /* get absolute difference */
      
      status = yearsec_dt_to_timet(obsPtr->obstime, &obs_time);
      timediff = abs(datatime - obs_time);
      
      
      /* make sure that the candidate value is within the 
	 given time window. if so, see if it is the closest */
      
      if (timediff <= seconds_window)
      {
	 if (nearest_set == 0 || timediff < smallest_diff)
	 {
	    if (time_flag)
	       rawvalue->t = obs_time;
	    else
	       rawvalue->f = obsPtr->value;
	    
	    smallest_diff = timediff;
	    
	    nearest_set = 1;
	 }
      }
      
      obsPtr = (Observation *) ListNext(&obsPtr->node);
   }
   
   return;
}


/*********************************************************************

   get_nearest_fcst()
   
   This function relies on the data being time sorted
   by basis time descending.
   
   Only consider forecast data with the latest basis time.
   For cases where there is a short-term forecast and a long-
   term forecast for the same lid-pedtse, hopefully the time window
   specified in the data request will allow the proper forecast 
   to be found. For example, if requesting data for day 10-14, 
   it presumes that the short term forecast will not have any 
   of its values within the requested time window, so the only
   forecast data retrieved will be from the basis time that
   covers the long-term period, even if its basis time is older.
      
   *******************************************************************/
void get_nearest_fcst(Forecast	*fcstHead,
		      time_t	datatime,
		      time_t	seconds_window,
		      int	time_flag,
		      values 	*rawvalue,
		      char	*dqcode)
{
   time_t	timediff;
   int          smallest_diff = MISSINGVAL;
   int		nearest_set = 0;
   Forecast	*fcstPtr = NULL;
   int		status;
   time_t	fcst_time;
   time_t	basis_time, latest_basis_time;
   
   
   /* initialize */
   
   nearest_set = 0;
   
   if (time_flag)
      rawvalue->t = (time_t )MISSINGVAL;
   else
      rawvalue->f = (float  )MISSINGVAL;
      
    
   /* get the value of the basis time.  values for all other
      basis times will be ignored. */
   
   if (fcstHead != NULL)
   {
      fcstPtr = (Forecast *) ListFirst(&fcstHead->list);

      status  = yearsec_dt_to_timet(fcstPtr->basistime, &latest_basis_time);
      basis_time = latest_basis_time;
   }
 
   
   /* get the value that is closest in time */
   
   while (fcstPtr && (basis_time == latest_basis_time))	
   {
      /* get absolute difference */
      
      status = yearsec_dt_to_timet(fcstPtr->validtime, &fcst_time);
      timediff = abs(datatime - fcst_time);
      
      
      /* make sure that the candidate value is within the 
	 given time window. if so, see if it is the closest */
      
      if (timediff <= seconds_window)
      {
	 if (nearest_set == 0 || timediff < smallest_diff)
	 {
	    if (time_flag)
	       rawvalue->t = fcst_time;
	    else
	       rawvalue->f = fcstPtr->value;
	    
	    smallest_diff = timediff;
	    
	    nearest_set = 1;
	 }
      }
      
      
      /* get the next item, and not its basis time for checking
	 if we are looking at a later basis time. */
      
      fcstPtr = (Forecast *) ListNext(&fcstPtr->node);
      if (fcstPtr != NULL)
         status = yearsec_dt_to_timet(fcstPtr->basistime, &basis_time);
   }
   
   
   return;
}


/*********************************************************************
   
   get_derived_obs()
      
   *******************************************************************/
void get_derived_obs(Observation 	*obsHead,
		     varinfo_struct	varinfo, 
		     values 		*rawvalue, 
		     char 		*dqcode)
{
   int		minmax_set;
   double	data_val;
   time_t	data_time;
   Observation	*obsPtr = NULL;
   time_t	obs_time, seconds_window;
   values	current_value, previous_value;
   int		status;
   
   
   /* initialize */
   
   minmax_set = 0;
   data_val   = (double )MISSINGVAL;
   data_time  = (time_t )MISSINGVAL;
   
   
   /* if getting the min value, loop on the data and find the min */
   
   if (varinfo.min_flag)
   {
      if (obsHead != NULL)
	obsPtr = (Observation *) ListFirst(&obsHead->list);

      while (obsPtr)	
      {
	 status = yearsec_dt_to_timet(obsPtr->obstime, &obs_time);
	 
	 if (!minmax_set)
	 {
	    data_val  = obsPtr->value;
	    data_time = obs_time;
	    minmax_set = 1;
	 }
	 else
	 {
	    if (obsPtr->value < data_val)
	    {
	       data_val  = obsPtr->value;
	       data_time = obs_time;
	    }
	 }
	 
	 obsPtr = (Observation *) ListNext(&obsPtr->node);
      }
   }
   
   
   /* if getting the max value */
   
   else if (varinfo.max_flag)
   {
      if (obsHead != NULL)
	obsPtr = (Observation *) ListFirst(&obsHead->list);

      while (obsPtr)	
      {
	 status = yearsec_dt_to_timet(obsPtr->obstime, &obs_time);
	 
	 if (!minmax_set)
	 {
	    data_val  = obsPtr->value;
	    data_time = obs_time;	    
	    minmax_set = 1;
	 }
	 else
	 {
	    if (obsPtr->value > data_val)
	    {
	       data_val  = obsPtr->value;
	       data_time = obs_time;	    
	    }
	 }
	 
	 obsPtr = (Observation *) ListNext(&obsPtr->node);
      }
   }
   
   
   /* if getting the change over some duration */
   
   else if (varinfo.chg_flag)
   {
      
      /* the time is explicitly specified */
      
      if (varinfo.latest_flag == 0)
      {
	 obs_time = varinfo.datatime;     
	 get_nearest_obs(obsHead, obs_time, varinfo.hr_window * 3600,
			      varinfo.time_flag, &current_value, dqcode);
	 
	 obs_time = varinfo.datatime - (varinfo.derive_numhrs * 3600);     
	 get_nearest_obs(obsHead, obs_time, varinfo.hr_window * 3600, 
			      varinfo.time_flag, &previous_value, dqcode);
      }
      
      
      /* get the latest value, then use its time to set the time for getting
	 the previous value. set up a default window for the 
	 previous value, since the user cannot specify this through
	 the template, and setting it to 0 would be too harsh */
      
      else
      {
	 status = yearsec_dt_to_timet(obsHead->obstime, &obs_time);
	 
	 if (varinfo.time_flag)
	    current_value.t = obs_time;
	 else	    
	    current_value.f = obsHead->value;
	 
	 seconds_window = 2 * 3600;	 
	 obs_time = obs_time - (varinfo.derive_numhrs * 3600);
	 
	 get_nearest_obs(obsHead, obs_time, seconds_window,
			 varinfo.time_flag,
			 &previous_value, dqcode);
      }
      
      if (varinfo.time_flag)
      {
	 if ((int )current_value.t  != MISSINGVAL &&
	     (int )previous_value.t != MISSINGVAL)
	    data_time = current_value.t;
      }
      
      else
      {
	 if (current_value.f  != MISSINGVAL &&
	     previous_value.f != MISSINGVAL)
	    data_val  = current_value.f - previous_value.f;
      }
   }
   
   
   /* return with the appropriate value type */
   
   if (varinfo.time_flag)
      rawvalue->t = data_time;
   else
      rawvalue->f = data_val;
   
   
   return;
}


/*********************************************************************
   
   get_derived_fcst()
   
   Only considers forecast data with the latest basis time.
      
   *******************************************************************/
void get_derived_fcst(Forecast 		*fcstHead,
		      varinfo_struct	varinfo, 
		      values 		*rawvalue, 
		      char 		*dqcode)
{
   int		minmax_set;
   double	data_val;
   time_t	data_time;
   Forecast	*fcstPtr = NULL;
   time_t	fcst_time;
   int		status;
   time_t	basis_time, latest_basis_time;
   
   
   /* get the value of the basis time. values for all other
      basis times will be ignored. */
   
   if (fcstHead != NULL)
	fcstPtr = (Forecast *) ListFirst(&fcstHead->list);
   status  = yearsec_dt_to_timet(fcstPtr->basistime, &latest_basis_time);
   basis_time = latest_basis_time;
   
   
   /* initialize */
   
   minmax_set = 0;
   data_val   = (double )MISSINGVAL;
   data_time  = (time_t )MISSINGVAL;
   
   
   /* if getting the min value, loop on the data and find the min */
   
   if (varinfo.min_flag)
   {
      while (fcstPtr && basis_time == latest_basis_time)	
      {
	 status = yearsec_dt_to_timet(fcstPtr->validtime, &fcst_time);
	 
	 if (!minmax_set)
	 {
	    data_val   = fcstPtr->value;
	    data_time  = fcst_time;
	    minmax_set = 1;
	 }
	 else
	 {
	    if (fcstPtr->value < data_val)
	    {
	       data_val  = fcstPtr->value;
	       data_time = fcst_time;
	    }
	 }
	 
	 fcstPtr = (Forecast *) ListNext(&fcstPtr->node);
	 status = yearsec_dt_to_timet(fcstPtr->basistime, &basis_time);
      }
   }
   
   
   /* if getting the max value */
   
   else if (varinfo.max_flag)
   {
      while (fcstPtr && basis_time == latest_basis_time)	
      {
	 status = yearsec_dt_to_timet(fcstPtr->validtime, &fcst_time);
	 
	 if (!minmax_set)
	 {
	    data_val   = fcstPtr->value;
	    data_time  = fcst_time;	    
	    minmax_set = 1;
	 }
	 else
	 {
	    if (fcstPtr->value > data_val)
	    {
	       data_val  = fcstPtr->value;
	       data_time = fcst_time;	    
	    }
	 }
	 
	 fcstPtr = (Forecast *) ListNext(&fcstPtr->node);
	 status = yearsec_dt_to_timet(fcstPtr->basistime, &basis_time);
      }
   }
         
   
   /* return with the appropriate value type */
   
   if (varinfo.time_flag)
      rawvalue->t = data_time;
   else
      rawvalue->f = data_val;
   
   
   return;
}


/*********************************************************************
   
   get_converted_value()
   Converts the value numerically according to user instructions
      
   *******************************************************************/
void get_converted_value(char		*lid,
			 varinfo_struct	varinfo,
			 values 	*rawvalue)
{
   float	flt_value;


   /* convert the value, assumed to be in MSL, to a gagezero 
      referenced value */
   
   if (varinfo.gagezero_flag)
   {
      flt_value = load_riverstat_float(lid, "ZDatum");
      if (flt_value != MISSINGVAL)
	 rawvalue->f = rawvalue->f - flt_value;
      else
      {
	 log_msg(ZDATUM_MISSING, lid);
	 rawvalue->f = (float )MISSINGVAL;
      }
   }
   
   
   /* convert the value, assumed to be a gagezero referenced value,
      to an MSL value */
   
   else if (varinfo.msl_flag)
   {
      flt_value = load_riverstat_float(lid, "ZDatum");
      if (flt_value != MISSINGVAL)
	 rawvalue->f = rawvalue->f + flt_value;
      else
      {
	 log_msg(ZDATUM_MISSING, lid);
	 rawvalue->f = (float )MISSINGVAL;
      }
   }
   
   /* convert the value to a stage value */
   
   else if (varinfo.stage_flag)
   {
      flt_value = discharge2stage(lid, rawvalue->f);
      if (flt_value != RATING_CONVERT_FAILED)
      {
	 rawvalue->f = flt_value;	 
      }
      else
      {
	 log_msg(RATING_MISSING, lid);
	 rawvalue->f = (float )MISSINGVAL;
      }
   }
   
   
   /* convert the value to a flow value */
   
   else if (varinfo.flow_flag)
   {
      flt_value = stage2discharge(lid, rawvalue->f);
      if (flt_value != RATING_CONVERT_FAILED)
	 rawvalue->f = flt_value;
      else
      {
	 log_msg(RATING_MISSING, lid);
	 rawvalue->f = (float )MISSINGVAL;
      }
   }            

   
   return;
}


/*********************************************************************

   get_PPdur_match()
   
   Assumes data are ordered from most recent to oldest
      
   *******************************************************************/
void get_PPdur_match(CurPP 	*cppHead,
		     int	req_dur,
		     time_t	req_time,
		     int	latest_flag,
		     time_t	*valid_time,
		     float 	*value, 
		     char 	*valind)
{
   CurPP 	*cppPtr = NULL;
   int		status;
   time_t	datatime;
   time_t	timediff;
   time_t	prevdiff = MISSINGVAL;
   int		first;
   
   
   /* initialize */
   
   *value      = MISSINGVAL;
   *valid_time = 0;
   *valind     = MISSING_CHAR;   
   first       = 1;
   
   
   /* just in case */
   
   if (cppHead == (CurPP *)NULL)
      return;
   
   
   /* if looking for the latest, loop until a matching duration is found. */
   
   if (latest_flag)
   { 
      cppPtr = cppHead;
      
      while (cppPtr != (CurPP *)NULL)
      {
	 
	 if (cppPtr->dur == req_dur && cppPtr->value != MISSINGVAL)
	 {
	    *value      = cppPtr->value;	    
	    status      = yearsec_dt_to_timet(cppPtr->obstime, &datatime);
	    *valid_time = datatime;	    
	    *valind     = OK_CHAR;
	    
	    break;
	 }
	 
	 cppPtr = (CurPP *) ListNext(&cppPtr->node);
      }      
   }

   
   /* if looking for the closest, loop thru all and get the closest 
      to the requested time. */
   
   else
   {
      cppPtr = cppHead;
      
      while (cppPtr != (CurPP *)NULL)
      {
	 
	 if (cppPtr->dur == req_dur && cppPtr->value != MISSINGVAL)
	 {
	    status   = yearsec_dt_to_timet(cppPtr->obstime, &datatime);
	    timediff = abs(req_time - datatime);
	    
	    if (first || timediff < prevdiff)
	    {
	       *value      = cppPtr->value;
	       *valid_time = datatime;
	       *valind     = OK_CHAR;
	       
	       prevdiff = timediff;
	       first = 0;
	    }
	    
	    
	 }
	 
	 cppPtr = (CurPP *) ListNext(&cppPtr->node);
      }
   }
   
   
   return;
}


/*********************************************************************
   translate_weather()
   
   PURPOSE
   Tries to converts a weather element value to a specified
   text string, defined in a file.
   
   *******************************************************************/
void translate_weather(float	fltvalue,
		       char	longstring[])
{ 
   typedef struct
   {
      int	code;
      char	str[70];	
   } wx_struct;
   
   static int		first = 1;
   static int		wx_cnt = 0;
   static wx_struct	*wx;
   
   char			pe_file[250];
   FILE			*file_ptr;
   int			status, linelen, numitems, i;
   char			fileline[80], copyline[80];
   char			*fgets_ptr;
   char			*sptr;
   
   int			wx_code;
   char 		wx_str[70];

   int             	len=0, rlen=0, istatus=0;
   char            	rdir_path[128];


   
   
   /* read the special translation file if it exists */
   
   if (first)
   {     
      len = strlen("rpf_template_dir");
      istatus = get_apps_defaults("rpf_template_dir", &len, rdir_path, &rlen);

      sprintf(pe_file, "%s/xw_translate.dat", rdir_path);
      
      file_ptr = fopen(pe_file, "r");
      if(file_ptr == NULL)
      {	 
	 printf("Unable to open optional file: %s\n", pe_file);
      }
      
      else
      {
	 
	 /* first get a count of the number of entries in the file.
	    them allocate the entries in the structure to store the
	    information and reset the file pointer to the
	    beginning of the file. */
	 
	 wx_cnt = 0;
	 while ((fgets_ptr = fgets(fileline, 80, file_ptr)) != NULL)
	 {
	    wx_cnt++;
	 }
	 
	 wx = (wx_struct *)malloc(sizeof(wx_struct) * wx_cnt);
	 
	 status = fsetpos(file_ptr, 0);
	 if (status != 0)
	    printf("ERROR resetting file position of xw_translate.dat\n");

	 
	 /* now read the file and extract the weather code and string
	    from each record in the file */
	 
	 printf("Reading xw_translate.dat file.\n");
	 wx_cnt = 0;
	 
	 while ((fgets_ptr = fgets(fileline, 80, file_ptr)) != NULL) 
	 {
	    linelen = strlen(fileline);
	    if (linelen <= 3 || linelen >= 80)
	       printf("ERROR in line length read from file xw_translate.dat");	    
	    
	    else
	    {
	       numitems = sscanf(fileline, "%d", &wx_code);
	       strcpy(copyline, fileline);
	       sptr = strtok(copyline, "\t, ");
	       i = strlen(copyline);
	       if (strlen(fileline) > (i + 2))
	       {
		  strcpy(wx_str, &fileline[i + 1]);
		  i = strlen(wx_str);      /*strip off the newline */
		  memset(&wx_str[i - 1], 0 , 1);
	       }
	       else
		  memset(wx_str, 0, 1);
	       	       
	       printf("code,str=%d:%s:\n", wx_code, wx_str); 
	       
	       /* load the entry with the value read in the file. 
		  allow negative numbers to be ignored. */
	       
	       if (wx_code >= 0)
	       {
		  if (numitems == 1  && strlen(wx_str) > 0)
		  {		  
		     wx[wx_cnt].code = wx_code;
		     strcpy(wx[wx_cnt].str, wx_str);
		     wx_cnt++;
		  }
		  else
		     printf("Invalid data in xw_translate.dat file: %s",
			    fileline);
	       }
	       else
		  printf("Ignoring lines with negative values: %s", fileline);
	    }
	 }
	 
	 printf("Successfully loaded %d entries.\n", wx_cnt);
	 fclose(file_ptr);
      }
      
      first = 0;
   }
   
   
   /* now check if the raw value matches any of those specified
      in the file. if so, then load it into the special longstring */
   
   for (i = 0; i < wx_cnt; i++)
   {
      if (wx[i].code == (int )fltvalue)
      {
	 strcpy(longstring, wx[i].str);
	 break;
      }
   }
   
   return;
}


/*********************************************************************
   check_if_translate()
   
   PURPOSE
   Checks to see if given pe code needs to be translated.
   
   *******************************************************************/
void check_if_translate(char 	*pe,
			float 	value,
			char 	*translation)
{ 
   static UniqueList	*ulHead = NULL;
   static int	first = 1;
   static int	ul_count;
   
   UniqueList 	*ulPtr = NULL;
   ShefPETrans	*transPtr;
   char		where[60];
   int		match_found;
   
   
   /* initialize */
   
   strcpy(translation, "");
   
   
   /* if the first time in, then load the list of PEs
      that have a translation scheme.  this is done to avoid having
      to query the table for every single value that needs to 
      be considered. */
   
   if (first)
   {
      ulHead = LoadUnique("pe", "ShefPETrans", "", &ul_count);
      
      first = 0;
   }
      
   
   /* if the data is certain snow or precip data and the amount is a trace
      amount, then show the letter T and load into the longstring
      variable, which is not formatted in the normal fashion. 
      this longstring trick is used elsewhere also. */
   
   if ((strncmp(pe, "SD", 2) == 0 ||
	strncmp(pe, "SF", 2) == 0 || 
	strncmp(pe, "SI", 2) == 0 || 
	strncmp(pe, "SW", 2) == 0 || 
	strncmp(pe, "PP", 2) == 0))
   {
      if (value > 0.00 && value < TRACE_THRESHOLD)
	 strcpy(translation, "   T");
      
      return;
   }
   
   
   /* don't bother continuing if there are no translation records. */
   
   if (ul_count <= 0) return; 
   
   
   /* now check if the pe is one of the one for which
      a translation exists. only check the first 2 bits, even
      though the field is 3 characters.  the 3 characters 
      are needed by shefpetrans to handle the very special cases
      of data for physical element IR and SR */
   
   match_found = 0;
   if (ulHead != NULL)
	ulPtr = (UniqueList *) ListFirst(&ulHead->list);

   while(ulPtr)
   {
      if (strncmp(pe, ulPtr->uchar, SHEF_PE_LEN) == 0)
      {
	 match_found = 1;
	 break;
      }
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);
   }
   
   
   /* if match found, then get the string from the table */
   
   if (match_found)
   {  
      if (strncmp(pe, "IR", 2) != 0 && strncmp(pe, "SR", 2) != 0)
      {
	 sprintf(where, " WHERE pe= '%s' AND coded_value= %d ",
		 pe, (int )value);
	 
	 transPtr = GetShefPETrans(where);
	 if (transPtr)
	 {
	    strcpy(translation, transPtr->value_trans);
	    FreeShefPETrans(transPtr);
	 }
      }
      
      
      /* value reported as either x, xx, or xxx */
      
      else if (strncmp(pe, "IR", 2) == 0)
      {
      }
      
      else if (strncmp(pe, "SR", 2) == 0)
      {
      }
      
   }
   
   
   return;
}

/***********************************************************
 unit_conversion()
 
 For value stored in rawvalue->f, change the value and convert unit
 (english unit to metric unit)  if metric_flag=1 in PE variable.
 ***********************************************************/ 
 void unit_conversion(values          *rawvalue,
		      varinfo_struct  varinfo)
 {
     
     static ShefPe	*peHead = NULL;
     static int		first = TRUE;
     ShefPe	*pePtr = NULL;
     char       where[500];
     int        i;
     
     
     /* only read in the table the first time */
     
     if (first)
     {
        sprintf(where, " ");		
        peHead = (ShefPe *)GetShefPe(where);
        first = FALSE;
	
	if (peHead == NULL)
	  fprintf(stderr, "Error - No data found in SHEFPE table for unit_conversion()\n.");
     }
     
     
     /* if derived flag for flow or stage is set, then convert and that's it */
    
     if (varinfo.flow_flag == TRUE)
     {
	rawvalue->f = 0.028317 * rawvalue->f;
	return;
     } 
             
     else if (varinfo.stage_flag == TRUE)
     {
	rawvalue->f = 0.3048 * rawvalue->f;
	return;
     }	
     
     /* check the units for the variable and apply the conversion
        that matches the PE */
     
     if (peHead != NULL)
     {
        pePtr = (ShefPe *) ListFirst(&peHead->list);	
		
	for (i=0; pePtr; i++)
	{
	  if (strcmp(pePtr->pe, varinfo.pe) == 0)
	  {
	  
	     /* for units that we know are always converted to the same unit,
	        don't need to worry about specific metirc unit converting to */
			
	     if (strcmp(pePtr->eng_unit, "ft")==0) 
		 rawvalue->f = 0.3048 * rawvalue->f ;
			     
	     else if (strcmp(pePtr->eng_unit, "mi") == 0)
		 rawvalue->f = 1.609 * rawvalue->f;
	     
	     else if (strcmp(pePtr->eng_unit, "deg F") == 0)
	         rawvalue->f = 5.0*(rawvalue->f - 32.0)/9.0;
		 
             else if (strcmp(pePtr->eng_unit, "degFdays") == 0)
	         rawvalue->f = 5.0*(rawvalue->f - 32.0)/9.0;
          
	     else if (strcmp(pePtr->eng_unit, "ft/s") == 0)
	         rawvalue->f = 0.3048 * rawvalue->f;
		 
	     else if (strcmp(pePtr->eng_unit, "g/ft3") == 0)
	         rawvalue->f = (1.0/pow(0.3048,3.0)) * rawvalue->f;
		 
	     else if (strcmp(pePtr->eng_unit, "in/day") == 0)
	         rawvalue->f = 25.4 * rawvalue->f;
		 
             else if (strcmp(pePtr->eng_unit, "k-acres") == 0)
	         rawvalue->f = 0.004047 * rawvalue->f;
		 
             else if (strcmp(pePtr->eng_unit, "kaf") == 0)
	         rawvalue->f = 1.23 * rawvalue->f;
		 
		 
	     /* note that even though Q* data comes in as kcfs, it is stored in cfs,
	        despite what the SHEFpe table says, so treat any PEs for which the
		SHEFpe table says kcfs as cfs */
		 
             else if (strcmp(pePtr->eng_unit, "cfs") == 0 || 
                      strcmp(pePtr->eng_unit, "kcfs") == 0)
	         rawvalue->f = 0.028317 * rawvalue->f;
		 
             else if (strcmp(pePtr->eng_unit, "mb") == 0)
	         rawvalue->f = 0.1 * rawvalue->f;
		 
             else if (strcmp(pePtr->eng_unit, "ppm") == 0)
	         rawvalue->f = 1.0 * rawvalue->f;	 		 		 		 		      		 		      		 


	     /* these units are converted to the different units
	        depending on the PE being processing. */
		 
             else if (strcmp(pePtr->eng_unit, "in") == 0)
	     {
	         if (strcmp(pePtr->met_unit, "cm") == 0)
	            rawvalue->f = 2.54 * rawvalue->f;
		 
                 else if (strcmp(pePtr->met_unit, "mm") == 0)
                    rawvalue->f = 25.4 * rawvalue->f;
		    
	         else
	           log_msg("","No unit conversion applied for in");
 	     }
		 
             else if (strcmp(pePtr->eng_unit, "mph") == 0)
	     { 
	         if (strcmp(pePtr->met_unit, "km/hr") == 0)
		    rawvalue->f = 1.609 *rawvalue->f;
           
	         else if (strcmp(pePtr->met_unit, "m/s") == 0)
                    rawvalue->f = 0.45 * rawvalue->f;
		    
	         else
	           log_msg("","No unit conversion applied for mph");
             }
		 
             else if (strcmp(pePtr->eng_unit, "kft") == 0)
	     {
	        if (strcmp(pePtr->met_unit, "km") == 0)
                   rawvalue->f =0.3048 * rawvalue->f;
		 
                else if (strcmp(pePtr->met_unit, "m") == 0)
                    rawvalue->f = 304.8 * rawvalue->f;
		    
	        else
	           log_msg("","No unit conversion applied for kft");
 	     }
		 
             else if (strcmp(pePtr->eng_unit, "in-HG") == 0)
	     {
	        if (strcmp(pePtr->met_unit, "k-pascal") == 0)
                   rawvalue->f = 3.386 * rawvalue->f;
		 
                else if (strcmp(pePtr->met_unit, "mm-HG") == 0)
                   rawvalue->f = 25.4 * rawvalue->f;
		 
	        else
	           log_msg("","No unit conversion applied for in-HG");
             }
			
	     else
	         log_msg("","No unit conversion applied");
		  		       	 		 		 
	     break;	    	       	
	 
	  }
	 
	    
	  pePtr = (ShefPe *) ListNext(&pePtr->node);
	  
	} /*end of for loop*/
      }/*end of if statement*/     	  			
          

   return;
}

/***********************************************************
 load_paired_value()
 
 If PV flag is on in the PE var, and the reference number after PV is 
 specified (default is 0), then look for value from pairedvalue table
 corresponding to this reference. For example, the soil temperature
 and the soil depth could be a pair, the PE var could get the
 soil temperature for different soil depth.
 ***********************************************************/ 
 void load_paired_value(values          *rawvalue,
		        varinfo_struct  varinfo)
 {
     
      static PairedValue	*pvHead = NULL;
      PairedValue	*pvPtr = NULL;
      char               where[800]="";
      char		ansi_btime[ANSI_TIME_LEN], ansi_etime[ANSI_TIME_LEN];
      time_t	        tnow, obs_time, fcst_time, basis_time, pv_time;
      char		clause[460], qcwhere[MAXLEN_QCWHERE];
      int                status;


      /* initilize */

      if (varinfo.time_flag)
         rawvalue->t = (time_t )MISSINGVAL;
      else
         rawvalue->f = (float  )MISSINGVAL;

	
      /* define the where clause */

      build_qc_where(QC_NOT_FAILED, qcwhere);


      /*get value from pairedvalue table*/

      sprintf(where, " WHERE lid = '%s' AND pe='%s' AND dur='%d' "
              "AND extremum='%s' AND ref_value='%d' AND ts='%s' AND %s ", varinfo.lid,
              varinfo.pe, varinfo.dur, varinfo.extremum, varinfo.pv_value,
	      varinfo.ts, qcwhere);
      
      
      /* get the current system time for possible later use */
   
      time(&tnow);
      
      if (varinfo.ts[0] == 'R' || varinfo.ts[0] == 'P')
      {
      
      /* if getting the latest min or max data then limit the
	 lookback window to the exact window requested.  if getting
	 the change value, then limit the retrieval based in part 
	 on the maximum lookback time considered by riverpro to avoid
	 treating very old data as latest */
      
         if (varinfo.latest_flag)
         {	 
	    /* combining min max requests with latest implies 
	    a reference time of now. */
	 
	    if (varinfo.min_flag || varinfo.max_flag)
	    {
	       obs_time = tnow - (varinfo.derive_numhrs * 3600);
	    }
	 
	 
	    /* if a change value requested */
	 
	    else if (varinfo.chg_flag)
	    {
	       set_timevals(tnow, &obs_time, &fcst_time, &basis_time);
	       obs_time = obs_time - (varinfo.derive_numhrs * 3600);
	    }
	 
	    else
	    {
	       set_timevals(tnow, &obs_time, &fcst_time, &basis_time);
	    }
	 
	 
	    status = timet_to_yearsec_ansi(obs_time, ansi_btime);      
	    sprintf(clause, " AND validtime >= '%s' ORDER BY validtime DESC ",
		    ansi_btime);
         }
         
      
	 /* if getting a value within a specific time window.
	    then use an earlier, larger window for certain derived data */

	 else
	 {
	    if (varinfo.min_flag || varinfo.max_flag || varinfo.chg_flag)
	    {
	       obs_time = varinfo.datatime - (varinfo.hr_window * 3600) -
		  (varinfo.derive_numhrs * 3600);
	       status = timet_to_yearsec_ansi(obs_time, ansi_btime);

	       obs_time = varinfo.datatime + (varinfo.hr_window * 3600);
	       status = timet_to_yearsec_ansi(obs_time, ansi_etime);

	    } 

	    else	 
	    {
	       obs_time = varinfo.datatime - (varinfo.hr_window * 3600);
	       status = timet_to_yearsec_ansi(obs_time, ansi_btime);

	       obs_time = varinfo.datatime + (varinfo.hr_window * 3600);
	       status = timet_to_yearsec_ansi(obs_time, ansi_etime);	 
	    }

	    sprintf(clause, " AND validtime >= '%s' AND validtime <= '%s' "
		    " ORDER BY validtime DESC ", ansi_btime, ansi_etime);
	 }
      
         strcat(where, clause);
      }
   
      else
      {
       
	 if (varinfo.next_flag == 1)
	 { 
	    status = timet_to_yearsec_ansi(tnow, ansi_btime);      

	    if (varinfo.min_flag == 1 || varinfo.max_flag == 1)
	       fcst_time = tnow + (varinfo.derive_numhrs * 3600);      
	    else
	       set_timevals(tnow, &obs_time, &fcst_time, &basis_time);

	    status = timet_to_yearsec_ansi(fcst_time, ansi_etime);


	    if (strcmp(varinfo.extremum, "Z") == 0)	 
	      sprintf(clause, " AND validtime >= '%s' AND validtime <= '%s' "
		      " ORDER BY basistime DESC, validtime ASC ",
		      ansi_btime, ansi_etime);

	    else
	    {
	      sprintf(clause, " AND validtime >= '%s' AND validtime <= '%s' "
	              " AND basistime = (SELECT MAX(basistime) FROM pairedvalue "
		      " WHERE lid= '%s' AND pe= '%s' AND dur= %d"
	              " AND ts= '%s' AND %s) " 	   
		      " ORDER BY basistime DESC, validtime ASC ",
		      ansi_btime, ansi_etime,
		      varinfo.lid, varinfo.pe, varinfo.dur, varinfo.ts,  qcwhere);
	    }	 
	 }
      
      
	 /* the latest flag is allowed for contingency data.
	    its data can be in the past or the future so use a window
	    that straddles the current time, and order by times
	    in descending order so the newest value is used. */

	 else if (varinfo.latest_flag == 1)
	 {
	    set_timevals(tnow, &obs_time, &fcst_time, &basis_time);      

	    status = timet_to_yearsec_ansi(obs_time,  ansi_btime);
	    status = timet_to_yearsec_ansi(fcst_time, ansi_etime);

	    sprintf(clause, " AND validtime >= '%s' AND validtime <= '%s' "
		    " ORDER BY basistime DESC, validtime DESC ",
		    ansi_btime, ansi_etime);   
	 }

      
	 /* getting a value within a specific time window.
	    use an earlier, larger window for derived data */

	 else
	 {	 
	    if (varinfo.min_flag == 1|| varinfo.max_flag == 1)	 
	    {
	       fcst_time = varinfo.datatime - (varinfo.hr_window * 3600) -
		  (varinfo.derive_numhrs * 3600);
	       status = timet_to_yearsec_ansi(fcst_time, ansi_btime);

	       fcst_time = varinfo.datatime + (varinfo.hr_window * 3600);
	       status = timet_to_yearsec_ansi(fcst_time, ansi_etime);	 
	    } 

	    else	 
	    {
	       fcst_time = varinfo.datatime - (varinfo.hr_window * 3600);
	       status = timet_to_yearsec_ansi(fcst_time, ansi_btime);

	       fcst_time = varinfo.datatime + (varinfo.hr_window * 3600);
	       status = timet_to_yearsec_ansi(fcst_time, ansi_etime);	 
	    }

	    sprintf(clause, " AND validtime >= '%s' AND validtime <= '%s' "
		    " ORDER BY basistime DESC, validtime ASC ",
		    ansi_btime, ansi_etime);
	 }
      
         strcat(where, clause);
      }
      
      pvHead = GetPairedValue(where);

      if (pvHead == (PairedValue *) NULL)
	 return;


      pvPtr = (PairedValue *) ListFirst(&pvHead->list);

      while (pvPtr)
      { 
	 if (varinfo.time_flag)
	 {
	    status = yearsec_dt_to_timet(pvPtr->validtime, &pv_time);
	    rawvalue->t = pv_time;
	    break;
	 }
	 else
	 {
	     rawvalue->f = pvPtr->value;	  
	     break;
	 }

	 pvPtr = (PairedValue *) ListNext(&pvPtr->node);
      }

      /* free any retrieved data */

      if (pvHead)
	 FreePairedValue(pvHead);

      return;
}      	 
      	     

   
