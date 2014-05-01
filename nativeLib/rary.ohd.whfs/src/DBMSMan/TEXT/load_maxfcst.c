/*****************************************************************
   
   Contains functions related to loading the max fcst
   info into the RiverStatus table. 
  
   
   calling tree:
   
   load_maxfcst
   -->load_max_fcstdata[_lidpe]
   ----->load_maxfcst_item
   -------->(calls bldts_fcstriv)
   -------->(calls find_maxfcst)
   -------->load_riverstatus
   
   ******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <datetime.h>
#include <string.h>
#include <time.h>


#include <sqlca.h>

#include "load_maxfcst.h"

#include "Riverstat.h"
#include "RiverStatus.h"
#include "LoadUnique.h"

#include "Report.h"

#include "DbmsDefs.h"

#include "time_convert.h"
#include "bldts_height.h"


/* sql return structure, useful for detecting number of rows updated */

/* extern struct sqlca_s sqlca; */


/****************************************************************
   
   Load the maximum forecast data.
   Loop on the forecast data available and process each location
   and each pe,ts that has forecast height or discharge data.
   Ignore all probability coded data.
   Then load the RiverStatus table with the maximum info.
   
   This function returns nothing; its effect is in its actions
   on the database.
   
   **************************************************************/

void load_maxfcst()
{
   
   /* process the two table's data */
   
   load_max_fcstdata("FcstHeight");
   
   load_max_fcstdata("FcstDischarge");
   
   return;
}


/****************************************************************
   
   Process forecast data for the given tablename.
   Don't consider any probabilistic values.
   
   **************************************************************/

void load_max_fcstdata(char *tablename)
{
   UniqueList	*ulistPtr = NULL , *ulPtr = NULL ;
   char 	lid[LOC_ID_LEN + 1];
   char		pe[SHEF_PE_LEN + 1], ts[SHEF_TS_LEN + 1];
   char         ** values = NULL;
   char		where[80];
   int 		cnt;
   
   
   /* loop on all the location, pe, and ts entries
      for forecast data in the future and for the
      appropriate table */
   
   sprintf(where, " where validtime > CURRENT_TIMESTAMP AND "
                  " probability < 0.0 "); 
   
   ulistPtr = LoadUnique("lid||pe||ts", tablename, where, &cnt);

   if ( ulistPtr == NULL )
   {
      return ;
   }

   ulPtr = (UniqueList *) ListFirst(&ulistPtr->list);
   
   while (ulPtr)
   {     
      /* extract the id, pe,and ts code from the unique string. */
      values = ParseUnique ( ulPtr, &cnt );

      if ( ( values == NULL ) || ( cnt < 3 ) )
      {
          /* An error was ecountered parsing the unique string. */
          break;
      }

      memset(lid, 0, LOC_ID_LEN + 1);
      strncpy(lid, values[0], LOC_ID_LEN);
      
      memset(pe, 0, SHEF_PE_LEN + 1);
      strncpy(pe, values[1], SHEF_PE_LEN);
      
      memset(ts, 0, SHEF_TS_LEN + 1);
      strncpy(ts, values[2], SHEF_TS_LEN);

      FreeParseUnique ( values );
      
      /* perform the load_maxfcst operations for this
	 lid, pe, ts */
      
      load_maxfcst_item(lid, pe, ts);
        
      
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);
   }
   
   
   FreeUnique(ulistPtr);   
   
   return;
}


/****************************************************************
   
   Process forecast data for the given tablename.
   
   **************************************************************/

void load_max_fcstdata_lidpe(char 	*tablename,
			     char	*lid,
			     char	*pe)
{
   UniqueList	*ulistPtr = NULL , *ulPtr = NULL ;
   char 	ts[SHEF_TS_LEN + 1];
   char		where[160];
   int 		cnt;
   
   
   /* loop on all the ts entries for forecast data in the
      future and for the appropriate table */
   
   sprintf(where, " WHERE lid='%s' AND pe='%s' AND validtime > CURRENT_TIMESTAMP AND "
                  " probability < 0.0 ", lid, pe); 

   
   ulistPtr = LoadUnique("ts", tablename, where, &cnt);   

   if ( ulistPtr == NULL )
   {
      return ;
   }

   ulPtr = (UniqueList *) ListFirst(&ulistPtr->list);
   
   while (ulPtr)
   {     
      /* extract the ts code from the unique string. */
            
      memset(ts, 0, SHEF_TS_LEN + 1);
      strncpy(ts, ulPtr->uchar, SHEF_TS_LEN);
      
      
      /* perform the load_maxfcst operations for this
	 lid, pe, ts */
      
      load_maxfcst_item(lid, pe, ts);
       
      
      /* loop to the next ts entry if there is one. */
      
      ulPtr = (UniqueList *) ListNext(&ulPtr->node);
   }
   
   
   FreeUnique(ulistPtr);   
   
   return;
}

/****************************************************************
   
   Loads the max fcst info into the RiverStatus table for the
   current location and pe.
   
   **************************************************************/
void load_maxfcst_item(char 	*lid, 
		       char	*pe,
		       char	*ts)
{
   Riverstat	*rstationPtr;
   Report	*fcst_ts;
   Report	max_fcst_record;
   int		fcst_ts_cnt;
   int		ret;
   int		use_latest, qc_filter;
   char		where[80];
   time_t	obs_btime, fcst_endvalidtimet, basis_begintimet; 
   time_t	curtimet;
   
   
   /* get the setting for the use_latest_fcst field
      for the current location from the riverstat table. */
   
   sprintf(where, " where lid='%s' ", lid);
   rstationPtr = GetRiverstat(where);
   
   if (rstationPtr == NULL)
      use_latest = 1;
   
   else
   {
      if (strcmp(rstationPtr->use_latest_fcst, "F") == 0)
	 use_latest = 0;
      else
	 use_latest = 1;
      FreeRiverstat(rstationPtr);
   }
   
   
   /* get the forecast time series for this location, pe, and ts
      using any instructions on any type-source to screen and
      whether to use only the latest basis time  */
   
   qc_filter = 1;
   
   time(&curtimet);
   set_timevals(curtimet,
                &obs_btime, &fcst_endvalidtimet, &basis_begintimet);
   
   bldts_fcstriv(lid, pe, ts, qc_filter, use_latest, basis_begintimet,
		 &fcst_ts, &fcst_ts_cnt);
   
   
   /* find the data for this location, pe, and ts given the
      forecast time-series and the count of values in it.	  
      if data found, determine its max and load the value */
   
   if (fcst_ts_cnt > 0)
   {
      find_maxfcst(fcst_ts, fcst_ts_cnt, &max_fcst_record);
      
      
      /* load the maximum data into the RiverStatus table. */
      
      /* fprintf(stderr, "Loading riverstatus for: %s %s %s\n",
	 lid, pe, ts); */
      
      load_riverstatus(lid, max_fcst_record);
      
      
      /* free the forecast time series  */
      
      free(fcst_ts);
   }
   
   
   /* if no data were found, then delete any entries that
      may exist for this key.  this is needed if general 
      applications are using this function directly and
      delete all forecast data for a given key */
   
   else
   {
      sprintf(where, " where lid='%s' and pe='%s' and ts='%s' ",
	      lid, pe, ts);
      ret = DeleteRiverStatus(where);
   }
   
   return;
}

/****************************************************************
   
   Loads the max fcst info into the RiverStatus table for the
   current location and pe.
   
   **************************************************************/

void load_riverstatus(char	*lid,
		      Report	max_fcst)
{
   
   RiverStatus	rstatus;
   char 	where[80];
   int		ret;
   
   
   /* update the value in the RiverStatus table.  if no record
      currently in the table, then insert/put a new record.
      first build the record to update/insert */
   
   strcpy(rstatus.lid,       lid);
   strcpy(rstatus.pe,        max_fcst.pe);	 
   strcpy(rstatus.ts,        max_fcst.ts);
   
   rstatus.dur =             max_fcst.dur;
   strcpy(rstatus.extremum,  max_fcst.extremum);
   rstatus.probability =     max_fcst.probability;
   
   ret = timet_to_yearsec_dt(max_fcst.validtime, &rstatus.validtime);
   ret = timet_to_yearsec_dt(max_fcst.basistime, &rstatus.basistime);
   rstatus.value =           max_fcst.value;
   
   
   /* try an update */
   
   sprintf(where, " WHERE lid='%s' AND pe='%s' AND ts='%s' ",
	   lid, max_fcst.pe, max_fcst.ts);

   ret = UpdateRiverStatus(&rstatus, where);

   
   /* update the record if number of records uupdated is zero */
   
   if (ret < 0 || sqlca.sqlerrd[2] == 0)
   {
      ret = PutRiverStatus(&rstatus);
      if (ret < 0)
	 fprintf(stderr,
		 "Error %d in PutRiverStatus - lid,pe,ts= %s %s %s\n",
		 ret, lid, max_fcst.pe, max_fcst.ts);
   }
   
   
   return;
}


