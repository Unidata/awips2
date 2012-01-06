#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "get_curobs_maxfcst.h"

#include "LoadUnique.h"
#include "RiverStatus.h"
#include "load_maxfcst.h"

#include "time_convert.h"

 
/********************************************************************
   
   get_curobs_maxfcst() 
   loads the current observed and max forecast values
   
   if the forecast for highest ranked type-source forecast
   value is in the past, then this function recomputes the
   max forecast.
   
   The fcst_hours_ahead argument is not used.
     
*********************************************************************/

void get_curobs_maxfcst(char		*lid,
			char 		*pe,
			int		obs_hours_ago,
			int		fcst_hours_ahead,
			int		fcst_basis_hours_ago,
			int		*obs_found,
			int		*fcst_found,
			struct Report	*obsReport,
			struct Report	*fcstReport)
{
   char 		where[350];
   int			cnt;
   UniqueList		*ulistHead = NULL, *ulPtr = NULL;
   RiverStatus		*rsHead = NULL, *rsPtr = NULL;
   char			use_ts[SHEF_TS_LEN + 1];
   time_t		curtimet;
   time_t		beg_validtimet, beg_basistimet;
   time_t		value_timet, validtime;
   char			ansi_validtime[ANSI_TIME_LEN];
   char			ansi_basistime[ANSI_TIME_LEN];
   char                 ** values = NULL;
   int			recompute_maxfcst;  
   time_t              best_obsvalidtime = -1;
   int                  use_tsrank_int,prev_tsrank_int = +99;
   
   /* initialize arguments */
   
   *obs_found = *fcst_found = 0;          
   
   /* define the beginning of the window for looking for the observed data,
      and the beginning of the window for the basis time of the forecast data.  */
	       
   time(&curtimet);

   beg_validtimet = curtimet - (obs_hours_ago * 3600);
   timet_to_yearsec_ansi(beg_validtimet, ansi_validtime);
   
   beg_basistimet = curtimet - (fcst_basis_hours_ago * 3600);
   timet_to_yearsec_ansi(beg_basistimet, ansi_basistime);
    
   
   /* in an effort to minimize reads of the database, get the RiverStatus
      info all at once, for all ts's and for both observed and forecast.
      There is validtime limit for observed data */
   
   sprintf(where, " WHERE lid='%s' AND pe='%s' AND "
	   " (validtime >= '%s' OR ts LIKE 'F%%') AND "
	   " (basistime IS NULL OR basistime >= '%s' )",
	   lid, pe, ansi_validtime, ansi_basistime);   	      
   
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
      /* loop on the observed entries and try and get the observed data.
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
         
      
      /* --------------------------------------------------------------------*/
      /* now loop on the ingestfilter entries again, in a similar fashion but
         this time for the best forecast */
      
      /* initialize */
      
      recompute_maxfcst =  0;
      
      
      /* loop on candidate type-sources */
      
      ulPtr = (UniqueList *) ListFirst(&ulistHead->list);
            
      while (ulPtr) 
      { 
         values = ParseUnique ( ulPtr, &cnt );

         if ( ( values == NULL ) || ( cnt < 2 ) )
         {
            /* Could not parse the unique string. */
            FreeUnique(ulistHead);
            FreeRiverStatus(rsHead);
            return;
         }
         
	 memset(use_ts, 0, SHEF_TS_LEN + 1);
	 strncpy(use_ts, values[1], SHEF_TS_LEN);

         FreeParseUnique ( values );
         values = NULL;
	 
	 if (use_ts[0] == 'F')
	 {	    
            rsPtr = (RiverStatus *) ListFirst(&rsHead->list);
	    
	    while (rsPtr)
	    {
	       if (strcmp(rsPtr->ts, use_ts) == 0)
	       {
		  
		  /* if the time of returned max fcst value is in the past,
		     then we need to recompute the max fcst information
		     and reload the recomputed value into the RiverStatus table.
		     the rule is that the RiverStatus max fcst value should always be
		     the max forecast that has a valid time after NOW, and with a basis
		     time that is not too old.  
		     
		     by allowing the earlier RiverStatus retrieval above to get old data, 
		     then possibly rejecting it because it is old or too far, this method 
		     ensures that a lesser ts-ranked value, that may otherwise be used
		     because its max is not in the past, is not necesarily used 
		     instead of a higher ranked type-source which has a forecast
		     in the future*/
		  
		  yearsec_dt_to_timet(rsPtr->validtime, &value_timet);
		  if (value_timet < curtimet)
		  {
		     recompute_maxfcst  = 1;
		  }
		  		  
		  else
		  {
		     strcpy(fcstReport->pe,       rsPtr->pe);
		     fcstReport->dur            = rsPtr->dur;
		     strcpy(fcstReport->ts,       rsPtr->ts);
		     strcpy(fcstReport->extremum, rsPtr->extremum);
		     fcstReport->value          = rsPtr->value;
		     yearsec_dt_to_timet(rsPtr->validtime, &fcstReport->validtime);
		     yearsec_dt_to_timet(rsPtr->basistime, &fcstReport->basistime);
		     
		     *fcst_found = 1;		  
		  }
		  
		  break;
	       }  /* end of if check on type-source match */
	       
	       rsPtr = (RiverStatus *) ListNext(&rsPtr->node);
	       
	    }  /* end of while check on whether any more riverstat entries */
	 }     /* end of if check on forecast type-sources */
	 
	 
	 /* don't continue with the next type-source if value is found or 
	    if have to recompute maxfcst info */
	 
	 if ((*fcst_found) || recompute_maxfcst)
	    break;
	 else
	    ulPtr = (UniqueList *) ListNext(&ulPtr->node);
	    
      }  /* end of while check on ulPtr */
      
      
      /* if the max forecast data needs to be recomputed because
	 the max forecast value is in the past, then recompute them.
	 then, loop on all the forecast ts entries again.  we can't just loop
	 on the one ts that had the old max fcst because it is possible
	 that a better choice forecast ts (i.e. higher ts_rank)
	 also had an old max fcst, and we should pick that value up
	 if it is updated. also note that the way the recompute flag is
	 defined, that if no forecast data exists for a location, it won't
	 bother trying to recompute it, since there should be nothing
	 to recompute; thereby this algorithm doesn't waste energy
	 looking for data that isn't there. */
      
      if (recompute_maxfcst)
      {
	 /* recompute them accordingly */
	 
	 printf("recomputing maxfcst since time < NOW; lid, pe=%s %s\n", lid, pe);
	 
	 if (pe[0] == 'H')
	    load_max_fcstdata_lidpe("FcstHeight",    lid, pe);
	 
	 else if (pe[0] == 'Q')
	    load_max_fcstdata_lidpe("FcstDischarge", lid, pe);
	 
	 
	 /* now that the data are recomputed, get the value in a 
	    similar fashion as before */
	 
	 FreeRiverStatus(rsHead);
	 
	 sprintf(where, " WHERE lid='%s' AND pe='%s' AND ts='%s' "
		 " AND validtime >= CURRENT_TIMESTAMP  "
		 " AND basistime >= '%s'  ",
		 lid, pe, use_ts, ansi_basistime);
	 
	 rsHead = GetRiverStatus(where);
	 
	 
	 /* find the best value, considering the different type-sources */
	 
	 ulPtr = (UniqueList *) ListFirst(&ulistHead->list);
	 
	 while (rsHead && ulPtr && !(*fcst_found)) 
	 {
            values = ParseUnique ( ulPtr, &cnt );

            if ( ( values == NULL ) || ( cnt < 2 ) )
            {
               /* Could not parse the unique string. */
               FreeUnique(ulistHead);
               FreeRiverStatus(rsHead);
               return;
            }

	    memset(use_ts, 0, SHEF_TS_LEN + 1);
	    strncpy(use_ts, values[1], SHEF_TS_LEN);
            FreeParseUnique ( values );
            values = NULL;
	    
	    if (use_ts[0] == 'F')
	    {	       	       
	       rsPtr = (RiverStatus *) ListFirst(&rsHead->list);
	       
	       while (rsPtr && !(*fcst_found))
	       {
		  if (strcmp(rsPtr->ts, use_ts) == 0)
		  {
		     strcpy(fcstReport->pe,       rsPtr->pe);
		     fcstReport->dur            = rsPtr->dur;
		     strcpy(fcstReport->ts,       rsPtr->ts);
		     strcpy(fcstReport->extremum, rsPtr->extremum);
		     fcstReport->value     = rsPtr->value;
		     yearsec_dt_to_timet(rsPtr->validtime, &fcstReport->validtime);
		     yearsec_dt_to_timet(rsPtr->basistime, &fcstReport->basistime);
		     
		     *fcst_found = 1;		     
		  }
		  
		  rsPtr = (RiverStatus *) ListNext(&rsPtr->node);
	       }       
	    }  /* end of if check on whether forecast type-source */ 
	    
	    ulPtr = (UniqueList *) ListNext(&ulPtr->node);
	 }
	 
	 
      }  /* end of if recompute maxfcst */
      
      
      /* finally free the lists */
      
      FreeUnique(ulistHead);
      FreeRiverStatus(rsHead);
      
   }  /* end of if check on data to process */
   
   
   return;
}

