#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

#include "DbmsUtils.h"
#include "time_convert.h"
#include "AlertAlarmVal.h"
#include "DbmsDefs.h"

#include "alert_util.h"

/* use these for Informix error checking; the SQLCODE
   is returned in most cases as a function argument; the
   structure is useful for identifying the ISAM errors */

#include <sqlca.h>
/*
extern struct sqlca_s sqlca;
extern long SQLCODE;
*/


/***************************************************************
   purge_fcst_alerts()
   
   Deletes alerts AND alarms of "old" forecast data, based on the 
   forecast basis time.  Specifically, this function deletes all forecast
   data, for each unique combination of lid-pe-dur-ts, that does not 
   match the LATEST basis time for the same combination of data.
   For each call to this function, the function purges data for
   only those alert/alarms that match the specified aa_check.
   
   All alert and alarms have a defined: 
   aa_categ (alert, alarm) aa_check (value, roc).
   
   This method handles deletes of the multiple basis times that 
   may be in the alert-alarm table (for a given combination), but
   also makes sure the the basis time remaining is the latest one
   in comparison to what is in the appropriate master forecast
   physical element table (i.e. Fcstheight, Fcstprecip, etc.) 
      
   Return status < 0 if there is an error, otherwise the return
   value gives the number of delete requests.
   If error, then details described in returned err message.
   The calling program must allocate enough space for the
   error message - 160 bytes should be more than enough.
   
   **************************************************************/

int purge_fcst_alerts(char *aa_check,
		      char *err_msg)
{
   char			where[240];
   AlertAlarmVal 	*aaHead, *aaPtr;
   int			status;
   int			cnt;
   char			tablename[40];
   
   char			prev_lid[LOC_ID_LEN + 1] = "";
   char			prev_pe[SHEF_PE_LEN + 1] = "";
   int			prev_dur = -1.0;
   char			prev_ts[SHEF_TS_LEN + 1] = "";
      
      
   /* initialize the returned error message */
   
   strcpy(err_msg, "");
   
     
   /* retrieve all the forecast data matching the specified check,
      order the data by the key except for the aa_categ and aa_check,
      and extremum and probability.  The algorithm assumes the data are
      ordered as basistime descending */
   
   sprintf(where, " WHERE ts LIKE 'F%%' AND aa_check = '%s' "
	          " ORDER BY lid, pe, dur, ts, basistime DESC ",
		  aa_check);  
   aaHead = GetAlertAlarmVal(where);
   
   
   /* only continue if there is data to process.
      this check for Null fixed a shefdecode bug found in April, 2003 */
      
   if (aaHead != NULL)
      cnt = ListCount(&aaHead->list);
   else
      return(0);
      
      
   /* initialize */
      
   cnt = 0;
     
         
   /* loop thru all the entries and for each unique lid-pe-dur-ts, 
      ignoring any extremum and probability uniqueness, and 
      delete any alert/alarm data that does not match the 
      latest basis time for the lid-pe-dur-ts in the appropriate
      table */
         
   aaPtr = (AlertAlarmVal *) ListFirst(&aaHead->list);
   while(aaPtr)
   {      
      
      /* if a new lid-pe-dur-ts, then these record(s) have not
         been processed, so go to it.  */ 
      
      if (strcmp(aaPtr->lid, prev_lid) != 0 ||
	  strcmp(aaPtr->pe,  prev_pe)  != 0 ||
	  aaPtr->dur !=      prev_dur       ||
	  strcmp(aaPtr->ts,  prev_ts)  != 0)
      {
         /* get the name of the forecast table that is the master 
            source for all the forecast data for this combination. */
	    
         getTableName(aaPtr->pe,  aaPtr->ts, tablename);
	 
	 
	 /* use a subquery to control what is deleted. */
	 
	 sprintf(where,
		 " WHERE lid= '%s' AND pe= '%s' AND dur= %d AND ts= '%s' AND "
		 " basistime != (SELECT MAX(basistime) FROM %s "
		 " WHERE lid= '%s' AND pe= '%s' AND dur= %d AND ts= '%s') ",
		 aaPtr->lid, aaPtr->pe, aaPtr->dur, aaPtr->ts, tablename,
                 aaPtr->lid, aaPtr->pe, aaPtr->dur, aaPtr->ts);
	 status = DeleteAlertAlarmVal(where);
	 
	 
	 /* if error deleting then set err msg argument 
	    and return immediately */
	 
	 if (status < 0)
	 {
	    sprintf(err_msg,
		    "Informix error %d/%ld deleting from AlertAlarm "
		    "for %s,%s,%d,%s",
		    status, sqlca.sqlerrd[1],
		    aaPtr->lid, aaPtr->pe, aaPtr->dur, aaPtr->ts);
	    return(-1);
	 }
	 
	 else
	    cnt++;
   	
      
         /* since we processed this group, store the info on the 
            group so that it is skipped if there are more records
	    for it in this loop. */
	 
         strcpy(prev_lid, aaPtr->lid);
         strcpy(prev_pe,  aaPtr->pe);
         prev_dur =       aaPtr->dur;
         strcpy(prev_ts,  aaPtr->ts);
      }
      
      
      /* get the next record */
      
      aaPtr = (AlertAlarmVal *) ListNext(&aaPtr->node); 
   }
   
   
   /* return with the count of combinations for which data
      were deleted. */
   
   return(cnt);
} 
