#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "get_limits.h"

/* Use these for Informix error checking; the SQLCODE
   is returned in most cases as a function argument; the
   structure is useful for identifying the ISAM errors */

#include <sqlca.h>
/* extern struct sqlca_s sqlca; */
/* extern long SQLCODE; */

 
   
/*******************************************************************
   
   get_limits()
   
   Given a location id, physical element, duration and time 
   this function returns the data limits for the qc and alert/alarm
   thresholds.  This performs the necessary checks of the two tables
   that may have the limits in them, and makes sure that the
   time range requirement is satisfied.
   
   A DataLimits record structure is used to transfer the information 
   back to the calling function.  In the returned record, note that only 
   the threshold arguments are filled out.  The calling argument
   limits_found indicates whether any data were found
     
     *****************************************************************/
DataLimits * get_limits(char 	*lid,
			char 	*pe,
			int  	dur,
			time_t	obstime,
			int	*limits_found)
   
{   
   static DataLimits	dataLimits;
   LocDataLimits	*locHead = NULL, *locPtr; 
   DataLimits	        *defHead = NULL, *defPtr;
   char			where[120];
   int                  date_within; 
   int                  loc_range_found; 
   
   
   /* initialize */
   
   *limits_found = 0;
   loc_range_found     = 0;
   memset(&dataLimits, 0, sizeof(DataLimits));
   
   dataLimits.gross_range_min  = MISSING_VAL;	 
   dataLimits.gross_range_max  = MISSING_VAL;	 
   dataLimits.reason_range_min = MISSING_VAL;	 
   dataLimits.reason_range_max = MISSING_VAL;	 
   dataLimits.roc_max          = MISSING_VAL;	 
   dataLimits.alert_upper_limit      = MISSING_VAL;	 
   dataLimits.alert_lower_limit      = MISSING_VAL;	 
   dataLimits.alert_diff_limit      = MISSING_VAL;	 
   dataLimits.alert_roc_limit  = MISSING_VAL;	 
   dataLimits.alarm_upper_limit      = MISSING_VAL;
   dataLimits.alarm_lower_limit      = MISSING_VAL;
   dataLimits.alarm_diff_limit      = MISSING_VAL;
   dataLimits.alarm_roc_limit  = MISSING_VAL; 
   
   /* build the where clause and extract data limit information,
      if available,  from the LocDataLimits table for the
      particular request */
   
   sprintf(where, " WHERE lid='%s' AND pe='%s' AND dur=%d ",
	   lid, pe, dur);
   locHead = GetLocDataLimits(where);
   
   
   /* if some entries found, then get the first record and start
      looping to find a record that matches */
   
   if (locHead != NULL)
      locPtr = (LocDataLimits *) ListFirst(&locHead->list);
   else
      locPtr = NULL;
   
   
   while (locPtr != NULL)      
   {       
      /* Check if the location matches. If so, then check the
	 remaining information to see if there is a match. */
      
      date_within = check_date_range(obstime,
				     locPtr->monthdaystart, locPtr->monthdayend);
      
      if (date_within == 1)
      {
	 loc_range_found = 1;
	 
	 
	 /* copy only the thresholds themselves into the returned record.
	    need to check for nulls always */
	 
	 if (IsNull(DOUBLE, &locPtr->gross_range_min) == NOTNULL)
	    dataLimits.gross_range_min= locPtr->gross_range_min;	 
	 
	 if (IsNull(DOUBLE, &locPtr->gross_range_max) == NOTNULL)
	    dataLimits.gross_range_max= locPtr->gross_range_max;	 
	 
	 if (IsNull(DOUBLE, &locPtr->reason_range_min) == NOTNULL)
	    dataLimits.reason_range_min= locPtr->reason_range_min;	 
	 
	 if (IsNull(DOUBLE, &locPtr->reason_range_max) == NOTNULL)
	    dataLimits.reason_range_max= locPtr->reason_range_max;	 
	 
	 if (IsNull(DOUBLE, &locPtr->roc_max) == NOTNULL)
	    dataLimits.roc_max = locPtr->roc_max;	 
	 
	 if (IsNull(DOUBLE, &locPtr->alert_upper_limit) == NOTNULL)
	    dataLimits.alert_upper_limit= locPtr->alert_upper_limit;	 
	 
	 if (IsNull(DOUBLE, &locPtr->alert_lower_limit) == NOTNULL)
	    dataLimits.alert_lower_limit= locPtr->alert_lower_limit;	 
	 
	 if (IsNull(DOUBLE, &locPtr->alert_diff_limit) == NOTNULL)
	    dataLimits.alert_diff_limit= locPtr->alert_diff_limit;	 
	 
	 if (IsNull(DOUBLE, &locPtr->alert_roc_limit) == NOTNULL)
	    dataLimits.alert_roc_limit= locPtr->alert_roc_limit;	 
	 
	 if (IsNull(DOUBLE, &locPtr->alarm_upper_limit) == NOTNULL)
	    dataLimits.alarm_upper_limit= locPtr->alarm_upper_limit;	 
	 
	 if (IsNull(DOUBLE, &locPtr->alarm_lower_limit) == NOTNULL)
	    dataLimits.alarm_lower_limit= locPtr->alarm_lower_limit;	 
	 
	 if (IsNull(DOUBLE, &locPtr->alarm_diff_limit) == NOTNULL)
	    dataLimits.alarm_diff_limit= locPtr->alarm_diff_limit;	 
	 
	 if (IsNull(DOUBLE, &locPtr->alarm_roc_limit) == NOTNULL)
	    dataLimits.alarm_roc_limit= locPtr->alarm_roc_limit;	 
	 
	 
	 *limits_found = 1;
	 	 
	 break;
      }	 	 
      
      
      /* get the next entry */
      
      locPtr = (LocDataLimits *) ListNext(&locPtr->node);         
   } 
   
   
   /* can't forget to free the memory */
   
   if (locHead != NULL)
      FreeLocDataLimits(locHead);
   
    
   /* if a location specific range is not found, then check if the
      default range is defined and process in a similar fashion */
   
   if (!loc_range_found)      
   {      
      sprintf(where, " WHERE pe='%s' AND dur=%d ", pe, dur);     
      defHead = GetDataLimits(where);
      
      if (defHead != NULL)
	 defPtr = (DataLimits *) ListFirst(&defHead->list); 
      else
         defPtr = NULL;     	 
      
      while (defPtr != NULL)	 
      {  
	 date_within = check_date_range(obstime,
					defPtr->monthdaystart,
					defPtr->monthdayend);
	 
	 if (date_within == 1)
	 {	    
	    if (IsNull(DOUBLE, &defPtr->gross_range_min) == NOTNULL)
	       dataLimits.gross_range_min= defPtr->gross_range_min;	 
	    
	    if (IsNull(DOUBLE, &defPtr->gross_range_max) == NOTNULL)
	       dataLimits.gross_range_max= defPtr->gross_range_max;	 
	    
	    if (IsNull(DOUBLE, &defPtr->reason_range_min) == NOTNULL)
	       dataLimits.reason_range_min= defPtr->reason_range_min;	 
	    
	    if (IsNull(DOUBLE, &defPtr->reason_range_max) == NOTNULL)
	       dataLimits.reason_range_max= defPtr->reason_range_max;	 
	    
	    if (IsNull(DOUBLE, &defPtr->roc_max) == NOTNULL)
	       dataLimits.roc_max = defPtr->roc_max;	 
	    
	    if (IsNull(DOUBLE, &defPtr->alert_upper_limit) == NOTNULL)
	       dataLimits.alert_upper_limit= defPtr->alert_upper_limit;	 
	    
	    if (IsNull(DOUBLE, &defPtr->alert_lower_limit) == NOTNULL)
	       dataLimits.alert_lower_limit= defPtr->alert_lower_limit;	 
	    
	    if (IsNull(DOUBLE, &defPtr->alert_diff_limit) == NOTNULL)
	       dataLimits.alert_diff_limit= defPtr->alert_diff_limit;	 
	    
	    if (IsNull(DOUBLE, &defPtr->alert_roc_limit) == NOTNULL)
	       dataLimits.alert_roc_limit= defPtr->alert_roc_limit;	 
	    
	    if (IsNull(DOUBLE, &defPtr->alarm_upper_limit) == NOTNULL)
	       dataLimits.alarm_upper_limit= defPtr->alarm_upper_limit;	 
	    
	    if (IsNull(DOUBLE, &defPtr->alarm_lower_limit) == NOTNULL)
	       dataLimits.alarm_lower_limit= defPtr->alarm_lower_limit;	 
	    
	    if (IsNull(DOUBLE, &defPtr->alarm_diff_limit) == NOTNULL)
	       dataLimits.alarm_diff_limit= defPtr->alarm_diff_limit;	 
	    
	    if (IsNull(DOUBLE, &defPtr->alarm_roc_limit) == NOTNULL)
	       dataLimits.alarm_roc_limit= defPtr->alarm_roc_limit;	 
	    
	    *limits_found = 1;
	    
	    break;
	 }
	 
	 
	 /* get the next entry */
	 
	 defPtr = (DataLimits *) ListNext(&defPtr->node);    		 
      } 
      
      if (defHead != NULL)
	 FreeDataLimits(defHead);
            
   } 
   
   
   
   /* return with the data record */

   return(&dataLimits);
   
} 


/*******************************************************************
   
  Check if the data value's time is within the 
  given day-of-the-year window.  [sql: extend(xxx, year to second)]
   
   ******************************************************************/
int  check_date_range(time_t	data_timet,
		      char *	start_ansi_monthday,
		      char *	end_ansi_monthday)
{

   int returnValue = 0;

   monthday_t data_monthday_t = timet_to_monthday_t(data_timet);
   
   monthday_t start_monthday_t = 0;
   monthday_t end_monthday_t = 0;
   
   ansi_monthday_to_monthday_t(start_ansi_monthday, &start_monthday_t);
   ansi_monthday_to_monthday_t(end_ansi_monthday, &end_monthday_t);
   
   if ((data_monthday_t >= start_monthday_t) && 
       (data_monthday_t <= end_monthday_t))
   {
       returnValue = 1;
   } 
   else
   {
       returnValue = 0;
   }
   
   return (returnValue);
}
