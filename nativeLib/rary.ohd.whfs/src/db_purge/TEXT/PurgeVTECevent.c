#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <errno.h>

#include "db_purge.h"

#include "DbmsDefs.h"   /* database definitions */
#include "DbmsUtils.h"
#include "LoadUnique.h"
#include "time_convert.h"
#include "interval.h"
#include "dt.h"
#include "pgtypes_interval.h"
#include "pgtypes_timestamp.h"
#include "VTECevent.h"
#include "PurgeDynData.h"
#include "time_defs.h"

/***********************************************************************
   PurgeVTECevent.c
   
   PURPOSE
   Delete old records in the VTECevent table.  Purge products older than 
   the specified number of days, but always keep the most recent product for
   each unique combination of the geoid, office, phenomena, and significance.
   
   By keepng the latest product, this approach assumea that the latest
   product time, for the given combination, will have the highest ETN for the
   current year.
   
   ************************************************************************/
void PurgeVTECevent(int backup_use)
{
   UniqueList  *ulistHead = NULL, *ulistPtr = NULL;
   VTECevent   *eventHead = NULL;
   char 	where[400];
   char 	geoid[VTEC_GEOID_LEN + 1];
   char 	office[RFC_LEN + 1];
   char 	phenom[VTEC_PHENOM_LEN + 1], signif[VTEC_SIGNIF_LEN + 1];
   char         ** values = NULL;
   int		ul_count;
   int		status;
   time_t 	checktime, producttimet;
   char 	timestr[ANSI_TIME_LEN + 1];   
   interval     * int_day;
   char         strday[13], strdelt[22]; 
   timestamp    dtcurdate, delete_date;
   PurgeDynData *purgedyndataHead = NULL;
   int          vtec_purge_time_host, vtec_purge_time_backup;
   int          vtec_purge_time;
   
   time(&checktime);
   printf("Begin purge of VTECevent table at %s", asctime(gmtime(&checktime)));
   
   /* get purge time from PurgeDynData table for host site or backup site,
      token db_purge_backup_reention_use determine it is host site or backup site */
      
   sprintf(where, " WHERE table_name = 'vtecevent' ");
      
   purgedyndataHead = GetPurgeDynData(where);

   if (purgedyndataHead != NULL)
   {
      vtec_purge_time_host = (purgedyndataHead->num_hours_host)/HOURS_PER_DAY;
      vtec_purge_time_backup = (purgedyndataHead->num_hours_backup)/HOURS_PER_DAY;
   }   
   else
   {
      vtec_purge_time_host = VTEC_PURGE_TIME_HOST;  /* default as 30 days */
      vtec_purge_time_backup = VTEC_PURGE_TIME_BACKUP;  /* default as 14 days */
   }   
   
   if (backup_use == 0)   
      vtec_purge_time = vtec_purge_time_host;
   else if (backup_use == 1)
      vtec_purge_time = vtec_purge_time_backup;
   else
      vtec_purge_time = VTEC_PURGE_TIME_HOST;      
              
   printf("  deleting events older than %d days and which are not the most\n"
	  "  recent for a given geoid/officeid/phenom/signif combination.\n",
	  vtec_purge_time);
   
   /* determine the unique combinations in the VTECevent table,
      where a combination is defined by the geoid, office,
      phenomena, and significance code */
   
   sprintf(where, " "); 
   ulistHead = LoadUnique("geoid||office_id||phenom||signif",
			  "VTECevent", where, &ul_count);

   if (ulistHead== NULL )
   {
      printf("  no records in VTECevent table\n\n");
      return;
   }
   else
   {
      printf("  purging info for %d unique event types\n", ul_count);
   }
   
   
   /* loop on each combination and delete the old, non-latest records */
   ulistPtr = (UniqueList *) ListFirst(&ulistHead->list);
   
   while (ulistPtr)
   {
      values = ParseUnique ( ulistPtr, &ul_count );

      /* Make sure the correct number of fields were parsed from the
         load unique output.  */
      if ( ( values == NULL ) || ( ul_count < 4 ) )
      {
         printf ("  Could not parse the LoadUnique output.\n" ); 
         break;
      }

      /* extract the fields, knowing their order and size */
      memset(geoid, 0, VTEC_GEOID_LEN + 1);
      strncpy(geoid, 
	      values[0], 
	      VTEC_GEOID_LEN);
            
      memset(office, 0, RFC_LEN + 1);
      strncpy(office, 
	      values[1],
	      RFC_LEN);
      
      memset(phenom, 0, VTEC_PHENOM_LEN + 1);
      strncpy(phenom, 
	      values[2], 
	      VTEC_PHENOM_LEN);
      
      memset(signif, 0, VTEC_SIGNIF_LEN + 1);
      strncpy(signif, 
	      values[3],
	      VTEC_SIGNIF_LEN);

      FreeParseUnique ( values );
      values = NULL;
      
      /* first get the maximum time for the event combination.
	 this must be done separately and not as a subqeuery because
	 SQL won't allow subquery deletes off the same table for fear
	 of an infinite loop condition */
      
      sprintf(where, 
	      " WHERE producttime = "
	      " (SELECT MAX(producttime) from VTECevent WHERE "
	      " geoid='%s'  AND office_id = '%s' AND "
	      " phenom='%s' AND signif='%s') ", 
	      geoid, office, phenom, signif);
      
      eventHead = GetVTECevent(where);

      
      /* convert the time.  there should always be a record at this point,
	 but check anyways */
      
      if (eventHead != NULL)
      {
	 status = yearsec_dt_to_timet(eventHead->producttime, &producttimet);
	 FreeVTECevent(eventHead);
      }
      else
	 producttimet = 0;
      
      status = timet_to_yearsec_ansi(producttimet, timestr);      
           		
      
      /*----------------------------------------------------------------------*/
      /* now purge all records older than the maximum time, unless they match
	 the most recent product determined above */

/*
      sprintf(where, 
	      " WHERE geoid='%s' AND office_id = '%s' "
	      " AND phenom='%s' AND signif='%s' AND producttime < "
	      " (CURRENT YEAR TO DAY - INTERVAL (%d) DAY(5) TO DAY) "
              " AND producttime != '%s' ", 
	      geoid, office, phenom, signif, 
	      VTEC_PURGE_TIME, timestr);
*/
      
      /*  subtract vtec_purge_time number of days from current date  */
      /*  and add to WHERE clause                                    */

      PGTYPEStimestamp_current(&dtcurdate);

      sprintf(strday,"%d 00:00:00",vtec_purge_time);
      int_day = PGTYPESinterval_from_asc(strday, NULL);
      if (int_day == NULL )
      {
         printf("Error converting interval %s - errno = %d\n",strday, errno);
         return;
      }

      if (PGTYPEStimestamp_sub_interval(&dtcurdate,  int_day, &delete_date))
      {
         printf("Error subtracting interval\n");
         return;
      }

      strcpy(strdelt,PGTYPEStimestamp_to_asc(delete_date));

      sprintf(where, 
	      " WHERE geoid='%s' AND office_id = '%s' "
	      " AND phenom='%s' AND signif='%s' AND producttime < '%s'"
              " AND producttime != '%s' ", 
	      geoid, office, phenom, signif, 
	      strdelt, timestr);

      /*----------------------------------------------------------------------*/
      
      /* log a typical purge request the first time thru */
      
      printf(" For %s:%s:%s:%s: last producttime= %s\n", 
	     geoid, office, phenom, signif,timestr);
      
      
      /* now do the delete */
      
      status = DeleteVTECevent(where);
      if (status != 0) printf("  ERROR on delete, status = %d\n  where=%s",
                              status, where);
      
      
      /* loop to the next pointer */
      
      ulistPtr = (UniqueList *) ListNext(&ulistPtr->node);
   }
   
   /* free the memory */
   
   FreeUnique(ulistHead);
  
   if (purgedyndataHead != NULL)
      FreePurgeDynData(purgedyndataHead);
      
   time(&checktime);
   printf("End purge of VTECevent table at   %s", asctime(gmtime(&checktime)));
   
   
   return;
}          

