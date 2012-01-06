#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "time_convert.h"
#include "time_defs.h"

#include "ContingencyValue.h"
#include "GeoArea.h"

#include "DbmsDefs.h"

#include "ArealProductSettings.h"

/*******************************************************************
   bld_ffg_area.c	
   
   PURPOSE
   Builds a consistent set of ffg data for a given area type
   (zone, county, basin) by using ffg data since a specified
   ending time for the specified boundary type and duration.
   
   
   *****************************************************************/

ArealData * bld_ffg_area(char 	*boundary_type,
			 int	ffg_hours,
			 time_t	ffgtime,
			 int	since_flag,
			 int	*ffg_status,
			 long	*ffg_cnt)
{
   char		where[260];
   time_t 	durtime;
   time_t	valid_timet;
   char		ansi_ffgtime[ANSI_YEARSEC_TIME_LEN + 1];
   int		status, i, area_cnt;
   int		numitems;
   int		shefdur;
   ContingencyValue	*cvHead = NULL, *cvPtr = NULL;
   GeoArea		*gaHead = NULL, *gaPtr = NULL;
   ArealData	*ffg_data = NULL;
   
   
   /* initialize returned values */
   
   *ffg_cnt    = 0;
   *ffg_status = -1;
   
    
   /* convert the times for the query */
   
   status = timet_to_yearsec_ansi(ffgtime, ansi_ffgtime);
   if (status < 0) 
   {
      fprintf(stderr,
	      "Error converting ffgtime in bld_ffg_area(); status = %d\n",
	      status);
      return(NULL);
   }
   
   durtime = SECONDS_PER_HOUR * ffg_hours;
   status = timet_to_shefdur(durtime, &shefdur);
   if (status < 0) 
   {
      fprintf(stderr,
	      "Error converting duration in bld_ffg_area(); status = %d\n",
	      status);
      return(NULL);
   }
   
   
   /* get the list of areas matching the specified type */
   
   sprintf(where, " WHERE boundary_type = UPPER ( '%s' ) ORDER BY area_id ",
	   boundary_type);       
   gaHead = GetGeoArea(where);
   
      
   /* allocate the array of structures to hold the returned data */
   
   if (gaHead == NULL)
      return(NULL);
   
   else
   {
      numitems = ListCount(&gaHead->list);
      
      ffg_data = (ArealData *)malloc(sizeof(ArealData) * numitems);
      if (ffg_data == NULL)
      {
	 fprintf(stderr, "Error in malloc of ffg_data in bld_ffg_data()\n");
	 return(NULL);
      }
   }
   
   
   /* if at this point, then assume status is ok, even
      though data may be missing */
   
   *ffg_status = 1;
   area_cnt = 0;
   
   
   /* loop on each the areas */
   
   gaPtr = (GeoArea *) ListFirst(&gaHead->list); 
   for (i = 0; gaPtr; i++)
   {  
      /* get the records matching the specified criteria. 
	 the since_flag controls whether an exact match
	 be time is made or whether it just takes the latest */
      
      if (since_flag)
	 sprintf(where,
		 " WHERE pe = 'PP' AND ts = 'CF' AND "
		 " validtime >= '%s' AND lid = '%s' AND "
		 " dur = %d ORDER BY validtime DESC ",
		 ansi_ffgtime, gaPtr->area_id, shefdur);
      else
	 sprintf(where,
		 " WHERE pe = 'PP' AND ts = 'CF' AND "
		 " validtime = '%s' AND lid = '%s' AND "
		 " dur = %d",
		 ansi_ffgtime, gaPtr->area_id, shefdur);
	 
      
      cvHead = GetContingencyValue(where);
      
      
      /* now use the first (i.e. most recent) record found for
	 this station */
            
      if (cvHead)
      {
         cvPtr = (ContingencyValue *) ListFirst(&cvHead->list); 

	 strcpy(ffg_data[area_cnt].lid, cvPtr->lid);
	 ffg_data[area_cnt].value = cvPtr->value;
	 
	 status = yearsec_dt_to_timet(cvPtr->validtime, &valid_timet);	 
	 ffg_data[area_cnt].validtime = valid_timet;
	 
	 ffg_data[area_cnt].isOk  = 1;
	 
	 FreeContingencyValue(cvHead);
      }
      
      else
      {
	 strcpy(ffg_data[area_cnt].lid, gaPtr->area_id);
	 ffg_data[area_cnt].value     = -1.0;
	 ffg_data[area_cnt].validtime = 0;
	 ffg_data[area_cnt].isOk      = 0;	 
      }
      
      area_cnt++;
      
      
      /* process the next area in the list */
      
      gaPtr = (GeoArea *) ListNext(&gaPtr->node);
   }
   
   if (gaHead) FreeGeoArea(gaHead);
   
   
   /* return with the data */
   
   *ffg_cnt = area_cnt;
   return(ffg_data);
}

