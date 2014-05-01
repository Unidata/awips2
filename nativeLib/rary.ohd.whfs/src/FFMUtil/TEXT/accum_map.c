/*******************************************************************
   
   accum_map.c
   
   Compute accumulated MAP data for those areas matching
   the specified area type, and for the specified time period,
   and using the specified data source.
   
   *****************************************************************/   



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <sqltypes.h>

#include "GeoArea.h"
#include "ProcValue.h"


#include "DbmsAccess.h"
#include "DbmsDefs.h"
#include "time_convert.h"

#include "ArealProductSettings.h"
#include "read_radargrids.h"

#include "FfmUtils.h"

#define MISSING_ACCUMVAL -99.



/**********************************************************************
   
   pass in the number of hours to keep data, and the number of
   hours ago to serve as the obs, or ending time.
   
   *******************************************************************/
ArealData * accum_map(ArealProductTypeDescriptor	prod_descr,
		      ArealProductSpecifier		prod_spec,
		      float				min_duration_filled,
		      int				*status,
		      long				*precip_cnt)
{
   time_t	begintime, endtime;
   GeoArea	*gaHead = NULL, *gaPtr = NULL;
   char 	where[140];
   int		area_cnt;
   int 		ret;
   int		hrs_filled;
   float 	duration_filled;
   float 	accumval;
   char		boundary_type[40];
   ArealData	*areal_data;
   
   
   /* initialize */
   
   *status     = -1;
   *precip_cnt = 0;
      
   
   /* define a convenient local var and 
      set the start time for the query */
   
   endtime   = prod_spec.endTime;   
   begintime = endtime - prod_spec.duration;
   
   
   /* get the list of map areas that match the boundary type */
   
   set_boundary_string(prod_descr.resolutionLevel, boundary_type);
   
   sprintf(where, " WHERE boundary_type = '%s' ORDER BY area_id ",
	   boundary_type);       
   gaHead = GetGeoArea(where);
   
   
   /* malloc memory for the returned array */
   
   if (gaHead == NULL)
      return(NULL);
   
   else
   {
      area_cnt = ListCount(&gaHead->list);   
      *status = 1;
      
      areal_data = (ArealData *)malloc(sizeof(ArealData) * area_cnt);
      if (areal_data == NULL)
      {
	 printf("Error in malloc of areal_data in accum_map().\n");
	 return(NULL);
      }
   }
   
   
   /* loop on the areas and process each */
   
   area_cnt = 0;
   
   gaPtr = (GeoArea *) ListFirst(&gaHead->list); 
   while(gaPtr)
   {      
      
      /* process the data for each area */	
      
      ret = do_map_area(gaPtr->area_id, prod_descr.precipType,
			begintime, endtime,
			&accumval, &hrs_filled);
            
      
      /* determine if enough valid data were found to use the
	 total value. if so then load the array to return */
      
      duration_filled = (float )(hrs_filled * SECONDS_PER_HOUR) / 
	 (float )(endtime - begintime);
      
      /*
      printf("%s: accumval %f, hrs_filled %d, percent %f\n", 
	     gaPtr->area_id, accumval, hrs_filled, duration_filled);
      */
      
      if (duration_filled >= min_duration_filled)
      {
	 strcpy(areal_data[area_cnt].lid, gaPtr->area_id); 
	 areal_data[area_cnt].value     = accumval;
	 areal_data[area_cnt].validtime = endtime;
	 areal_data[area_cnt].isOk      = 1;
	 
      }
      
      else
      {
	 strcpy(areal_data[area_cnt].lid, gaPtr->area_id); 
	 areal_data[area_cnt].value     = -1.0;
	 areal_data[area_cnt].validtime = 0;	 
	 areal_data[area_cnt].isOk      = 0;
      }
      
      area_cnt++;
      
      
      /* process the next area in the list */
      
      gaPtr = (GeoArea *) ListNext(&gaPtr->node);
   }
   
   FreeGeoArea(gaHead);
   
   
   /* return with values */
   
   *precip_cnt = area_cnt;
   return(areal_data);   
}   


/**********************************************************************
   
   do_map_area()
   
   process the MAP based data for this station
   find the accumulations for certain hours 
   
   *******************************************************************/

int do_map_area(char 		*lid,
		PrecipType	precipType,
		time_t		begintime,
		time_t		endtime,
		float		*accumval,
		int		*hrs_filled)
{  
   ProcValue	*procHead = NULL;
   int 		status;
   char		beginstr[ANSI_YEARSEC_TIME_LEN + 1];
   char		endstr[ANSI_YEARSEC_TIME_LEN + 1];
   char 	where[180];
   char		typesrc[SHEF_TS_LEN + 1];   
   int		shef_hourdur;
   
   
   /* initialize */
   
   *accumval = MISSING_ACCUMVAL;
   *hrs_filled = 0;
   
   
   /* load the hourly map data for the given type source.
      build the where clause for the data query */
   
   status = timet_to_yearsec_ansi(begintime, beginstr);
   status = timet_to_yearsec_ansi(endtime,   endstr);
   
   
   /* set the type source code depending on the grid type used */
   
   status = cvt_preciptype_to_ts(precipType, typesrc);
      
   status = timet_to_shefdur(SECONDS_PER_HOUR, &shef_hourdur);

   
   sprintf(where, " WHERE lid = '%s' AND pe = 'PP' AND "
	   " dur = %d AND ts = '%s' AND "
           " obstime > '%s' AND obstime <= '%s' "
	   " ORDER BY obstime DESC",
	   lid, shef_hourdur, typesrc, beginstr, endstr); 
   
   procHead = GetProcValue(where);
   
   if (procHead == NULL)
      return(-1);
   
   else
   {
      /* now get the value and the hours filled with
	 data within the given duration */
      
      total_map_data(begintime, endtime, procHead,
		     accumval, hrs_filled);
      
      
      /* free the data */
      
      FreeProcValue(procHead);
   }
   
   return(0);  
}   


/**********************************************************************

   total_map_data()
   
   compute the total map for the specified duration using
   the hourly data.  
   
   *******************************************************************/

void total_map_data(time_t 		begintime,
		    time_t 		endtime,
		    ProcValue 		*procHead,
		    float		*value,
		    int			*hrs_filled)
{
   
   ProcValue	*procPtr = NULL;   
   float	total;
   int		status;
   time_t	datatime;
   time_t	secs_filled;
      
     
   /* initialize */
   
   total       = MISSING_ACCUMVAL;
   secs_filled = 0;
   
    
   /* check for null input */
   
   if (procHead == NULL)
      return;
   
   
   /* process the data */
   
   procPtr =  (ProcValue *) ListFirst(&procHead->list);
   
   
   if (procPtr)
   { 
      status =  yearsec_dt_to_timet(procPtr->obstime, &datatime);
      
      while (procPtr && 
	     (datatime > begintime) && (datatime <= endtime))
      {
	 
	 /* total the data if valid data */
	 
	 if (procPtr->value >= 0.0)
	 {	    	    
	    if (total == MISSING_ACCUMVAL)
	       total  = procPtr->value;
	    else
	       total += procPtr->value;
	    
	    secs_filled += SECONDS_PER_HOUR;	       	       
	 }
	 
	 
	 /* get the next record */
	 
	 procPtr = (ProcValue *) ListNext(&procPtr->node);
	 
	 status =  yearsec_dt_to_timet(procPtr->obstime, &datatime);
      }   
   }
   
   
   /*  determine amount of duration that is covered */
   
   if (total != MISSING_ACCUMVAL)
      *hrs_filled = (secs_filled / SECONDS_PER_HOUR);       
   
   else
      *hrs_filled = 0;
   
   
   *value = total;
   
   return;
}
