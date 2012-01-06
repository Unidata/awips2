/*********************************************************
   
   Prepares data for entry into the GeoArea table.
   
   ***********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "DbmsDefs.h"
#include "GeoArea.h"
#include "geo_dbutil.h"

int put_geoarea( const GeoAreaData * pGeoAreaDataNode )
{
   GeoArea	geoarea;
   int    	i, rv;
   double	lat_total, lon_total;
   double	int_lat, int_lon;
   double	pos_weight;
   double       weight_total;
   int 		status;
   const double * lon = NULL;
   const double * lat = NULL;
    
   /* if the interior lat, lon were provided from the input file, then
      use them. otherwise compute them. */
   
   if (pGeoAreaDataNode == NULL)
   {    
      fprintf(stderr, "NULL GeoArea pointer passed into put_geoarea "
                      "routine.\n" );
      status = INSERT_FAILED;

      return (status) ;
   }
   lat = pGeoAreaDataNode->lat;
   lon = pGeoAreaDataNode->lon;

   if (pGeoAreaDataNode->interior_lat != UNASSIGNED)
   {
      int_lat = pGeoAreaDataNode->interior_lat;
      int_lon = pGeoAreaDataNode->interior_lon;
   }
   
   /* compute the interior lat-lon using weighted method;
      subtract numbers to reduce magnitude out of paranoyya */
   
   else
   {
      
      lat_total = lon_total = 0;
      
      pos_weight = 0;
      weight_total = 0;

      for (i = 0; i < pGeoAreaDataNode->npts; i++)
      {
	 
	 if (i > 0)
	    pos_weight = 
	       sqrt( pow((lat[i] - lat[i-1]),2) + pow((lon[i] - lon[i-1]),2));     	 
	 else
	    pos_weight = 0;  
	 
	 weight_total += pos_weight;
	 lat_total += pos_weight * lat[i];
	 lon_total += pos_weight * lon[i];  	   
      }
      
      int_lat = (lat_total / weight_total); 
      int_lon = (lon_total / weight_total);
   }
   
   /* load the data into the structure */ 
   
   strcpy(geoarea.area_id, pGeoAreaDataNode->area_id);
   strcpy(geoarea.name, pGeoAreaDataNode->name);
   strcpy(geoarea.boundary_type, pGeoAreaDataNode->boundary_type);
   geoarea.interior_lat = int_lat;
   geoarea.interior_lon = int_lon;
   
   /*  insert the row into database */
   
   rv = PutGeoArea(&geoarea);
   
   if (rv == -239 || rv == -268)
   {
      fprintf(stderr,
	      "Error - ignoring duplicate %s data for GeoArea for id %s, %s:%d\n",
              pGeoAreaDataNode->boundary_type, pGeoAreaDataNode->area_id,
              pGeoAreaDataNode->name, pGeoAreaDataNode->npts);

      status = DUPLICATE_IGNORED;
   }
   
   else if (rv != 0)
   {
      fprintf(stderr,
              "Error %d inserting %s data to GeoArea for: %s:%s:%d\n",
              rv, pGeoAreaDataNode->boundary_type, pGeoAreaDataNode->area_id, 
              pGeoAreaDataNode->name, pGeoAreaDataNode->npts);

      status = INSERT_FAILED;
   }
   
   else
      status = INSERT_SUCCESS;
   
   
   return(status);
}
