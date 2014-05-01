
/*******************************************************************************
* FILENAME:            get_geoareainfo.h
* DESCRIPTION:         Contains prototypes and user defined types for the
*                      get_geoareainfo routine.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       May 2, 2003
* ORGANIZATION:        OHD / HSEB
* MACHINES:            HP-UX / Redhat Linux
*
* MODIFICATION HISTORY:
*    DATE               PROGRAMMER                 DESCRIPTION/REASON
*    May 2, 2003        Bryon Lawrence             Original Coding    
********************************************************************************
*/

#ifndef GET_GEOAREAINFO_H
#define GET_GEOAREAINFO_H

#include "DbmsDefs.h"

/* Hopefully, we can set up dbgen to automatically generate manifest
   constants for the lengths of character and string fields.  This
   would make WHFS life better. For now, these pound defines are here.*/
#define GEOAREA_ID_LEN 9
#define GEOAREA_NAME_LEN 41

/* Handy constants for plotting the geoarea values and identifiers
   (in pixels). */
#define GEOAREA_ID_OFFSET -10
#define GEOAREA_VALUE_OFFSET 0

/* This is a "watered down" version of the GeoArea structure. 
   Specifically, it does not include the boundary points and the
   number of those boundary points.  Also, it does not include the
   node and list members required for linked list operations. */
struct GeoAreaInfo {

   char area_id [ GEOAREA_ID_LEN ] ;
   char name [ GEOAREA_NAME_LEN ] ;
   char boundary_type [ BOUNDARY_TYPE_LEN + 1 ] ;
   double interior_lat ;
   double interior_lon ;

} ;

typedef struct GeoAreaInfo GeoAreaInfo ;

GeoAreaInfo * get_geoareainfo ( char * area_id , int * status ) ;
void free_geoareainfo ( ) ;

/* Error codes that can be returned by this utility.  Note that a NULL
   pointer is returned as value of the get_geoarea function when an
   error is encountered.  The status parameter of the function provides
   a numeric code explaining why the routine did not find GeoArea information
   pertaining to the user supplied area identifier. It does supply the 
   area identifier, the area name, the area centroid latitude and longitude,
   and the type of area it is e.g. zone, county, basin, reservoir, lake.*/

#define GEOAREAINFO_OK 0
#define GEOAREAINFO_MALLOC_ERROR 1
#define GEOAREAINFO_EMPTY_TABLE_ERROR 2 

#endif /* #ifndef GET_GEOAREAINFO_H */
