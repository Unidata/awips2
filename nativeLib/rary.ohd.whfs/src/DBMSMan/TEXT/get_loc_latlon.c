/*

       File:                       get_loc_latlon.c
       Date of last revision:      05/21/2003
       Author:                     Gautam Sood

       Purpose:                    This file contains the function that
                                   retrieves the lat/lon values for a specified
                                   lid via a binary search.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "get_loc_latlon.h"
#include "Location.h"
#include "BinarySearch.h"

static int first = 1;
static latlon_list_struct *latlon_id_list = NULL;

/**********************************************************************
   compare_latlon_id()

   Compare function used for the binary search of the id list
   done when checking if the location is a valid one.

   ********************************************************************/
static int compare_latlon_id(void * search_value,
                             void * array_value)
{
   /* declare and define the values for two variables mapped
      against the input arguments */

   char * pSearchValue = ( char * ) search_value ;
   latlon_list_struct * pArrayValue = ( latlon_list_struct * ) array_value ;


   /* return the result of the simple string comparison */

   return ( strcmp ( pSearchValue , pArrayValue->id) ) ;
}

/**********************************************************************
   get_loc_latlon_nrv()

  get location's lat and lon.  This routine has been set up to call 
  get_loc_latlon.  It returns the search status as a calling argument
  as opposed to a return value.  This is useful if being called by Fortran.

   ********************************************************************/
void get_loc_latlon_nrv ( char * lid ,
                          double * dlat ,
                          double * dlon ,
                          int * status )
{
   * status = get_loc_latlon ( lid , dlat , dlon ) ;   
}


/**********************************************************************
   get_loc_latlon()

  get location's lat and lon.

   ********************************************************************/

int get_loc_latlon(char        *lid,
                    double      *dlat,
                    double      *dlon)
{
   static int                   first = 1;
   static int                   latlon_id_cnt = 0;

   Location             *locHead, *locPtr;
   int                  i;
   latlon_list_struct   *latlon_id = NULL;


   /* initialize */

   *dlat = 39.0;
   *dlon = 77.0;


   /* the first time, get the location info
      and copy the relevant info to local buffers. */

   if (first)
   {
      first = 0;

      locHead = GetLocation(" ORDER BY lid ASC ");
      if (locHead)
      {
         latlon_id_cnt = ListCount(&locHead->list);
         latlon_id_list = (latlon_list_struct *)
            malloc(sizeof(latlon_list_struct) * latlon_id_cnt);

         if (latlon_id_list == NULL)
         {
            fprintf(stderr, "Critical error allocating latlon_id_list.");
            return(LATLONNOTFOUND);
         }

         else
         {
            locPtr = (Location *) ListFirst(&locHead->list);
            for (i = 0; i < latlon_id_cnt; i++)
            {
               strcpy(latlon_id_list[i].id, locPtr->lid);
               latlon_id_list[i].lat =      locPtr->lat;
               latlon_id_list[i].lon =      locPtr->lon;

               locPtr = (Location *) ListNext(&locPtr->node);
            }
         }

         FreeLocation(locHead);
      }

      else
         latlon_id_cnt = 0;
   }


   /* get the info for the matching identifier, if there is a match. */

   if (latlon_id_cnt > 0)
   {
      latlon_id = (latlon_list_struct *)
         binary_search(latlon_id_list, lid, latlon_id_cnt,
                       sizeof(latlon_list_struct), compare_latlon_id);
   }


   /* load in the info */

   if (latlon_id)
   {
      *dlat = latlon_id->lat;
      *dlon = latlon_id->lon;
      return(LATLONFOUND);
   }

   else
   {
      fprintf(stderr, "No lat lon info found for '%s'\n", lid);
      return(LATLONNOTFOUND);
   }
}


/**********************************************************************
   free_loc_latlon_list

   Used to free memory used by latlon_id_list

   ********************************************************************/

void free_loc_latlon_list ( )
{
   if ( latlon_id_list != NULL )
   {
      free ( latlon_id_list ) ;
      latlon_id_list = NULL ;
      first = 1 ;
   }

}
