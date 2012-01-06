/*

       File:                       get_loc_info.c
       Date of last revision:      05/21/2003
       Author:                     Gautam Sood
       Modified by:                Jingtao Deng (06/2003)
         
       Purpose:                    This file contains the function that
                                   retrieves the lat/lon values for a specified
                                   lid via a binary search.
       Modification:               Retrieves name also from
                                   Locationt able for particular lid. Use new
				   structure loc_info_struct.            				   
        
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "get_loc_info.h"
#include "Location.h"
#include "BinarySearch.h"

static int first = 1;
static loc_info_struct *loc_info_list = NULL;

/**********************************************************************
   compare_loc_id()

   Compare function used for the binary search of the id list
   done when checking if the location is a valid one.

   ********************************************************************/
static int compare_loc_id(void * search_value,
                        void * array_value)
{
   /* declare and define the values for two variables mapped
      against the input arguments */

   char * pSearchValue = ( char * ) search_value ;
   loc_info_struct * pArrayValue = ( loc_info_struct * ) array_value ;


   /* return the result of the simple string comparison */

   return ( strcmp ( pSearchValue , pArrayValue->id) ) ;
}
/**********************************************************************
   get_loc_latlon()

  get lid, name, county,state, lat, lon from table Location for particular lid.

********************************************************************/

int get_loc_info(char       *lid,
                double      *dlat,
                double      *dlon,
		char        *name)
{   
   static int            loc_info_cnt = 0;

   Location            *locHead = NULL, *locPtr = NULL;
   int                  i;
   loc_info_struct     *loc_id = NULL;


   /* initialize */

   *dlat = 39.0;
   *dlon = 77.0;
   strcpy(name, "");

   /* the first time, get the location info
      and copy the relevant info to local buffers. */

   if (first)
   {
      first = 0;

      locHead = GetLocation(" ORDER BY lid ASC ");
      if (locHead)
      {
         loc_info_cnt = ListCount(&locHead->list);
	 
	 /*malloc loc_info_list */
	 
         loc_info_list = (loc_info_struct *)
            malloc(sizeof(loc_info_struct) * loc_info_cnt);

         if (loc_info_list == NULL)
         {
            fprintf(stderr, "Critical error allocating loc_info_list in get_loc_info.");
            return(LOCINFONOTFOUND);
         }

         else
         {
            locPtr = (Location *) ListFirst(&locHead->list);
	    	    
            for (i = 0; i < loc_info_cnt; i++)
            {
	       if (locPtr != NULL)
	       {
        	  strcpy(loc_info_list[i].id, locPtr->lid);
        	  loc_info_list[i].lat =      locPtr->lat;
        	  loc_info_list[i].lon = locPtr->lon;
		  strncpy(loc_info_list[i].name, locPtr->name,ALLOWED_NAME_LEN);
		  loc_info_list[i].name[ALLOWED_NAME_LEN + 1]= '\0';		  		 
               }
	       
               locPtr = (Location *) ListNext(&locPtr->node);
            }
         }

         FreeLocation(locHead);
      }

      else
         loc_info_cnt = 0;
   }


   /* get the info for the matching identifier, if there is a match. */

   if (loc_info_cnt > 0)
   {
      loc_id = (loc_info_struct *)
         binary_search(loc_info_list, lid, loc_info_cnt,
                       sizeof(loc_info_struct), compare_loc_id);
   }


   /* load in the info */

   if (loc_id)
   {
      *dlat = loc_id->lat;
      *dlon = loc_id->lon;
      strcpy(name, loc_id->name);      
      
      return(LOCINFOFOUND);
   }

   else
   {
      fprintf(stderr, "No location info (lat,lon,name) found for '%s'\n", lid);
      return(LOCINFONOTFOUND);
   }
}



/**********************************************************************
   free_loc__list

   Used to free memory used by loc_info_list

   ********************************************************************/

void free_loc_list ( )
{
   if ( loc_info_list != NULL )
   {
      free ( loc_info_list ) ;
      loc_info_list = NULL ;
      first = 1 ;
   }

}
