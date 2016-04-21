
/*******************************************************************************
* FILENAME:             get_geoarea.c
* NUMBER OF MODULES:    
* GENERAL INFORMATION:
*   MODULE 1:           compare_geoareainfo 
* DESCRIPTION:          Contains the comparison logic required by the 
*                       binary search routine to find the geo area data
*                       for the area id the user is looking for. 
*   MODULE 2:           get_geoareainfo
* DESCRIPTION:          Allows the user to quickly retrieve GeoArea data
*                       corresponding to an area id.
*   MODULE 3:           free_geoareainfo 
* DESCRIPTION:          Deallocates the memory used by the array of 
*                       GeoAreaInfo structures.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        May 2, 2003
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / LINUX
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        May 2, 2003  Bryon Lawrence    Original Coding. 
*          2        May 3, 2003  Bryon Lawrence    Original Coding. 
*          3        May 3, 2003  Bryon Lawrence    Original Coding. 
********************************************************************************
*/

#include <stdlib.h>
#include <string.h>

#include "BinarySearch.h"
#include "GeoArea.h"
#include "get_geoareainfo.h"
#include "List.h"

static GeoAreaInfo * pGeoAreaInfoArray = NULL ; 
static int first = 1 ;

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   compare_geoareainfo
* PURPOSE:       This routine provides the comparison logic to be used by
*                the binary search routine.  It compares the geo area area id i
*                being searched for with the area id contained in one element
*                of the GeoAreaInfo array.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  void *      search_value         Contains the address of the value
*                                           to be searched for.  In this case
*                                           it is the area id string.
*   Input  void *      array_value          Contains the address of the
*                                           array element which is being tested
*                                           as to whether or not it contains
*                                           the search value. 
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*   int         NA                          A value of 0 if the search_value
*                                           is found in the array element, a
*                                           value > 0 if the search value 
*                                           is greater than the value in the
*                                           array element, or a value < 0 if
*                                           the search value is smaller than
*                                           the value in the array element. 
*
* APIs UTILIZED:
*   None
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*   char *     pSearchValue                 Contains the address of the 
*                                           area id string being searched
*                                           for.
*   GeoAreaInfo * pArrayValue               Contains the address of the
*                                           GeoAreaInfo array element the
*                                           array id whose area id is being
*                                           compared to the search area id.
*
* DATA FILES AND/OR DATABASE:
*    None
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/
static int compare_geoareainfo ( void * search_value , void * array_value )
{
   char * pSearchValue = ( char * ) search_value ;
   GeoAreaInfo * pGeoAreaInfo = ( GeoAreaInfo * ) array_value ;

   return ( strcmp ( pSearchValue , pGeoAreaInfo->area_id ) ) ;
}

/*******************************************************************************
* MODULE NUMBER:   2
* MODULE NAME:     get_geoareainfo
* PURPOSE:     Allows the user to quickly retrieve GeoArea data corresponding
*              to an area id.  This information is returned as a pointer
*              to a GeoAreaInfo structure.  This structure is a watered down
*              version of the GeoArea structure.  It does not contain the
*              actual latitude / longitude points defining the geo area's 
*              boundary.  It also does not contain the number of points 
*              defining the geo area's boundary. It does supply the
*              area identifier, the area name, the area centroid latitude 
*              and longitude, and the type of area it is e.g. zone, county,
*              basin, reservoir, lake.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  char *      area_id              The identifier of the geo area to 
*                                           search for and retrieve information
*                                           for.
*   Output int *       status               A value indicating why 
*                                           an attempt to find geo area
*                                           information for a station failed.
*                                           This will be nonzero only when
*                                           a NULL GeoAreaInfo pointer is
*                                           returned to the caller. See 
*                                           Error Handling below.
*
* RETURNS:
*   DATA TYPE                               DESCRIPTION
*   GeoAreaInfo *                           Points to the GeoArea data found
*                                           for the user-supplied area id. 
*                                           This will be NULL if no data could
*                                           be found and the status parameter
*                                           will contain a numeric value
*                                           indicating why data could not be
*                                           found.
*
* APIs UTILIZED:
*   NAME             HEADER FILE        DESCRIPTION
*   binary_search    BinarySearch.h     Contains the binary search routine
*                                       prototype.
*   GetGeoArea       GeoArea.h          Contains the GetGeoArea routine
*                                       prototype and GeoArea structure
*                                       definition.
*   ListCount        List.h             Returns a count of the nodes in 
*                                       a linked list.
*   ListFirst        List.h             Returns the first node in a linked list.
*   ListNext         List.h             Returns the next node in a linked list. 
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE    NAME                  DESCRIPTION
*   int          first                 Indicates whether or not the
*                                      pGeoAreaInfo array has been built.
*   int          geo_area_count        Contains the number of elements in
*                                      in the array of GeoAreaInfo structures.
*   int          i                     A loop index.
*   GeoArea    * pGeoAreaHead          Contains the address of the first node
*                                      in the linked list of GeoArea
*                                      structures as returned from the GeoArea
*                                      table in the IHFS database.
*   GeoArea    * pGeoAreaNode          Used to examine each node in the
*                                      linked list of GeoArea structures.
*   GeoAreaInfo* pReturnedGeoAreaInfo  Contains the address of the GeoAreaInfo
*                                      structure which contains the area id
*                                      the user was searching for. 
*
* DATA FILES AND/OR DATABASE:
*    Requires the GeoArea table in the IHFS database.
*
* ERROR HANDLING:
*    ERROR CODE                          DESCRIPTION
*    GEOAREAINFO_EMPTY_TABLE_ERROR       There are no entries in the GeoArea
*                                        table.
*    GEOAREAINFO_MALLOC_ERROR            Could not allocate memory for the
*                                        array of GeoAreaInfo structures.
*    GEOAREAINFO_OK                      This routine functioned correctly
*                                        within the bounds set by internal
*                                        error checking.
********************************************************************************
*/
GeoAreaInfo * get_geoareainfo ( char * area_id , int * status )
{
   static int         geo_area_count = 0 ;
   int                i ;
   GeoAreaInfo        * pReturnedGeoAreaInfo = NULL ;
   GeoArea            * pGeoAreaHead = NULL ; 
   GeoArea            * pGeoAreaNode = NULL ;

   * status = GEOAREAINFO_OK ;

   /* buffer the lookup info the first time thru */
   if ( first == 1 )
   {
      pGeoAreaHead = GetGeoArea ( " ORDER BY area_id " ) ;

      if ( pGeoAreaHead != NULL )
      {
         geo_area_count = ListCount ( & pGeoAreaHead->list ) ;

         pGeoAreaInfoArray = ( GeoAreaInfo * ) malloc ( 
                                 sizeof ( GeoAreaInfo ) * geo_area_count ) ;

         if ( pGeoAreaInfoArray == NULL )
         {
            /* Memory allocation problem. */ 
            * status = GEOAREAINFO_MALLOC_ERROR ;
            return NULL ; 
         }

         pGeoAreaNode = ( GeoArea * ) ListFirst ( & pGeoAreaHead->list ) ;

         for ( i = 0 ; i < geo_area_count ; ++ i )
         {
            strcpy ( pGeoAreaInfoArray [ i ].area_id , 
                     pGeoAreaNode->area_id ) ;
            strcpy ( pGeoAreaInfoArray [ i ].name , 
                     pGeoAreaNode->name ) ;
            strcpy ( pGeoAreaInfoArray [ i ].boundary_type ,
                     pGeoAreaNode->boundary_type ) ;
            pGeoAreaInfoArray [ i ].interior_lat = pGeoAreaNode->interior_lat ;
            pGeoAreaInfoArray [ i ].interior_lon = pGeoAreaNode->interior_lon ;

            pGeoAreaNode = ( GeoArea * ) ListNext ( & pGeoAreaNode->node ) ;
         }

         FreeGeoArea ( pGeoAreaHead ) ;
         pGeoAreaHead = NULL ;
      }
      else
      {
         * status = GEOAREAINFO_EMPTY_TABLE_ERROR ; 
         return NULL ; 
      }

      first = 0;
   }

   /* Retrieve the geo area information for the requested area id. */
   pReturnedGeoAreaInfo = ( GeoAreaInfo * ) binary_search ( pGeoAreaInfoArray ,
                                                 area_id ,
                                                 geo_area_count ,
                                                 sizeof ( GeoAreaInfo ) ,
                                                 compare_geoareainfo ) ;
   return ( pReturnedGeoAreaInfo ) ;

}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
void free_geoareainfo ( ) 
{
   if ( pGeoAreaInfoArray != NULL )
   {
      free ( pGeoAreaInfoArray ) ;
      pGeoAreaInfoArray = NULL ;
      first = 1 ;
   }
}
