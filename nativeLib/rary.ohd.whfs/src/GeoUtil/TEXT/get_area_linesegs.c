#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsDefs.h"
#include "geoutil.h"
#include "GeoArea.h"
#include "LineSegs.h"
#include "List.h"
#include "sqlca.h"


/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

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

void * get_area_linesegs ( const char * area_type ,
                           size_t size_of_structure )
{
   char              where_lseg [ 80 ] ;
   char              where_geo [ 200 ] ;
   GeoArea *         geoHead = NULL ;
   GeoArea *         geoPtr = NULL ;
   GeoAreaLineSegs * pHead = NULL ;
   GeoAreaLineSegs * pNode = NULL ;
   int               i ;
   LineSegs *        linesegPtr = NULL ;
   long              numrows ;
 
  /* extern long int SQLCODE ;*/

   /* Nest the area type to make sure that it is not NULL and represents
      a valid area identifier. */
   sprintf ( where_geo , "WHERE boundary_type = '%s' AND area_id IN "
                         "(SELECT area_id FROM linesegs)" ,
                         area_type ) ;

   /*--------------------------*/
   /*  begin loop on basins    */
   /*--------------------------*/
   geoHead = GetGeoArea ( where_geo ) ;

   if ( geoHead == NULL )
   {
      fprintf ( stderr , "\nIn routine \"process_areas\":\n"
                         "Could not retrieve any basin data from "
                         "the \"GeoArea\" table.\n" ) ;
      return NULL ;
   }

   if ( SQLCODE != 0 )
   {
      fprintf ( stderr , "\nIn routine \"get_area_linesegs\":\n"
                         "PostgreSQL error %ld encountered while attempting to\n"
                         "select in GeoArea table. Query: \"%s\".\n", 
                         SQLCODE , where_geo ) ;
      FreeGeoArea ( geoHead ) ;
      return NULL ;
   }
   else
   {
      geoPtr = ( GeoArea * ) ListFirst ( & geoHead->list ) ;

      while ( geoPtr )
      {

         /* read the HRAP bin coords for the area and
            extract the information from the linesegs table. */
         sprintf ( where_lseg , " where area_id = '%s' " ,
                                  geoPtr->area_id ) ;
 
         linesegPtr = GetLineSegs(where_lseg);

         numrows = ListCount(&linesegPtr->list);

         if (linesegPtr == NULL)
         {
            fprintf ( stderr , "\nIn routine \"get_area_linesegs\":\n"
                               "LineSeg information not available for %s.\n" ,
                               geoPtr->area_id ) ;
            continue ;
         }

         /* Build linked list of GeoAreaLineSegs structures. */
         pNode = ( struct GeoAreaLineSegs * ) malloc ( size_of_structure ) ;

         if ( pNode == NULL )
         {
            fprintf ( stderr , "\nIn routine \"get_area_linesegs\":\n"
                               "Could not allocate memory for a new node\n"
                               "in the linked list in the linked list of\n"
                               "GeoAreaLineSegs.\n" ) ;
            free_area_linesegs_list ( ( void * ) pHead ) ;
            pHead = NULL;
            return NULL ;
         }

         memset(pNode->area_id, '\0', LOC_ID_LEN+1);
         strcpy(pNode->area_id, geoPtr->area_id);
         memset(pNode->name, '\0', LOC_AREANAME_LEN + 1 ) ;
         strcpy(pNode->name, geoPtr->name); 
         pNode->numrows = numrows ;
         pNode->rows = NULL ;
         pNode->beg_cols = NULL ;
         pNode->end_cols = NULL ;

         /* Perform a "deep" copy of the "rows", "beg_cols", and "end_cols"
            arrays. */
         pNode->rows = ( long * ) malloc ( pNode->numrows *
                                                 sizeof ( long ) ) ;

         if ( pNode->rows == NULL )
         {
            fprintf ( stderr , "\nIn routine \"get_area_linesegs\":\n"
                               "Could not allocate %d bytes for the \"rows\"\n"
                               "element of a GetAreaLinesegs structure.\n" ,
                               ( pNode->numrows * 4 ) ) ;
            free_area_linesegs_list ( ( void * ) pNode ) ;
            pNode = NULL; 
            return NULL ;
         }

         pNode->beg_cols = ( long * ) malloc ( pNode->numrows *
                                               sizeof ( long ) ) ;

         if ( pNode->beg_cols == NULL )
         {
            fprintf ( stderr , "\nIn routine \"get_area_linesegs\":\n"
                               "Could not allocate %d bytes for the\n"
                               "\"beg_cols\" element of a GetAreaLinesegs\n"
                               "structure.\n" , ( pNode->numrows * 4 ) ) ;
            free_area_linesegs_list ( ( void * ) pNode ) ;
            pNode = NULL;
            return NULL ;
         }

         pNode->end_cols = ( long * ) malloc ( pNode->numrows *
                                                     sizeof ( long ) ) ;

         if ( pNode->end_cols == NULL )
         {
            fprintf ( stderr , "\nIn routine \"get_area_linesegs\":\n"
                               "Could not allocate %d bytes for the\n"
                               "\"end_cols\" element of a GeoAreaLineSegs\n"
                               "structure.\n" , ( pNode->numrows * 4 ) ) ;
            free_area_linesegs_list ( ( void * ) pNode ) ;
            pNode = NULL;
            return NULL ;
         }

         for ( i = 0 ; i < pNode->numrows ; ++ i )
         {
            pNode->rows [ i ] = linesegPtr->hrap_row; ;
            pNode->beg_cols [ i ] = linesegPtr->hrap_beg_col ;
            pNode->end_cols [ i ] = linesegPtr->hrap_end_col; 

	    linesegPtr = ( LineSegs *) ListNext(&linesegPtr->node); 
         }

         if ( pHead == NULL )
         {
            pHead = pNode ;
            ListInit ( & pHead->list ) ;
            ListAdd ( & pHead->list , ( Node * ) pNode ) ;
         }
         else
         {
            ListAdd ( & pHead->list , ( Node * ) pNode ) ;
        }

         /* free the memory */
         FreeLineSegs ( linesegPtr ) ;
         linesegPtr = NULL ;

         geoPtr = ( GeoArea * ) ListNext(&geoPtr->node);

      }  /* end while(geoPtr)  */

      FreeGeoArea ( geoHead ) ;
      geoHead = NULL ;

   }  /* end if((geoHead = GetGeoArea(where_geo))) */

   return ( void * ) pHead ;
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
void free_area_linesegs_list ( void * pHead )
{
   GeoAreaLineSegs * pListHead = ( GeoAreaLineSegs * ) pHead ;
   struct GeoAreaLineSegs * pNext = NULL ;

   /* Free the linked list and start a new one. */
   while ( pListHead != NULL )
   {
      pNext = ( struct GeoAreaLineSegs * )
              ListNext ( & pListHead->node ) ;

      if ( pListHead->rows != NULL )
      {
         free ( ( void * )  pListHead->rows ) ;
         pListHead->rows = NULL ;
      }

      if ( pListHead->beg_cols != NULL )
      {
         free ( ( void * ) pListHead->beg_cols ) ;
         pListHead->beg_cols = NULL ;
      }
      if ( pListHead->end_cols != NULL )
      {
         free ( ( void * ) pListHead->end_cols ) ;
         pListHead->end_cols = NULL ;
      }

      free ( ( void * ) pListHead ) ;
      pListHead = pNext ;
   }
}
