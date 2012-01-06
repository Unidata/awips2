/*******************************************************************************
* FILENAME:            get_last_times.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          get_last_times
* DESCRIPTION:         Reads the last execute and last save times from the
*                      RWResult table.  This information is used to populate
*                      the Choose Hour window.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 4, 2004
* ORGANIZATION:        HSEB/OHD
* OPERATING SYSTEM:    Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/4/2004    Bryon Lawrence    Converted to use DBGen
*                                                  routines.
********************************************************************************
*/

#include <string.h>

#include "get_last_times.h"
#include "RWResult.h"
#include "time_convert.h"

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    get_last_times
* PURPOSE:        Retrieves the last execute and last save times from the
*                 RWResult table.  This information is used for the entries 
*                 in the Choose Hour window.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME     DESCRIPTION/UNITS
*   Input  char *      rfc      The id of the office to retrieve the
*                               RWResult information for.
*   Input  char *      datetime The time to retrieve the last exec and last 
*                               save information for.
*   Output char        str1[]   The ASCII representation of the last exec time.
*   Output char        str2[]   ThE ASCII representation of the last save time.
*
* RETURNS:
*   No return value
*
* APIs UTILIZED:
*   NAME             HEADER FILE            DESCRIPTION
*   FreeRWResult     RWResult.h             Frees memory used in the
*                                           RWResult linked list.
*   GetRWResult      RWResult.h             Retrieves rows in the RWResult
*                                           table.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME           DESCRIPTION
*   char       where          Contains the where clause used to query the
*                             RWResult table.
*   RWResult * pRWResultHead  Pointer to the head of the linked list of
*                             RWResult information.
*   RWResult * pRWResultNode  Point to a node in the linked list of RWResult
*                             information.
*
* DATA FILES AND/OR DATABASE:
*   Requires an open connection to the IHFS database.  This routine reads
*   The RWResult table.
*
* ERROR HANDLING:
*   None.
********************************************************************************
*/

void get_last_times ( const char *rfc, const char * datetime, 
                      char str1[ ], char str2[ ] )
{
   char where [ 50 ];
   RWResult * pRWResultHead = NULL;
   RWResult * pRWResultNode = NULL;

   /* Build the where clause. */
   sprintf ( where, "WHERE rfc='%s' AND obstime='%s'", rfc, datetime );

   /* Read the RWResult table for the last exec and last save data. */
   pRWResultHead = GetRWResult ( where );

   if ( pRWResultHead != NULL )
   {
      pRWResultNode = ( RWResult * ) ListFirst ( & pRWResultHead->list );
      yearsec_dt_to_ansi ( pRWResultNode->last_exec_time, str1 ); 
      yearsec_dt_to_ansi ( pRWResultNode->last_save_time, str2 );

      /* Free any memory used by the linked list of RWResult information. */ 
      FreeRWResult ( pRWResultHead ) ;
      pRWResultHead = NULL ;
   }
   else
   {
      strcpy( str1, "      n/a      " );
      strcpy( str2, "      n/a      " );
   }

   return ;
} 
