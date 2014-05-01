/*******************************************************************************
* FILENAME:             read_radar_results_table.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*   MODULE 1:           read_radar_results_table
* DESCRIPTION:          This function retrieves the mean field bias value
*                       used for a given radar for a given hourly radar 
*                       precipitation field.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        November 4, 2004
* ORGANIZATION:         HSEB/OHD
* OPERATING SYSTEM:     Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/4/2004    Bryon Lawrence    Converted to use DBgen.
********************************************************************************
*/

#include <string.h>

#include "rfcwide.h"
#include "RWRadarResult.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   read_radar_results_table 
* PURPOSE:       Retrieves the mean field bias value used for a radar site's
*                precipitation estimate for a given hour. 
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input   char *     rid                  The radar identifier.
*   Input   char *     datetime             The time, year to sec, to 
*                                           retrieve the mean field bias
*                                           for.
* RETURNS:
*   None. 
*
* APIs UTILIZED:
*   NAME                 HEADER FILE      DESCRIPTION
*   FreeRWRadarResult    RWRadarResult.h  Frees the memory used by the linked
*                                         list of RadarResult structures.
*   GetRWRadarResult     RWRadarResult.h  Retrieves records from the 
*                                         RWRadarResult table.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE         NAME                 DESCRIPTION
*   char [ ]          where                The where clause.
*   RWRadarResult *   pRWRadarResultHead   Points to the head of the linked 
*                                          list of RWRadarResult structures.
*   RWRadarResult *   pRWRadarResultNode   Points to a note of the linked
*                                          list of RWRadarResult structures. 
*
* DATA FILES AND/OR DATABASE:
*   Requires an open connection to the IHFS database.
*   Reads the RWRadarResult table.
*
* ERROR HANDLING:
*   None
********************************************************************************
*/
void read_radar_results_table ( const char * rid, const char * datetime )
{
   char where [ 75 ];
   RWRadarResult * pRWRadarResultHead = NULL;
   RWRadarResult * pRWRadarResultNode = NULL;

   radarresultdata.rw_bias_val_used = 0.0;
   strcpy(radarresultdata.edit_bias_values,"n");

   /* Build the where clause. */
   memset ( where, '\0', 75 );
   sprintf ( where , "WHERE radid='%s' AND obstime='%s'", rid, datetime );

   /* Get the record from the RWRadarResult table. */
   pRWRadarResultHead = GetRWRadarResult ( where ) ;

   if ( pRWRadarResultHead != NULL )
   {
      pRWRadarResultNode = ( RWRadarResult * ) 
                           ListFirst ( & pRWRadarResultHead->list ) ;
      radarresultdata.rw_bias_val_used = pRWRadarResultNode->rw_bias_val_used;
      strcpy ( radarresultdata.edit_bias_values, 
               pRWRadarResultNode->edit_bias );

      /* Free the memory used by the linked list of RWRadarResult
         structures. */
      FreeRWRadarResult ( pRWRadarResultHead ) ;
      pRWRadarResultHead = NULL ;
   }

   return ;
}
