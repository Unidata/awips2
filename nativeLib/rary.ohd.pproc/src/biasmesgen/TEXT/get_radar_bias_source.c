
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
#include <stdio.h>
#include <string.h>

#include "DbmsDefs.h"
#include "RadarLoc.h"

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

char * get_radar_bias_source ( const char * radar_id )
{
   static char radar_bias_source_id[ RADAR_ID_LEN + 1 ];
   char where_clause [ 100 ];
   RadarLoc * pRadarLoc = NULL;

   memset ( radar_bias_source_id, '\0', RADAR_ID_LEN + 1 );
   sprintf ( where_clause, "WHERE radid = '%s'", radar_id );

   pRadarLoc = GetRadarLoc ( where_clause );

   if ( pRadarLoc != NULL )
   {
      strncpy ( radar_bias_source_id, pRadarLoc->office_id, RADAR_ID_LEN ); 
      FreeRadarLoc ( pRadarLoc );
      pRadarLoc = NULL;
      return radar_bias_source_id;
   }
   else
   {
      return NULL;
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
