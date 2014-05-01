/******************************************************************************** FILENAME:            update_flig_RFCW.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          update_flig_RFCW
* DESCRIPTION:         Updates the ignore radar flag
*                      in the RWRadarResult table.
*
* ORIGINAL AUTHOR:     Moria Shebsovich 
* CREATION DATE:       November 5, 2004
* ORGANIZATION:        HSEB/OHD
* OPERATING SYSTEM:    Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/5/2004    Moria Shebsovich  Upgraded to use DBGen.
*          1        12/15/2005   Bryon Lawrence    Fixed bug which prevented
*                                                  a radar from becoming 
*                                                  unignored.
*********************************************************************************/

#include <stdio.h>
#include <string.h>

#include "DbmsDefs.h"
#include "dbmserrs.h"
#include "mpe_log_utils.h"
#include "read_rresult.h"
#include "RWRadarResult.h"
#include "update_flig_RFCW.h"

/******************************************************************************** MODULE NUMBER: 1
* MODULE NAME:   update_flig_RFCW
* PURPOSE:       This routine toggles the ignore flag 
*                in the RWRadarResult table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME            DESCRIPTION/UNITS
*   Input  char *      rid             The radar identifier.
*   Input  char *      datetime        The datetime of the gage.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME               HEADER FILE    DESCRIPTION
* FreeRWiRadarResult   RWRadarResult.h  Frees memory used by linked list of
*                                       RWRadarResult structures.
* GetRWRadarResult     RWRadarResult.h  Retrieves rows from RWRadarResult table.
*                                       Builds a linked list where a node
*                                       is a record in the table.
* ListFirst            List.h           Lists the first node in the linked
*                                       list.
* UpdateRWRadarResult  RWRadarResult.h  Updates a record in the RWRadarResult
*
* LOCAL DATA ELEMENTS:
*  DATA TYPE        NAME                 DESCRIPTION
*
*  char [ ]         where                Contains the where clause
*                                        for retrieving and updating records
*                                        in the RWiRadarResult table.
*  int              status               Indicates update success or failure.
*  RWRadarResult  * pRWRadarResultHead A pointer to the head node in the linked
*                                       list of RWRadarResult structures.
*  RWRadarResult  * pRWadarResultNode  A pointer to a node in the linked list
*                                       of RWRadarResult structures.
* DATA FILES AND/OR DATABASE:
*  Requires an open connection to the IHFS database.
*  Reads and Updates the RWRadarResult table.

* ERROR HANDLING:
*  indicates if the update of the RWRadarResult table failed.
*
* CALLING ROUTINE:
*  ignore_radar_RFCW 
*
*********************************************************************************/

void update_flig_RFCW ( short ignore_radar_flag, char * rid , char * datetime )
{
   char ifl [ BOOL_LEN + 1] ;
   int status ;

   char where [ 100 ];
   RWRadarResult * pRWRadarResultHead = NULL;
   RWRadarResult * pRWRadarResultNode = NULL;
 
   memset ( where , '\0', 100 );

   /* Build the where clause. */

   sprintf ( where , "WHERE radid='%s' AND obstime='%s'",
             rid , datetime );

   /* Retrieve the data from RWRadarResult. */

   pRWRadarResultHead = GetRWRadarResult ( where ) ;

   if ( pRWRadarResultHead == NULL )
   {
     flogMessage ( stderr, "In routine 'update_flig_RFCW':\n"
                       "Error attempting to find record in RWRadarResult\n"
                       "table for rid='%s' and obstime='%s'.\n" , rid, datetime );
     return;
   }

   pRWRadarResultNode = ( RWRadarResult * ) ListFirst ( & pRWRadarResultHead->list );

   /* Update the ignore flag in the RWRadarResult node. */
 
   if ( ignore_radar_flag == ( enum RadarIgnoreFlag ) IgnoreRadar )
   {
     strcpy(ifl,"n");
   }
   else
   {
     strcpy(ifl,"y");
   }
 
   strcpy ( pRWRadarResultNode->ignore_radar , ifl ) ;

   /*----------------------------------*/
   /*  update RWRadarResult record     */
   /*----------------------------------*/

   /* Update the record in RWRadarResult. */
   status = UpdateRWRadarResult ( pRWRadarResultNode , where );
   if ( status != ERR_OK )
   {
      flogMessage ( stderr, "In routine 'update_flig_RFCW':\n"
                        "could not update record in\n"
                        "RWRadarResult table for rid='%s'"
                        "and obstime='%s'.\n" , rid , datetime ) ;
   }

   /* Free memory used by RWRadarResult linked list. */
   FreeRWRadarResult ( pRWRadarResultHead ) ;
   pRWRadarResultHead = NULL ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
