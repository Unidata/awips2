/******************************************************************************** FILENAME:            update_bias_RFCW.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          update_bias_RFCW
* DESCRIPTION:         This function updat    es the bias value and the mem_span
*                      value in the RWRadarResult table.
*
* ORIGINAL AUTHOR:    Moria Shebsovich
* CREATION DATE:       November 4, 2004
* ORGANIZATION:        HSEB/OHD
* OPERATING SYSTEM:    Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/4/2004    Moria Shebsovich  Upgraded to use DBGen.
*********************************************************************************/
#include <stdio.h>
#include <string.h>

#include "DbmsDefs.h"
#include "mpe_log_utils.h"
#include "update_bias_RFCW.h"
#include "RWRadarResult.h"
#include "dbmserrs.h"

/******************************************************************************** MODULE NUMBER: 1
* MODULE NAME:   update_bias_RFCW
* PURPOSE:       This function updates the bias value and the mem_span
*                value in the RWRadarResult table. Edited bias values
*                are assigned a mem span value of -99.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  char *      datetime        The datetime of the gage.
*   Input  char *      rid             The radar identifier.
*   Input  float                       The bias value.
   
* RETURNS:
*    Nothing
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*   FreeRWRadarResult   RWRadarResult.h Frees memory used by linked list of
*                                       RWRadarResult structures.
*   GetRWRadarResult    RWRadarResult.h Retrieves rows from RWRadarResult table.
*                                       Builds a linked list where a node
*                                       is a record in the table.
*   UpdateRWRadarResult RWRadarResult.h Updates a record in the RWRadarResult 
*                                       table.                                  *            
*   ListFirst           List.h          Lists the first node in the linked
*                                       list.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*  char [ ]    where              Contains the where clause 
*                                 for retrieving and updating records
*                                 in the RWiRadarResult table.
*  int         status             Indicates update success or failure.
*  RWResult *  pRWRadarResultHead A pointer to the head of the RWResult 
*                                 linked list.
*  RWResult *  pRWRadarResultNode A pointer to a node in the RWResult 
*                                 linked list.
*
* DATA FILES AND/OR DATABASE:
*   Requires an open connection to the IHFS database.
*   Reads and Updates the RWRadarResult table.
*
* ERROR HANDLING:
*   Indicates if the update of the RWRadarResult table failed.
*
* CALLED BY:
*    write_editbias_RFCW
*********************************************************************************/

void update_bias_RFCW(char *rid, char datetime[22], float *bias)
{
   float bbias , memspan ;
   char biasflg [ BOOL_LEN + 1] ;
   int status ;
   memspan = -99. ;
   bbias = *bias ;
   strcpy ( biasflg,"y" ) ;

   char where[100] ;
   RWRadarResult * pRWRadarResultHead = NULL ;
   RWRadarResult * pRWRadarResultNode = NULL ;

   memset ( where , '\0', 100 );

   /* Build the where clause. */

   sprintf ( where , "WHERE radid='%s' AND obstime='%s'",
             rid , datetime );
   /* Retrieve the data from RWRadarResult. */

   pRWRadarResultHead = GetRWRadarResult ( where ) ;

   if ( pRWRadarResultHead == NULL )
   {
     flogMessage ( stderr, "In routine 'update_bias_RFCW':\n"
                       "Error attempting to find record in RWRadarResult\n"
                       "table for rid='%s' and obstime='%s'.\n" , rid, datetime );
     return;
   }
   pRWRadarResultNode = ( RWRadarResult * ) ListFirst ( & pRWRadarResultHead->list );
   
   /* Update the elements in the RWResult node. */
   
   pRWRadarResultNode->rw_bias_val_used = bbias ;
   pRWRadarResultNode->mem_span_used = memspan ;
   strcpy ( pRWRadarResultNode->edit_bias , biasflg ) ;
   
   /*-----------------------------*/
   /*  update RWRadarResult record     */
   /*-----------------------------*/

   /* Update the record in RWRadarResult. */
   status = UpdateRWRadarResult ( pRWRadarResultNode , where );

   if ( status != ERR_OK )
   {
      flogMessage ( stderr, "In routine 'update_bias_RFCW':\n"
                        "could not update record in\n"
                        "RWRadarResult table for rid='%s'"
                        "and obstime='%s'.\n" , rid , datetime ) ;
   }

   /* Free memory used by RWRadarResult linked list. */
   FreeRWRadarResult ( pRWRadarResultHead ) ;
   pRWRadarResultHead = NULL ;

   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
