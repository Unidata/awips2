
/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:  This function restores the bias modify bias flag to N in the 
*               RWRadarResult table
* 
* ORIGINAL AUTHOR:  Bryon Lawrence
* CREATION DATE:    November 5, 2004
* ORGANIZATION:     HSEB/OHD
* OPERATING SYSTEM: Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE     PROGRAMMER        DESCRIPTION/REASON
*          1        11/5/04  Bryon Lawrence    Converted to use DBGen.
*          1        12/15/05 Bryon Lawrence    The RWRadarResult query
*                                              had an error in it.  
*                                              rid was changed to radid.
********************************************************************************
*/

#include <stdio.h>
#include <string.h>

#include "DbmsDefs.h"
#include "dbmserrs.h"
#include "List.h"
#include "mpe_log_utils.h"
#include "RWRadarResult.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   restore_bias_RFCW 
* PURPOSE:       This function restores the bias modify flag to N in the 
*                RWRadarResult table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME           DESCRIPTION/UNITS
*   Input  char *      rid            The id of the office Hydroview/MPE
*                                     is being run for.
*   Input  char [ ]    datetime       The time of the MPE data run.
*
* RETURNS:
*   No return.
*
* APIs UTILIZED:
*   NAME                HEADER FILE     DESCRIPTION
*   FreeRWRadarResult   RWRadarResult.h Frees memory used for the linked list
*                                       of RWRadarResult structures.
*   GetRWRadarResult    RWRadarResult.h Retrieves records from the
*                                       RWRadarResult table and stores them
*                                       as a linked list of 
*                                       RWRadarResult structures.
*   ListFirst           List.ha         Retrieves the first node in a linked
*                                       list. 
*   UpdateRWRadarResult RWRadarResult.h Updates a record in the 
*                                       RWRadarResult table.
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE       NAME               DESCRIPTION
*   char [ ]        where              Contains the where clause of the 
*                                      SQL to retrieve and update a
*                                      record in the RWRadarResult table.
*   int             status             Indicates where or not the update
*                                      succeeded.
*   RWRadarResult * pRWRadarResultHead Points to the retrieved RWRadarResult
*                                      linked list.
*   RWRadarResult * pRWRadarResultNode Points to a record retrieved from
*                                      the RWRadarResult table.
*
* DATA FILES AND/OR DATABASE:
* Requires an open connection to the IHFS database.
* Reads and updates the RWRadarResult table.
*
* ERROR HANDLING:
*  Writes an error message to the standard error stream if 
*  the select or update on the RWRadarResult table fails.
*
* CALLING FUNCTION:
*    display_bias_table.c
*
********************************************************************************
*/
void restore_bias_RFCW( const char * rid, const char * datetime )
{   
   char where [ 100 ];
   int status ;
   RWRadarResult * pRWRadarResultHead = NULL;
   RWRadarResult * pRWRadarResultNode = NULL;
   
   /* Build the where clause. */
   memset ( where, '\0', 100 );
   sprintf ( where, "WHERE radid='%s' AND obstime='%s'",
                    rid, datetime ); 

   /* Retrieve the record from the RWRadarResult table. */
   pRWRadarResultHead = GetRWRadarResult ( where );

   if ( pRWRadarResultHead != NULL )
   {
      pRWRadarResultNode = ( RWRadarResult * ) 
                           ListFirst ( & pRWRadarResultHead->list );

      /* Update the bias flag in this RWRadarResult record. */ 
      memset ( pRWRadarResultNode->edit_bias, '\0', BOOL_LEN + 1 );
      strncpy ( pRWRadarResultNode->edit_bias, "n", BOOL_LEN );

      /* Update the record. */
      status = UpdateRWRadarResult ( pRWRadarResultNode, where ) ;

      if ( status != ERR_OK )
      {
         flogMessage ( stderr, "\nIn routine 'restore_bias_RFCW':\n"
                           "Could not update a record in RWRadarResult for\n"
                           "query %s.\n", where ) ;
      }

      FreeRWRadarResult ( pRWRadarResultHead ) ;
      pRWRadarResultHead = NULL ;
   } 
   else
   {
      flogMessage ( stderr, "\nIn routine 'restore_bias_RFCW':\n"
                        "Could not select a record from table\n"
                        "RWRadarResult for query '%s',\n", where );
   }

   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
