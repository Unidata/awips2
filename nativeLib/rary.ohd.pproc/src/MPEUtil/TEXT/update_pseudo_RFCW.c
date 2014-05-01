/*******************************************************************************
* FILENAME:              update_pseudo_RFCW.c
* NUMBER OF MODULES:     1
* GENERAL INFORMATION:
*   MODULE 1:            update_pseudo_RFCW
* DESCRIPTION:           This function updates pseudo gage values in the
*                        PseudoGageVal table.
*
* ORIGINAL AUTHOR:       Bryon Lawrence
* CREATION DATE:         November 5, 2004
* ORGANIZATION:          HSEB/OHD
* OPERATING SYSTEM:      Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/5/04      Bryon Lawrence    Converted to use DBgen Code.
********************************************************************************
*/

#include <stdio.h>
#include <string.h>

#include "DbmsDefs.h"
#include "dbmserrs.h"
#include "mpe_log_utils.h"
#include "PseudoGageVal.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   update_pseudo_RFCW
* PURPOSE:       Updates pseudo gage values in the PseudoGageVal table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME    DESCRIPTION/UNITS
*   Input  char *      gid     The id of the pseudo gage whose value is 
*                              being updated.  
*   Input  char *      dt      The date and time of the pseudo gage being
*                              updated.
*   Input  float       gval    The new value of the gage.
*   Output int *       nupd    The number of rows updated.  A value of 99
*                              means that no rows where updated.  At most
*                              one row should be updated. 
*
* RETURNS:
*   No return value. Void.
*
* APIs UTILIZED:
*   NAME                 HEADER FILE        DESCRIPTION
*   GetPseudoGageVal     PseudoGageVal.h    Retrieves rows from the
*                                           PseudoGageVal table based on
*                                           the user-supplied where clause. 
*   ListFirst            List.h             Lists the first node in a linked 
*                                           list.
*   UpdatePseudoGageVal  PseudoGageVal.h    Updates a record in the
*                                           PseudoGageVal table.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE       NAME                DESCRIPTION
*   char [ ]        where               The clause which controls which
*                                       records are selected and updated
*                                       in the PseudoGageVal table. 
*   int             status              The status of the PseudoGageVal
*                                       table update.
*   PseudoGageVal * pPseudoGageValHead  Points to the head node in a linked
*                                       list of PseudoGageVal nodes.  
*   PseudoGageVal * pPseudoGageValNode  Points to a node in a linked list
*                                       of PseudoGageVal nodes.
*
* DATA FILES AND/OR DATABASE:
* Requires an open connection to the IHFS database.  Reads and updates
* the PseudoGageVal table. 
*
* ERROR HANDLING:
* Reports errors to the standard error stream when a record cannot be 
* retrieved from the PseudoGageVal table or when an record can not be
* updated in the PseudoGageVal table.
*    nupd = 99 means that this routine failed.
*    nupd = 1 means that this routine was successful and one record 
*           was updated in the PseudoGageVal table.
********************************************************************************
*/
void update_pseudo_RFCW ( const char * gid, const char *dt, 
                          float gval, long int *nupd )
{
   char where [ 100 ];
   int status ;
   PseudoGageVal * pPseudoGageValHead = NULL ;
   PseudoGageVal * pPseudoGageValNode = NULL ; 

   * nupd = 99;  /* initialize return code  */

   /* Construct the query. */
   sprintf ( where, "WHERE pseudo_gage_id='%s' AND obstime='%s'",
                    gid, dt ) ;   

   /* Retrieve the PseudoGageVal record for this pseudo gage id and time. */
   pPseudoGageValHead = GetPseudoGageVal ( where ) ;

   if ( pPseudoGageValHead != NULL )
   {
      pPseudoGageValNode = ( PseudoGageVal * ) 
                           ListFirst ( & pPseudoGageValHead->list ) ;
      pPseudoGageValNode->gage_value = gval;
      memset ( pPseudoGageValNode->man_edited, '\0', BOOL_LEN + 1 );
      strncpy ( pPseudoGageValNode->man_edited, 
                "T" , BOOL_LEN ) ; 

      /* Update the record. */
      status = UpdatePseudoGageVal ( pPseudoGageValNode, where );


      if ( status == ERR_OK )
      {
         * nupd = 1 ;
      }
      else
      {
         flogMessage ( stderr, "In routine 'update_pseudo_RFCW':\n"
                           "Could not update the record in the\n"
                           "PseudoGageVal table for query '%s'.\n" , where );
      }

      /* Free the memory used in the linked list of PseudoGageVal 
         structures. */
      FreePseudoGageVal ( pPseudoGageValHead ) ;
      pPseudoGageValHead = NULL ;
   }
   else
   {
      flogMessage ( stderr, "In routine 'update_pseudo_RFCW':\n"
                        "Could not retrieve the record in the\n"
                        "PseudoGageVal table for query '%s'.\n" , where );
   }

   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
