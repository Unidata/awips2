/*******************************************************************************
* FILENAME:            update_numpseudo_RFCW.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          update_numpseudo_RFCW
* DESCRIPTION:         Updates the number of pseudo gages in the
*                      RWResult table. 
*
* ORIGINAL AUTHOR:     Bryon Lawremce
* CREATION DATE:       November 4, 2004
* ORGANIZATION:        HSEB/OHD
* OPERATING SYSTEM:    Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/4/2004    Bryon Lawrence    Upgraded to use DBGen.
********************************************************************************
*/
#include <stdio.h>
#include <string.h>

#include "dbmserrs.h"
#include "mpe_log_utils.h"
#include "read_numpseudo_RFCW.h"
#include "RWResult.h"
#include "stage3.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   update_numpseudo_RFCW
* PURPOSE:       This function updates the number of pseudo gages in the
*                RWResult table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME            DESCRIPTION/UNITS
*   Input  char *      datetime        The datetime of the pseudo gage.    
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME               HEADER FILE    DESCRIPTION
*   FreeRWResult       RWResult.h     Frees linked list of RWResult structures.
*   GetRWResult        RWResult.h     Retrieves records of RWResult table 
*                                     using supplied where clause.  Returns
*                                     rows as linked list of RWResult 
*                                     structures.
*   UpdateRWResult     RWResult.h     Updates a record in the RWResult table.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME          DESCRIPTION
*  char [ ]    RFC           The name of the office the pseudo gage data is
*                            being stored for.
*  char [ ]    where         Contains the where clause for retrieving and
*                            updating records in the RWResult table.
*  int         status        Indicates update success or failure.
*  RWResult *  pRWResultHead A pointer to the head of the RWResult linked list. 
*  RWResult *  pRWResultNode A pointer to a node in the RWResult linked list.
*
* DATA FILES AND/OR DATABASE:
*   Requires an open connection to the IHFS database.
*   Reads and Updates the RWResult table.
*
* ERROR HANDLING:
*   Indicates if the update of the RWResult table failed.
*
* CALLED BY:
*   insert_pseudo_RFCW
*
********************************************************************************
*/
void update_numpseudo_RFCW ( const char * datetime )
{
   extern char RFC [ ] ;
   char where [ 100 ];
   int status ;
   RWResult * pRWResultHead = NULL ;
   RWResult * pRWResultNode = NULL ;

/*-------------------------------------------------------------------------*/
/*  select number of pseudo gages previously defined from RWResult record  */
/*  if record not found, then return without updating                      */
/*-------------------------------------------------------------------------*/

   memset ( where , '\0', 100 );

   /* Build the where clause. */
   sprintf ( where , "WHERE rfc='%s' AND obstime='%s'",
             RFC , datetime );

   /* Retrieve the data from RWResult. */
   pRWResultHead = GetRWResult ( where ) ;

   if ( pRWResultHead == NULL )
   {
     flogMessage ( stderr, "In routine 'update_numpsuedo_RFCW':\n"
                       "Error attempting to find record in RWResult table\n"
                       "for rfc='%s' and obstime='%s'.\n" , RFC, datetime );
     return;
   }

   pRWResultNode = ( RWResult * ) ListFirst ( & pRWResultHead->list );

   /* Increment the number of pseudo gages by one. */
   pRWResultNode->num_pseudo_gages++ ;

   /*-----------------------------*/
   /*  update RWResult record     */
   /*-----------------------------*/

   /* Update the record in RWResult. */
   status = UpdateRWResult ( pRWResultNode, where ); 

   if ( status != ERR_OK )
   {
      flogMessage ( stderr, "In routine 'update_numpseudo_RFCW':\n"
                        "postgres error#= %d attempting to update\n"
                        "RWResult table.\n", status );
   }

   /* Free memory uese by RWRResult linked list. */
   FreeRWResult ( pRWResultHead ) ;
   pRWResultHead = NULL ;

   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
