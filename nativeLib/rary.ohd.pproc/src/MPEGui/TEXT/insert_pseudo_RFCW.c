/*******************************************************************************
* FILENAME:            insert_pseudo_RFCW.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          insert_pseudo_RFCW
* DESCRIPTION:         Inserts a pseudo gage value into the 
*                      PseudoGageVal table.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       November 4, 2004
* ORGANIZATION:        HSEB/OHD
* OPERATING SYSTEM:    Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER      DESCRIPTION/REASON
*          1        11/4/2004    Bryon Lawrence  Converted to use DBgen
********************************************************************************
*/

#include <stdio.h>
#include <string.h>

#include "DbmsDefs.h"
#include "dbmserrs.h"
#include "insert_pseudo_RFCW.h"
#include "PseudoGageVal.h"
#include "read_numpseudo_RFCW.h"
#include "time_convert.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   insert_pseudo_RFCW
* PURPOSE:       Inserts a pseudo gage value into the PseudoGageVal table.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME     DESCRIPTION/UNITS
*   Input  char *      gid      The pseudo gage identifier.
*   Input  char *      dt       The date time of the pseudo gage observation.
*   Input  float       lat      The latitude of the pseudo gage.
*   Input  float       lon      The longitude of the pseud gage.
*   Input  float       val      The value of the pseudo gage.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE     NAME          DESCRIPTION
*   int           status        The return code of the insert.
*   PseudoGageVal pseudoStruct  Contains the record to be inserted into the
*                               PseudoGageVal table.     
*
* DATA FILES AND/OR DATABASE:  
* Requires an open connetion to the IHFS database.
* Inserts into the PseudoGageval table.
* Updates the RWResult table.
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
void insert_pseudo_RFCW ( const char * gid, char *dt, float lat, float lon, 
                          float val )
{
   int status ;
   PseudoGageVal pseudoStruct ;

   memset ( pseudoStruct.pseudo_gage_id, '\0', LOC_ID_LEN + 1 ) ;
   strncpy ( pseudoStruct.pseudo_gage_id, gid, LOC_ID_LEN );

   yearsec_ansi_to_dt ( dt, & pseudoStruct.obstime );

   pseudoStruct.lat = lat;
   pseudoStruct.lon = lon;
   pseudoStruct.gage_value = val;
   strcpy ( pseudoStruct.man_edited, "F" ) ;
   pseudoStruct.prev_gage_value = -99.0 ;

   /* Attempt the insert. */
   /* Begin a transaction here.*/
   status = PutPseudoGageVal ( & pseudoStruct ) ;

   if ( status == ERR_OK )
   {
      /* The insert was successful. Update the record in the
         Pseudogage Table. */
      update_numpseudo_RFCW ( dt );

      /* Complete the transaction here if the update was successful. */
      /* Otherwise, rollback. */
   }
   /* Otherwise, rollback. */

   return ;
}
