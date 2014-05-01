
/*******************************************************************************
* FILENAME:            check_autosave.c
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:
*   MODULE 1:          check_autosave
* DESCRIPTION:         Checks the auto_save field of the IHFS RWResult table.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       March 14, 2005
* ORGANIZATION:        OHD-11, HSEB
* MACHINE:             Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        3/14/2005    Bryon Lawrence    Original Coding
********************************************************************************
*/
#include <stdio.h>
#include <string.h>

#include "GeneralUtil.h"
#include "RWResult.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   check_autosave
* PURPOSE:       Checks the auto save field of a record in the
*                RWResult tables.  The ioverwrt flag is set to 
*                indicate whether the auto save field is 'T' or 
*                'F'.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*   Input  char *      rfcname              The id of the site this program
*                                           is being run at. 
*   Input  char [ ]    dt                   The datetime of the MPE run.
*                                           YYYY-MM-DD HH:00:00
* RETURNS:
*  Void 
*
* APIs UTILIZED:
*   NAME                  HEADER FILE       DESCRIPTION
*   GetRWResult           GetRWResult.h     DBGen routine for projections
*                                           on the RWResult table.
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME           DESCRIPTION
*   char       datetime []    Contains a C copy of the FORTRAN formatted
*                             dt string.
*   char       rrfc []        Contains a C copy of the FORTRAN formatted
*                             rfcname string.
*   char       where []       The where clause passed into GetRWResult.  Used
*                             to get a projection from the RWResult table.
*   RWResult * pRWResult      Points to the results from the RWResult table.
*
* DATA FILES AND/OR DATABASE:
*   Needs the RWResult table in the IHFS database.
*
* ERROR HANDLING:
*   None.
*   
********************************************************************************
*/
void MPEFieldGen_check_autosave ( const char * rfcname, const int * rfclen, 
                      const char dt[ANSI_YEARSEC_TIME_LEN],
		      const int * datelen,
                      int * ioverwrt )
{
   char datetime [ *datelen + 1 ];
   char rrfc [ *rfclen + 1];
   char where [ 100 ];
   RWResult * pRWResult = NULL;

   /* Initialize the overwrite flag.
      A value of 0 means either the record does not exist in the RWResult
      table for the rfc/datetime combination or it does but the auto save
      field is set to T.  A value of 1 means the record exists and the
      auto_save field is set to F. */
   * ioverwrt = 0;

   /* Prepare the RFC name. Translate it from a FORTRAN string into 
      a C string. */
   memset ( rrfc, '\0', *rfclen + 1 );
   strncpy ( rrfc, rfcname, *rfclen );  

   /* Remove trailing spaces. */
   strip_tblanks ( rrfc );

   /* Prepare the datetime. Translate it from a FORTRAN string into
      a C string. */
   memset ( datetime, '\0', *datelen + 1 );
   strncpy ( datetime, dt, *datelen );

   /* Remove trailing spaces. */
   strip_tblanks ( datetime );

   /* Build the where clause. */
   sprintf ( where, "WHERE rfc = '%s' AND obstime = '%s'",
                    rrfc, datetime );

   /* Retrieve the record from the RWResult table for this
      rfcname and date. */
   pRWResult = GetRWResult ( where );

   if ( pRWResult != NULL )
   {
      /* Check the value of the auto_save field. */
      if ( pRWResult->auto_save[0] == 'F' )
      {
         * ioverwrt = 1;
      }

      /* Free the memory used by the RWResult table. */
      FreeRWResult ( pRWResult );
      pRWResult = NULL;
   }

   return;
}
