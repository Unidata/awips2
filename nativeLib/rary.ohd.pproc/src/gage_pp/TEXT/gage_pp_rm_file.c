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
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "gage_pp_log.h"

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

void gage_pp_rm_file ( const char * rmfile )
{
   int status ;
   char msgstr [ MAX_LOG_LENGTH ] ;
   
   /* delete the file */
   status = unlink(rmfile);
   
   if (status != 0)
   {
      sprintf(msgstr, "Error deleting file: %s; errno = %d", 
	      rmfile, errno);
      writelog ( msgstr );
   }
   
   return ;
}

