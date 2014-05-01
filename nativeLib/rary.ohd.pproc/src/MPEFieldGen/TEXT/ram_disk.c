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

#include "mpe_constants.h"
#include "mpe_fieldgen.h"

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
bool use_ram_disk ( )
{
   static bool first = true;
   static bool use_ram = false;
   static char response [ 20 ] = {'\0'};
   register int status;

   if ( first )
   {
      first = false;
      status = getAppsDefaults ( "rfcwide_use_ram_disk", response );

      if ( status == 0 )
      {
         if ( ( * response == 'Y' ) || ( * response == 'y' ) )
         {
            use_ram = true;
         }
      }
   }

   return use_ram;
}

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

const char * get_ram_disk_path ( )
{
   static bool first = true;
   static char ram_path [ PATH_LEN ] = {'\0'};
   register int status;

   if ( first )
   {
      first = false;
      status = getAppsDefaults ( "rfcwide_ramdisk_dir", ram_path ); 
   }

   return ram_path;
} 
