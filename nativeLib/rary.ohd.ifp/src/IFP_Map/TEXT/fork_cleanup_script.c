/*.......................................
* File: fork_cleanup_script.c
* Author(s): DHM Team
* Date Created: 5/5/07
* Development group: OHD HSEB
* Purpose: This subroutine calls the ifp_cleanup script to remove any user DHM Grids
* Module(s): 
*     fork_cleanup_script()
* Module(s) Used:
*     system()
* Modified:
*  */

#define PATH_LENGTH 100

#include <stdio.h>
#include <string.h> 
#include <stdlib.h>
 
void fork_cleanup_script()
{
   int     len, len2; 
   char    scripts_path[PATH_LENGTH];
   len = strlen("ifp_scripts_dir");
   get_apps_defaults("ifp_scripts_dir", &len, scripts_path, &len2);
   strcat(scripts_path, "/ifp_cleanup");
   printf("start ifp cleanup script... in %s\n",scripts_path);
   system(scripts_path);
   printf("finished ifp cleanup script... in %s\n",scripts_path); 
  return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
