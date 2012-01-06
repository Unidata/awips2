#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "mtr.h"
#include "global_external.h"
extern int I_FILE;
extern int metarop();
extern int metardrive();
extern void  metarcl();
/*---------------------------------------------------------------------
  
       FUNCTION 
         metar_ifile
  
       PURPOSE  
         Used for the -i filename option.  A single file to be decoded
         can be specified from the command line.
  
       VERSION and UPDATES
         1.0    FEB 22 96   David G. Brandon
                Original Version
         1.1    FEB 27 96   DGB
                Change strcpy to strcat.

  --------------------------------------------------------------------- */

void metar_ifile()
{

   strcpy(stats_.product_name,tempfiles_.ifile);
   strcat(files_.shef_in,"/");
   strcat(files_.shef_in,tempfiles_.ifile);       /* dgb 02/27/96 */

   /* call decoder */
   if ( metarop() == 0 )
   {
     metardrive();
     metarcl();
   }

}
