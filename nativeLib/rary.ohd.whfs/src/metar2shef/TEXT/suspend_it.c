#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

/*---------------------------------------------------------------------
 
      FUNCTION:  suspend_it
 
      PURPOSE:  To check if background processes started in proc_loop
                should be suspended.  This function will be called
                before executing each program started in proc_loop.

                return values:  0 = file does not exist
                                1 = file exists

      VERSION and UPDATES
        1.0    SEP 95   David G. Brandon
               Original Version
     
 *--------------------------------------------------------------------- */


int suspend_it( char dirname[] )
{
   /*
       dirname = name of directory to look in
   */

   char buff_temp[100];
   char *suspendfile = "suspend";

   struct stat buf;

   sprintf(buff_temp,"%s/%s",dirname,suspendfile);

   /* Check for existence of the file */
   if ( stat(buff_temp,&buf) == -1 )
        return(0);
   else
        return(1);

}
