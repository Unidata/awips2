
#define _POSIX_SOURCE
#include <stdio.h>
#include <time.h>
#include <sys/stat.h>
char filename[200];
int how_old;
	
struct stat buf;
extern int DEBUG, VERBOSE;

int is_file_closed(char filename[], int how_old)
{
   /*
   filename   =  file name to look at
   how_old    =  number of seconds to wait before opening file
   */

   struct stat buf;
   long size_1,  current_time;
   int time_space;
   
   /*  Ver 1.0   JUL 95   Dave Brandon
    
       An assumption is made that while files are opened
       they are being written too, and are growing.  This
       is a fair assumption for files being written remotely
       via RCP, or CP. If there is an error on getting the status
       through 'stat' the return is assumed to be opened.
       The tolerance level can be set higher to be more 
       liberal on busy systems.  

       return values   0 = file exists and is closed
                       1 = file either exists and is too young
                           or does not exist for some reason
                           or there is a system problem

       VER 1.1   SEP 95   DGB
       Add capability to wait until the file is at least x minutes
       old, where x is a user defined tolerance.  how_old is the
       variable name, and is in units of minutes.
 
       VER 1.2 DEC 18 1999 dgb
       Scrapped the check for files growing....  Just check the age of the
       file.  It is now checked in seconds, instead of minutes.      
   */

 
   time_space = 1;
    
   /*  get size and time of file   */       
   if ( stat(filename,&buf) == -1 )
        return(1);
   size_1 = buf.st_size;


   /*  see if file is old enough to look at */
   current_time = time(NULL);

  if  ( current_time - buf.st_ctime < time_space * how_old )
  {
       return(1);
  }

   return(0);

}

