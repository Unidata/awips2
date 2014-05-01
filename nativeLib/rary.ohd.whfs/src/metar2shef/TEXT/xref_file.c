#define _POSIX_SOURCE
#define MAX_LOOP 10000                                     /* dgb:11/28/97 */
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

/* --------------------------------------------------------------------
  
       FUNCTION
         read_xref

         1.1    NOV 14   David G. Brandon
	        Add files_.national_cat
           
*/

void read_xref()
{

char buffy[20];
int i;

/* tempname = "/home/dgb/apps/metar/alpha/national_category_table.template"; */


    if ( (luns_.just = fopen(files_.national_cat,"r")) == NULL)  /* dgb:11/14/00 */
    {
       printf("\nmetar_decode: Error on opending %s. ABORT!",files_.national_cat);
       exit(1);
    }
    
     i = 0;
     
     while( fgets(buffy,sizeof(buffy),luns_.just) != NULL )
     {
       strcpy(&xref_file[i][0],buffy);
       memset(buffy,0,sizeof(buffy));
       i++;
       if ( i > MAX_XREF-1 ) 
          break;
     }
     if ( luns_.just != NULL )
     {
        fclose(luns_.just);
	luns_.just = 0;
     }
}
