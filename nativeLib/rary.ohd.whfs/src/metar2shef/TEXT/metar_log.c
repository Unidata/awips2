#define _POSIX_SOURCE
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include "mtr.h"
#include "global_external.h"
/*---------------------------------------------------------------------
 
      FUNCTION:  metar_log
 
      PURPOSE:  To log information on current file being processed.

      VERSION and UPDATES
        1.0    NOV 20 04   David G. Brandon
    
 *--------------------------------------------------------------------- */
extern int print_obs( FILE *fp, int comment, int header );

void metar_log( )
{
   FILE *fperr;
   char temp[100], buff_temp[100], tmp[30];
   long current_time;


    /*  get date/time and construct appended part of filename */
    strcpy(tmp,"latest_metar_file");
    current_time = time(NULL);
    strftime(buff_temp,sizeof(buff_temp),"%m%d.%H%M%S",gmtime(&current_time));
          sprintf(temp,"%s/%s",
          tempfiles_.shef_decode_err,tmp);
    if ( ( fperr = fopen(temp,"w") ) != NULL )
    {
       fprintf(fperr,"\n %s %s\n",stats_.product_name,buff_temp);
       print_obs( fperr, 1,0 );
       fflush(fperr);
       fclose(fperr);
    }
}
