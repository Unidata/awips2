#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <time.h>
#include "mtr.h"
#include "global_external.h"
extern int print_obs( FILE *fp, int comment, int header );
/*---------------------------------------------------------------------
 
      FUNCTION:  metar_error_handler
 
      PURPOSE:  To print out bad unrecoverable errors.

      VERSION and UPDATES
        1.0    NOV 28 97   David G. Brandon
               Original Version
        1.1    DEC 14 97 DGB
               Print name of looperr file for test option. 
        1.2    SEP 14 05 RAE
               Move "bad" input file from inout directory so it doesn't
               get processed again. 
     
 *--------------------------------------------------------------------- */


int metar_error_handler( char function[] )
{
   FILE *fperr;
   char temp[100], buff_temp[100];
   long current_time;

    /*  get date/time and construct appended part of filename */
    current_time = time(NULL);
    strftime(buff_temp,sizeof(buff_temp),".%m%d.%H%M%S",gmtime(&current_time));

    if ( test_.test_flag )
    {
        if ( strlen(stats_.product_name) >= 9 )
        {
           sprintf(temp,"looperr.%-9.9s%s",stats_.product_name,buff_temp);
        }
        else
           sprintf(temp,"looperr.%s%s",stats_.product_name,buff_temp);
    }
    else
    {
       if ( strlen(stats_.product_name) >= 9 )
       {
          sprintf(temp,"%s/looperr.%-9.9s%s",
          tempfiles_.shef_decode_err,stats_.product_name,buff_temp);
       }
       else
       {
          sprintf(temp,"%s/looperr.%s%s",
          tempfiles_.shef_decode_err,stats_.product_name,buff_temp);
       }
    }

    if ( ( fperr = fopen(temp,"w") ) != NULL )
    {

       fprintf(fperr,"\nEndless loop error in function: %s for file: %s\n",function,stats_.product_name);

       fprintf(fperr,"Removing input file: %s\n", files_.shef_in);
       if ( luns_.lchn != NULL )                            /* rae:09/14/05 */
       {
          fclose( luns_.lchn );
          remove( files_.shef_in );
       }

       print_obs( fperr, 1,0 );
       if ( test_.test_flag )                              /* dgb:12/14/97 dgb */
       {
          fprintf(stdout,"  loop error:    %s\n",temp);    /* dgb:12/14/97 dgb */
       }
       fflush(fperr);
       exit(1);
    }
    else
    {
       fprintf(stdout,"\nError in opening error file in metar_error_handler");
       fflush(stdout);
       exit(1);
    }
}
