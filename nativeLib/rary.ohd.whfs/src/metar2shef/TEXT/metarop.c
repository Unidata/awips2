#define MAX_LOOP 5000                                      /* dgb:11/28/97 */
#define _POSIX_SOURCE
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/stat.h>
#include "mtr.h"
#include "global_external.h"

extern int I_FILE, VERBOSE, DEBUG, i_total;       /* dgb 12/13/99 */
extern void check_dtg();
 
/*---------------------------------------------------------------------
  
       NAME
         metarop
  
       PURPOSE                                                    
         Open files the data file for the metar decoder.
  
       VERSION and UPDATES
         1.0    JUN 95   David G. Brandon  
                Original Version
         1.1    FEB 96   DGB
                Include code for '-i filename ' option.
         1.2    MAY 28 96 DGB
                Include switch "-oh".  This allows the capability
                to produce names compatible with the OH shef decoder.
         1.3    DEC 13 99 DGB
                Provided a capability to add an extension on the err
                and out files byond that of .mmdd.hrmnsc.  There were
                some file contentions if the files had the same 
                9 character prefix.  The extension will be a counter
                that goes from 01 to 99.
         1.4    JAN 10 00 DGB
	        Add check_dtg to check for date override from -x switch.
                Remove references and code for -oh option.         
 *--------------------------------------------------------------------- */


int /*FUNCTION*/ metarop()
{   
    char  buff_temp[50], buff_temp2[50];
    long current_time;
    void date_time();
    short *const Idate = &datim_.idate[0] - 1;

    memset(tempfiles_.shef_in,0,sizeof(tempfiles_.shef_in));
    strcpy(tempfiles_.shef_in,files_.shef_in);

    memset(tempfiles_.shef_decode_err,0,sizeof(tempfiles_.shef_decode_err));
    strcpy(tempfiles_.shef_decode_err,files_.shef_decode_err);

    memset(tempfiles_.shef_out,0,sizeof(tempfiles_.shef_out));
    strcpy(tempfiles_.shef_out,files_.shef_out);

    memset(tempfiles_.shef_log,0,sizeof(tempfiles_.shef_log));
    strcpy(tempfiles_.shef_log,files_.shef_log);

    memset(tempfiles_.log_file,0,sizeof(tempfiles_.log_file));
    memset(tempfiles_.err_file,0,sizeof(tempfiles_.err_file));


	/*             Get the Current Date
	 *             This will vary depending upon the operating system
	 *             used.  Place the year, month, day, etc. into the
	 *             array values as follows:
	 *               IDATE(1) = YEAR(4 digit year, e.g., 1994
	 *               IDATE(2) = MONTH
	 *               IDATE(3) = DAY)
	 *               IDATE(4) = HOUR
	 *               IDATE(5) = MINUTE
	 *               IDATE(6) = SECOND
     */

	 date_time( &Idate[1], &Idate[2], &Idate[3], &Idate[4], &Idate[5], 
	            &Idate[6] );       
         
	 check_dtg( );                            /* dgb:01/10/00 */

    /*  get date/time and construct appended part of filename */

    current_time = time(NULL);
    memset(buff_temp,0,sizeof(buff_temp));
    memset(buff_temp2,0,sizeof(buff_temp2));
    strftime(buff_temp2,sizeof(buff_temp2),".%m%d.%H%M%S",gmtime(&current_time));
    sprintf(buff_temp,"%s.%02d",buff_temp2,i_total); /* dgb:12/13/99 */

    /*  Open the metar_in file */

    if ( ( luns_.lchn = fopen(files_.shef_in,"r") ) == NULL )
		goto L_900;

	/*             Open a sao_in file if SAOOUT is set to '+'.
                   The file name will be constructed from the
                   name of the sao_out file in the metar.cfg file
                   with the datetime stamp.
        */

    cont_.out_flag[0] = '+';
	if( cont_.out_flag[0] == '+' )
	{

            /*  dgb: 09/20/95 added change to out file name */
            if ( test_.test_flag  )
            {
               sprintf(tempfiles_.log_file,"%s%s",
                       tempfiles_.shef_out,buff_temp);
            }
            else
            {
                  if ( strlen(stats_.product_name) >= 9 )
                  {
                     sprintf(tempfiles_.log_file,"%s/out.%-9.9s%s",
                     tempfiles_.shef_out,stats_.product_name,buff_temp);
                  }
                  else
                  {
                     sprintf(tempfiles_.log_file,"%s/out.%s%s",
                     tempfiles_.shef_out,stats_.product_name,buff_temp);
                  }
            }

            /* open as ascii file */

           if ( (luns_.jchn=fopen(tempfiles_.log_file,"w")) == NULL )
                 goto L_920;
         
           /* print to screen if in test mode */
           if ( test_.test_flag )
                fprintf(stdout,"  shef_out file: %s\n",tempfiles_.log_file);
    }

			/*  Open the sao_decode_err file */
            /* dgb: 09/20/95 added change to err file name */
            if ( test_.test_flag )
                 sprintf(tempfiles_.err_file,"%s%s",
                       tempfiles_.shef_decode_err,buff_temp);
            else
            {
                 if ( strlen(stats_.product_name) >= 9 )
                 {
                    sprintf(tempfiles_.err_file,"%s/err.%-9.9s%s",
                    tempfiles_.shef_decode_err,stats_.product_name,buff_temp);
                 }
                 else
                 {
                    sprintf(tempfiles_.err_file,"%s/err.%s%s",
                    tempfiles_.shef_decode_err,stats_.product_name,buff_temp);
                 }


            }
            if ( ( luns_.icher = fopen(tempfiles_.err_file,"w") ) == NULL )
	        	goto L_910;

            /* print to screen if in test mode */
            if ( test_.test_flag )
               fprintf(stdout,"  error file:    %s\n",tempfiles_.err_file);

	/*             OK - Thats All */

	return(0);


L_900:
    fprintf(stdout,"\nmetar_decode:error on opening input file --> %s",
    files_.shef_in);
        return(1);

L_910:
    fprintf(stdout,"\nmetar_decode:error on opening error file --> %s",
    tempfiles_.err_file);
	exit(0);

L_920:
    fprintf(stdout,"\nmetar_decode:error on opening ouput file --> %s",
    files_.shef_out);
	exit(0);


} /* end of function */

