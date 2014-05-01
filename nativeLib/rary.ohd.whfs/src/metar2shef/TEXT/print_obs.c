/* --------------------------------------------------------------------
  
       FUNCTION
         print_obs

       PURPOSE  

         Print the input buffer to the stdout, error file or both.        

       VERSION and UPDATES (for this function only)
         1.0    FEB 96   David G. Brandon
                Original Version
         1.1    DEC 2 96 DGB
                Incoporate changes to accomodate SDO and SCD type obs.
         1.2    NOV 28 97 DGB
                Add check for endless while loops.
         1.3    FEB 01 2006 RAE
                Changed function return type from int to void since it
                does not return a value.
 *--------------------------------------------------------------------- */

#define MAX_LOOP 5000                                      /* dgb:11/28/97 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "mtr.h"
#include "global_external.h"
extern int metar_error_handler( char function[] );
extern int DEBUG, VERBOSE;
extern int AO2, AO2A, RAMOS, AWOS, AMOS, OTHER, SA, SP, RS, SM, REMARKS, METAR; 
extern int WMO;
extern int SDO, SCD;                                           /* dgb:12/02/96 */

void print_obs( FILE *fp, int comment, int header )
{
    int ii, jj;                                            /* dgb:11/28/97 */
    static short int i, k;
    char temp_buff[SIZE_OF_MSG];                           /* dgb:12/02/96 */
    
    char spot = ':';

    memset(temp_buff,0,sizeof(temp_buff));

    if ( SDO || SCD )                                       /* dgb:12/02/96 */
      strcpy(temp_buff,databuf_.remarks);                   /* dgb:12/02/96 */
    else
       strcpy(temp_buff,databuf_.sdata);                    /* dgb:12/02/96 */

    if ( comment )
    {
         if ( header )
         {

            fprintf(fp,"\n%c----------------------------",spot);
            fprintf(fp,"\n%c INPUT: OBSERVATION",spot);
            fprintf(fp,"\n%c----------------------------\n\n",spot);

            if ( WMO )
              fprintf(fp,"%cZCZC %-9.9s\n%s\n",spot,stats_.product_name,buffer_.wmo_line);
         }
         
         k = 0;
         i = 0;
         fprintf(fp,"%c",spot);
         ii = 0;                                           /* dgb:11/28/97 */
         while( i < strlen(temp_buff) )                    /* dgb:12/02/96 */
         {
            if ( k < 65 )
            { 
               if ( temp_buff[i] != '\0'  )                 /* dgb:12/02/96 */
                    fprintf(fp,"%c",temp_buff[i]);          /* dgb:12/02/96 */
               else
                    break;
               i++;
               k++;
            }
            else
            {

               if ( temp_buff[i] != ' ' )                   /* dgb:12/02/96 */
               {
                  jj = 0;                                   /* dgb:11/28/97 */
                  while( temp_buff[i] != ' ')               /* dgb:12/02/96 */
                  {                  
                     if ( temp_buff[i] != '\0' )            /* dgb:12/02/96 */
                          fprintf(fp,"%c",temp_buff[i]);    /* dgb:12/02/96 */
                     else
                          break;
                     i++;
                     jj++;                                 /* dgb:11/28/97 */
                     if ( jj > MAX_LOOP ) metar_error_handler("print_obs"); /* dgb:11/28/97 */
                  }
               }

                    fprintf(fp,"\n%c",spot);
               k = 0;
            }
            ii++;                                          /* dgb:11/28/97 */
            if ( ii > MAX_LOOP ) metar_error_handler("print_obs"); /* dgb:11/28/97 */
         }

         if ( header )
            if ( WMO ) fprintf(fp,"\n\n%cNNNN\n",spot);
         fprintf(fp,"\n");


     if ( fp != NULL )
          fflush(fp);

    }
    else
    {
         if ( header )
         {

            fprintf(fp,"\n----------------------------");
            fprintf(fp,"\n INPUT: OBSERVATION");
            fprintf(fp,"\n----------------------------\n\n");

            if ( WMO )
              fprintf(fp,"ZCZC %-9.9s\n%s\n",stats_.product_name,buffer_.wmo_line);
         }
        
         k = 0;
         i = 0;
         ii = 0;                                           /* dgb:11/28/97 */
         while( i < strlen(temp_buff))                     /* dgb:12/02/96 */
         {
            if ( k < 65 )
            { 

               if ( temp_buff[i] != '\0' )                  /* dgb:12/02/96 */
                    fprintf(fp,"%c",temp_buff[i] );         /* dgb:12/02/96 */
               else
                   break;
               i++;
               k++;
            }
            else
            {

               if ( temp_buff[i] != ' ' )                   /* dgb:12/02/96 */
               {
                  jj = 0;                                   /* dgb:11/28/97 */
                  while( temp_buff[i] != ' ')               /* dgb:12/02/96 */
                  {
                     if ( temp_buff[i] != '\0' )            /* dgb:12/02/96 */
                          fprintf(fp,"%c",temp_buff[i]);    /* dgb:12/02/96 */
                     else
                         break;
                     i++;
                     jj++;                                 /* dgb:11/28/97 */
                     if ( jj > MAX_LOOP ) metar_error_handler("print_obs"); /* dgb:11/28/97 */
                  }
               }

               fprintf(fp,"\n");
               k = 0;
            }
            ii++;                                          /* dgb:11/28/97 */
            if ( ii > MAX_LOOP ) metar_error_handler("print_obs"); /* dgb:11/28/97 */
         }

         if ( header )
            if ( WMO ) fprintf(fp,"\n\nNNNN\n");
         fprintf(fp,"\n");


     if ( fp != NULL )
          fflush(fp);

    }

} /* end of function */

