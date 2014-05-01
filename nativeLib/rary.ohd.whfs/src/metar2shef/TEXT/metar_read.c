/* --------------------------------------------------------------------
  
       FUNCTION
         metar_read

       PURPOSE  

         Read data for METAR decoder.        

       VERSION and UPDATES (for this function only)
         1.0    APR 95   David G. Brandon
                Original Version
         1.1	JAN 96   David G. Brandon
                Increase buffer size and checks when SAO
                is on the same line as the WMO line. 
                Add capability to look for SM reports.
         1.2    FEB 21 1996 DGB
                Catch AMOS obs that do not include 'SA', 'SP' or 'RS'
         1.3    FEB 22 1996 DGB
                Fix end of line problem...if there was one or no line 
                feed(s) at the end of the observation, a strchr of the 
                line feed would give a memory exception.
         1.4    FEB 25 96 DGB
                Improve detection search for SM by also searching for
                missing values /////, $$///, etc.
                Allow for phantom character in function 'strpat'.
         1.5    MAR 28 96 DGB
                Add strpat search fow WMO line with the following
                pattern, %%%%$$ %$%% $$$$$$
         1.6    APR 15 96 DGB
                Truncate wmo line to 18 characters.
                Make 'no METAR, RS, SA...etc' comment print only if
                VERBOSE option on. 
         1.7    MAY 7 96 DGB
                Completely rewrote this function to handle the 
                collective capability.  This included the new 
                function, type_of_record.  WMO, ZCZC and NNN lines
                are now skipped over.
         1.8    JUL 3 96 DGB
                Check NO_SPECI flag and bypass specials if flag is on.
         1.9    JUL 8 96 DGB
                Remove statements refering to NO_SPECI flag.  The logic
                and statements have been moved to other routines.
                Change token from " SPECI " to "SPECI " when looking for
                SPECI in METAR obs.  The SPECI was the first item on the
                line it was not being detected before.

                Include three additional checks to determine METAR obs.
                They are, (1) $$$$$KT, $$$$$G$$KT, (3) $$$$$$G$$$KT.
                If one of these patterns is found in the line, after
                the initial checks are done and not verified, the 
                ob is assumed a METAR.
         2.0    NOV 21 96 dgb
                Change the searching algorithm for looking for 'SPECI '.
                Previously, if e.g., NOSPECI was included as a remark
                and was present within the first 30 characters of a line,
                the pattern search would find it, parse of 'SPECI ' and
                assume that this was the start of a special metar
                report.  A more restrictive search pattern was chosen
                to overcome this.  This fix was first thought to be
                the result of a space at the end of each line...this
                was not the case and the interim code was removed.
         2.1    DEC 2 96 DGB
                Add SDO and SCD capabilities.                  
         2.2    NOV 28 97 DGB
                Add checks for endless while loops.
         2.3    May 15 98 DGB
                Check for non-printable characters on the WMO line.
                This is true for use on AWIPS.  Bypass these characters
                when printing the WMO line.
         2.4    Jan 16 00 DGB
	        Change the argument for calling strpat.
	 2.5    FEB 18 00 DGB
	        Add a new way to look for an SM report...i.e. the 5 char
		id is first and not the 3 char id.
 	 2.6    JUN 30 2005  DGB
	        Fix the pattern matching algorithm to determine a SPECI.
 	 2.7    FEB 01 2006  RAE
           Added function prototype for strpat()
            
*--------------------------------------------------------------------- */
#define MAX_LOOP 5000                                      /* dgb:11/28/97 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "mtr.h"
#include "global_external.h"
extern int type_of_record();
extern int metar_error_handler( char function[] );
int FF ;
long record_position;                                       /* dgb:12/03/96 */

extern int DEBUG, VERBOSE;
extern int AO2, AO2A, RAMOS, AWOS, AMOS, OTHER, SA, SP, RS, SM, REMARKS, METAR; 
extern int GLOBAL_EOF,GLOBAL_TYPE;
extern int SDO, SCD;                                       /* dgb:12/02/96 */
extern int strpat(char buffer[],char token[],int numtemp,int chartemp,int either, int phantom,int buffer_len);

int metar_read()
{
    char *str1, *str2, *c;
	short int i, k, n, truth;                           /* dgb:12/18/96 */
    long position, pos; 
    char time_record[SIZE_OF_RECORD];
    int ii;                                                /* dgb:11/28/97 */
    
    if ( DEBUG ) fprintf(stdout,"\nbegin:metar_read");
 
    memset(time_record,0,sizeof(time_record));
    memset(databuf_.record,0,sizeof(databuf_.record));

	for( i = 0; i < SIZE_OF_MSG; i++ )
		databuf_.sdata[i] = ' ';

    i = 0;
    truth = 1;
    ii = 0;                                                /* dgb:11/28/97 */
    while ( truth )
    {
       k = type_of_record();

       if ( k == 2 || k == 8 || k == 10)                   /* dgb:12/02/96 */
            continue;
       else

       if ( k >=3   &&   k <= 7 )                          /* dgb:12/02/96 */
       {

          if ( ( c = strchr(databuf_.record,10)) != NULL )      /* dgb:02/22/96 */
          {
             memset(c,32,1);
             memset(c+1,0,1);
          }

          strncpy(&databuf_.sdata[i],databuf_.record,strlen(databuf_.record));
          i = strlen(databuf_.record);
          n = 0;                                           /* dgb:12/18/96 */
          while ( (k = type_of_record()) == 8  )           /* dgb:12/02/96 */
          {


             if ( ( c = strchr(databuf_.record,10)) != NULL )      /* dgb:02/22/96 */
             {
                memset(c,32,1);
                memset(c+1,0,1);
             }

             strncpy(&databuf_.sdata[i],databuf_.record,strlen(databuf_.record));
             i = i + strlen(databuf_.record);
             n++;                                          /* dgb:12/18/96 */
             if ( n > 6000 ) break;                        /* dgb:12/18/96 protect against endless loop */
          }

          /* back file pointer to beginning of record */
          pos = record_position;                           /* dgb:12/03/96 */
          position = ftell(luns_.lchn);
          fseek(luns_.lchn,-pos,SEEK_CUR);
       }

       truth = 0;
       ii = 0;                                             /* dgb:11/28/97 */
       if ( ii > MAX_LOOP ) metar_error_handler("metar_read"); /* dgb:11/28/97 */
    }

    ii = 0;                                               /* dgb:11/28/97 */
    i = SIZE_OF_MSG-1;
    n = 0;                                                /* dgb:12/18/96 */
    while ( databuf_.sdata[i] == 32 || databuf_.sdata[i] == 10 )
    {
       n++;                                                /* dgb:12/18/96 */
       if ( n > 6000 ) 
       {
          if ( VERBOSE )  fprintf(stdout,"\nEndless loop detecte in metar_read!\n");
          break;
       }
       i--;
       ii++;                                               /* dgb:11/28/97 */
       if ( ii > MAX_LOOP ) metar_error_handler("metar_read"); /* dgb:11/28/97 */
    }

    memset(&databuf_.sdata[i+1],0,1);
    databuf_.len_of_msg = strlen(databuf_.sdata);

    ii = 0;                                                /* dgb:11/28/97 */
    str2 = time_record;
    i = 0;
    /* extract day and time from WMO header line */
    while ( (str1 = strtok(str2," \t\r\n")) != NULL )
    {
      str2 = 0;
      if ( i == 2 ) strcpy(buffer_.wmo,str1);
      i++;
      ii++;                                                /* dgb:11/28/97 */
      if ( ii > MAX_LOOP ) metar_error_handler("metar_read"); /* dgb:11/28/97 */
    }

	return(0);

}

int type_of_record()
{
    char *file_char;
    static char temp_buff[200], temp_buff1[30], temp_buff2[30];
    char temp_rec[SIZE_OF_RECORD], temp_rec1[SIZE_OF_RECORD];
    char time_record[SIZE_OF_RECORD];
    int i, k, blank_on;
    int ii;                                                /* dgb:11/28/97 */
    int kk;                                                /* dgb:05/15/98 */
    memset(temp_buff,0,sizeof(temp_buff));
    memset(temp_buff1,0,sizeof(temp_buff1));
    memset(temp_buff2,0,sizeof(temp_buff2));
    memset(databuf_.record,0,sizeof(databuf_.record));
  
    file_char =  fgets(databuf_.record,SIZE_OF_RECORD,luns_.lchn);

    record_position = strlen(databuf_.record);

   /* 
      scan record and replace any multiple blank areas 
      with a single blank delimeter
   */

   blank_on = 0;
   k = 0;
   i = 0;
   memset(temp_rec,0,sizeof(temp_rec));
   memset(temp_rec1,0,sizeof(temp_rec1));
   strcpy(temp_rec,databuf_.record);

   ii = 0;                                                 /* dgb:11/28/97 */
   while ( i < strlen(temp_rec)+1 )
   {
       if ( blank_on && temp_rec[i] == 32 )
       {
            i++;
            continue;
       }
       else
       if ( temp_rec[i] == 32 )
       {
            temp_rec1[k] = temp_rec[i];
            blank_on = 1;
            i++;
            k++;
            continue;
       }
       else
       {
          temp_rec1[k] = temp_rec[i];
          blank_on = 0;
          i++;
          k++;
       }
      ii++;                                                /* dgb:11/28/97 */
      if ( ii > MAX_LOOP ) metar_error_handler("metar_read"); /* dgb:11/28/97 */
   }

       strcpy(databuf_.record,temp_rec1);
       strncpy(temp_buff,temp_rec1,sizeof(temp_buff));
       strncpy(temp_buff1,temp_rec1,17);
       strncpy(temp_buff2,temp_rec1,25);


       /*  (1) = EOF */
       if ( file_char == NULL )
       {
            GLOBAL_EOF = 1;
            return(1);
       }

       /* (2) = WMO line */
       if ( (kk=strpat(temp_buff,"%%%%$$ %%%% $$$$$$",'$','%',(int)NULL,(int)NULL,strlen(temp_buff) )) >= 0 ) /*dgb:01/16/00 */
            {
            strncpy(time_record,databuf_.record,sizeof(time_record));
            strncpy(buffer_.wmo_line,&temp_buff[kk],18);    /* dgb:05/15/98 */
            return(2);
            }
       else
       /* look for WMO line another way */
       if ( (kk=strpat(temp_buff,"%%%%$$ %$%% $$$$$$",'$','%',(int)NULL,(int)NULL, strlen(temp_buff) )) >= 0 ) /*dgb:01/16/00 */
            {
            strncpy(time_record,databuf_.record,sizeof(time_record));
            strncpy(buffer_.wmo_line,&temp_buff[kk],18);    /* dgb:05/15/98 */
            return(2);
            }
       else
       /* look for WMO line another way */
       if ( (kk=strpat(temp_buff,"%%%%$$ %$%% $$$$$$",'$','%',(int)NULL,(int)NULL,strlen(temp_buff) )) >= 0 ) /*dgb:01/16/00 */
            {
            strncpy(time_record,databuf_.record,sizeof(time_record));
            strncpy(buffer_.wmo_line,&temp_buff[kk],18);    /* dgb:05/15/98 */
            return(2);
            }


        /* (3) = SAO first line */
       if ( GLOBAL_TYPE == 0 || GLOBAL_TYPE == 3 )
       {      
           if ( strstr(temp_buff1," SA ") != NULL )
           { 
              SA = 1;
              strcpy(forms_.type_report,"SA"); 
              GLOBAL_TYPE = 3;
              return(3);
           }
           else
           if ( strstr(temp_buff1," SP ") != NULL )
           {
              SP = 1;
              strcpy(forms_.type_report,"SP");
              GLOBAL_TYPE = 3;
              return(3);
           }
           else
           if ( strstr(temp_buff1," RS ") != NULL )
           {
              RS = 1;
              strcpy(forms_.type_report,"RS");
              GLOBAL_TYPE = 3;
              return(3);
           }
           else
           if ( strstr(temp_buff1," AMOS ") != NULL )  /* dgb:02/21/96 */
           {
              SA = 1;
              strcpy(forms_.type_report,"SA");
              GLOBAL_TYPE = 3;
              return(3);
           }
       }

       /* (4) = begin sm ob */
       if ( GLOBAL_TYPE == 0 || GLOBAL_TYPE == 4 )
       {

          /* add the following patter for sm without 3 char id */      /* dgb: 02/18/00 */
	  /* 70 = alaska, 71 = canada, 72,74 contiguous US */
          if ( strpat(temp_buff2,"72$$$ $$$$$ $$$$$ $$$$$ ",'$','%',(int)NULL,'/',strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              SM = 2;                                       /* dgb: 02/18/00 */
              strcpy(forms_.type_report,"SM");              /* dgb: 02/18/00 */
              GLOBAL_TYPE = 4;                              /* dgb: 02/18/00 */
              return(4);                                    /* dgb: 02/18/00 */
          }
          else
          if ( strpat(temp_buff2,"74$$$ $$$$$ $$$$$ $$$$$ ",'$','%',(int)NULL,'/',strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              SM = 2;                                       /* dgb: 02/18/00 */
              strcpy(forms_.type_report,"SM");              /* dgb: 02/18/00 */
              GLOBAL_TYPE = 4;                              /* dgb: 02/18/00 */
              return(4);                                    /* dgb: 02/18/00 */
          }
          else
          if ( strpat(temp_buff2,"71$$$ $$$$$ $$$$$ $$$$$ ",'$','%',(int)NULL,'/',strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              SM = 2;                                       /* dgb: 02/18/00 */
              strcpy(forms_.type_report,"SM");              /* dgb: 02/18/00 */
              GLOBAL_TYPE = 4;                              /* dgb: 02/18/00 */
              return(4);                                    /* dgb: 02/18/00 */
          }
          else
          if ( strpat(temp_buff2,"70$$$ $$$$$ $$$$$ $$$$$ ",'$','%',(int)NULL,'/',strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              SM = 2;                                       /* dgb: 02/18/00 */
              strcpy(forms_.type_report,"SM");              /* dgb: 02/18/00 */
              GLOBAL_TYPE = 4;                              /* dgb: 02/18/00 */
              return(4);                                    /* dgb: 02/18/00 */
          }
          else
          if ( strpat(temp_buff2,"73$$$ $$$$$ $$$$$ $$$$$ ",'$','%',(int)NULL,'/',strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              SM = 2;                                       /* dgb: 02/18/00 */
              strcpy(forms_.type_report,"SM");              /* dgb: 02/18/00 */
              GLOBAL_TYPE = 4;                              /* dgb: 02/18/00 */
              return(4);                                    /* dgb: 02/18/00 */
          }
          else
          if ( strpat(temp_buff2,"%%% $$$$$ $$$$$ ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              SM = 1;
              strcpy(forms_.type_report,"SM");
              GLOBAL_TYPE = 4;
              return(4);
          }
          else
          if ( strpat(temp_buff2,"%%% $$$$$ $$/// ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              SM = 1;
              strcpy(forms_.type_report,"SM");
              GLOBAL_TYPE = 4;
              return(4);
          }
          else
          if ( strpat(temp_buff2,"%%% $$$$$ ///// ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              SM = 1;
              strcpy(forms_.type_report,"SM");
              GLOBAL_TYPE = 4;
              return(4);
          }
          else
          if ( strpat(temp_buff2,"%%% $$$$$ $$$// ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              SM = 1;
              strcpy(forms_.type_report,"SM");
              GLOBAL_TYPE = 4;
              return(4);
          }
       }

       /* (6) = begin SDO ob */                            /* dgb:12/02/96 */
       if ( GLOBAL_TYPE == 0 || GLOBAL_TYPE == 6 )
       {
          if ( strpat(temp_buff2,"%%%% SDO $$$$ ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              SDO = 1;
              strcpy(forms_.type_report,"SDO");
              GLOBAL_TYPE = 6;
              return(6);
          }
        }


       /* (7) = begin SCD ob */
       if ( GLOBAL_TYPE == 0 || GLOBAL_TYPE == 6 )         /* dgb:12/02/96 */
       {
          if ( strpat(temp_buff2,"%%%% SCD $$$$ ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              SCD = 1;
              strcpy(forms_.type_report,"SCD");
              GLOBAL_TYPE = 6;
              return(6);
          }
       }


       /* (5) = begin metar ob */
       if ( GLOBAL_TYPE == 0 || GLOBAL_TYPE == 5)
       {
          if ( strpat(temp_buff2,"METAR ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              METAR = 1;
              strcpy(forms_.type_report,"METAR");
              GLOBAL_TYPE = 5;
              return(5);
          }
          else
          if ( strpat(temp_buff2,"SPECI %%%% $$$$$$Z",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) /*dgb:06/30/2005 */
          {
              METAR = 1;
              strcpy(forms_.type_report,"SPL METAR");
              GLOBAL_TYPE = 5;
              return(5);
          }
          else
          if ( strpat(temp_buff2," $$$$Z ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) /*dgb:01/16/00 */
          {
              METAR = 1;
              strcpy(forms_.type_report,"METAR");
              GLOBAL_TYPE = 5;
              return(5);
          }

/*          else
          if ( strpat(temp_buff2," $$$$$$Z ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) 
          {
              METAR = 1;
              strcpy(forms_.type_report,"METAR");
              GLOBAL_TYPE = 5;
              return(5);
          }
          else
          if ( strpat(temp_buff2," $$$$$KT ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2 ) )  >= 0 ) 
          {
              METAR = 1;
              strcpy(forms_.type_report,"METAR");
              GLOBAL_TYPE = 5;
              return(5);
          }
          else
          if ( strpat(temp_buff2," $$$$$G$$KT ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) 
          {
              METAR = 1;
              strcpy(forms_.type_report,"METAR");
              GLOBAL_TYPE = 5;
              return(5);
          }
          else
          if ( strpat(temp_buff2," $$$$$$G$$$KT ",'$','%',(int)NULL,(int)NULL,strlen(temp_buff2) )  >= 0 ) 
          {
              METAR = 1;
              strcpy(forms_.type_report,"METAR");
              GLOBAL_TYPE = 5;
              return(5);
          }
*/
       }

       /* (9) = other lines to skip */
       if ( strpat(temp_buff,"NNNN",'$','%',(int)NULL,(int)NULL,strlen(temp_buff) ) >= 0 ) /*dgb:01/16/00 */
            {
            return(9);
            }
       if ( strpat(temp_buff,"ZCZC",'$','%',(int)NULL,(int)NULL,strlen(temp_buff) ) >= 0 ) /*dgb:01/16/00 */
            {
            return(9);
            }

       /* (8) = any other data line */
       return(8);

}
