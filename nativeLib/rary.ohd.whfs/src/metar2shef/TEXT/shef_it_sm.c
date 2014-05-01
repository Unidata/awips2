/*-------------------------------------------------------------------

   TITLE:    shef_it_sm                                                 
             Used in conjunction with the sao_decode software
             to encode sm reports into SHEF.        
                                                                     
   PURPOSE:  To print out sm reports into SHEF.                        
                                                                     
   AUTHOR:   David G. Brandon, CBRFC, Salt Lake City, UT             
                                                                     
   VERSION:  
             1.0   APR 1 1996 DGB
                   Adapted from original shef_it program.  Split up
                   SAO and SM obs for easier handling.  Refer to shef_it.c
                   for changes.
             1.1   APR 15 1996 dgb
                   Add a line feed at the end of the ZCZC wmo header line.
                   Remove format specifier, %-9.9s, from ZCZC line.
                   Add computation of precipitation values for codes
                   990 thru 999 (0 mm thru 0.9 mm).  Translate 9999 for
                   the 24 hr amount to 0.
                   Change current precip type from 'XW' to 'PT'.
                   Add PD (pressure charactersitc), PE (pressure tendency) 
                   PA (Station Pressure), PL (SEA Level Pressure) and 
                   XV (Prevaling surface visibility).
             1.2   APR 17 96 DGB
                   Change shef code 'PD' to 'PDIR' to make it unique
                   in the shefstr.pecodes.  Before it would print out if the
                   PPD was set, since it found PD in the PPD string.
             1.3   APR 23 96 DGB
                   Change code 'PT' to 'PTIR" to make it unique 
                   in the shefstr.pecodes.  Before it would print out if the
                   PPT code was set, since if found PT in the PPT string.
             1.4   JUL 12 96
                   Change shef code from "TA" to "TAIRZZ" for 
                   screening.
             1.5   JUL 28 96 DGB
                   Add check for -pall6 and -pall24 options.             
             1.6   SEP 5 96 DGb
                   Tighten up bounds for -pall24 switch to 11:40 - 12:30
             1.7   DEC  8 96 DGB
                   Correct algorithm for converting centigrade to
                   fahrenheit.
             1.8   FEB 27 97 DGB
                   Add a blank at the end of the pecodes for checking.
                   Change peak wind from YU to UP.
             1.9   AUG 13 dgb
                   Add 'y2k' switch.
             2.0   JAN 3 98 dgb
                   Change shef code for peak gust from 'UP' to 'UG'.
                   Make changes to accomodate switch '-kt'.
             2.1   FEB 18 2000 DGB
	           Handle 5 digit id in sm reports in lieu of 3 char id.
             2.2   FEB 1 2006 RAE
	                Modified code to get rid of compilation warnings.
 ---------------------------------------------------------------------*/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "mtr.h"
#include "global_external.h"

#define  VAL_CHARS  27

extern int  METRIC, DEBUG, VERBOSE, WMO, SM, REVISION, SP, P24, P6,
            AMOS, RAMOS, AWOS;
extern int PALL6, PALL24;                                   /* dgb:07/28/96 */
extern int Y2K;                                             /* dgb:08/13/98 */
extern int KNOTS;                                           /* dgb:01/03/98 */
extern int SALIAS;                                          /* dgb:02/28/00 */
extern int print_obs( FILE *fp, int comment, int header );
extern int strpat(char buffer[],char token[],int numtemp,int chartemp,int either, int phantom,int buffer_len);

void shef_it_sm()

{



   float values[VAL_CHARS], temp_precip;                   /* dgb: 04/15/96 */

   int pe_flag[VAL_CHARS], sm_time6[10], sm_time3[8], addon ;
   int j,i=0 ;
   int tempyear, tempyeardc;                               /* dgb:08/13/98 */
   int notfound;                                           /* dgb:02/18/00 */
   char  buff_temp[80], dota[4]; 
   unsigned char hb5_id[10], pc[9];




   /* When adding addtional PEDTSEP codes, ensure that the VAL_CHARS
      is updated, and the check for pe_buffer is updated 
   */

char pe_buffer[VAL_CHARS][8] = 
   {                                     /* order   description               */
     {'T','A','I','R','Z','Z','Z','0'},  /*  0      Temperature Instantaneous */
     {'T','D','I','R','Z','Z','Z','0'},  /*  1      Dew Point                 */
     {'U','S','I','R','Z','Z','Z','0'},  /*  2      Wind Speed                */
     {'U','D','I','R','Z','Z','Z','0'},  /*  3      Wind Direction            */
     {'U','G','I','R','Z','Z','Z','0'},  /*  4      Peak Wind/Gust            */ /* dgb:0227/97 */
     {'U','Q','I','R','Z','Z','Z','0'},  /*  5      Speed/Direction sss.sddd  */
     {'P','A','I','R','Z','Z','Z','0'},  /*  6      Altimeter                 */
     {'P','P','Q','R','Z','Z','Z','0'},  /*  7      Precip 6 hour             */
     {'P','P','D','R','Z','Z','Z','0'},  /*  8      Precip 24 hour            */
     {'T','A','I','R','Z','X','Z','0'},  /*  9      Temperature Max 24 hour   */
     {'T','A','I','R','Z','N','Z','0'},  /* 10      Temperature Min 24 hour   */
     {'P','P','H','R','Z','Z','Z','0'},  /* 11      Precip 1 hour             */
     {'S','D','I','R','Z','Z','Z','0'},  /* 12      Snow depth on ground      */
     {'S','W','I','R','Z','Z','Z','0'},  /* 13      Water Equivalent          */
     {'R','T','I','R','Z','Z','Z','0'},  /* 14      Sunshine                  */
     {'T','A','I','R','Z','R','Z','0'},  /* 15      Max Temp past 6 hours     */
     {'T','A','I','R','Z','H','Z','0'},  /* 16      Min Temp past 6 hours     */
     {'P','P','T','R','Z','Z','Z','0'},  /* 17      Precip past 3 hours       */
     {'T','A','I','R','Z','Y','Z','0'},  /* 18      Max Temp past 12 hours    */
     {'T','A','I','R','Z','P','Z','0'},  /* 19      Min Temp past 12 hours    */
     {'P','C','I','R','Z','Z','Z','0'},  /* 20      PRECIP accumulator        */
     {'P','T','I','R','Z','Z','Z','0'},  /* 21      Precip Type               */ /* dgb:04/15/96 */
     {'P','L','I','R','Z','Z','Z','0'},  /* 22      Pressure sea level        */ /* dgb:04/15/96 */
     {'P','D','I','R','Z','Z','Z','0'},  /* 23      Pressure characteristic   */ /* dgb:04/15/96 */
     {'P','E','I','R','Z','Z','Z','0'},  /* 24      Pressure tendency         */ /* dgb:04/15/96 */
     {'X','V','I','R','Z','Z','Z','0'},  /* 25      Visibility surface        */ /* dgb:04/15/96 */
     {'H','G','I','R','Z','Z','Z','0'}   /* 26      River Stage               */ /* dgb:04/15/96 */

               
   };

   /* store sm_time6 values for time window */
      sm_time6[0] =  530;
      sm_time6[1] =  830;
      sm_time6[2] = 1130;
      sm_time6[3] = 1430;
      sm_time6[4] = 1730;
      sm_time6[5] = 2030;
      sm_time6[6] = 2330;
      sm_time6[7] = 2400;
      sm_time6[8] =    0;
      sm_time6[9] =  230;

   /* store sm_time3 values for time window */
      sm_time3[0] =  230;
      sm_time3[1] =  530;
      sm_time3[2] =  830;
      sm_time3[3] = 1130;
      sm_time3[4] = 1430;
      sm_time3[5] = 1730;
      sm_time3[6] = 2030;
      sm_time3[7] = 2330;


   /* Construct the handbook 5 id  */ 
      if ( SM == 1 )
      {
       hb5_id[0]  = (char) buffer_.ibuf[1];       /*  1st Char of ID  */
       hb5_id[1]  = (char) buffer_.ibuf[2];
       hb5_id[2]  = (char) buffer_.ibuf[3];

       for ( i = 3; i < 11; i++ )
             hb5_id[i] = 0;
       for (i=0; i < 3; i++)
          if(hb5_id[i] == ' ')
             hb5_id[i] =  0;
     }
     else
     if ( SM == 2 )                                         /* dgb:02/18/00 */
     {                                                      /* dgb:02/18/00 */
        strcpy(hb5_id,sm.s000);                             /* dgb:02/18/00 */
        if ( SALIAS )                                       /* dgb:02/18/00 */
        {                                                   /* dgb:02/18/00 */  
	   notfound = 1;                                    /* dgb:02/18/00 */
           while ( notfound && i < MAX_NUM_MTR_NAMES )      /* dgb:02/18/00 */
	   {                                                /* dgb:02/18/00 */
	      if ( strlen(&sm_name[i][0]) == 0 )            /* dgb:02/18/00 */ 
	           notfound = 0;                            /* dgb:02/18/00 */
              if ( strpat(&sm_name[i][0],sm.s000,(int)NULL,(int)NULL,(int)NULL,(int)NULL,strlen(&sm_name[i][0])) >= 0 ) /* dgb:02/18/00 */
	      {                                             /* dgb:02/18/00 */
                        strcpy(hb5_id, &sm_alias[i][0]);    /* dgb:02/18/00 */
			notfound = 0;                       /* dgb:02/18/00 */
	      }                                             /* dgb:02/18/00 */
              i++;                                          /* dgb:02/18/00 */
	   }                                                /* dgb:02/18/00 */
	}                                                   /* dgb:02/18/00 */
	else                                                /* dgb:02/18/00 */
         strcpy(hb5_id,sm.s000);                            /* dgb:02/18/00 */
     }                                                      /* dgb:02/18/00 */
      

   /* Store/Compute data values into value array */
      for ( i = 0; i < VAL_CHARS; i++)
        values[i] = -999;


      /* TEMPERATURE */
      if ( buffer_.ibuf[35] > -998 )
      {
        values[0] = (float) buffer_.ibuf[35];
        values[0] = 1.8 * ( values[0]/10 ) +32 ;  /* dgb:12/08/96 */
      }


      /* DEW POINT */
      if ( buffer_.ibuf[36]  > -998 )  
      {
        values[1] = (float) buffer_.ibuf[36];
        values[1] = 1.8 * ( values[1]/10 ) + 32 ; /* dgb:12/08/96 */
      }


      /* WIND SPEED */
      if ( buffer_.ibuf[39] > -998 )
      {
         if ( KNOTS )
              values[2] = ( (float) buffer_.ibuf[39]) ;
         else
              values[2] = ( ((float) buffer_.ibuf[39] * 1.151555 ) +.5 ); /* dgb:01/03/98 */

      }
            
      /* WIND DIRECTION */
      if ( buffer_.ibuf[38] > -998 )
        values[3] = ( (float) buffer_.ibuf[38]) ;


      /* WIND GUST */
      if ( buffer_.ibuf[40] > -998 )            
      {
        values[4] = ((float) buffer_.ibuf[40]) ;
        values[4] = ( values[4] * 2.23694) ;
      }


      /* WIND SPEED/DIRECTION UQ */
      if ( buffer_.ibuf[39] > -998 && buffer_.ibuf[38] > -998 )
      {
            memset(buff_temp,0,sizeof(buff_temp));
            sprintf(buff_temp,"%5.1f%3d", values[2], buffer_.ibuf[38]*100);
            values[5] = atof(buff_temp) ;
      }

      /* STATION PRESSURE */
      if ( buffer_.ibuf[41] > -998 )
      {
        if ( buffer_.ibuf[41] < 500 )
             addon = 1000;
        else
             addon = 0;
        values[6] = (((float) buffer_.ibuf[41] ) / 10) + addon ;
/*        values[6] = ( values[6] / 0.0345316) ; */
      }

      /* SEA LEVEL PRESSURE */
      if ( buffer_.ibuf[34] > -998 )
      {
        if ( buffer_.ibuf[34] < 500 )
             addon = 1000;
        else
             addon = 0;
        values[22] = (((float) buffer_.ibuf[34] ) / 10) + addon ;
      }

      /* PRESSURE CHARACTERISTIC - 3 HOUR */
      if ( buffer_.ibuf[50] > -998 )
      {
          values[23] = (float) buffer_.ibuf[50]  ;
      }

      /* PRESSURE CHARACTERISTIC - 3 HOUR */
      if ( buffer_.ibuf[51] > -998 )
      {
          values[24] = ((float) buffer_.ibuf[51]) / 10  ;
/*        values[6] = ( values[6] / 0.0345316) ; */
      }


      /* 6HR PRECIP */

      if ( 
         (buffer_.ibuf[5]  >=  sm_time6[0] && buffer_.ibuf[5]  <= sm_time6[1] ) ||
         (buffer_.ibuf[5]  >=  sm_time6[2] && buffer_.ibuf[5]  <= sm_time6[3] ) ||
         (buffer_.ibuf[5]  >=  sm_time6[4] && buffer_.ibuf[5]  <= sm_time6[5] ) ||
         (buffer_.ibuf[5]  >=  sm_time6[6] && buffer_.ibuf[5]  <= sm_time6[7] ) ||
         (buffer_.ibuf[5]  >=  sm_time6[8] && buffer_.ibuf[5]  <= sm_time6[9] )    /* dgb:03/27/96 */
         )
      {
         buffer_.idate1[4] = 0;
         if (buffer_.ibuf[5]  >=  sm_time6[0] && buffer_.ibuf[5]  <=  sm_time6[1] )
             buffer_.idate1[3] = 6;
         else
         if (buffer_.ibuf[5]  >=  sm_time6[2] && buffer_.ibuf[5]  <=  sm_time6[3] ) 
             buffer_.idate1[3] = 12;
         else
         if (buffer_.ibuf[5]  >=  sm_time6[4] && buffer_.ibuf[5]  <=  sm_time6[5] ) 
             buffer_.idate1[3] = 18;        
         else
         if (buffer_.ibuf[5]  >=  sm_time6[6] && buffer_.ibuf[5]  <= sm_time6[7] )
             buffer_.idate1[3] = 0;
         else
         if (buffer_.ibuf[5]  >= sm_time6[8] && buffer_.ibuf[5] <=   sm_time6[9] )    /* dgb:03/27/96 */
             buffer_.idate1[3] = 0;

         if ( buffer_.ibuf[52] > -998 ) 
         {

              if ( buffer_.ibuf[52] == 990 )
                   temp_precip = 0;
              else
              if ( buffer_.ibuf[52] == 991 )
                   temp_precip = 0.1;
              else
              if ( buffer_.ibuf[52] == 992 )
                   temp_precip = 0.2;
              else
              if ( buffer_.ibuf[52] == 993 )
                   temp_precip = 0.3;
              else
              if ( buffer_.ibuf[52] == 994 )
                   temp_precip = 0.4;
              else
              if ( buffer_.ibuf[52] == 995 )
                   temp_precip = 0.5;
              else
              if ( buffer_.ibuf[52] == 996 )
                   temp_precip = 0.6;
              else
              if ( buffer_.ibuf[52] == 997 )
                   temp_precip = 0.7;
              else
              if ( buffer_.ibuf[52] == 998  )
                   temp_precip = 0.8;
              else
              if ( buffer_.ibuf[52] == 999 )
                   temp_precip = 0.9;
              else
                   temp_precip = (float) buffer_.ibuf[52];
 
              values[7] = temp_precip * .03937;            /* dgb:03/22/96 */
         }
         else
         {

          if ( P6 || PALL6 )                               /* dgb:07/28/96 */
          {
                   values[7] = 0;
          }
        }
      }


      /* 3HR precipitation */

      if ( 
         (buffer_.ibuf[5]  >  sm_time3[0] && buffer_.ibuf[5] <  sm_time3[1] ) ||
         (buffer_.ibuf[5]  >  sm_time3[2] && buffer_.ibuf[5] <  sm_time3[3] ) ||
         (buffer_.ibuf[5]  >  sm_time3[4] && buffer_.ibuf[5] <  sm_time3[5] ) ||
         (buffer_.ibuf[5]  >  sm_time3[6] && buffer_.ibuf[5] <  sm_time3[7] ) 
         )

      {
         buffer_.idate1[4] = 0;
         if (buffer_.ibuf[5]  >  sm_time3[0] && buffer_.ibuf[5] <  sm_time3[1] ) 
             buffer_.idate1[3] = 3;
         else
         if (buffer_.ibuf[5]  >  sm_time3[2] && buffer_.ibuf[5] <  sm_time3[3] ) 
             buffer_.idate1[3] = 9;
         else
         if (buffer_.ibuf[5]  >  sm_time3[4] && buffer_.ibuf[5] <  sm_time3[5] ) 
             buffer_.idate1[3] = 15;
         else
         if (buffer_.ibuf[5]  >  sm_time3[6] && buffer_.ibuf[5] <  sm_time3[7] ) 
             buffer_.idate1[3] = 21;


              if ( buffer_.ibuf[52] > -998 )                  /* precip 3 hr */
              {
              if ( buffer_.ibuf[52] == 990 )
                   temp_precip = 0;
              else
              if ( buffer_.ibuf[52] == 991 )
                   temp_precip = 0.1;
              else
              if ( buffer_.ibuf[52] == 992 )
                   temp_precip = 0.2;
              else
              if ( buffer_.ibuf[52] == 993 )
                   temp_precip = 0.3;
              else
              if ( buffer_.ibuf[52] == 994 )
                   temp_precip = 0.4;
              else
              if ( buffer_.ibuf[52] == 995 )
                   temp_precip = 0.5;
              else
              if ( buffer_.ibuf[52] == 996 )
                   temp_precip = 0.6;
              else
              if ( buffer_.ibuf[52] == 997 )
                   temp_precip = 0.7;
              else
              if ( buffer_.ibuf[52] == 998  )
                   temp_precip = 0.8;
              else
              if ( buffer_.ibuf[52] == 999 )
                   temp_precip = 0.9;
              else
                   temp_precip = (float) buffer_.ibuf[52];

                 values[17] = temp_precip * .03937 ; /* dgb:03/22/96 */
              }
      }
           else
              values[17] = -998;


  
      /* 24HR PRECIP */

      if ( P24 || PALL24)                                  /* dgb:07/28/96 */
      {
         if ( buffer_.ibuf[5] > 1140 && buffer_.ibuf[5] < 1230 )  /* dgb:09/04/96 */
         {
            if ( buffer_.ibuf[57] == 9999 )                /* dgb:04/15/96 */
                 values[8] = 0;
            else
            if ( buffer_.ibuf[57] > -998 )
                 values[8] = ( (float) buffer_.ibuf[57] ) * .003937 ;  /* dgb:03/22/96 */
            else
                 values[8] = 0;
         } 
      }
      else
      {
            if ( buffer_.ibuf[57] == 9999 )                /* dgb:04/15/96 */
                 values[8] = 0;
            else
            if ( buffer_.ibuf[57] > -998 )
                 values[8] = ( (float) buffer_.ibuf[57] ) * .003937 ; /* dgb:03/22/96 */
      }


      /* 24HR MAX TEMPERATURE */
      if ( buffer_.ibuf[58] > -998 )
      {
         values[9] = (float) buffer_.ibuf[58];
         values[9] = 1.8 * ( values[9]/10 ) +32 ; /* dgb:12/08/96 */
      }



      /* 24HR MIN TEMPERATURE */
      if ( buffer_.ibuf[59] > -998 )
      {
         values[10] = (float) buffer_.ibuf[59];
         values[10] = 1.8 * ( values[10]/10 ) + 32 ; /* dgb:12/08/96 */
      }



      /* HOURLY PRECIP */
      if ( buffer_.ibuf[60] > -998 ) 
           values[11] = (((float) buffer_.ibuf[60]) * .03937)  ; /* dgb:03/22/96 */



      /* SNOW DEPTH */
      if ( buffer_.ibuf[16] > -998 )
           values[12] = (((float) buffer_.ibuf[16] ) * .3937)  ;



      /* WATER EQUIVALENT */
      if ( buffer_.ibuf[62] > -998 )
           values[13] = (((float) buffer_.ibuf[62] ) * .03937)  ; /* dgb: 03/22/96 */



      /* SUNSHINE */
      if ( buffer_.ibuf[63] > -998 )
         values[14] = (float) buffer_.ibuf[63];



      /* 6HR MAX TEMPERATURE */
      if ( buffer_.ibuf[64] > -998 )  
      {
         values[15] = (float) buffer_.ibuf[64];
         values[15] = 1.8 * ( values[15]/10 ) +32 ; /* dgb:12/08/96 */
      }


      /* 6HR MIN TEMPERATURE */
      if ( buffer_.ibuf[65] > -998 )
      {
         values[16] = (float) buffer_.ibuf[65];
         values[16] = 1.8 * ( values[16]/10 ) +32 ; /* dgb:12/08/96 */
      } 



      /* 12HR MAX TEMPERATURE */
      if ( buffer_.ibuf[14] > -998 )
      {
         values[18] = (float) buffer_.ibuf[14];
         values[18] = 1.8 * ( values[18]/10 ) +32 ; /*dgb:12/08/96 */
      }



      /* 12 HR MIN TEMPERATURE */
      if ( buffer_.ibuf[15] > -998 )
      {
         values[19] = (float) buffer_.ibuf[15];
         values[19] = 1.8 * ( values[19]/10 ) +32 ; /* dgb:12/08/96 */
      }



      /* PRECIP ACCUMULATOR */
      if ( buffer_.ibuf[61] > -998 )                          /* PRECIP accumulator */
         values[20] = buffer_.ibuf[61];
      
      
      /* CURRENT PRECIP TYPE */
      if ( buffer_.ibuf[17] > -998 )
         values[21] = (float) buffer_.ibuf[17];

      /* CURRENT VISIBILITY */
      if ( buffer_.ibufchar[1] > -998 )
         values[25] = buffer_.ibufchar[1];


 
	for( i = 0; i < VAL_CHARS; i++ )
		 pe_flag[i] = 0;

		/* NOTE: these must be in the same order as the 
		   array pe_buffer above
		*/
		if( strstr( shefstr_.pecodes, "TAIRZZ " ) != 0 )  /* dgb:01/12/96 */
			pe_flag[0] = 1;
		if( strstr( shefstr_.pecodes, "TA " ) != 0 )      /* dgb:02/27/97 */
			pe_flag[0] = 1;
		if( strstr( shefstr_.pecodes, "TD " ) != 0 )
			pe_flag[1] = 1;
		if( strstr( shefstr_.pecodes, "US " ) != 0 )
			pe_flag[2] = 1;
		if( strstr( shefstr_.pecodes, "UD " ) != 0 )
			pe_flag[3] = 1;
		if( strstr( shefstr_.pecodes, "UP " ) != 0 )     /* dgb:02/27/97 */
			pe_flag[4] = 1;
		if( strstr( shefstr_.pecodes, "UQ " ) != 0 )
			pe_flag[5] = 1;
		if( strstr( shefstr_.pecodes, "PA " ) != 0 )
			pe_flag[6] = 1;
		if( strstr( shefstr_.pecodes, "PPQ ") != 0 )
			pe_flag[7] = 1;
		if( strstr( shefstr_.pecodes, "PPD ") != 0 )
			pe_flag[8] = 1;
		if( strstr( shefstr_.pecodes, "TX " ) != 0 )
			pe_flag[9] = 1;
		if( strstr( shefstr_.pecodes, "TN " ) != 0 )
			pe_flag[10] = 1;
                if(strstr( shefstr_.pecodes,  "PPH ") != 0 )
			pe_flag[11] = 1;
       	        if( strstr( shefstr_.pecodes, "SD " ) != 0 )
                        pe_flag[12] = 1;
		if( strstr( shefstr_.pecodes, "SW " ) != 0 )
		        pe_flag[13] = 1;
		if( strstr( shefstr_.pecodes, "RT " ) != 0 )
                        pe_flag[14] = 1;
		if( strstr( shefstr_.pecodes, "TAIRZR " ) != 0 )
		        pe_flag[15] = 1;
		if( strstr( shefstr_.pecodes, "TAIRZH " ) != 0 )
                        pe_flag[16] = 1;
		if( strstr( shefstr_.pecodes, "PPT " ) != 0 )
                        pe_flag[17] = 1;
		if( strstr( shefstr_.pecodes, "TAIRZY " ) != 0 )
		        pe_flag[18] = 1;
		if( strstr( shefstr_.pecodes, "TAIRZP " ) != 0 )
                        pe_flag[19] = 1;
		if( strstr( shefstr_.pecodes, "PC " ) != 0 )
                        pe_flag[20] = 1;
		if( strstr( shefstr_.pecodes, "PTIR " ) != 0 )      /* dgb:04/22/96 */
                        pe_flag[21] = 1; 
		if( strstr( shefstr_.pecodes, "PL " ) != 0 )        /* dgb:04/15/96 */
                        pe_flag[22] = 1;
		if( strstr( shefstr_.pecodes, "PDIR " ) != 0 )      /* dgb:04/17/96 */
                        pe_flag[23] = 1;
		if( strstr( shefstr_.pecodes, "PE " ) != 0 )        /* dgb:04/15/96 */
                        pe_flag[24] = 1;
		if( strstr( shefstr_.pecodes, "XV " ) != 0 )        /* dgb:04/15/96 */
                        pe_flag[25] = 1;

      /* write header to error log */
         if ( luns_.icher != NULL )
         {
            fprintf(luns_.icher,"\n\n----------------------------");
            fprintf(luns_.icher,"\n ENCODED: SHEF OBSERVATION");
            fprintf(luns_.icher,"\n----------------------------\n\n");
         }

      /* write header to stdout */
      if ( VERBOSE ) 
      {
            fprintf(stdout,"\n\n----------------------------");
            fprintf(stdout,"\n ENCODED: SHEF OBSERVATION");
            fprintf(stdout,"\n----------------------------\n\n");
      }

      if ( WMO )
         fprintf(luns_.jchn,"ZCZC %s\n%s\n",stats_.product_name,buffer_.wmo_line); /* dgb:04/15/96 */

      if ( luns_.jchn != NULL )
         print_obs( luns_.jchn, 1, 0 );



      for ( j = 0; j < VAL_CHARS; j++ )
      {
        if ( pe_flag[j] && values[j] > -998)     /* process this pe code */
        {

          for (i=0; i < 7; ++i)
             pc[i] = pe_buffer[j][i]; /* build PEDSTEP string */
          pc[7] =  0;                 /* string terminator    */
          pc[8] =  0;

              /* check if revision/correction is set on */
              memset(dota,0,sizeof(dota));
              if ( REVISION ) 
                   strcpy(dota,".AR");
               else
                   strcpy(dota,".A");          

              if ( !Y2K )                                  /* dgb:08/13/98 */
              {
              /* write report to output file */
                 if ( buffer_.idate1[0] < 2000 )
                      tempyear = buffer_.idate1[0]  - 1900;  /* dgb:13/05/98 */
                 else
                      tempyear = buffer_.idate1[0] - 2000;  /* dgb:13/05/98 */
                 if ( buffer_.idate[0] < 2000 )             /* dgb:13/05/98 */
                      tempyeardc = buffer_.idate[0] - 1900; /* dgb:13/05/98 */
                 else
                      tempyeardc = buffer_.idate[0] - 2000; /* dgb:13/05/98 */

                 sprintf(buff_temp,
                 "%s %s :%5s %3s %5s: %02d%02d%02d Z DH%02d%02d/DC%02d%02d%02d%02d%02d/%s ",
                 dota,
                 hb5_id,
                 sm.s000,
                 forms_.type_report,
                 forms_.type_sensor,
                 tempyear,
                 buffer_.idate1[1],
                 buffer_.idate1[2],
                 buffer_.idate1[3],
                 buffer_.idate1[4],
                 tempyeardc,
                 buffer_.idate[1],
                 buffer_.idate[2],
                 buffer_.idate[3],
                 buffer_.idate[4],
                 pc);
              }
              else
              {
              /* write report to output file */
                 sprintf(buff_temp,
                 "%s %s :%5s %3s %5s: %04d%02d%02d Z DH%02d%02d/DC%04d%02d%02d%02d%02d/%s ",
                 dota,
                 hb5_id,
                 sm.s000,
                 forms_.type_report,
                 forms_.type_sensor,
                 buffer_.idate1[0],  
                 buffer_.idate1[1],
                 buffer_.idate1[2],
                 buffer_.idate1[3],
                 buffer_.idate1[4],
                 buffer_.idate[0],
                 buffer_.idate[1],
                 buffer_.idate[2],
                 buffer_.idate[3],
                 buffer_.idate[4],
                 pc);
               }


                switch (j)
                {
                    case 0 : /* temp */
                    {

                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 1: /* dew */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 2 : /* wind sp */ 
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 3: /* dir */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 4 : /* gust */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 5: /* speed/dir */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %8.4f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s %8.4f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s %8.4f\n",buff_temp,values[j]);
                        break;
                    }

                    case 6: /* altimeter */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s%7.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s%7.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s%7.2f\n",buff_temp,values[j]);
                        break;
                    }

                    case  7: /* precip 6 */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s %6.2f\n",buff_temp,values[j]);
                        break;
                    }

                    case 8: /* precip 24 */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s %6.2f\n",buff_temp,values[j]);
                        break;
                    }

                    case 9: /* temp mx */ 
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 10: /* temp mn */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 11: /* hourly precip */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s %6.2f\n",buff_temp,values[j]);
                        break;
                    }

                    case 12: /* snow depth */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 13: /* water equivalent */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s %6.2f\n",buff_temp,values[j]);
                        break;
                    }

                    case 14: /* sunshine */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 15: /* temp mx past 6 hours */ 
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 16: /* temp mn past 6 hours */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case  17: /* precip 3 hour */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s %6.2f\n",buff_temp,values[j]);
                        break;
                    }

                    case 18: /* temp mx past 12 hours */ 
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 19: /* temp mn past 12 hours */
                    {
                       if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 20: /* PC accumulator */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.jchn,"%s %6.2f\n",buff_temp,values[j]);
                        fprintf(luns_.icher,"%s %6.2f\n",buff_temp,values[j]);
                        break;
                    }

                    case 21: /* PT type */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 22: /* PT sea level pressure */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s%7.2f\n",buff_temp, values[j]);
                        fprintf(luns_.jchn,"%s%7.2f\n",buff_temp, values[j]);
                        fprintf(luns_.icher,"%s%7.2f\n",buff_temp, values[j]);
                        break;
                    }

                    case 23: /* pressure characterstic */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.jchn,"%s %3d\n",buff_temp,(int) values[j]);
                        fprintf(luns_.icher,"%s %3d\n",buff_temp,(int) values[j]);
                        break;
                    }

                    case 24: /* pressure tendency */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %6.2f\n",buff_temp, values[j]);
                        fprintf(luns_.jchn,"%s %6.2f\n",buff_temp, values[j]);
                        fprintf(luns_.icher,"%s %6.2f\n",buff_temp, values[j]);
                        break;
                    }

                    case 25: /* visibility */
                    {
                        if ( VERBOSE )
                           fprintf(stdout,"%s %6.2f\n",buff_temp, values[j]);
                        fprintf(luns_.jchn,"%s %6.2f\n",buff_temp, values[j]);
                        fprintf(luns_.icher,"%s %6.2f\n",buff_temp, values[j]);
                        break;
                    }

          }
        } 
      }  

      if ( WMO )
         fprintf(luns_.jchn,"NNNN\n");



}
