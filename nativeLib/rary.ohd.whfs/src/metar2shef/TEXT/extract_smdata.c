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
#define  SIZE_1 100

/* --------------------------------------------------------------------
  
       FUNCTION
         extract_smdata

       PURPOSE  
         Extract data from each section.

      VERSION and UPDATES
         1.0    JAN 96   David G. Brandon
                Original Version
         1.1    FEB 25 96 DGB
                Make minor changes for outputing data to screen and
                error file.  
                Improve error check for '/' marks in certain groups.
                Allow for a phantom character in function 'strpat'.
         1.2    JUL 9 96 DGB
                Changed type_report to type_sensor.
         1.3    JAN 16 00 DGB
                Change argument list for calls to strpat.
         1.4    FEB 18 00 DGB
                Add change for finding 5 digit id in lieu of 3 char id
                for sm reports, this will be set if SM = 2, else if 
                SM = 1 assume a 3 char id.
         1.4    FEB 01 2006 RAE
                Added function prototype for strpat().

 *--------------------------------------------------------------------- */

extern int    VERBOSE, DEBUG;
extern int    AO2, AO2A, RAMOS, AWOS, AMOS, OTHER, SA, SP, RS, SM, METAR, REMARKS; 
extern int strpat(char buffer[],char token[],int numtemp,int chartemp,int either, int phantom,int buffer_len);


int extract_smdata()
{

   int temp, ptr;
   char buff_temp[SIZE_OF_MSG], buff[SIZE_OF_MSG];
   float v;


   /* get 5 digit id or 3 char id from either message or product name */
      if ( SM == 2 )                                        /* dgb:02/18/00 */
      {
        if ( ( ptr = strpat(databuf_.sdata,"$$$$$ ",'$','%',(int)NULL,(int)NULL,strlen(databuf_.sdata) ) ) >= 0 ) /*dgb:01/16/00 */
        {
          strncpy(buffer_.tempid,&databuf_.sdata[ptr],5);  /* dgb:02/18/00 */    
          strncpy(sm.s000,&databuf_.sdata[ptr],5);          /* dgb:02/18/00 */
        }      
      }
      else
      {
      if ( ( ptr = strpat(databuf_.sdata,"%%%",'$','%',(int)NULL,(int)NULL,strlen(databuf_.sdata) ) ) >= 0 ) /*dgb:01/16/00 */
      {
        buffer_.ibuf[1] = databuf_.sdata[ptr];
        buffer_.ibuf[2] = databuf_.sdata[ptr+1];
        buffer_.ibuf[3] = databuf_.sdata[ptr+2];
        strncpy(buffer_.tempid,&databuf_.sdata[ptr],3);    /* dgb 02/25/95 */
        strncpy(sm.s000,&databuf_.sdata[ptr+4],5);
      }
      else
      {
        buffer_.ibuf[1] = stats_.product_name[6];
        buffer_.ibuf[2] = stats_.product_name[7];
        buffer_.ibuf[3] = stats_.product_name[8]; 
        strncpy(buffer_.tempid,&stats_.product_name[6],3); /* dgb 02/25/95 */
        strncpy(sm.s000,databuf_.sdata,5);
      }
 
      }
   /* get type of station, manned or automatic and visibility */
   /* 
      iihVV
      i   = precipitation indicator
      i   = type of station
      VV  =  vsby according to table
   */

      if ( ( ptr = strpat(sm.s111,"$$$$$ ",'$','%','/',(int)NULL,strlen(sm.s111) ) ) >= 0 ) /*dgb:01/16/00 */
      {
           memset(buff_temp,0,sizeof(buff_temp));
           strncpy(buff_temp,&sm.s111[ptr+1],1);
           if ( atoi(buff_temp) < 4 )
                strcpy(forms_.type_sensor,"MANNED");  /* dgb:07/09/96 */
           else
                strcpy(forms_.type_sensor,"  AUTO");  /* dgb:07/09/96 */
           memset(buff_temp,0,sizeof(buff_temp));
           strncpy(buff_temp,&sm.s111[ptr+3],2);
           if ( strchr(buff_temp,'/') == NULL )
           {
           temp = atoi(buff_temp);
           if ( temp == 0 ) v = .0625;
           else
           if ( temp == 1 ) v = .0625;
           else
           if ( temp == 2 ) v = .125;
           else
           if ( temp == 3 ) v = .1875;
           else
           if ( temp == 4 ) v = .25;
           else
           if ( temp == 5 ) v = .3125;
           else
           if ( temp == 6 ) v = .375;
           else
           if ( temp == 8 ) v = .5;
           else
           if ( temp == 10 ) v = .625;
           else
           if ( temp == 12 ) v = .75;
           else
           if ( temp == 14 ) v = .875;
           else
           if ( temp == 16 ) v = 1;
           else
           if ( temp == 18 ) v = 1.125;
           else
           if ( temp == 20 ) v = 1.25;
           else
           if ( temp == 22 ) v = 1.375;
           else
           if ( temp == 24 ) v = 1.5;
           else
           if ( temp == 26 ) v = 1.625;
           else
           if ( temp == 28 ) v = 1.75;
           else
           if ( temp == 30 ) v = 1.875;
           else
           if ( temp == 32 ) v = 2;
           else
           if ( temp == 36 ) v = 2.25;
           else
           if ( temp == 40 ) v = 2.5;
           else
           if ( temp == 44 ) v = 2.75;
           else
           if ( temp == 48 ) v = 3;
           else
           if ( temp == 56 ) v = 4;
           else
           if ( temp == 58 ) v = 5;
           else
           if ( temp == 60 ) v = 6;
           else
           if ( temp == 61 ) v = 7;
           else
           if ( temp == 63 ) v = 8;
           else
           if ( temp == 64 ) v = 9;
           else
           if ( temp == 66 ) v = 10;
           else
           if ( temp == 68 ) v = 11;
           else
           if ( temp == 69 ) v = 12;
           else
           if ( temp == 71 ) v = 13;
           else
           if ( temp == 73 ) v = 14;
           else
           if ( temp == 74 ) v = 15;
           else
           if ( temp == 80 ) v = 20;
           else
           if ( temp == 82 ) v = 25;
           else
           if ( temp == 84 ) v = 30;
           else
           if ( temp == 85 ) v = 35;
           else
           if ( temp == 87 ) v = 40;
           else
           if ( temp == 89 ) v = 45;
           else
                v = -999;
           buffer_.ibufchar[1] = v;
           }
      }
   


   /* 
      get wind data 
      Nddff
      N   =  sky cover in eights
      dd  =  direction in degrees
      ff  =  speed in knots
   */
   
      if ( ( ptr = strpat(sm.s111,"$$$$$ $$$$$",'$','%','/',(int)NULL,strlen(sm.s111) ) ) >= 0 ) /*dgb:01/16/00 */
      {

           /* get direction or dd */
           memset(buff_temp,0,sizeof(buff_temp));
           strncpy(buff_temp,&sm.s111[ptr+7],2);
           if ( strchr(buff_temp,'/') == NULL )            /* dgb 02/25/96 */
               buffer_.ibuf[38] = atoi(buff_temp);

           /* get speed or ff */
           memset(buff_temp,0,sizeof(buff_temp));
           strncpy(buff_temp,&sm.s111[ptr+9],2);
           if ( strchr(buff_temp,'/') == NULL )            /* dgb 02/25/96 */
              buffer_.ibuf[39] = atoi(buff_temp);
      }

   /* 
      get current temperature 1sTTT 
      2  =  temp indicator
      s  =  sign indicator, 0=+, 1=-
      TTT = temperature in celcius, in tenths
   */ 

      memset(buff,0,sizeof(buff));
      strcpy(buff,&sm.s111[11]);
      if ( ( ptr = strpat(buff," 1$$$$",'$','%','/',(int)NULL,strlen(buff) ) )  >= 0 ) /*dgb:01/16/00 */
      {
                 memset(buff_temp,0,sizeof(buff_temp));
                 strncpy(buff_temp,&buff[ptr+2],4);
                 if ( strchr(buff_temp,'/') == NULL )      /* dgb 02/25/96 */
                 {
                   /* security check...no decimal should exist */
                   if ( ( strchr(buff_temp,'.' )) == NULL )
                   {
                      if ( buff_temp[0] == '1' ) buff_temp[0] = '-';
                         else
                      if ( buff_temp[0] == '0' ) buff_temp[0] = '+';
                         buffer_.ibuf[35] = atoi(buff_temp);
                   }
                 }
      }

   /* 
      get current dew point 2sTTT 
      2  =  temp indicator
      s  =  sign indicator, 0=+, 1=-
      TTT = temperature in celcius, in tenths 
            e.g. 123  =  12.3 C 
   */
      memset(buff,0,sizeof(buff));
      strcpy(buff,&sm.s111[12]);

      if ( ( ptr = strpat(buff," 2$$$$",'$','%','/',(int)NULL,strlen(buff) ) )  >= 0 ) /*dgb:01/16/00 */
      {
                 memset(buff_temp,0,sizeof(buff_temp));
                 strncpy(buff_temp,&buff[ptr+2],4);
                 if ( strchr(buff_temp,'/') == NULL )      /* dgb 02/25/96 */
                 {
                   /* security check...no decimal should exist */
                   if ( ( strchr(buff_temp,'.' )) == NULL )
                   {
                      if ( buff_temp[0] == '1' ) buff_temp[0] = '-';
                         else
                      if ( buff_temp[0] == '0' ) buff_temp[0] = '+';
                         buffer_.ibuf[36] = atoi(buff_temp);
                   }
                 }
      }


   /* 
      get current station pressure 3PPPP 
      3    =  station pressure indicator
      PPPP = station presure in hecopascals with last char the tenths digit
             e.g. 39786 = 978.6 hPa, 30173 = 1017.3 hPa 
   */
      memset(buff,0,sizeof(buff));
      strcpy(buff,&sm.s111[12]);

      if ( ( ptr = strpat(buff," 3$$$$",'$','%','/',(int)NULL,strlen(buff) ) )  >= 0 ) /*dgb:01/16/00 */
      {
                 memset(buff_temp,0,sizeof(buff_temp));
                 strncpy(buff_temp,&buff[ptr+2],4);
                 if ( strchr(buff_temp,'/') == NULL )      /* dgb 02/25/96 */
                 {
                   /* security check...no decimal should exist */
                   if ( ( strchr(buff_temp,'.' )) == NULL )
                         buffer_.ibuf[41] = atoi(buff_temp);
                 }
      }

   /* 
      get current station pressure 4PPPP 
      4    =  sea level pressure indicator
      PPPP =  sea level presure in hecopascals with last char the tenths digit
   */
     
   
      memset(buff,0,sizeof(buff));
      strcpy(buff,&sm.s111[12]);

      if ( ( ptr = strpat(buff," 4$$$$",'$','%','/',(int)NULL,strlen(buff) ) )  >= 0 ) /*dgb:01/16/00 */
      {
                 memset(buff_temp,0,sizeof(buff_temp));
                 strncpy(buff_temp,&buff[ptr+2],4);
                 if ( strchr(buff_temp,'/') == NULL )    
                 {
                   if ( ( strchr(buff_temp,'.' )) == NULL )
                         buffer_.ibuf[34] = atoi(buff_temp);
                 }
      }



   /* 
      get current 3 hour pressure characteristic and change 5appp 
      5    =  identifier for 3 hour pressure tendency group
      a    =  characteristic of pressure tendency 0 thru 8
      ppp  =  change in pressure during the past 3 hours in tenths of HPa
              e.g. 52103 indicates:
                   2 = increasing (steadily or unsteadily)
                   103 = 10.3 hPa change 
   */
      memset(buff,0,sizeof(buff));
      strcpy(buff,&sm.s111[12]);

      if ( ( ptr = strpat(buff," 5$$$$",'$','%','/',(int)NULL,strlen(buff) ) )  >= 0 ) /*dgb:01/16/00 */
      {
                 memset(buff_temp,0,sizeof(buff_temp));
                 strncpy(buff_temp,&buff[ptr+2],1);
                 if ( strchr(buff_temp,'/') == NULL )      /* dgb 02/25/96 */
                 {
                   /* security check...no decimal should exist */
                   if ( ( strchr(buff_temp,'.' )) == NULL )
                         buffer_.ibuf[50] = atoi(buff_temp);
                 }
                 memset(buff_temp,0,sizeof(buff_temp));
                 strncpy(buff_temp,&buff[ptr+3],3);
                 if ( strchr(buff_temp,'/') == NULL )      /* dgb 02/25/96 */
                 {
                   /* security check...no decimal should exist */
                   if ( ( strchr(buff_temp,'.' )) == NULL )
                         buffer_.ibuf[51] = atoi(buff_temp);
                 }
      }



   /* 
      get six hour precip 6RRRt 
      6  =  indicator of precip group
      RRR = depth in millimeters
      t   = period indicator, 1=6hr, 2=12hr, 3=18hr, 4=24 hr
   */

      memset(buff,0,sizeof(buff));
      strcpy(buff,&sm.s111[12]);

      if ( ( ptr = strpat(buff," 6$$$$", '$','%','/',(int)NULL,strlen(buff) ) ) >= 0 ) /*dgb:01/16/00 */
      {
        memset(buff_temp,0,sizeof(buff_temp));
        strncpy(buff_temp,&buff[ptr+2],3);
        temp = atoi(buff_temp);
        if ( strchr(buff_temp,'/') == NULL )                   /* dgb 02/25/96 */
        {
          /* if ( temp > 989 )
             temp = 0;      
          */

          /* set as 0 if 990 to 999 */
          /* determine the time period of the observation and place
             precip in the correct place accordingly
          */
          memset(buff_temp,0,sizeof(buff_temp));
          strncpy(buff_temp,&buff[ptr+5],1);
          ptr = atoi(buff_temp);

          /* 
             12 & 18 hour totals not encoded 
             ptr = 2  - 12 hour indicator
             ptr = 3  - 18 hour indicator
          */

          /* 6 hour indicator */
          if ( ptr == 1 )
             buffer_.ibuf[52] = temp;

          /* 24 hour indicator */
          if ( ptr == 4 )
             buffer_.ibuf[57] = temp;
        }
      }


   /* get TAIRZRZ, TAIRZHZ, SD, PPD from group 3 */
      
      /* max temp past 6 hours */
      if ( ( ptr = strpat(sm.s333," 1$$$$",'$','%','/',(int)NULL,strlen(sm.s333) ) )  >= 0 ) /*dgb:01/16/00 */
      {
                 memset(buff_temp,0,sizeof(buff_temp));
                 strncpy(buff_temp,&sm.s333[ptr+2],4);
                 if ( strchr(buff_temp,'/') == NULL )      /* dgb 02/25/96 */
                 {
                   /* security check...no decimal should exist */
                   if ( ( strchr(buff_temp,'.' )) == NULL )
                   {
                      if ( buff_temp[0] == '1' ) buff_temp[0] = '-';
                         else
                      if ( buff_temp[0] == '0' ) buff_temp[0] = '+';
                         buffer_.ibuf[64] = atoi(buff_temp);
                   }
                 }
      }

      /* min temp past 6 hours */
      if ( ( ptr = strpat(sm.s333," 2$$$$",'$','%','/',(int)NULL,strlen(sm.s333) ) )  >= 0 ) /*dgb:01/16/00 */
      {
                 memset(buff_temp,0,sizeof(buff_temp));
                 strncpy(buff_temp,&sm.s333[ptr+2],4);
                 if ( strchr(buff_temp,'/') == NULL )      /* dgb 02/25/96 */
                 {
                   /* security check...no decimal should exist */
                   if ( ( strchr(buff_temp,'.' )) == NULL )
                   {
                      if ( buff_temp[0] == '1' ) buff_temp[0] = '-';
                           else
                       if ( buff_temp[0] == '0' ) buff_temp[0] = '+';
                           buffer_.ibuf[65] = atoi(buff_temp);
                   }
                 }
      }

      /* snow depth */
      if ( ( ptr = strpat(sm.s333," 4/$$$",'$','%','/',(int)NULL,strlen(sm.s333) )) >= 0 ) /*dgb:01/16/00 */
      {
            memset(buff_temp,0,sizeof(buff_temp));
            strncpy(buff_temp,&sm.s333[ptr+3],3);
            if ( strchr(buff_temp,'/') == NULL )           /* dgb 02/25/96 */
               buffer_.ibuf[16] = atoi(&sm.s333[ptr+3]);
      }
      
      /* 24 hour precip */
      if ( ( ptr = strpat(sm.s333," 7$$$$", '$','%','/',(int)NULL,strlen(sm.s333) ) ) >= 0 ) /*dgb:01/16/00 */
      {
            memset(buff_temp,0,sizeof(buff_temp));
            strncpy(buff_temp,&sm.s333[ptr+2],4);
            if ( strchr(buff_temp,'/') == NULL )           /* dgb 02/25/96 */
                 buffer_.ibuf[57] = atoi(&sm.s333[ptr+2]);
      }

return(0);
}


