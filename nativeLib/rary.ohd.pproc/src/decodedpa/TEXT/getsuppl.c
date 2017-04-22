#include <string.h>
#include "decodedpa.h"

/**************************************************************************
 *							
 *  get_suppl()
 *		
 *  PURPOSE
 *  This routine reads the supplemental data portion of the DPA product and outputs
 *  the values for inclusion in the DPARadar table 
 *
 *  ARGUMENTS    
 *  build_num = ORPG build number
 *  nisolbin = total number of isolated bins (Bld 4 only)
 *  noutint = total number of outliers interpolated (Bld 4 only)
 *  noutrep = total number of outliers replaced (Bld 4 only)
 *  areared = mean percent area reduction (Bld 4 only)
 *  biscanr = mean biscan ratio (Bld 4 only)
 *  nbadscan = number bad scans in hour (Bld 4 only)
 *  nhourout = number hourly outliers (Bld 4 only)
 *  blocked_bins = total number of blocked bins rejected (Bld 5 only) 
 *  clutter_bins = total number of clutter bins rejected (Bld 5 only)
 *  smoothed_bins = total number of bins smoothed (Bld 5 only)
 *  bins_filled = percent of hybrid scan bins filled (Bld 5 only)
 *  elev_angle = highest elevation angle used in hybrid scan (Bld 5 only)
 *  rain_area = total hybrid scan rain area (Bld 5 only)
 *  volcovpat = current volume coverage pattern
 *  opermode = current operational (weather) mode
 *  missper = missing period flag
 *          = F if "NO MISSING ..." message found after "OPERATIONAL WEATHER MODE" field
 *              or no supplemental data
 *              else = T
 *           
 *
 *  FILES
    Before this function is called, it is assumed that the file is open and that
    the data portion and adaptable parameter portion of the product have already
    been read.  The beginning of the supplemental data is found by searching for
    for the header "SUPL(nn)" where nn = 27.
 *
 *  RETURNS 
 *  pcipflg = 0 if the file indicates no precipitation detected.
 *	      1 if the file indicates a bad rate scan.
 *	      2 if the file indicates not enough data in hour.
 *	      3 if the file indicates a disk error.
 *	      4 if the file indicates precipitation.
 *
 *  NOTES
 *  This routine assumes that all supplemental data except the missing period
 *  flag is stored in 80 byte records, with the last 8 bytes containing the
 *  numerical value.
 *
 *  This routine reads both ORPG Bld 4 and ORPG Bld 5 format DPA products.
 *
 *  The two formats differ in the supplemental data portion after the rate scan info
 *  as follows:
 *
 *  Build 4 product :                     
 *  (1) hourly accumulation end date
 *  (2) hourly accumulation end time
 *  (3) total no. of isolated bins
 *  (4) total no. of outliers interpolated
 *  (5) total no. of outliers replaced
 *  (6) mean percent area reduction
 *  (7) mean bi-scan ratio
 *  (8) number of bad scans in hour 
 *  (9) number of hourly outliers
 *
 *  
 *  Build 5 product :                     
 *  (1) hourly accumulation end date
 *  (2) hourly accumulation end time
 *  (3) total no. of blockage bins rejected
 *  (4) total no. of clutter bins rejected
 *  (5) number of bins smoothed
 *  (6) percent of hybrid scan bins filled
 *  (7) highest elev. angle used in hybscan
 *  (8) total hybrid scan rain area
 *  (9) number of bad scans in hour
 *
    calling function: decodeDPA

 ************************************************************************/

int getsuppl(short int build_num, int *nisolbin, int *noutint, int *noutrep, float *areared,
             float *biscanr, int *nbadscan, int *nhourout, 
             int *blocked_bins, int *clutter_bins, int *smoothed_bins,
             float *bins_filled, float *elev_angle, float *rain_area,
             int *volcovpat, int *opermode, char missper[2])
             
{

   int n, numparm, cnt;
   char ch, prevchar;
   char head[4],str4[4],head4[4],head5[5],str5[5],str3[3],pcip[48],buf[9];
   int pcipflg;
   
/* Initialize all output variables */

   *areared = - 99.0;
   *biscanr= -99.0;
   *nisolbin = - 99;
   *noutint = -99;
   *noutrep = -99;
   *nbadscan = -99; 
   *nhourout = -99; 
   *blocked_bins = -99;
   *clutter_bins = -99;
   *smoothed_bins = -99;
   *bins_filled = -99.;
   *elev_angle = -99.;
   *rain_area = -99.;
   *volcovpat = -99;
   *opermode = -99;
   missper[1] = '\0';
   strcpy(missper,"F");
   pcipflg = 4;
  
/*--------------------------------------------------*/
/*  search for the supplemental data header         */
/*--------------------------------------------------*/

   head[0]='P';
   head[1]='L';
   head[2]='(';
   head[3]='\0';
   prevchar = ' ';

   for (;;)
   {
     n = fscanf(dpafile,"%c",&ch);
     if (n == EOF)
     {
        return 99;
     }

     if (ch == 'U' && prevchar == 'S')
     {
       fgets(str4, 4, dpafile);
       if (strcmp(str4,head) == 0) break;
     }
     prevchar=ch;

   }

/*----------------------------------------*/
/*  read number of parameters from header */
/*----------------------------------------*/

    fgets(str3, 3, dpafile);
    numparm = atoi(str3);

/*-----------------------------*/
/*  read past ')'              */
/*-----------------------------*/

    fseek(dpafile, 1L, SEEK_CUR);

/*---------------------------------------------------------------------------*/
/*  if numparm = 1, then check for messages                                  */
/*  else read past rate scans, hourly accum end date, hourly accum end time  */
/*---------------------------------------------------------------------------*/

    if(numparm == 1)
    {

      /*--------------------------------------------------------------------------*/
      /* Search for one of the four possible strings that indicate there is       */
      /* no supplemental data.  The strings are in the last two records of        */
      /* the DPA file.  If one is found, then output variables will remain        */
      /* as initialized, pcipflg will be set, and control will return to calling  */
      /* routine                                                                  */
      /*--------------------------------------------------------------------------*/
   
         cnt = 0; 
         while (!feof(dpafile) && cnt < 160)
         {
   
             /* Read one character and check for the shared first letter of all
               the strings */
      
             fread(&ch,sizeof(char),1,dpafile); 
             cnt += 1 ;
            
             if (ch == 'N')
             {
       
               /* Check if the rest of the string matches one of the four. */
             
               fread(pcip,sizeof(char),48,dpafile);
         
               /* Search for "NO PRECIPITATION IN PREVIOUS HOUR" */
               if (strstr(pcip,"O PRECIP") != NULL) 
   	       {       	   
       	          pcipflg = 0;
       	          return (pcipflg);
       	       }
       	 
               /* Search for "NO HOURLY ACCUMULATION BECAUSE RATE SCAN FLAGGED BAD" */       	   
               else if (strstr(pcip,"FLAGGED") != NULL)
               {       	   
       	          pcipflg = 1;
       	          return (pcipflg);
               }
       	 
               /* Search for "NO HOURLY ACCUMULATION BECAUSE NOT ENOUGH DATA IN HOUR" */        	 
               else if (strstr(pcip,"NOT ENOUGH") != NULL)     
	       {       	   
       	          pcipflg = 2;
       	          return (pcipflg);
       	       }
       	 
               /* Search for "NO SUPPLEMENTAL DATA AVAILABLE DUE TO DISK ERROR" */ 
               else if (strstr(pcip,"DISK ERROR") != NULL) 
       	       {       	   
       	          pcipflg = 3;
       	          return (pcipflg);
       	       }
       	     
             }

             printf("Unknown supplemental data message found\n");
             pcipflg = 0;
             return (pcipflg);
          	 
          }	 
            	  
          printf("No supplemental data message found\n");
          pcipflg = 0;
          return (pcipflg);
    }
    else
    {

       if(build_num == 4)
       {
          /*-------------------------------------------------------------------------*/
          /*  search for "TOTAL" from string "TOTAL NO. OF ISOLATED BINS.........:"  */
          /*-------------------------------------------------------------------------*/

          head5[0]='T';
          head5[1]='A';
          head5[2]='L';
          head5[3]=' ';
          head5[4]='\0';

          for (;;)
          {
            n = fscanf(dpafile,"%c",&ch);
            if (n==EOF)
            {
               printf("     EOF encountered before finding TOTAL NO. OF ISOLATED BINS\n");
               return 0;
            }

            if (ch == 'O' && prevchar == 'T')
            {
               fgets(str5, 5, dpafile);
               if (strcmp(str5,head5) == 0) break;
            }
            prevchar=ch;

          }

          /* Get the number of isolated bins. */
           fseek(dpafile,30L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *nisolbin = atoi(buf);

          /* Number of outliers interpolated is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *noutint = atoi(buf);
   
          /* Number of outliers replaced is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *noutrep = atoi(buf);
  
          /* Mean percent area reduction is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *areared = atof(buf);
 
          /* Mean biscan ratio is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *biscanr = atof(buf);

          /* Number of bad scans in hour is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *nbadscan = atoi(buf);

          /* Number of hourly outliers is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *nhourout = atoi(buf);

       }
       else
       {

          /*-------------------------------------------------------------------------*/
          /*  search for "TOTAL" from string "TOTAL NO. OF BLOCKAGE BINS.........:"  */
          /*-------------------------------------------------------------------------*/

          head5[0]='T';
          head5[1]='A';
          head5[2]='L';
          head5[3]=' ';
          head5[4]='\0';

          for (;;)
          {
            n = fscanf(dpafile,"%c",&ch);
            if (n==EOF)
            {
               printf("     EOF encountered before finding TOTAL NO. OF BLOCKAGE BINS\n");
               return 0;
            }

            if (ch == 'O' && prevchar == 'T')
            {
               fgets(str5, 5, dpafile);
               if (strcmp(str5,head5) == 0) break;
            }
            prevchar=ch;

          }

          /* Get the number of blockage bins. */
           fseek(dpafile,30L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *blocked_bins = atoi(buf);

          /* Number of clutter bins rejected is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *clutter_bins = atoi(buf);
   
          /* Number of bins smoothed is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *smoothed_bins = atoi(buf);
  
          /* Percent of hybrid scan bins filled is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *bins_filled = atof(buf); 
 
          /* Highest elevation angle is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *elev_angle = atof(buf);

          /* Total hybrid scan rain area is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *rain_area = atof(buf);

          /* Number of bad scans in hour is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *nbadscan = atoi(buf);

       }

          /* Skip bias estimate and bias error variance records */
          /* Skip bias estimate, effective # gr pairs, mem span records */
          /* Search for and read Current Volume Coverage Pattern */

          head5[0]='R';
          head5[1]='R';
          head5[2]='E';
          head5[3]='N';
          head5[4]='\0';

          for (;;)
          {
            n = fscanf(dpafile,"%c",&ch);
            if (n==EOF)
            {
               printf("     EOF encountered before finding CURRENT VOLUME COVERAGE PATTERN\n");
               return 0;
            }

            if (ch == 'U' && prevchar == 'C')
            {
               fgets(str5, 5, dpafile);
               if (strcmp(str5,head5) == 0) break;
            }
            prevchar=ch;

          }
           fseek(dpafile,30L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *volcovpat = atoi(buf);

          /* Current operational (weather) mode is in the next 80 byte record. */
           fseek(dpafile,72L,SEEK_CUR);
           fgets(buf,9,dpafile);
           *opermode = atoi(buf);

          /* Missing period statement is in the next 80 byte record. */

          /*----------------------------------------------------------------------*/
          /*  search for "NO MI" from string "NO MISSING PERIODS IN CURRENT HOUR" */
          /*  if found, then set missper=F                                        */
          /*  else  missper=T and set pcipflg=4                                   */
          /*----------------------------------------------------------------------*/

          head4[0]=' ';
          head4[1]='M';
          head4[2]='I';
          head4[3]='\0';

          strcpy(missper,"T");

          for (;;)
          {
            n = fscanf(dpafile,"%c",&ch);
            if (n==EOF)
            {
               break;
            }

            if (ch == 'O' && prevchar == 'N')
            {
               fgets(str4, 4, dpafile);
               if (strcmp(str4,head4) == 0)
               {
                  strcpy(missper,"F");
                  break;
               }
            }
            prevchar=ch;
          }

    }

    return (pcipflg);
}
