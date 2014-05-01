#include "decodedpa.h"
#include "DPARadar.h"
#include "time_convert.h"

void wrtodb_suppl(short int build_num, char str[22],
            short *minoff, float *maxvalh, float *maxvald,
            float *bias,
            char *gyear, char *gmonth, char *gday,
            char *ghour, char *gminute,
            int *nisolbin, int *noutint, int *noutrep, float *areared,
            float *biscanr, int *nbadscan, int *nhourout, int *volcovpat,
            int *opermode,
            int *blocked_bins, int *clutter_bins, int *smoothed_bins,
            float *bins_filled, float *elev_angle, float *rain_area,
            char missper[2], int *pcipflg, char *filen)

{
/*

     this subroutine inserts a record into the DPARadar table

     calling subroutines: decodeDPA

*/
      DPARadar fppp;
      int ret;
      char str1[22],where[100];
      dtime_t edttm, gdttm;
      bool    isUnique;

     if((ret = yearsec_ansi_to_dt(str,&edttm)))
     {
        printf("error number %d",ret);
        printf(" generating edttm in function wrtodb_suppl ");
        printf("-- record not written to database\n");
        exit(0);
     }

/* create generation date/time value  */

     sprintf(str1, "20%c%c-%c%c-%c%c %c%c:%c%c:00",gyear[0],gyear[1],
       gmonth[0],gmonth[1],gday[0],gday[1],ghour[0],ghour[1],
       gminute[0],gminute[1]);

     if((ret = yearsec_ansi_to_dt(str1,&gdttm)))
     {
        printf("error number %d",ret);
        printf(" generating gdttm in function wrtodb_suppl ");
     }

      strcpy(fppp.radid,radid);
      fppp.obstime = edttm;
      fppp.producttime = gdttm;
      fppp.minoff = *minoff;
      fppp.maxvald = *maxvald;
      fppp.maxvalh = *maxvalh;
      fppp.s1_bias_value = *bias;
      fppp.nbadscan = *nbadscan;
      fppp.opermode = *opermode;
      strcpy(fppp.missper,missper);
      fppp.volcovpat = *volcovpat;
      fppp.supplmess = *pcipflg;

      if(build_num == 4)
      {
         fppp.nisolbin = *nisolbin;
         fppp.noutint  = *noutint;
         fppp.noutrep  = *noutrep; 
         fppp.nhourout = *nhourout;
         fppp.areared  = *areared;
         fppp.biscanr  = *biscanr;

         fppp.block_bins_reject = -99;
         fppp.clutter_bins_rej = -99;
         fppp.bins_smoothed = -99; 
         fppp.scan_bins_filled = -99.;
         fppp.high_elev_angle = -99.;
         fppp.scan_rain_area = -99.;
      }
      else
      {
         fppp.nisolbin = -99.;
         fppp.noutint  = -99.;
         fppp.noutrep  = -99.; 
         fppp.nhourout = -99.;
         fppp.areared  = -99.;
         fppp.biscanr  = -99.;

         fppp.block_bins_reject = *blocked_bins;
         fppp.clutter_bins_rej = *clutter_bins;
         fppp.bins_smoothed = *smoothed_bins; 
         fppp.scan_bins_filled = *bins_filled;
         fppp.high_elev_angle = *elev_angle;
         fppp.scan_rain_area = *rain_area;
      }

      strcpy(fppp.grid_filename,filen);

      ret = InsertIfUniqueDPARadar(&fppp, &isUnique);
      if (ret == 0)
      {
         if( !isUnique)
         {
            sprintf(where,"WHERE radid= '%s' AND obstime= '%s'",radid,str);
   
            ret = UpdateDPARadar(&fppp,where);
            if(ret != 0)
              printf("PostgreSQL error %d attempting update to DPARadar table\n",ret);
         }
      }
      else
      {
         printf("PostgreSQL error %d attempting insert into DPARadar table\n",ret);
      }

}  
