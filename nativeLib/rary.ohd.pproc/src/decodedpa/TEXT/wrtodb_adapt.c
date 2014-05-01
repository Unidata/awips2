#include "decodedpa.h"
#include "DPAAdapt.h"
#include "time_convert.h"

void wrtodb_adapt(short int build_num, char strdt[22], float params[37], char param38[2])

{
/*

     this subroutine inserts a record into the DPAAdapt table
  
     ORPG Build 8 products have 32 adaptable parameters
     ORPG Build 4 and Build 5 products have 38 adaptable parameters
     the build_num variable distinguishes bet build numbers
     parameters defined for the build are stored in the structure 
     parameters not defined for the build are set to -99.

     calling subroutines: decodeDPA

*/
      DPAAdapt fppp;
      int ret;
      char where[100];
      dtime_t edttm;
      bool    isUnique;

      strcpy(fppp.radid,radid);
      if((ret = yearsec_ansi_to_dt(strdt,&edttm)))
      {
         printf("error number %d",ret);
         printf(" generating edttm in function wrtodb_adap ");
         printf("-- record not written to DPAAdapt table\n");
         return;
      }

      fppp.obstime = edttm;

      if(build_num == 8)
      {
         fppp.mlt_zrcoef = params[9];
         fppp.pwr_zrcoef = params[10];
         fppp.min_zrefl  = params[11];
         fppp.max_zrefl  = params[12];

         fppp.beam_width          = params[0];
         fppp.blockage_thresh     = params[1];
         fppp.clutter_thresh      = params[2];
         fppp.weight_thresh       = params[3];
         fppp.hybrid_scan_thresh  = params[4];
         fppp.low_reflect_thresh  = params[5];
         fppp.detect_reflect_thr  = params[6];
         fppp.detect_area_thresh  = params[7]; 
         fppp.detect_time_thresh  = params[8];
         fppp.exclusion_zones     = params[13];

         fppp.min_reflth = -99.;
         fppp.max_reflth = -99.;
         fppp.ref_tltest = -99.;
         fppp.rng_tltin  = -99.;
         fppp.rng_tltout = -99.;
         fppp.max_birng  = -99.;
         fppp.min_echoar = -99.;
         fppp.min_awrefl = -99.; 
         fppp.max_pctred = -99.;
         fppp.min_birng  = -99.;

         fppp.max_stmspd = -99.;
         fppp.max_timdif = -99.;
         fppp.min_artcon = -99.;
         fppp.tim_p1cont = -99.;
         fppp.tim_p2cont = -99.;
         fppp.max_ecarch = -99.;
         fppp.rng_cutoff = params[14];
         fppp.rng_e1coef = params[15];
         fppp.rng_e2coef = params[16];
         fppp.rng_e3coef = params[17];
         fppp.min_prate  = params[18];
         fppp.max_prate  = params[19];

         fppp.tim_restrt  = params[20];
         fppp.max_timint  = params[21];
         fppp.min_timprd  = params[22];
         fppp.thr_hlyout  = params[23];
         fppp.end_timgag  = params[24];
         fppp.max_prdval  = params[25];
         fppp.max_hlyval  = params[26];
         fppp.tim_biest   = params[27];
         fppp.thr_nosets  = params[28];
         fppp.res_bias    = params[29];
         fppp.longest_lag = params[30];
      }
      else if(build_num == 4)
      {
         fppp.mlt_zrcoef = params[9];
         fppp.pwr_zrcoef = params[10];
         fppp.min_zrefl  = params[11];
         fppp.max_zrefl  = params[12];

         fppp.min_reflth = params[0];
         fppp.max_reflth = params[1];
         fppp.ref_tltest = params[2];
         fppp.rng_tltin  = params[3];
         fppp.rng_tltout = params[4];
         fppp.max_birng  = params[5];
         fppp.min_echoar = params[6];
         fppp.min_awrefl = params[7]; 
         fppp.max_pctred = params[8];
         fppp.min_birng  = params[13];

         fppp.beam_width          = -99.;
         fppp.blockage_thresh     = -99.;
         fppp.clutter_thresh      = -99.;
         fppp.weight_thresh       = -99.;
         fppp.hybrid_scan_thresh  = -99.;
         fppp.low_reflect_thresh  = -99.;
         fppp.detect_reflect_thr  = -99.;
         fppp.detect_area_thresh  = -99.;
         fppp.detect_time_thresh  = -99.;
         fppp.exclusion_zones     = -99.;

         fppp.max_stmspd = params[14];
         fppp.max_timdif = params[15];
         fppp.min_artcon = params[16];
         fppp.tim_p1cont = params[17];
         fppp.tim_p2cont = params[18];
         fppp.max_ecarch = params[19];
         fppp.rng_cutoff = params[20];
         fppp.rng_e1coef = params[21];
         fppp.rng_e2coef = params[22];
         fppp.rng_e3coef = params[23];
         fppp.min_prate  = params[24];
         fppp.max_prate  = params[25];

         fppp.tim_restrt  = params[26];
         fppp.max_timint  = params[27];
         fppp.min_timprd  = params[28];
         fppp.thr_hlyout  = params[29];
         fppp.end_timgag  = params[30];
         fppp.max_prdval  = params[31];
         fppp.max_hlyval  = params[32];
         fppp.tim_biest   = params[33];
         fppp.thr_nosets  = params[34];
         fppp.res_bias    = params[35];
         fppp.longest_lag = params[36];
      }
      else
      {
         fppp.mlt_zrcoef = params[9];
         fppp.pwr_zrcoef = params[10];
         fppp.min_zrefl  = params[11];
         fppp.max_zrefl  = params[12];

         fppp.beam_width          = params[0];
         fppp.blockage_thresh     = params[1];
         fppp.clutter_thresh      = params[2];
         fppp.weight_thresh       = params[3];
         fppp.hybrid_scan_thresh  = params[4];
         fppp.low_reflect_thresh  = params[5];
         fppp.detect_reflect_thr  = params[6];
         fppp.detect_area_thresh  = params[7]; 
         fppp.detect_time_thresh  = params[8];
         fppp.exclusion_zones     = params[13];

         fppp.min_reflth = -99.;
         fppp.max_reflth = -99.;
         fppp.ref_tltest = -99.;
         fppp.rng_tltin  = -99.;
         fppp.rng_tltout = -99.;
         fppp.max_birng  = -99.;
         fppp.min_echoar = -99.;
         fppp.min_awrefl = -99.; 
         fppp.max_pctred = -99.;
         fppp.min_birng  = -99.;

         fppp.max_stmspd = params[14];
         fppp.max_timdif = params[15];
         fppp.min_artcon = params[16];
         fppp.tim_p1cont = params[17];
         fppp.tim_p2cont = params[18];
         fppp.max_ecarch = params[19];
         fppp.rng_cutoff = params[20];
         fppp.rng_e1coef = params[21];
         fppp.rng_e2coef = params[22];
         fppp.rng_e3coef = params[23];
         fppp.min_prate  = params[24];
         fppp.max_prate  = params[25];

         fppp.tim_restrt  = params[26];
         fppp.max_timint  = params[27];
         fppp.min_timprd  = params[28];
         fppp.thr_hlyout  = params[29];
         fppp.end_timgag  = params[30];
         fppp.max_prdval  = params[31];
         fppp.max_hlyval  = params[32];
         fppp.tim_biest   = params[33];
         fppp.thr_nosets  = params[34];
         fppp.res_bias    = params[35];
         fppp.longest_lag = params[36];
      }

      strcpy(fppp.bias_applied,param38);

      ret = InsertIfUniqueDPAAdapt(&fppp, &isUnique);
      if(ret == 0)
      {
         if( !isUnique)
         {
            sprintf(where,"WHERE radid= '%s' AND obstime= '%s'",radid,strdt);

            ret = UpdateDPAAdapt(&fppp,where);
            if(ret != 0)
              printf("PostgreSQL error %d attempting update to DPAAdapt table\n",ret);
         }
      }
      else
      {
         printf("PostgreSQL error %d attempting insert into DPAAdapt table\n",ret);
      }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc/src/decodedpa/RCS/wrtodb_adapt.c,v $";
 static char rcs_id2[] = "$Id: wrtodb_adapt.c,v 1.6 2007/09/10 13:05:31 gsood Exp $";}
/*  ===================================================  */

}  
