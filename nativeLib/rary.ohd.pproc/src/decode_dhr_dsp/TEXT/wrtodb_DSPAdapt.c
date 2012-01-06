/***********************************************************************
* Filename: wrtodb_DSPAdapt.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: 7/17/2006
*
* Development Group: OHD
*
* Description:
* Contains routine for inserting a record into the DSPAdapt table.
*
* Modules:
* wrtodb_DSPAdapt
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include "DSPAdapt.h"
#include "decode_radar_product.h"
#include "time_convert.h"

/***********************************************************************
* Module Name: wrtodb_DSPAdapt
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: 7/17/2006
*
* Description:
*   This subroutine inserts a record into the DSPAdapt table.
*
*    calling function: decodeDSP
*
* Calling Arguments:
* Name         Input/Output Type          Description
* obstime      Input        char *        The obstime string
* radid        Input        char *        The radar id
* params       Input        float[45]     The adaptable parameter array.
* param46      Input        char[2]       The adaptable flag parameter.
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* 
*
* Return Value:
* Type          Description
* None
*
* Error Codes/Exceptions:
* 
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
* Date        Developer     Action
* 7/19/2006   Guoxian Zhou  Build the operational version
*10/26/2006   Guoxian Zhou  Modification for parameter structure
*                           change from OPRG build 8 
*
***********************************************************************/

void wrtodb_DSPAdapt(char * obstime,
                     const char * radid, 
                     const float params[45],
                     const char param46[2])
{
    DSPAdapt fppp;
    int status;
    dtime_t edttm;

    static int first = 0;
    static int build_version = 8;

    int len;
    char dsp_build_version[5] = {'\0'};

    strcpy(fppp.radid, radid);

    status = yearsec_ansi_to_dt(obstime, &edttm) ;
    if(status != 0)
    {
        printf("error number %d",status);
        printf(" generating edttm in function wrtodb_DSPAdapt ");
        printf("-- record not written to DSPAdapt table\n");
        return;
    }

    if(first == 0)
    {
    	/*
    	 * load the DSP ORPG build version
    	 */

	    len = strlen("dsp_build_version");
	
	    get_apps_defaults("dsp_build_version", &len, dsp_build_version, &len);
	    int build = atoi(dsp_build_version);
	    if( (build == 5) || (build == 8) )
	    {
	    	build_version = build;
	    }

	    first = 1;
    }

    fppp.obstime = edttm;

    if(build_version == 5)
    { 
        fppp.min_reflth = params[0];
        fppp.max_reflth = params[1];
        fppp.ref_tltest = params[2];
        fppp.rng_tltin  = params[3];
        fppp.rng_tltout = params[4];
        fppp.max_birng  = params[5];
        fppp.min_echoar = params[6];
        fppp.min_awrefl = params[7]; 
        fppp.max_pctred = params[8];
        fppp.mlt_zrcoef = params[9];
        fppp.pwr_zrcoef = params[10];
        fppp.min_zrefl  = params[11];
        fppp.max_zrefl  = params[12];
        fppp.min_birng  = params[13];

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
    else if(build_version >= 8)
    {
        fppp.min_reflth = params[0];
        fppp.max_reflth = params[1];
        fppp.ref_tltest = params[2];
        fppp.rng_tltin  = params[3];
        fppp.rng_tltout = params[4];
        fppp.max_birng  = params[5];
        fppp.min_echoar = params[6];
        fppp.min_awrefl = params[7]; 
        fppp.max_pctred = params[8];
        fppp.mlt_zrcoef = params[9];
        fppp.pwr_zrcoef = params[10];
        fppp.min_zrefl  = params[11];
        fppp.max_zrefl  = params[12];
        fppp.min_birng  = params[13];

        /*
         * The following 6 parameters have been removed
         * from ORPG build 8.
         * set them to default missing value.
         */    

        fppp.max_stmspd = FLOAT_MISSING;
        fppp.max_timdif = FLOAT_MISSING;
        fppp.min_artcon = FLOAT_MISSING;
        fppp.tim_p1cont = FLOAT_MISSING;
        fppp.tim_p2cont = FLOAT_MISSING;
        fppp.max_ecarch = FLOAT_MISSING;

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

    strcpy(fppp.bias_applied, param46);

    InsertOrUpdateDSPAdapt(&fppp);

}  
