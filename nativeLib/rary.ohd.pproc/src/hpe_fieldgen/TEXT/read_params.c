/*******************************************************************************
* FILENAME:            read_params.c
*
* Purpose:
* This function loads static data from .Apps_defaults token list.
* parameters are plugged into mpe_params_struct.
*
* calling function: main_mpe_fieldgen
* functions called: NULL
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         March, 2005
* ORGANIZATION:          HSEB / OHD
* MACHINE:               HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   March 2005   Guoxian Zhou      first version
********************************************************************************
*/

#include "empe_fieldgen.h"
#include "get_os_system.h"

extern char * Mosaics[];

void readParams(empe_params_struct * pMPEParams )
{
    char systname[10] = {'\0'};
    char tokenvalue[TOKEN_VALUE_LEN] = {'\0'};

    /*
     * initialize variables written to header of xmrg files
     * operating system (HP or LX) is prepended to user name
     */

    get_os_system(systname);
    hpe_fieldgen_toLowerCase(systname);

    if(strcmp(systname, "hp-ux") == 0)
    {
        strcpy(pMPEParams->os, "HP");
    }
    else
    {
        strcpy(pMPEParams->os, "LX");
    }

    sprintf(pMPEParams->user, "%sSAN", pMPEParams->os);

    sprintf( message , "STATUS: loading parameters...") ;
    hpe_fieldgen_printMessage( message);

    /****** read from .Apps_Defaults files *******/

    /*
     *  read db_name = database name.
     */

    if(hpe_fieldgen_getAppsDefaults("db_name", tokenvalue) == -1)
    {
        sprintf ( message , "ERROR: The value of token"
            " \"db_name\" is not available."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    strncpy(pMPEParams->db_name, tokenvalue, DB_DESCR_LEN);
    pMPEParams->db_name[DB_DESCR_LEN] = '\0';

    /*
     * read save_gif = flag to create and save GIF image.
     * if token is not found, then 0 is assumed.
     */

    pMPEParams->save_gif = 0;
/*
    if((getAppsDefaults("mpe_save_gif", tokenvalue) != -1)
         && (strcmp(toLowerCase(tokenvalue), "save") == 0))
    {
        pMPEParams->save_gif = 1;
    }
*/
    /*
     * read save_jpeg = flag to create and save JPEG image.
     * if token is not found, then 0 is assumed.
     */

    pMPEParams->save_jpeg = 0;
/*
    if((getAppsDefaults("mpe_save_jpeg", tokenvalue) != -1)
         && (strcmp(toLowerCase(tokenvalue), "save") == 0))
    {
        pMPEParams->save_jpeg = 1;
    }
*/
    /*
     * read save_grib = flag to create and save GRIB image.
     * if token is not found, then 0 is assumed.
     */

    pMPEParams->save_grib = 0;
/*
    if((getAppsDefaults("mpe_save_grib", tokenvalue) != -1)
         && (strcmp(toLowerCase(tokenvalue), "save") == 0))
    {
        pMPEParams->save_grib = 1;
    }
*/
    /*
     * read save_netcdf = flag to create and save netCDF image.
     * if token is not found, then 0 is assumed.
     */

    pMPEParams->save_netcdf = 0;
/*
    if((getAppsDefaults("mpe_save_netcdf", tokenvalue) != -1)
         && (strcmp(toLowerCase(tokenvalue), "save") == 0))
    {
        pMPEParams->save_netcdf = 1;
    }
*/

    /*
     * read rfc name
     */

    if(hpe_fieldgen_getAppsDefaults("rfcw_rfcname", tokenvalue) == -1)
    {
        sprintf( message , "\tRFC Name is NULL" ) ;
        hpe_fieldgen_printMessage( message);
    }
    else
    {
        strncpy(pMPEParams->rfc_name, tokenvalue, RFC_NAME_LEN);
        pMPEParams->rfc_name[RFC_NAME_LEN] = '\0';
    }

    /*
     * read dpa_window = number of minutes
     * around top of hour in which to search
     * for a top-of-hour product
     * if token is not found, then 10 minutes is assumed.
     */

    pMPEParams->dpa_window = 10;
/*
    if((getAppsDefaults("dpa_wind", tokenvalue) != -1)
         && (isDigits(tokenvalue) == 1))
    {
        pMPEParams->dpa_window = atoi(tokenvalue);
    }
    else
    {
        sprintf( message , "\tDefault DPA_WINDOW = 10 minutes" ) ;
        printMessage( message);
    }
*/

    /*
     * read dhr_window = number of minutes
     * around top of hour in which to search
     * for a top-of-hour product
     * if token is not found, then DHR_WINDOW_DEFAULT minutes is assumed.
     */

    pMPEParams->dhr_window = DHR_WINDOW_DEFAULT;
    if((hpe_fieldgen_getAppsDefaults("dhr_window", tokenvalue) != -1)
         && (hpe_fieldgen_isDigits(tokenvalue) == 1))
    {
        pMPEParams->dhr_window = atoi(tokenvalue);
    }
    else
    {
        sprintf( message , "\tDefault DHR_WINDOW = 10 minutes" ) ;
        hpe_fieldgen_printMessage( message);
    }

    /*
     * read dsp_window = number of minutes
     * around top of hour in which to search
     * for a top-of-hour product
     * if token is not found, then DSP_WINDOW_DEFAULT minutes is assumed.
     */

    pMPEParams->dsp_window = DSP_WINDOW_DEFAULT;
    if((hpe_fieldgen_getAppsDefaults("dsp_window", tokenvalue) != -1)
         && (hpe_fieldgen_isDigits(tokenvalue) == 1))
    {
        pMPEParams->dsp_window = atoi(tokenvalue);
    }
    else
    {
        sprintf( message , "\tDefault DSP_WINDOW = 10 minutes" ) ;
        hpe_fieldgen_printMessage( message);
    }

    /*
     * read dsp_duration = duration minutes
     * for dsp product
     * if token is not found,
     * then DSP_DURATION_DEFAULT minutes is assumed.
     */

    pMPEParams->dsp_duration = DSP_DURATION_DEFAULT;
    if((hpe_fieldgen_getAppsDefaults("dsp_duration", tokenvalue) != -1)
         && (hpe_fieldgen_isDigits(tokenvalue) == 1))
    {
        pMPEParams->dsp_duration = atoi(tokenvalue);
    }
    else
    {
        sprintf( message , "\tDefault DSP_DURATION = 60 minutes" ) ;
        hpe_fieldgen_printMessage( message);
    }

    /*
     * read hpe_hrap_grid_facor = grid factor
     *      1 -- HRAP grid
     *      4 -- quarter HRAP grid
     * if token is not found, then quarter HRAP grid is assumed.
     */

    pMPEParams->hrap_grid_factor = 4;
    if((hpe_fieldgen_getAppsDefaults("hpe_hrap_grid_factor", tokenvalue) != -1)
         && (hpe_fieldgen_isDigits(tokenvalue) == 1))
    {
    	int value = atoi(tokenvalue);
    	if( (value == 1) || (value == 4) )
    	{
    		pMPEParams->hrap_grid_factor = value;
    		switch(value)
    		{
    			case 1:
    			    sprintf( message , "\tHRAP Grid = HRAP" ) ;
    			    break;
    			case 4:
    			    sprintf( message , "\tHRAP Grid = Quarter HRAP" ) ;
    			    break;
    		}
	        hpe_fieldgen_printMessage( message);
    	}
    	else
    	{
	        sprintf( message , "\tINVALID token value for "
	                           "\"hpe_hrap_grid_factor\". "
	                           "Default grid facor = 4" ) ;
	        hpe_fieldgen_printMessage( message);
    	}
    }
    else
    {
        sprintf( message , "\tDefault grid factor = 4" ) ;
        hpe_fieldgen_printMessage( message);
    }

    /*
     * read del_gage_zeros = flag if need delete zeros in gage data
     * if token is not found, then "off" is assumed.
     */

    pMPEParams->del_gage_zeros = 0;
    if((hpe_fieldgen_getAppsDefaults("hpe_del_gage_zeros", tokenvalue) != -1)
         && (strcmp(hpe_fieldgen_toLowerCase(tokenvalue), "on") == 0))
    {
        pMPEParams->del_gage_zeros = 1;
        sprintf( message , "\tDelete Gages Zeros = ON" ) ;
        hpe_fieldgen_printMessage( message);
    }
    else
    {
        sprintf( message , "\tDelete Gages Zeros = OFF" ) ;
        hpe_fieldgen_printMessage( message);
    }

    /*
     * read xmrgdtform token
     * xmrgdtform = format of time stamp in xmrg filename
     *            = mdY (= mmddyyyyhh)  OR  Ymd (= yyyymmddhh)
     *    default = mdY
     */

    if(hpe_fieldgen_getAppsDefaults("st3_date_form", tokenvalue) == -1)
    {
        strcpy(pMPEParams->xmrgdtform, "mdY");
        sprintf( message , "\tDefault XMRGDTFORM = \"mdY\"" ) ;
        hpe_fieldgen_printMessage( message);
    }
    else
    {
        strncpy(pMPEParams->xmrgdtform, tokenvalue, XMRGDTFORM_LEN);
        pMPEParams->xmrgdtform[XMRGDTFORM_LEN] = '\0';
    }

    /*
     * read gage_qc = flag to run Gauge QC.
     * if token is not found, then 0 is assumed.
     */

    pMPEParams->gage_qc = 0;
    if((hpe_fieldgen_getAppsDefaults("hpe_gage_qc", tokenvalue) != -1)
         && (strcmp(hpe_fieldgen_toLowerCase(tokenvalue), "on") == 0))
    {
        pMPEParams->gage_qc = 1;
        sprintf( message , "\tGauge QC = ON" ) ;
        hpe_fieldgen_printMessage( message);
    }
    else
    {
        sprintf( message , "\tGauge QC = OFF" ) ;
        hpe_fieldgen_printMessage( message);
    }

    /*
     * read locbias_1hr_rerun = the local bias rerun.
     * if token is not found, then 0 is assumed.
     */

    pMPEParams->locbias_1hr_rerun = 0;
    if((hpe_fieldgen_getAppsDefaults("hpe_locbias_1hr_rerun", tokenvalue) != -1)
     && (strcmp(hpe_fieldgen_toLowerCase(tokenvalue), "on") == 0))
    {
        pMPEParams->locbias_1hr_rerun = 1;
        sprintf( message , "\tLocal bias recalculation on rerun = ON" ) ;
        hpe_fieldgen_printMessage( message);
    }
    else
    {
        sprintf( message , "\tLocal bias recalculation on rerun = OFF" ) ;
        hpe_fieldgen_printMessage( message);
    }

    /*
     * read FXA_LOCAL_SITE environment variable.
     */

    if((hpe_fieldgen_getAppsDefaults("FXA_LOCAL_SITE", tokenvalue) != 0) &&
       (hpe_fieldgen_getAppsDefaults("fxa_local_site", tokenvalue) != 0))
    {
       sprintf ( message , "ERROR: The value of token"
                 " \"FXA_LOCAL_SITE\" is not available."
                 "\n\tProgram exit.") ;
       hpe_fieldgen_printMessage( message);		 
       shutdown( message );
    }
    else
    {
       strncpy ( pMPEParams->fxa_local_site, tokenvalue, WFO_LEN );

       sprintf ( message, "\tfxa_local_site = %s\n",                          
                           pMPEParams->fxa_local_site );
       hpe_fieldgen_printMessage( message);

    }

    /*
     * read hpe_load_misbin = loading the misbin file.
     * if token is not found, then 0 is assumed.
     */

    pMPEParams->load_misbin = 0;
    if((hpe_fieldgen_getAppsDefaults("hpe_load_misbin", tokenvalue) != -1)
     && (strcmp(hpe_fieldgen_toLowerCase(tokenvalue), "on") == 0))
    {
        pMPEParams->load_misbin = 1;
        sprintf( message , "\tLoad misbin token = ON" ) ;
        hpe_fieldgen_printMessage( message);
    }
    else
    {
        sprintf( message , "\tLoad misbin token = OFF" ) ;
        hpe_fieldgen_printMessage( message);
    }

    /*
     * read hpe_use_locbias = Flag for building EBMosaic and BDHRMosaic
     *                         by using mean field bias or local bias.
     *                         0 -- use mean field bias
     *                         1 -- use local bias
     * if token is not found, then 0 is assumed.
     */

    pMPEParams->blnUseLocalBias = 0;
    if((hpe_fieldgen_getAppsDefaults("hpe_use_locbias", tokenvalue) != -1)
     && (strcmp(hpe_fieldgen_toLowerCase(tokenvalue), "on") == 0))
    {
        pMPEParams->blnUseLocalBias = 1;
        sprintf( message , "\tUse local bias = ON" ) ;
        hpe_fieldgen_printMessage( message);
    }
    else
    {
        sprintf( message , "\tUse mean field bias = ON" ) ;
        hpe_fieldgen_printMessage( message);
    }


    /*
      * read hpe_run_nowcast = Flag for running nowcast
      *                         0 -- not run nowcast
      *                         1 -- run nowcast
      * if token is not found, then 0 is assumed.
      */

     pMPEParams->blnRunNowcast = 0;
     if ((hpe_fieldgen_getAppsDefaults("hpe_run_nowcast", tokenvalue) != -1) && (strcmp(
             toLowerCase(tokenvalue), "on") == 0))
     {
         pMPEParams->blnRunNowcast = 1;
         sprintf(message, "\tRun Nowcast = ON") ;
         hpe_fieldgen_printMessage(message);
     } else
     {
         sprintf(message, "\tRun Nowcast = OFF") ;
         hpe_fieldgen_printMessage(message);
     }

    /*
     * read qpe_fieldtype = qpe best estimate type.
     */

    int empe_qpe_fieldtype_len = BESTFIELD_LEN ;
    int status ;
    int verbose = VERBOSE ;
    get_empe_qpe_fieldtype ( &verbose , tokenvalue ,
                             &empe_qpe_fieldtype_len , &status ) ;

    if(status == 0)
    {
        hpe_fieldgen_toLowerCase(tokenvalue);

        /*
         * Check if the qpe_fieldgen is correct
         * by comparing it with the mosaic list.
         */

        mosaicType index = dhrmosaic ;
        while((index < num_mosaics) &&
             (strcmp(tokenvalue, Mosaics[index]) != 0))
        {
            index ++ ;
        }

        /*
         * if not found, invalid qpe and then exit program.
         */

        if(index == num_mosaics)
        {
            sprintf ( message , "ERROR: token \"hpe_qpe_fieldtype\" IS"
                                " INVALID. reset this token and run again.") ;
            shutdown( message );
        }

        strcpy( pMPEParams->qpe_fieldtype, tokenvalue);
        sprintf( message , "\tqpe best estimate type = %s",
             pMPEParams->qpe_fieldtype ) ;
        hpe_fieldgen_printMessage( message );

        /*
         * This needs to be stored so that the correct set of polygons
         * can be applied to the Best Estimate QPE product.
         */

        switch ( index )
        {
           case dhrmosaic:
              pMPEParams->mosaic_type = display_dhrMosaic;
              break;

           case bdhrmosaic:
              pMPEParams->mosaic_type = display_bdhrMosaic;
              break;

           case ermosaic:
              pMPEParams->mosaic_type = display_erMosaic;
              break;

           case avgermosaic:
              pMPEParams->mosaic_type = display_avgerMosaic;
              break;

           case maxermosaic:
              pMPEParams->mosaic_type = display_maxerMosaic;
              break;

           case gaugeonly:
              pMPEParams->mosaic_type = display_gageOnly;
              break;

           case ebmosaic:
              pMPEParams->mosaic_type = display_ebMosaic;
              break;

           case lmosaic:
              pMPEParams->mosaic_type = display_lMosaic;
              break;

           case mmosaic:
              pMPEParams->mosaic_type = display_mMosaic;
              break;

           case mlmosaic:
              pMPEParams->mosaic_type = display_mlMosaic;
              break;

           case lsatpre:
              pMPEParams->mosaic_type = display_lsatPrecip;
              break;

           case p3lmosaic:
              pMPEParams->mosaic_type = display_p3Mosaic;
              break;

           default:
              sprintf ( message , "ERROR: could not find enum DisplayFieldData"
                                  " type for %s."
                                  "\n\tProgram exit.", Mosaics[index]) ;
              shutdown( message );
              break;
        }
    }
    else
    {
        sprintf ( message , "ERROR: fail to load token \"hpe_qpe_fieldtype\". "
                            "Reset this token and run again.");
        shutdown( message );
    }

}
