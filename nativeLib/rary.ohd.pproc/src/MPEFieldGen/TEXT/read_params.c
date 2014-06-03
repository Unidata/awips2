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
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2007   P Tilles          added srmosaic, sgmosaic, srgmosaic fields
*   August 2008  P Tilles          added Q2 related fields
*   May 2011     S Naples          added read of mpe_process_PC token
********************************************************************************
*/

#include "mpe_fieldgen.h"
#include "get_os_system.h"

extern char * MPEFieldGen_Mosaics[];

void MPEFieldGen_readParams(mpe_params_struct * pMPEParams )
{
    char    token[TOKEN_LEN], systname[10];
    char    tokenvalue[TOKEN_VALUE_LEN];

extern int mpe_editor_call;

    /**
     * initialize variables written to header of xmrg files
     * operating system (HP or LX) is prepended to user name
     **/
    MPEFieldGen_get_os_system(systname);
    toLowerCase(systname);
    strcpy(pMPEParams->os, "LX");

    sprintf(pMPEParams->user, "%sSAN", pMPEParams->os);

    sprintf( message , "STATUS: loading parameters...") ;

    /****** read from .Apps_Defaults files *******/

    /**
      *  read db_name = database name.
     **/
    if(getAppsDefaults("db_name", tokenvalue) == -1)
    {
        sprintf ( message , "ERROR: The value of token"
            " \"%s\" is not available."
            "\n\tProgram exit.", token) ;
        shutDownMPE( message, logFile );
    }
    strncpy(pMPEParams->db_name, tokenvalue, DB_DESCR_LEN);
    pMPEParams->db_name[DB_DESCR_LEN] = '\0';

    /**
     * read mpe_save_gif = flag to create and save GIF image.
     * if token is not found, then 0 is assumed.
     **/
    pMPEParams->mpe_save_gif = 0;
    if((getAppsDefaults("mpe_save_gif", tokenvalue) != -1)
         && (strcmp(toLowerCase(tokenvalue), "save") == 0))
    {
        pMPEParams->mpe_save_gif = 1;
    }

    /**
     * read mpe_save_jpeg = flag to create and save JPEG image.
     * if token is not found, then 0 is assumed.
     **/
    pMPEParams->mpe_save_jpeg = 0;
    if((getAppsDefaults("mpe_save_jpeg", tokenvalue) != -1)
         && (strcmp(toLowerCase(tokenvalue), "save") == 0))
    {
        pMPEParams->mpe_save_jpeg = 1;
    }

    /**
     * read mpe_save_grib = flag to create and save GRIB image.
     * if token is not found, then 0 is assumed.
     **/
    pMPEParams->mpe_save_grib = 0;
    if((getAppsDefaults("mpe_save_grib", tokenvalue) != -1)
         && (strcmp(toLowerCase(tokenvalue), "save") == 0))
    {
        pMPEParams->mpe_save_grib = 1;
    }

    /**
     * read mpe_save_netcdf = flag to create and save netCDF image.
     * if token is not found, then 0 is assumed.
     **/
    pMPEParams->mpe_save_netcdf = 0;
    if((getAppsDefaults("mpe_save_netcdf", tokenvalue) != -1)
         && (strcmp(toLowerCase(tokenvalue), "save") == 0))
    {
        pMPEParams->mpe_save_netcdf = 1;
    }

    /**
     *    read rfc name
     **/
    if(getAppsDefaults("rfcw_rfcname", tokenvalue) == -1)
    {
        sprintf( message , "\tRFC Name is NULL" ) ;
        printMessage( message, logFile );
    }
    else
    {
        strncpy(pMPEParams->rfc_name, tokenvalue, RFC_NAME_LEN);
        pMPEParams->rfc_name[RFC_NAME_LEN] = '\0';
    }

    /**
     * read dpa_wind = number of minutes
     * around top of hour in which to search
     * for a top-of-hour product
     * if token is not found, then Delete Gages Zeros = OFF is assumed
     **/
    pMPEParams->dpa_wind = 10;
    if((getAppsDefaults("dpa_wind", tokenvalue) != -1)
         && (isDigits(tokenvalue) == 1))
    {
        pMPEParams->dpa_wind = atoi(tokenvalue);
    }
    else
    {
        sprintf( message , "\tDefault DPA_WIND = 10 minutes" ) ;
        printMessage( message, logFile );
    }

    /**
     * read daa_wind = number of minutes
     * around top of hour in which to search
     * for a top-of-hour DAA product
     * if token is not found, then 5 minutes is assumed.
     **/
    pMPEParams->daa_wind = 5;    
    if((getAppsDefaults("daa_wind", tokenvalue) != -1)
         && (isDigits(tokenvalue) == 1))
    {
        pMPEParams->daa_wind = atoi(tokenvalue);
    }
    else
    {
        sprintf( message , "\tDefault DAA_WIND = 5 minutes" ) ;
        printMessage( message, logFile );
    }

    /**
     * read daa_min_coverage_dur = minimum coverage of an hour
     * for a radar product to be considered good
     * units = minutes                        
     * if token is not found, then 60 minutes is assumed.
     **/
    pMPEParams->daa_min_coverage_dur = 60;    
    if((getAppsDefaults("daa_min_coverage_dur", tokenvalue) != -1)
         && (isDigits(tokenvalue) == 1))
    {
        pMPEParams->daa_min_coverage_dur = atoi(tokenvalue);
    }
    else
    {
        sprintf( message , "\tDefault min_coverage_dur = 60 minutes" ) ;
        printMessage( message, logFile );
    }

   /**
     * read del_gage_zeros = flag if need delete zeros in gage data
     * if token is not found, then Delete Gages Zeros = OFF is assumed
     **/
    pMPEParams->del_gage_zeros = 0;
    if((getAppsDefaults("mpe_del_gage_zeros", tokenvalue) != -1)
         && (strcmp(toLowerCase(tokenvalue), "on") == 0))
    {
        pMPEParams->del_gage_zeros = 1;
        sprintf( message , "\tDelete Gages Zeros = ON" ) ;
        printMessage( message, logFile );
    }
    else
    {
        sprintf( message , "\tDelete Gages Zeros = OFF" ) ;
        printMessage( message, logFile );
    }

    /**
      * read process_PC = flag
      * this flag is used to turn off the PC to PPH processing
      * if token is not found, then Process PC = OFF is assumed
      **/
     pMPEParams->process_PC = 1;
     if((getAppsDefaults("mpe_process_PC", tokenvalue) != -1)
          && (strcmp(toLowerCase(tokenvalue), "off") == 0))
     {
         pMPEParams->process_PC = 0;
         sprintf( message , "\tProcess PC Data = OFF" ) ;
         printMessage( message, logFile );
     }
     else
     {
         sprintf( message , "\tProcess PC Data = ON " ) ;
         printMessage( message, logFile );
     }

    /**
     * read xmrgdtform token
     * xmrgdtform = format of time stamp in xmrg filename
     *            = mdY (= mmddyyyyhh)  OR  Ymd (= yyyymmddhh)
     *    default = mdY
     **/
    if(getAppsDefaults("st3_date_form", tokenvalue) == -1)
    {
        strcpy(pMPEParams->xmrgdtform, "mdY");
        sprintf( message , "\tDefault XMRGDTFORM = \"mdY\"" ) ;
        printMessage( message, logFile );
    }
    else
    {
        strncpy(pMPEParams->xmrgdtform, tokenvalue, XMRGDTFORM_LEN);
        pMPEParams->xmrgdtform[XMRGDTFORM_LEN] = '\0';
    }

    /**
     * read gage_qc = flag to run Gauge QC.
     * if token is not found, then 0 is assumed.
     **/
    pMPEParams->gage_qc = 0;
    if(!mpe_editor_call && (getAppsDefaults("mpe_gage_qc", tokenvalue) != -1)
         && (strcmp(toLowerCase(tokenvalue), "on") == 0))
    {
        pMPEParams->gage_qc = 1;
        sprintf( message , "\tGauge QC = ON" ) ;
        printMessage( message, logFile );
    }
    else
    {
        sprintf( message , "\tGauge QC = OFF" ) ;
        printMessage( message, logFile );
    }

    /**
     * read locbias_1hr_rerun = the local bias rerun.
     * if token is not found, then 0 is assumed.
     **/
    pMPEParams->locbias_1hr_rerun = 0;
    if((getAppsDefaults("mpe_locbias_1hr_rerun", tokenvalue) != -1)
     && (strcmp(toLowerCase(tokenvalue), "on") == 0))
    {
        pMPEParams->locbias_1hr_rerun = 1;
        sprintf( message , "\tLocal bias recalculation on rerun = ON" ) ;
        printMessage( message, logFile );
    }
    else
    {
        sprintf( message , "\tLocal bias recalculation on rerun = OFF" ) ;
        printMessage( message, logFile );
    }

    /**
     * read FXA_LOCAL_SITE environment variable.
     **/
    memset ( pMPEParams->fxa_local_site, '\0', RFC_LEN + 1 );
    if((getAppsDefaults("FXA_LOCAL_SITE", tokenvalue) != 0))
    {
       sprintf ( message , "ERROR: The value of token"
                 " \"FXA_LOCAL_SITE\" is not available."
                 "\n\tProgram exit.") ;
       shutDownMPE( message, logFile );
    }
    else
    {
       strncpy ( pMPEParams->fxa_local_site, tokenvalue, RFC_LEN );
       sprintf ( message, "The value of FXA_LOCAL_SITE is %s\n"
                          "Bias information will be written to the "
			  "RWBiasStat and RWBiasDyn tables under this id.\n",
			   pMPEParams->fxa_local_site );
    }

    /**
     * read qpe_fieldtype = qpe best estimate type.
     **/
    if(getAppsDefaults("mpe_qpe_fieldtype", tokenvalue) != -1)
    {
        toLowerCase(tokenvalue);

        /**
         * Check if the qpe_fieldgen is correct
         * by comparing it with the mosaic list.
         **/
        mosaicType index = rmosaic ;
        while((index < num_mosaics) &&
             (strcmp(tokenvalue, MPEFieldGen_Mosaics[index]) != 0))
            index ++ ;

        /**
         * if not found, invalid qpe and then exit program.
         **/
        if(index == num_mosaics)
        {
            sprintf ( message , "ERROR: qpe best estimate type IS INVALID."
                        "\n\tProgram exit.") ;
            shutDownMPE( message, logFile  );
        }

        strcpy( pMPEParams->qpe_fieldtype, tokenvalue);
        sprintf( message , "\tqpe best estimate type = %s",
             pMPEParams->qpe_fieldtype ) ;
        printMessage( message, logFile  );

        /* This needs to be stored so that the correct set of polygons
           can be applied to the Best Estimate QPE product. */
        switch ( index )
        {
           case rmosaic:
              pMPEParams->mosaic_type = display_rMosaic;
              break;

           case avgrmosaic:
              pMPEParams->mosaic_type = display_avgrMosaic;
              break;

           case maxrmosaic:
              pMPEParams->mosaic_type = display_maxrMosaic;
              break;

           case gaugeonly:
              pMPEParams->mosaic_type = display_gageOnly;
              break;

           case bmosaic:
              pMPEParams->mosaic_type = display_bMosaic;
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

           case satpre:
              pMPEParams->mosaic_type = display_satPrecip;
              break;

           case srmosaic:
              pMPEParams->mosaic_type = display_srMosaic;
              break;

           case sgmosaic:
              pMPEParams->mosaic_type = display_sgMosaic;
              break;

           case srgmosaic:
              pMPEParams->mosaic_type = display_srgMosaic;
              break;
		   case qmosaic:
              pMPEParams->mosaic_type = display_qMosaic;
              break;

           case lqmosaic:
              pMPEParams->mosaic_type = display_lqMosaic;
              break;

           case mlqmosaic:
              pMPEParams->mosaic_type = display_mlqMosaic;
              break;
           case p3lmosaic:
              pMPEParams->mosaic_type = display_p3Mosaic;
              break;

           case rfcmosaic:
              pMPEParams->mosaic_type = display_rfcMosaic;
              break;

           case rfcmmosaic:
              pMPEParams->mosaic_type = display_rfcmMosaic;
              break;

           case rfcbmosaic:
              pMPEParams->mosaic_type = display_rfcbMosaic;
              break;


           default:
              sprintf ( message , "ERROR: Could not find enum DisplayFieldData "
                                  "type for %s."
                                  "\n\tProgram exit.", MPEFieldGen_Mosaics[index]) ;
              shutDownMPE( message, logFile  );
              break;

        }

    }
    else
    {
        sprintf ( message , "ERROR: qpe best estimate type IS NOT AVAILABLE."
                    "\n\tProgram exit.") ;
        shutDownMPE( message, logFile  );
    }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/read_params.c,v $";
 static char rcs_id2[] = "$Id: read_params.c,v 1.2 2007/11/01 17:27:46 lawrence Exp $";}
/*  ===================================================  */

}
