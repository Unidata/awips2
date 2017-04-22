/*******************************************************************************
* FILENAME:            write_qpe.c
*
* Purpose:
* This function writes out "best estimate" data
*
* calling function: main_mpe_fieldgen
* functions called: writeRWResult, writeArray,
*                    saveGrib, saveNetCDF, saveGif
*
* input variables
*
* pRunDate - date/time
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*           and dimension of the RFC estimation domain
*
* pMPEParams - static parameters
*
* gageNum - the total gage number of gage data
*
* pQPEMosaic - the "best estimate" mosaic of radars.
*
* output variables
*
* none
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   May 2005     Guoxian Zhou      finish conversion to C Language
*   04/10/2006   Guoxian Zhou      clean junk string in xmrg head file
*   Nov 17, 2006 Bryon Lawrence    Modified to write a 2nd QPE file with proc
*                                  flag of QPE01 if sbn transmission is
*                                  required.
*
********************************************************************************
*/
#include "mpe_fieldgen.h"
#include "save_grib.h"

void MPEFieldGen_writeQPE(const run_date_struct * pRunDate,
            const mpe_params_struct * pMPEParams,
            const geo_data_struct * pGeoData ,
            const int gageNum,
            const int gageNumP3,
            double ** pQPEMosaic)
{
    char * pGribCommand = NULL;
    char * pSaveDate = NULL;
    int day;
    int hour;
    int month;
    const int replace_missing = 1 ;
    int year;

    static char fname_xmrg[FNAME_LEN] = {'\0'};
    static char xmrg_dir[PATH_LEN] = {'\0'} ;
    static char qpe_sbn_dir[PATH_LEN] = {'\0'} ;
    static char send_to_sbn[PATH_LEN] = {'\0'} ;


    static char netcdf_dir[PATH_LEN] = {'\0'};
    static char netcdf_id[FNAME_LEN] = {'\0'} ;
    static char fname_netcdf[FNAME_LEN] = {'\0'};

    static char gif_dir[PATH_LEN] = {'\0'};
    static char gif_id[FNAME_LEN] = {'\0'};

    static char fname_grib[FNAME_LEN] = {'\0'};
    static char grib_id[FNAME_LEN] = {'\0'} ;

    static char save_file_path[PATH_LEN + FNAME_LEN] = {'\0'};

    static char datetime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'};
    static char dateYMD[YYYYMMDDHH_LEN + 1] = {'\0'};
    static char dateMDY[YYYYMMDDHH_LEN + 1] = {'\0'};
    int  hrap_x, hrap_y, hrap_cols, hrap_rows ;
    long int irc ;
    struct tm * pRunTime = NULL ;

    /* xmrg process flag needs 8 characters + term null.
       empty characters are blank-filled. */
    char proc_flag_sbn[9] = "QPE01   ";
    char proc_flag_local[9] = "MPA01   ";

    static int first = 1 ;

    /**
     * dateYMD string should be in format: yyyymmddhh
     * dateMDY string should be in format: mmddyyyyhh
     **/
    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime ( datetime, ANSI_YEARSEC_TIME_LEN + 1,
            "%Y-%m-%d %H:00:00", pRunTime ) ;
    strftime(dateYMD, YYYYMMDDHH_LEN + 1, "%Y%m%d%H", pRunTime);
    strftime(dateMDY, YYYYMMDDHH_LEN + 1, "%m%d%Y%H", pRunTime);

    hrap_x    = pGeoData->hrap_x ;
    hrap_y    = pGeoData->hrap_y ;
    hrap_cols = pGeoData->num_cols ;
    hrap_rows = pGeoData->num_rows ;

    /**
      * write xmrg file for use by CALB MAPX preprocessor
      * xmrgdtform determines form of dateYMD stamp in filename
      * if REPLACE_MISSING = 1, then replace values = missing with 0.0
      * proc_flag = MPA01 is for use by grib encoder
      **/
    if(first == 1)
    {
        getAppsDefaults("rfcwide_xmrg_dir", xmrg_dir) ;
        getAppsDefaults("mpe_qpe_sbn_dir", qpe_sbn_dir);
        getAppsDefaults("mpe_send_qpe_to_sbn", send_to_sbn);
    }

    if( pMPEParams->xmrgdtform[0] == 'm')
    {
        /* date time in mmddyyyyhh format */
        sprintf(fname_xmrg, "xmrg%sz", dateMDY);
        pSaveDate = dateMDY;
    }
    else
    {
        /**
         * date time in yyyymmddhh format
         **/
        sprintf(fname_xmrg, "xmrg%sz", dateYMD);
        pSaveDate = dateYMD;
    }

    /**
     * apply any persistent edit polygons to this product.
     **/
     year = pRunTime->tm_year + 1900;
     month = pRunTime->tm_mon + 1;
     day = pRunTime->tm_mday;
     hour = pRunTime->tm_hour;

     MPEFieldGen_apply_mpe_polygons ( pQPEMosaic,
                          dateYMD,
                          year,
                          month,
                          day,
                          hour,
                          pMPEParams->mosaic_type,
                          pGeoData,
                          FACTOR_PRECIP,
                          0,
		          0 );

    /**
      * write "best estimate" field specified by "mpe_qpe_fieldtype" token
      * to qpe directory.
      * files in qpe dir are in xmrg format
      **/
    MPEFieldGen_writeArray(pGeoData, xmrg_dir, fname_xmrg, FACTOR_PRECIP,
               replace_missing, pMPEParams->user, pRunDate->tRunTime,
               proc_flag_local, pQPEMosaic, &irc) ;

    /**
     * If the QPE file is going to be sent over the SBN, then create a QPE
     * file with the process flag set to QPE01.
     **/
    if ( ( send_to_sbn[1] == 'n' ) || ( send_to_sbn[1] == 'N' ) )
    {
       MPEFieldGen_writeArray(pGeoData, qpe_sbn_dir, fname_xmrg, FACTOR_PRECIP,
                  replace_missing, pMPEParams->user, pRunDate->tRunTime,
                  proc_flag_sbn, pQPEMosaic, &irc) ;
    }

    if(irc != 0)
    {
        sprintf ( message , "ERROR: error number = %ld"
            " attempting to write file = %s/%s",
            irc, xmrg_dir, fname_xmrg);
        printMessage( message, logFile );
    }
    else
    {
        sprintf ( message , "STATUS: QPE file written to: %s/%s",
                            xmrg_dir, fname_xmrg);
        printMessage( message, logFile );
    }

    /**
     * if mpe_save_netcdf is found as "ON",
     * then create and save netCDF image.
     **/
    if(pMPEParams->mpe_save_netcdf == 1)
    {
        /**
         * read netcdf file name.
         **/
        if(first == 1)
        {
            getAppsDefaults("mpe_netcdf_id", netcdf_id) ;
            getAppsDefaults("mpe_qpe_netcdf_dir", netcdf_dir) ;
        }

        if(strlen(netcdf_id) > 0)
            sprintf(fname_netcdf, "%s%sz.nc", netcdf_id, dateYMD);
        else
            sprintf(fname_netcdf, "%sz.nc", dateYMD);

        sprintf (save_file_path, "%s/%s", xmrg_dir, fname_xmrg);

        MPEFieldGen_saveNetCDF( pGeoData->num_cols, pGeoData->num_rows,
             fname_netcdf, netcdf_dir, &irc) ;

        if(irc == 1)
        {
            sprintf ( message , "malloc for precip array failed "
                      " netCDF file not saved" );
        }
        else if(irc == 2)
        {
            sprintf ( message , "error reading xmrg file %s "
                     " netCDF file not saved", save_file_path);
        }
        else if(irc == 3)
        {
            sprintf ( message , "error from nc_create routine "
                      " netCDF file not saved");
        }
        else
        {
            sprintf ( message , "netcdf file written to %s/%s", netcdf_dir, fname_netcdf);
        }

        printMessage( message, logFile );
    }

    /**
     * if mpe_save_gif is found as "ON",
     * then create and save gif image.
     **/
    if(pMPEParams->mpe_save_gif == 1)
    {
        if(pMPEParams->irc_load_stat == 0)
        {
            /**
             *    read directory for gif file.
             **/
            if(first == 1)
            {
                getAppsDefaults("mpe_qpe_gif_dir", gif_dir) ;
                getAppsDefaults("mpe_gif_id", gif_id) ;
            }
            if(strlen(gif_id) > 0 )
            {
                sprintf(save_file_path, "%s/%s%sz.gif",
                    gif_dir, gif_id, dateYMD);
            }
            else
            {
                sprintf(save_file_path, "%s/%sz.gif",
                    gif_dir, dateYMD);
            }

            MPEFieldGen_saveGif( pGeoData , dateYMD, save_file_path, pQPEMosaic, &irc) ;

            if(irc != 0)
                sprintf ( message , "ERROR: attempting to write file = %s",
                    save_file_path);
            else
                sprintf ( message , "STATUS: file written to: %s",
                    save_file_path);

            printMessage( message, logFile );
        }
    }

    /**
     * if mpe_save_jpeg is found as "ON",
     * then create and save jpeg image.
     **/
    if(pMPEParams->mpe_save_jpeg == 1)
    {
        /**
         * temporary no function call to create jpeg file.
         **/
        sprintf ( message , "STATUS: jpeg image not available - use gif");
        printMessage( message, logFile );
    }

    /**
     * if mpe_save_grib is found as "ON",
     * then create and save grib image.
     **/
    if(pMPEParams->mpe_save_grib == 1)
    {
        /**
         * read grib file name.
         **/
        if(first == 1)
        {
            getAppsDefaults("mpe_grib_id", grib_id) ;
        }
        if(strlen(grib_id) > 0)
            sprintf(fname_grib, "%s%sz.grib", grib_id, dateYMD);
        else
            sprintf(fname_grib, "%sz.grib", dateYMD);

        int leninf = strlen(fname_xmrg);
	int lenfn = strlen(fname_grib);
	pGribCommand = save_grib(fname_xmrg, &leninf, fname_grib, &lenfn) ;

        if ( pGribCommand != NULL )
        {
           sprintf ( message, "STATUS: process_grib_files script called "
                              "using command '%s'\n", pGribCommand );
           printMessage ( message, logFile );
           free ( pGribCommand );
           pGribCommand = NULL;
        }

    }

    first = 0 ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/pproc/src/mpe_fieldgen/RCS/write_qpe.c,v $";
 static char rcs_id2[] = "$Id: write_qpe.c,v 1.13 2007/11/09 19:59:46 lawrence Exp $";}
/*  ===================================================  */

}
