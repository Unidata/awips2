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
*
********************************************************************************
*/

#include "empe_fieldgen.h"
#include "save_grib.h"

void writeQPE(const run_date_struct * pRunDate,
            const empe_params_struct * pEMPEParams,
            const geo_data_struct * pGeoData ,
            const int gageNum,
            const int gageNumP3,
            double ** pQPEMosaic)
{
    char * pSaveDate = NULL;
    int day;
    int hour;
    int month;
    const int replace_missing = 1 ;
    int year;

    static char fname_xmrg[FNAME_LEN] = {'\0'} ; 
    static char xmrg_dir[PATH_LEN] = {'\0'} ; 

    const char * SAVE_NETCDF_TOKEN = "mpe_save_netcdf";
    static char netcdf_dir[FNAME_LEN] = {'\0'} ;
    static char netcdf_id[FNAME_LEN] = {'\0'} ;
    static char fname_netcdf[FNAME_LEN] = {'\0'} ;

    static char gif_dir[PATH_LEN] = {'\0'} ; 
    static char gif_id[FNAME_LEN] = {'\0'} ;

    static char fname_grib[FNAME_LEN] = {'\0'} ;
    static char grib_id[FNAME_LEN] = {'\0'};
    
    static char save_file_path[PATH_LEN + FNAME_LEN] = {'\0'} ;

    static char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    static char dateYMD[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'};
    static char dateMDY[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'};
    int  overwrite_flag ;
    int  hrap_x, hrap_y, hrap_cols, hrap_rows ;
    long int irc ;
    struct tm * pRunTime = NULL ;

    static const char * proc_flag = "MPA01";
    static int first = 1 ;
    
    
    /*
     * dateYMD string should be in format: yyyymmddHHMM
     * dateMDY string should be in format: mmddyyyyHHMM
     */

    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime ( strDateTime, ANSI_YEARSEC_TIME_LEN + 1,
            "%Y-%m-%d %H:%M:00", pRunTime ) ;
    strftime(dateYMD, ANSI_YEARSEC_TIME_LEN + 1, "%Y%m%d%H%M", pRunTime);
    strftime(dateMDY, ANSI_YEARSEC_TIME_LEN + 1, "%m%d%Y%H%M", pRunTime);

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
        hpe_fieldgen_getAppsDefaults("hpe_xmrg_dir", xmrg_dir) ;
    }

    if( pEMPEParams->xmrgdtform[0] == 'm')
    {
        /* date time in mmddyyyyHHMM format */
        sprintf(fname_xmrg, "xmrg%sz", dateMDY);
        pSaveDate = dateMDY;
    }
    else
    {
        /**
         * date time in yyyymmddHHMM format
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
    
     apply_mpe_polygons ( pQPEMosaic,
                          dateYMD,
                          year,
                          month,
                          day,
                          hour,
                          pEMPEParams->mosaic_type,
                          pGeoData,
                          FACTOR_PRECIP,
                          0,
                          1 ); 

    /**      
      * write "best estimate" field specified by "empe_qpe_fieldtype" token 
      * to qpe directory.
      * files in qpe dir are in xmrg format
      **/

    writeArray(pGeoData, xmrg_dir, fname_xmrg, FACTOR_PRECIP, 
               replace_missing, pEMPEParams->user, pRunDate->tRunTime,
               proc_flag, pQPEMosaic, &irc) ;

    if(irc != 0)
    {
        sprintf ( message , "ERROR: error number = %ld"
            " attempting to write file = %s/%s", 
            irc, xmrg_dir, fname_xmrg);
        hpe_fieldgen_printMessage( message );
    }
    else
    {
        sprintf ( message , "STATUS: QPE file written to: %s/%s",
                            xmrg_dir, fname_xmrg);
        hpe_fieldgen_printMessage( message );
    }

    /**
     * if save_netcdf is found as "ON",
     * then create and save netCDF image.
     **/

    if(pEMPEParams->save_netcdf == 1)
    {

        /*
         * read directory for netCDF file.
         */

        if(hpe_fieldgen_getAppsDefaults(SAVE_NETCDF_TOKEN, netcdf_dir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"%s\". netCDF file is skipped.",
                SAVE_NETCDF_TOKEN) ;
            printLogMessage( message );
        }
        else
        {
	        /**
	         * read netcdf file name.
	         **/
	
	        if(first == 1)
	        {
	            hpe_fieldgen_getAppsDefaults("mpe_netcdf_id", netcdf_id) ;
	        }
	
	        if(strlen(netcdf_id) > 0)
	        {
	            sprintf(fname_netcdf, "%s%sz.nc", netcdf_id, dateYMD);
	        }
	        else
	        {
	            sprintf(fname_netcdf, "%sz.nc", dateYMD);
	        }
	            
	        saveNetCDF( pGeoData->num_cols, pGeoData->num_rows, dateYMD, 
	             fname_netcdf, pQPEMosaic, netcdf_dir, proc_flag, &irc) ;
	
	        if(irc == 1)
	        {
	            sprintf ( message , "malloc for precip array failed "
	                      " netCDF file not saved" );
	        }
	        else if(irc == 2)
	        {
	            sprintf ( message , "error reading %s "
	                     " netCDF file not saved", save_file_path);
	        }
	        else if(irc == 3)
	        {
	            sprintf ( message , "error from nc_create routine "
	                      " netCDF file not saved");
	        }
	        hpe_fieldgen_printMessage( message );
        }
    }

    /**
     * if save_gif is found as "ON",
     * then create and save gif image.
     **/

    if(pEMPEParams->save_gif == 1)
    {
        if(pEMPEParams->irc_load_stat == 0)
        {
            /**
             *    read directory for gif file.
             **/

            if(first == 1)
            { 
                hpe_fieldgen_getAppsDefaults("mpe_gif_dir", gif_dir) ;
                hpe_fieldgen_getAppsDefaults("mpe_gif_id", gif_id) ;
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

            saveGif( pGeoData , dateYMD, save_file_path, pQPEMosaic, &irc) ;

            if(irc != 0)
            {
                sprintf ( message , "ERROR: attempting to write file = %s",
                    save_file_path);
            }
            else
            {
                sprintf ( message , "STATUS: file written to: %s",
                    save_file_path);
            }

            hpe_fieldgen_printMessage( message );        
        }
    }

    /**
     * if save_jpeg is found as "ON",
     * then create and save jpeg image.
     **/

    if(pEMPEParams->save_jpeg == 1)
    {
        /**
         * temporary no function call to create jpeg file.
         **/

        sprintf ( message , "STATUS: jpeg image not available - use gif");
        hpe_fieldgen_printMessage( message );
    }

    /**
     * if save_grib is found as "ON",
     * then create and save grib image.
     **/

    if(pEMPEParams->save_grib == 1)
    {
        /**
         * read grib file name.
         **/

        if(first == 1)
        { 
            hpe_fieldgen_getAppsDefaults("mpe_grib_id", grib_id) ;
        }
        if(strlen(grib_id) > 0)
        {
            sprintf(fname_grib, "%s%sz.grib", grib_id, dateYMD);
        }
        else
        {
            sprintf(fname_grib, "%sz.grib", dateYMD);
        }

		int leninf = strlen(fname_xmrg);
		int lenfn = strlen(fname_grib);
		save_grib ( fname_xmrg, &leninf, fname_grib, &lenfn) ;
    }

    first = 0 ;

    /**      
     * write record to RWResult table
     * return overwrite_flag flag
     *    = 0 -- no previous record OR previous record
     *            found with auto_save=T
     *        -- write xmrg file
     *    = 1 -- previous record found with auto_save=F
     *        -- do not write xmrg file
     *
     * update the number of radars available and the bias overwrite
     * flag in the rwresult table.
     **/

    writeRWResult(pEMPEParams->rfc_name,
                strDateTime,
                gageNum,
                pEMPEParams->sat_avail,
                pEMPEParams->radar_avail_num,
                pEMPEParams->qpe_fieldtype,
                &overwrite_flag,
                &irc) ;

    if(irc != 0)
    {
        overwrite_flag = 0 ;
        sprintf ( message , "Database error #%ld -- attempting to "
                 " write record to RWResult table.", irc ) ;
        hpe_fieldgen_printMessage( message );
    }

    if(overwrite_flag != 0)
    {
        sprintf ( message , "qpe/xmrg file not written due to "
                  "presence of manually saved version.");
        hpe_fieldgen_printMessage( message );

        return ;
    }
}
