/***********************************************************************
* Filename: run_bdhrmosaic.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: 10/24/2006
*
* Development Group: HSEB/OHD
*
* Description:
* compute the mean biased dhr mosaic data
*
* Modules:
* runBDHRMosaic
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: runBDHRMosaic
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: 10/24/2006
*
* Description:
* This function is converted from FORTRAN code: run_bmosaic.f.
* This function computes the mean biased mosaic data.
*
* Calling Arguments:
* Name          Input/Output Type             Description
*
* pRunDate      Input        run_date_struct* date/time
* pGeoData      Input        geo_data_struct* global HRAP lowerleft-corner
*                                             bin and dimension and dimension
*                                             of the RFC estimation domain
* pMPEParams    Input        mpe_params_struct*
*                                             static parameters
* meanFieldBias Input        double **        the mean field bias value
*                                             computed for each radar.
* ID            Input        int **           the mosaic id array of radars.
*
* DHRMosaic     Input        double **        the raw dsp mosaic.
* QPEMosaic     Output       double **        the best estimate mosaic
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* apply_mfb, writeArray
*
* Return Value:
* Type          Description
* None
*
* Error Codes/Exceptions:
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
* Date        Developer         Action
* 10/24/2006  Guoxian Zhou      Modified from run_bmosaic.c
* 07/2013     JingtaoD          dual pol
***********************************************************************/

static double ** Mosaic = NULL;

static void initMosaicArray(const geo_data_struct * pGeoData) ;
static void freeMosaicArray(const geo_data_struct * pGeoData) ;
extern int dualpol_used;

void runBDHRMosaic(const run_date_struct   * pRunDate ,
                   const geo_data_struct   * pGeoData ,
                   const empe_params_struct * pEMPEParams ,
                   double * meanFieldBias ,
                   int    ** ID ,
                   double ** DHRMosaic ,
                   double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int replace_missing = 0 ;

    const char * MOSAIC_DIR_TOKEN = "hpe_bdhrmosaic_dir";

    const char * SAVE_GRIB_TOKEN = "bdhrmosaic_save_grib";

    const char * SAVE_NETCDF_TOKEN = "bdhrmosaic_save_netcdf";
    const char * NETCDF_DIR_TOKEN = "bdhrmosaic_netcdf_dir";
    const char * NETCDF_ID_TOKEN = "bdhrmosaic_netcdf_id";

    const char * SAVE_GIF_TOKEN = "bdhrmosaic_save_gif";
    const char * GIF_DIR_TOKEN = "bdhrmosaic_gif_dir";
    const char * GIF_ID_TOKEN = "bdhrmosaic_gif_id";

    const char * SAVE_JPEG_TOKEN = "bdhrmosaic_save_jpeg";

    static char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    static char prdDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    static char fileName[PATH_LEN] = {'\0'} ;
    static char mosaicDir[PATH_LEN] = {'\0'} ;

    static int first = 1 ;
    struct tm * pRunTime = NULL ;
    long int irc ;

    int status = 0 ;

    /*
     * strDateTime string should be in format: yyyymmddHHMM
     */

    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(strDateTime, ANSI_YEARSEC_TIME_LEN + 1,
             "%Y%m%d%H%M", pRunTime);
    strftime(prdDateTime, ANSI_YEARSEC_TIME_LEN + 1,
                        "%Y-%m-%d %H:%M:%S", pRunTime);


    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin BDHRMosaic calculation.", currTime) ;
    hpe_fieldgen_printMessage( message);

    /*
     * Allocates memory for data arrays
     * based on the geo grid size data
     * and initialize array
     */

    initMosaicArray(pGeoData) ;


    if(pEMPEParams->blnUseLocalBias == 1)
    {
        /*
         * apply local bias values to DHRMOSAIC field
         * to create BDHRMOSAIC field
         */

        status = applyLocalBias(pRunDate->tRunTime ,
                                pGeoData , pEMPEParams ,
                                DHRMosaic , Mosaic);

    }

    if(status == 1)
    {
        sprintf( message , "STATUS: create BDHRMosaic by local bias.");
        hpe_fieldgen_printMessage( message);
    }
    else
    {

        /*
         * apply mean field bias values to DHRMOSAIC field
         * to create BDHRMOSAIC field
         */

        apply_mfb(meanFieldBias, rowSize, colSize, ID, DHRMosaic, Mosaic) ;

        sprintf( message , "STATUS: create BDHRMosaic by mean field bias.");
        hpe_fieldgen_printMessage( message);
    }


    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time end BDHRMosaic calculation.", currTime) ;
    hpe_fieldgen_printMessage( message);

    /*
     * write out gridded data in xmrg format to flat files
     */

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin writing fields to flat files." ,
                       currTime) ;
    hpe_fieldgen_printMessage( message);

    if(first == 1)
    {
        if(hpe_fieldgen_getAppsDefaults(MOSAIC_DIR_TOKEN, mosaicDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"%s\".\n\tProgram exit.",
                MOSAIC_DIR_TOKEN) ;
            shutdown( message );
        }
    }

    sprintf(fileName, "BDHRMOSAIC%sz", strDateTime );
    writeArray(pGeoData, mosaicDir, fileName,
               FACTOR_PRECIP, replace_missing,
               pEMPEParams->user, pRunDate->tRunTime,
               BDHR_PROC_FLAG, Mosaic, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number = %ld "
            "attempting to write file: %s/%s" ,
            irc, mosaicDir, fileName) ;
        printLogMessage( message);
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s" ,
                           mosaicDir, fileName) ;
        printLogMessage( message);

        writeFormattedXMRG(pEMPEParams, pGeoData,
                 mosaicDir, fileName, BDHR_PROC_FLAG,
                 SAVE_GRIB_TOKEN,
                 SAVE_GIF_TOKEN, GIF_DIR_TOKEN, GIF_ID_TOKEN,
                 SAVE_NETCDF_TOKEN, NETCDF_DIR_TOKEN, NETCDF_ID_TOKEN,
                 SAVE_JPEG_TOKEN,
                 Mosaic);

    }

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time end writing fields to flat files." ,
                    currTime) ;
    hpe_fieldgen_printMessage( message);
    
    sprintf ( message , "\nSTATUS:  In BDHRMOSAIC, insert/update HPERadarResult table");
    printLogMessage( message );

    wrtodb_HPERadarResult(fileName, prdDateTime, pEMPEParams, dualpol_used);
    

    sprintf ( message , "\nSTATUS:  In BDHRMOSAIC, complete insert/update HPERadarResult table");
    printLogMessage( message );

    /*
     * run the nowcast
     */
    if (pEMPEParams->blnRunNowcast == 1)
    {
        const char * mosaicID = "BDHR";
        runNowcast(pGeoData, pRunDate->tRunTime, mosaicID, pEMPEParams,
                mosaicDir, Mosaic, dualpol_used);
    }


    /*
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is bdhrmosaic.
     *
     * comment out feeding the qpe mosaic array.
     * --gzhou
     */

/*
    if(strcmp(pEMPEParams->qpe_fieldtype, "bdhrmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = Mosaic[i][j] ;
            }
        }
    }
*/

    first = 0 ;

    freeMosaicArray(pGeoData) ;

}


static void initMosaicArray(const geo_data_struct * pGeoData)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    Mosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );
}

static void freeMosaicArray(const geo_data_struct * pGeoData)
{
    const int rowSize = pGeoData->num_rows;

    free2DDoubleArray(Mosaic, rowSize );
}
