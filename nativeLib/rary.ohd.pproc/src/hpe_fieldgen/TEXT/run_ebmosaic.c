/***********************************************************************
* Filename: run_ebmosaic.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: 10/05/2006
*
* Development Group: HSEB/OHD
*
* Description:
* compute the mean field biased DSP mosaic data
* 
* Modules:
* runEBMosaic
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: runEBMosaic
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: 10/05/2006
* 
* Description:
* This function is converted from FORTRAN code: run_bmosaic.f.
* This function computes the mean field biased DSP mosaic data.
*
* Calling Arguments:
* Name          Input/Output Type             Description
*
* pRunDate      Input        run_date_struct* date/time 
* pGeoData      Input        geo_data_struct* global HRAP lowerleft-corner
*                                             bin and dimension and dimension
*                                             of the RFC estimation domain
* pMPEParams    Input        mpe_params_struct*     static parameters
* meanFieldBias Input        double *         the mean field bias value
*                                             computed for each radar.
* ID            Input        int **           the mosaic id array of radars.
*
* RMosaic       Input        double **        the raw dsp mosaic.
* BMosaic       Output       double **        the mean biased dsp mosaic.
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
*             C. R. Kondragunta Original FORTRAN code  
* April 2005  Guoxian Zhou      converted to C Language 
* 10/05/2006  Guoxian Zhou      Modified for empe version 
* 08/08/2007  Guoxian Zhou      add option to use local bias data 
*
***********************************************************************/
extern int dualpol_used;

void runEBMosaic(const run_date_struct   * pRunDate ,
                const geo_data_struct   * pGeoData ,
                const empe_params_struct * pEMPEParams ,
                double * meanFieldBias ,
                int    ** ID ,
                double ** ERMosaic ,
                double ** EBMosaic,
                double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int replace_missing = 0 ;

    const char * MOSAIC_DIR_TOKEN  = "hpe_ebmosaic_dir";

    const char * SAVE_GRIB_TOKEN = "ebmosaic_save_grib";

    const char * SAVE_NETCDF_TOKEN = "ebmosaic_save_netcdf";
    const char * NETCDF_DIR_TOKEN = "ebmosaic_netcdf_dir";
    const char * NETCDF_ID_TOKEN = "ebmosaic_netcdf_id";

    const char * SAVE_GIF_TOKEN = "ebmosaic_save_gif";
    const char * GIF_DIR_TOKEN = "ebmosaic_gif_dir";
    const char * GIF_ID_TOKEN = "ebmosaic_gif_id";

    const char * SAVE_JPEG_TOKEN = "ebmosaic_save_jpeg";

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
    sprintf( message , "%s = time begin EBMOSAIC calculation.", currTime) ;
    hpe_fieldgen_printMessage( message);

    if(pEMPEParams->blnUseLocalBias == 1)
    {
        /*
         * apply local bias values to ERMOSAIC field
         * to create EBMOSAIC field
         */

        status = applyLocalBias(pRunDate->tRunTime ,
                                pGeoData , pEMPEParams ,
                                ERMosaic , EBMosaic);
        
    }
    
    if(status == 1)
    {
        sprintf( message , "STATUS: create EBMosaic by local bias.");
        hpe_fieldgen_printMessage( message);
    }
    else
    {

        /*
         * apply mean field bias values to ERMOSAIC field
         * to create EBMOSAIC field
         */
    
        apply_mfb(meanFieldBias, rowSize, colSize, ID, ERMosaic, EBMosaic) ;

        sprintf( message , "STATUS: create EBMosaic by mean field bias.");
        hpe_fieldgen_printMessage( message);
    }

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time end EBMOSAIC calculation.", currTime) ;
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

    sprintf(fileName, "EBMOSAIC%s%sz",
            pEMPEParams->category_name, strDateTime ); 
    writeArray(pGeoData, mosaicDir, fileName,
               FACTOR_PRECIP, replace_missing,
               pEMPEParams->user, pRunDate->tRunTime,
               BDSP_PROC_FLAG, EBMosaic, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number = %ld "
            "attempting to write file: %s/%s" , 
            irc, mosaicDir, fileName) ;
        printLogMessage( message);
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s" , mosaicDir,
                           fileName) ;
        printLogMessage( message);

        writeFormattedXMRG(pEMPEParams, pGeoData, 
                 mosaicDir, fileName, BDSP_PROC_FLAG, 
                 SAVE_GRIB_TOKEN,
                 SAVE_GIF_TOKEN, GIF_DIR_TOKEN, GIF_ID_TOKEN,
                 SAVE_NETCDF_TOKEN, NETCDF_DIR_TOKEN, NETCDF_ID_TOKEN,
                 SAVE_JPEG_TOKEN,
                 EBMosaic);

    }

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time end writing fields to flat files." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message);

    sprintf ( message , "\nSTATUS:  In EBMOSAIC, insert/update HPERadarResult table");
    printLogMessage( message );

    wrtodb_HPERadarResult(fileName, prdDateTime, pEMPEParams, dualpol_used);

    sprintf ( message , "\nSTATUS:  In EBMOSAIC, complete insert/update HPERadarResult table");
    printLogMessage( message );

    /*
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is bmosaic.
     * 
     * comment out feeding the qpe mosaic array
     * -- gzhou
     */

/*
    if(strcmp(pEMPEParams->qpe_fieldtype, "ebmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = EBMosaic[i][j] ;
            }
        }
    }
*/

    first = 0 ;
}
