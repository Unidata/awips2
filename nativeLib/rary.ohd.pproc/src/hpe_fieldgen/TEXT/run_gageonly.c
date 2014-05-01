/*******************************************************************************
* FILENAME:   run_gageonly.c
*
* Purpose:
* This function computes the gageonly mosaic data
*
* calling function: main_mpe_fieldgen
* functions called: gage_only, writeArray
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
* gageSize - the number of gage records
*
* iug - the hrap x_direction value of gage records
*
* ivg - the hrap y_direction value of gage records
*
* zg - the gage value of gage records
*
* umeang  - the PRISM data
*
* output variables
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   Jun 23, 2005 Guoxian Zhou      finish conversion to C Language 
*   Sept 3, 2005 Guoxian Zhou      Use pre-calculated neighbor list
*   Jan 2007     Guoxian Zhou      modified for empe 
*
*********************************************************************************/

#include "empe_fieldgen.h"
#include "multi_sensor.h"

static double ** GMosaic = NULL ;

static void initGageonlyArray(const geo_data_struct * pGeoData) ;
static void freeGageonlyArray(const geo_data_struct * pGeoData) ;

void runGageonly(const run_date_struct * pRunDate,
                const geo_data_struct * pGeoData,
                empe_params_struct * pMPEParams,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** umeang,
                double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int replace_missing = 0 ;

    static char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ; 
    long int irc ;

    static char fileName[PATH_LEN] = {'\0'} ;
    static char mosaicDir[PATH_LEN] = { '\0'} ;
    static int  first = 1 ;
    struct tm * pRunTime = NULL ;

    int i, j ;    

    /*
     * strDateTime string should be in format: yyyymmddHHMM
     */

    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(strDateTime, ANSI_YEARSEC_TIME_LEN + 1,
        "%Y%m%d%H%M", pRunTime);

    GMosaic = NULL ;

    int errFlag ;

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin gageonly analysis." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message);        

    /*
     * Allocates memory for data arrays
     * based on the geo grid size data.
     */     

    initGageonlyArray(pGeoData) ;

    if(gageSize < 1)
    {
        sprintf( message , "STATUS: No hourly gages data available"
            " for current hour, "
            "\n\tdefault gageonly to missing value.\n") ;
        hpe_fieldgen_printMessage( message);
    }
    else
    {
        gage_only(pGeoData , pMPEParams, gageSize, iug, ivg, zg ,
            umeang , GMosaic , &errFlag);
    }

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end gageonly analysis." , 
                    currTime) ;
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
        if(hpe_fieldgen_getAppsDefaults("hpe_gageonly_dir", mosaicDir) == -1)
        {
            sprintf ( message , "Error: Invalid token value"
                " for token \"hpe_gageonly_dir\"."
                "\n\tProgram exit.") ;
            shutdown( message );
        }
        first = 0 ;
    }

    sprintf(fileName, "GAGEONLY%sz", strDateTime ); 
    writeArray(pGeoData, mosaicDir, fileName,
               FACTOR_PRECIP, replace_missing,
               pMPEParams->user, pRunDate->tRunTime,
               PROC_FLAG, GMosaic, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number = %ld "
            "attempting to write file: %s/%s." , 
            irc, mosaicDir, fileName) ;
        hpe_fieldgen_printMessage( message);
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s" , 
                 mosaicDir, fileName) ;
        hpe_fieldgen_printMessage( message);
    }

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end writing fields to flat files." , 
                       currTime) ;
    hpe_fieldgen_printMessage( message);
    
    /*
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is gageonly.
     */

    if(strcmp(pMPEParams->qpe_fieldtype, "gageonly") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = GMosaic[i][j] ;
            }
        }
    }

    freeGageonlyArray(pGeoData) ;
}


static void initGageonlyArray(const geo_data_struct * pGeoData)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    GMosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );
}

static void freeGageonlyArray(const geo_data_struct * pGeoData) 
{
    const int rowSize = pGeoData->num_rows ;

    free2DDoubleArray(GMosaic, rowSize );
}
