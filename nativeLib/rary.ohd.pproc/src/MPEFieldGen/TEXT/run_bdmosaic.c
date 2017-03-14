/*******************************************************************************
* FILENAME:  run_bdmosaic.c
*
* Purpose:
* This function computes the bdmosaic data.
*
* calling function: main_mpe_fieldgen
* functions called: apply_mfb, writeArray
*
* input variables
*
* pRunDate - date/time.
*
* pGeoData - global HRAP lowerleft-corner bin and dimension
*           and dimension of the RFC estimation domain. 
*
* pMPEParams - static parameters.
*
* meanFieldBias    - the mean field bias value computed for each radar.
*
* IDDP - the mosaic id array of radars.
*
* RDMosaic - the raw mosaic of radars.
*
* output variables
*
* BDMosaic - the bdmosaic array.
*
* QPEMosaic - the mosaic of radars for best estimate.
*
*********************************************************************************/

#include "mpe_fieldgen.h"

void runBDMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                const mpe_params_struct * pMPEParams ,
                double * meanFieldBias ,
                int ** IDDP ,
                double ** RDMosaic ,
                double ** BDMosaic,
                double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int replace_missing = 0 ;

    static char    dateYMD[YYYYMMDDHH_LEN + 1] ; 

    static char    fileName[PATH_LEN] = "" ;
    static char    mosaicDir[PATH_LEN] = "" ;
    static int    first = 1 ;
    struct tm * pRunTime = NULL ;
    long int irc ;

    register int i, j ;    

    /**      
     * dateYMD string should be in format: yyyymmddhh
     **/
    memset(dateYMD, '\0', YYYYMMDDHH_LEN + 1);
    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(dateYMD, YYYYMMDDHH_LEN + 1,
        "%Y%m%d%H", pRunTime);

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin BDMOSAIC calculation." , 
                    currTime) ;
    printMessage( message, logFile );

    /**      
     * Initialize arrays
     **/
    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            BDMosaic[i][j] = MOSAIC_DEFAULT ;
        }
    }

    /**
    * apply mean field bias values to RDMOSAIC field
    * to create BDMOSAIC field
    **/
    apply_mfb(meanFieldBias, rowSize, colSize, IDDP, RDMosaic, BDMosaic) ;

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time end BDMOSAIC calculation." , 
                    currTime) ;
    printMessage( message, logFile );        

    /**
    * write out gridded data in xmrg format to flat files
    **/

    if(first == 1)
    {
        if(getAppsDefaults("mpe_bdmosaic_dir", mosaicDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"mpe_bdmosaic_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        first = 0 ;
    }
    
    sprintf(fileName, "BDMOSAIC%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
               FACTOR_PRECIP, replace_missing,
               pMPEParams->user, pRunDate->tRunTime,
               PROC_FLAG, BDMosaic, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number = %ld "
            "attempting to write file: %s/%s." , 
            irc, mosaicDir, fileName) ;
        printMessage( message, logFile );
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s." , mosaicDir,
                           fileName) ;
        printMessage( message, logFile );
    }

    /**      
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is bdmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "bdmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = BDMosaic[i][j] ;
            }
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/run_bdmosaic.c,v $";
 static char rcs_id2[] = "$Id: run_bdmosaic.c,v 1.2 2012/08/23 16:51:32 pst Exp $";}
/*  ===================================================  */

}
