/*******************************************************************************
* FILENAME:  run_bmosaic.c
*
* Purpose:
* This function is converted from FORTRAN code: run_bmosaic.f.
* This function computes the bmosaic data.
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
* ID - the mosaic id array of radars.
*
* RMosaic - the raw mosaic of radars.
*
* output variables
*
* BMosaic - the bmosaic array.
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2005   Guoxian Zhou      finish conversion to C Language 
*
*********************************************************************************/

#include "mpe_fieldgen.h"

void runBMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                const mpe_params_struct * pMPEParams ,
                double * meanFieldBias ,
                int ** ID ,
                double ** RMosaic ,
                double ** BMosaic,
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
    sprintf( message , "%s = time begin BMOSAIC calculation." , 
                    currTime) ;
    printMessage( message, logFile );

    /**      
     * Initialize arrays
     **/
    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            BMosaic[i][j] = MOSAIC_DEFAULT ;
        }
    }

    /**
    * apply mean field bias values to RMOSAIC field
    * to create BMOSAIC field
    **/
    MPEFieldGen_apply_mfb(meanFieldBias, rowSize, colSize, ID, RMosaic, BMosaic) ;

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time end BMOSAIC calculation." , 
                    currTime) ;
    printMessage( message, logFile );        

    /**
    * write out gridded data in xmrg format to flat files
    **/
    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin writing fields to flat files." , 
                    currTime) ;
    printMessage( message, logFile );

    if(first == 1)
    {
        if(getAppsDefaults("rfcwide_bmosaic_dir", mosaicDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"rfcwide_bmosaic_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        first = 0 ;
    }
    
    sprintf(fileName, "BMOSAIC%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
               FACTOR_PRECIP, replace_missing,
               pMPEParams->user, pRunDate->tRunTime,
               PROC_FLAG, BMosaic, &irc) ;

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

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time end writing fields to flat files." , 
                    currTime) ;
    printMessage( message, logFile );
    
    /**      
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is bmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "bmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = BMosaic[i][j] ;
            }
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
