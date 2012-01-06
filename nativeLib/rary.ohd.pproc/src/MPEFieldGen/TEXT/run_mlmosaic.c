/*******************************************************************************
* FILENAME:  run_mlmosaic.c
*
* Purpose:
* This function is converted from FORTRAN code: run_mlmosaic.f.
* This function computes the mlmosaic data.
*
* calling function: main_mpe_fieldgen
* functions called: multi_sensor, writeArray.
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
* gageSize - the number of gage records
*
* iug - the hrap x_direction value of gage records
*
* ivg - the hrap y_direction value of gage records
*
* zg - the gage value of gage records
*
* ID      - the ID array.
*
* RMosaic - the raw mosaic of radars.
*
* LMosaic - the lmosaic array.
*
* umeang  - the PRISM data
*
* output variables
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2005   Guoxian Zhou      finish conversion to C Language
*
*********************************************************************************/

#include "mpe_fieldgen.h"
#include "multi_sensor.h"

double ** MLMosaic ;

void allocMLMosaicMemory(const geo_data_struct * pGeoData) ;
void releaseMLMosaicMemory(const geo_data_struct * pGeoData) ;

void MPEFieldGen_runMLMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** ID ,
                double ** RMosaic ,
                double ** LMosaic ,
                double ** umeang ,
                double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int replace_missing = 0 ;

    static char mosaicDir[PATH_LEN] = "" ;
    static int first = 1 ;

    int i, j ;
    int errFlag ;

    static char dateYMD[YYYYMMDDHH_LEN + 1] = "" ;
    static char fileName[PATH_LEN] = "" ;
    struct tm * pRunTime = NULL ;
    long int irc ;

    MLMosaic = NULL ;

    /**
     * Allocates memory for data arrays
     * based on the geo grid size data.
     **/
    allocMLMosaicMemory(pGeoData) ;

    /**
     * dateYMD string should be in format: yyyymmddhh
     **/
    memset(dateYMD, '\0', YYYYMMDDHH_LEN + 1);
    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(dateYMD, YYYYMMDDHH_LEN + 1,
        "%Y%m%d%H", pRunTime);

    /**
     * Initialize arrays
     **/
    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            MLMosaic[i][j] = MOSAIC_DEFAULT ;
        }
    }

    /**
     * multisensor analysis.
     **/
    if((pMPEParams->locbias_1hr_rerun == 1) ||
        (pRunDate->hourNum > 1))
    {
        getCurrentTime(currTime) ;
        sprintf( message , "%s = time begin mlmosaic calculation." ,
                        currTime) ;
        printMessage( message, logFile );

        /**
         * Multisensor analysis
         *
         * generate MLMOSAIC field = LMOSAIC field + gages.
         **/
        MPEFieldGen_multi_sensor(pGeoData, pMPEParams,
                    gageSize, iug, ivg, zg, LMosaic,
                    ID, umeang, MLMosaic, &errFlag);

        getCurrentTime(currTime) ;
        sprintf( message , "%s = time   end mlmosaic calculation." ,
                        currTime) ;
        printMessage( message, logFile );
    }

    /**
    * write mlmosaic based on input options
    **/
    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin writing fields to flat files." ,
                    currTime) ;
    printMessage( message, logFile );

    if((pMPEParams->locbias_1hr_rerun == 1) ||
        (pRunDate->hourNum > 1))
    {
        if(first == 1)
        {
            if(getAppsDefaults("rfcwide_mlmosaic_dir", mosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"rfcwide_mmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
            first = 0 ;
        }

        sprintf(fileName, "MLMOSAIC%sz", dateYMD );
        MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, MLMosaic, &irc) ;

        if(irc != 0)
        {
            sprintf( message , "ERROR: error number =%ld "
                "attempting to write file: %s/%s." ,
                irc, mosaicDir, fileName) ;
            printMessage( message, logFile );
        }
        else
        {
            sprintf( message , "STATUS: file written to: %s/%s." ,
            mosaicDir, fileName) ;
            printMessage( message, logFile );
        }
    }

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time end writing fields to flat files." ,
                    currTime) ;
    printMessage( message, logFile );

    /**
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is mlmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "mlmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = MLMosaic[i][j] ;
            }
        }
    }
    releaseMLMosaicMemory(pGeoData) ;
}


void allocMLMosaicMemory(const geo_data_struct * pGeoData)
{
    int i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**
     * allocate memory for MLMosaic (double data type) variable
     **/
    MLMosaic = (double **)malloc(rowSize * sizeof(double *));
    if(MLMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocMLMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MLMosaic[i] = (double *)malloc(colSize * sizeof(double));
        if(MLMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocMLMosaicMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }
}

void releaseMLMosaicMemory(const geo_data_struct * pGeoData)
{
    int i ;
    const int rowSize = pGeoData->num_rows;

    if(MLMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(MLMosaic[i] != NULL)
            {
                free(MLMosaic[i]);
                MLMosaic[i] = NULL;
            }
        }
        free(MLMosaic);
        MLMosaic = NULL;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
