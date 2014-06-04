/******************************************************************************
* FILENAME:  run_mlqmosaic.c
*
* Purpose:
* This function computes the mlqmosaic field.
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
* Q2ID      - array of all 1's.
*
* QMosaic - the raw mosaic of radars.
*
* LQMosaic - the lqmosaic array.
*
* umeang  - the PRISM data
*
* output variables
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   August 2008  P Tilles          initial version
*
*********************************************************************************/

#include "mpe_fieldgen.h"
#include "multi_sensor.h"

double ** MLQMosaic ;

/* NOTE - These function prototypes should go in a .h file. djsiii 17 Sep 2013 */

void allocMLQMosaicMemory(const geo_data_struct * pGeoData) ;
void releaseMLQMosaicMemory(const geo_data_struct * pGeoData) ;
void releaseMLQMosaicMemory(const geo_data_struct * pGeoData) ;


void runMLQMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** Q2ID ,
                double ** QMosaic ,
                double ** LQMosaic ,
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

    MLQMosaic = NULL ;

    /**
     * Allocates memory for data arrays
     * based on the geo grid size data.
     **/
    allocMLQMosaicMemory(pGeoData) ;

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
            MLQMosaic[i][j] = MOSAIC_DEFAULT ;
            Q2ID[i][j] = 1;
        }
    }

    /**
     * multisensor analysis.
     **/
    if((pMPEParams->locbias_1hr_rerun == 1) ||
        (pRunDate->hourNum > 1))
    {
        getCurrentTime(currTime) ;
        sprintf( message , "%s = time begin mlqmosaic calculation." ,
                        currTime) ;
        printMessage( message, logFile );

        /**
         * Multisensor analysis
         *
         * generate MLQOSAIC field = LQMOSAIC field + gages.
         **/
        MPEFieldGen_multi_sensor(pGeoData, pMPEParams,
                    gageSize, iug, ivg, zg, LQMosaic,
                    Q2ID, umeang, MLQMosaic, &errFlag);

        getCurrentTime(currTime) ;
        sprintf( message , "%s = time   end mlqmosaic calculation." ,
                        currTime) ;
        printMessage( message, logFile );
    }

    if((pMPEParams->locbias_1hr_rerun == 1) ||
        (pRunDate->hourNum > 1))
    {
        if(first == 1)
        {
            if(getAppsDefaults("mpe_mlqmosaic_dir", mosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"mpe_mlqmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
            first = 0 ;
        }

        sprintf(fileName, "MLQMOSAIC%sz", dateYMD );
        MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, MLQMosaic, &irc) ;

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

    /**
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is mlqmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "mlqmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = MLQMosaic[i][j] ;
            }
        }
    }

    releaseMLQMosaicMemory(pGeoData) ;

} /* end runMLQMosaic */


void allocMLQMosaicMemory(const geo_data_struct * pGeoData)
{
    int i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**
     * allocate memory for MLQMosaic (double data type) variable
     **/
    MLQMosaic = (double **)malloc(rowSize * sizeof(double *));
    if(MLQMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocMLQMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MLQMosaic[i] = (double *)malloc(colSize * sizeof(double));
        if(MLQMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocMLQMosaicMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }
} /* end allocMLQMosaicMemory */

void releaseMLQMosaicMemory(const geo_data_struct * pGeoData)
{
    int i ;
    const int rowSize = pGeoData->num_rows;

    if(MLQMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(MLQMosaic[i] != NULL)
            {
                free(MLQMosaic[i]);
                MLQMosaic[i] = NULL;
            }
        }
        free(MLQMosaic);
        MLQMosaic = NULL;
    }

} /* end releaseMLQMosaicMemory */
