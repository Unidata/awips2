/*******************************************************************************
* FILENAME:  run_sgmosaic.c
*
* Purpose:
* This function computes the sgmosaic data.
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
* ID_temp      - substitute for the ID array. Array of all 1's.
*
* LSatpre - local bias corrected satellite field array
*
* umeang  - the PRISM data
*
* output variables
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2007   P Tilles          Initial version
*
*********************************************************************************/

#include "mpe_fieldgen.h"
#include "multi_sensor.h"

double ** SGMosaic ;
int ** ID_temp ;

void allocSGMosaicMemory(const geo_data_struct * pGeoData) ;
void releaseSGMosaicMemory(const geo_data_struct * pGeoData) ;

void runSGMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** LSatpre ,
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

    SGMosaic = NULL ;

    /**
     * Allocates memory for data arrays
     * based on the geo grid size data.
     **/
    allocSGMosaicMemory(pGeoData) ;

    /**
     * dateYMD string should be in format: yyyymmddhh
     **/
    memset(dateYMD, '\0', YYYYMMDDHH_LEN + 1);
    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(dateYMD, YYYYMMDDHH_LEN + 1,
        "%Y%m%d%H", pRunTime);

    /**
     * Initialize array
     **/
    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            SGMosaic[i][j] = MOSAIC_DEFAULT ;
            ID_temp[i][j] = 1;
        }
    }

    /**
     * SGMOSAIC analysis.
     **/

        getCurrentTime(currTime) ;
        sprintf( message , "%s = time begin sgmosaic calculation." ,
                        currTime) ;
        printMessage( message, logFile );
        if ( pMPEParams->locbias_1hr_rerun == 0 )
        {
            sprintf ( message, "SGMosaic recalculation on rerun = OFF" );
        }
        printMessage ( message, logFile );

        if ( ( pMPEParams->locbias_1hr_rerun == 1 ) ||
             ( pRunDate->hourNum > 1 ) )
        {


        /**
         * Multisensor analysis
         *
         * generate SGMOSAIC field = LSATPRE field + gages.
         **/
        MPEFieldGen_multi_sensor(pGeoData, pMPEParams,
                    gageSize, iug, ivg, zg, LSatpre,
                    ID_temp, umeang, SGMosaic, &errFlag);
        }
        getCurrentTime(currTime) ;
        sprintf( message , "%s = time   end sgmosaic calculation." ,
                        currTime) ;
        printMessage( message, logFile );

    /**
    * write sgmosaic based on input options
    **/
        if ( ( pMPEParams->locbias_1hr_rerun == 1 ) ||
             ( pRunDate->hourNum > 1 ) )
        {
          if(first == 1)
          {
            if(getAppsDefaults("mpe_sgmosaic_dir", mosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"mpe_sgmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
            first = 0 ;
        }

        sprintf(fileName, "SGMOSAIC%sz", dateYMD );
        MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, SGMosaic, &irc) ;

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

    /**
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is sgmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "sgmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = SGMosaic[i][j] ;
            }
        }
    }
        }
    releaseSGMosaicMemory(pGeoData) ;
}


void allocSGMosaicMemory(const geo_data_struct * pGeoData)
{
    int i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**
     * allocate memory for SGMosaic (double data type) variable
     **/
    SGMosaic = (double **)malloc(rowSize * sizeof(double *));
    if(SGMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocSGMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        SGMosaic[i] = (double *)malloc(colSize * sizeof(double));
        if(SGMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocSGMosaicMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

    /**
     * allocate memory for ID_temp (int data type) variable
     **/
    ID_temp = (int **)malloc(rowSize * sizeof(int *));
    if(ID_temp == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocSGMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        ID_temp[i] = (int *)malloc(colSize * sizeof(int));
        if(ID_temp[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocSGMosaicMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

}

void releaseSGMosaicMemory(const geo_data_struct * pGeoData)
{
    int i ;
    const int rowSize = pGeoData->num_rows;

    if(SGMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(SGMosaic[i] != NULL)
            {
                free(SGMosaic[i]);
                SGMosaic[i] = NULL;
            }
        }
        free(SGMosaic);
        SGMosaic = NULL;
    }

    if(ID_temp != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(ID_temp[i] != NULL)
            {
                free(ID_temp[i]);
                ID_temp[i] = NULL;
            }
        }
        free(ID_temp);
        ID_temp = NULL;
    }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/run_sgmosaic.c,v $";
 static char rcs_id2[] = "$Id: run_sgmosaic.c,v 1.1 2007/10/15 12:19:15 dsa Exp lawrence $";}
/*  ===================================================  */

}
