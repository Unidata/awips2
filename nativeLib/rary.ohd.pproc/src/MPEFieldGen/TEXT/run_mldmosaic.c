/*******************************************************************************
* FILENAME:  run_mldmosaic.c
*
* Purpose:
* This function computes the mldmosaic data.
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
* RDMosaic - the raw mosaic of radars.
*
* LDMosaic - the lmosaic array.
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

double ** MLDMosaic ;

void allocMLDMosaicMemory(const geo_data_struct * pGeoData) ;
void releaseMLDMosaicMemory(const geo_data_struct * pGeoData) ;

void runMLDMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** ID ,
                double ** RDMosaic ,
                double ** LDMosaic ,
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

    MLDMosaic = NULL ;

    /**      
     * Allocates memory for data arrays
     * based on the geo grid size data.
     **/
    allocMLDMosaicMemory(pGeoData) ;

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
            MLDMosaic[i][j] = MOSAIC_DEFAULT ;
        }
    }

    /**      
     * multisensor analysis.
     **/
    if((pMPEParams->locbias_1hr_rerun == 1) ||
        (pRunDate->hourNum > 1))
    {
        getCurrentTime(currTime) ;
        sprintf( message , "%s = time begin mldmosaic calculation." , 
                        currTime) ;
        printMessage( message, logFile );        

        /**      
         * Multisensor analysis
         *
         * generate MLDMOSAIC field = LDMOSAIC field + gages.
         **/
        MPEFieldGen_multi_sensor(pGeoData, pMPEParams, 
                    gageSize, iug, ivg, zg, LDMosaic,
                    ID, umeang, MLDMosaic, &errFlag);

        getCurrentTime(currTime) ;
        sprintf( message , "%s = time   end mldmosaic calculation." , 
                        currTime) ;
        printMessage( message, logFile );
    }

    /**
    * write mldmosaic based on input options
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
            if(getAppsDefaults("mpe_mldmosaic_dir", mosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"mpe_mmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
            first = 0 ;
        }

        sprintf(fileName, "MLDMOSAIC%sz", dateYMD ); 
        MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, MLDMosaic, &irc) ;

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
     * if the qpe_fieldtype is mldmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "mldmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = MLDMosaic[i][j] ;
            }
        }
    }
    releaseMLDMosaicMemory(pGeoData) ;
}


void allocMLDMosaicMemory(const geo_data_struct * pGeoData)
{
    int i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**      
     * allocate memory for MLDMosaic (double data type) variable
     **/
    MLDMosaic = (double **)malloc(rowSize * sizeof(double *)); 
    if(MLDMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocMLDMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MLDMosaic[i] = (double *)malloc(colSize * sizeof(double)); 
        if(MLDMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocMLDMosaicMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    
}

void releaseMLDMosaicMemory(const geo_data_struct * pGeoData) 
{
    int i ;
    const int rowSize = pGeoData->num_rows;

    if(MLDMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(MLDMosaic[i] != NULL)
            {
                free(MLDMosaic[i]);
                MLDMosaic[i] = NULL;
            }
        }
        free(MLDMosaic);
        MLDMosaic = NULL;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/run_mldmosaic.c,v $";
 static char rcs_id2[] = "$Id: run_mldmosaic.c,v 1.1 2012/04/25 16:02:27 pst Exp $";}
/*  ===================================================  */

}
