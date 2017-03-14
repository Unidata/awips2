/*******************************************************************************
* FILENAME:  run_srdgmosaic.c
*
* Purpose:
* This function computes the srdgmosaic data.
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
* ID_temp      - substitute for the ID array.  Array of all 1's.
*
* SRDMosaic - the array of mosaicked satellite-radar field
*
* umeang  - the PRISM data
*
* output variables
*
* QPEMosaic - the mosaic of radars for best estimate.
*
*
*********************************************************************************/

#include "mpe_fieldgen.h"
#include "multi_sensor.h"

int ** ID_temp ;
double ** SRDMosaic ;
double ** SRDGMosaic ;

void allocSRDGMosaicMemory(const geo_data_struct * pGeoData) ;
void releaseSRDMosaicMemory(const geo_data_struct * pGeoData) ;
void releaseSRDGMosaicMemory(const geo_data_struct * pGeoData) ;

void runSRDGMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
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

    SRDGMosaic = NULL ;

    /**      
     * Allocates memory for data arrays
     * based on the geo grid size data.
     **/
    allocSRDGMosaicMemory(pGeoData) ;

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
            SRDGMosaic[i][j] = MOSAIC_DEFAULT ;
            ID_temp[i][j] = 1 ;
        }
    }

    /**      
     * SRDG analysis.
     **/

        getCurrentTime(currTime) ;
        sprintf( message , "%s = time begin srdgmosaic calculation." , 
                        currTime) ;
        printMessage( message, logFile );        

    if ( pMPEParams->locbias_1hr_rerun == 0 )
    {
       sprintf ( message, "SRDGMosaic recalculation on rerun = OFF" );
       printMessage ( message, logFile );
    }

    if ( ( pMPEParams->locbias_1hr_rerun == 1 ) ||
         ( pRunDate->hourNum > 1 ) ) 
    {

        /**      
         * Multisensor analysis
         *
         * generate SRDGMOSAIC field = SRDMOSAIC field + gages.
         **/
        MPEFieldGen_multi_sensor(pGeoData, pMPEParams, 
                    gageSize, iug, ivg, zg, SRDMosaic,
                    ID_temp, umeang, SRDGMosaic, &errFlag);
     }

        getCurrentTime(currTime) ;
        sprintf( message , "%s = time   end srdgmosaic calculation." , 
                        currTime) ;
        printMessage( message, logFile );

    if ( ( pMPEParams->locbias_1hr_rerun == 1 ) ||
         ( pRunDate->hourNum > 1 ) ) 
    {

    /**
    * write srdgmosaic based on input options
    **/

        if(first == 1)
        {
            if(getAppsDefaults("mpe_srdgmosaic_dir", mosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"mpe_srdgmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
            first = 0 ;
        }

        sprintf(fileName, "SRDGMOSAIC%sz", dateYMD ); 
        MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, SRDGMosaic, &irc) ;

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
     * if the qpe_fieldtype is srdgmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "srdgmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = SRDGMosaic[i][j] ;
            }
        }
    }

    }
    releaseSRDMosaicMemory(pGeoData) ;
    releaseSRDGMosaicMemory(pGeoData) ;
}


void allocSRDGMosaicMemory(const geo_data_struct * pGeoData)
{
    int i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**      
     * allocate memory for SRDGMosaic (double data type) variable
     **/
    SRDGMosaic = (double **)malloc(rowSize * sizeof(double *)); 
    if(SRDGMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocSRDGMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        SRDGMosaic[i] = (double *)malloc(colSize * sizeof(double)); 
        if(SRDGMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocSRDGMosaicMemory function."
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
            " in allocSRDGMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        ID_temp[i] = (int *)malloc(colSize * sizeof(int));
        if(ID_temp[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocSRDGMosaicMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

}

void releaseSRDGMosaicMemory(const geo_data_struct * pGeoData) 
{
    int i ;
    const int rowSize = pGeoData->num_rows;

    if(SRDGMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(SRDGMosaic[i] != NULL)
            {
                free(SRDGMosaic[i]);
                SRDGMosaic[i] = NULL;
            }
        }
        free(SRDGMosaic);
        SRDGMosaic = NULL;
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
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/run_srdgmosaic.c,v $";
 static char rcs_id2[] = "$Id: run_srdgmosaic.c,v 1.1 2012/04/25 16:03:18 pst Exp $";}
/*  ===================================================  */

}
