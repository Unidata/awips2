/*******************************************************************************
* FILENAME:            run_mdmosaic.c
*
* Purpose:
* This function computes the mdmosaic data.
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
* ID - the ID array.
*
* RDMosaic - the raw mosaic of radars.
*
* BDMosaic - the bmosaic array.
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

double ** MDMosaic ;

void allocMDMosaicMemory(const geo_data_struct * pGeoData) ;
void releaseMDMosaicMemory(const geo_data_struct * pGeoData) ;

void runMDMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** ID ,
                double ** RDMosaic ,
                double ** BDMosaic ,
                double ** umeang ,
                double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int replace_missing = 0 ;

    static char    mosaicDir[PATH_LEN] = "" ;
    static int    first = 1 ;

    int i, j ;
    int errFlag = 0 ;

    static char dateYMD[YYYYMMDDHH_LEN + 1] ; 
    static char fileName[PATH_LEN] = "" ;
    struct tm * pRunTime = NULL ;
    long int irc ;

    MDMosaic = NULL ;

    /**      
     * Allocates memory for data arrays
     * based on the geo grid size data.
     **/
    allocMDMosaicMemory(pGeoData) ;

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
            MDMosaic[i][j] = MOSAIC_DEFAULT ;
        }
    }

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin mdmosaic calculation." , 
                             currTime) ;
    printMessage( message, logFile );        

    /**      
     * Multisensor analysis
     * generate MDMOSAIC field = BDMOSAIC field + gages.
     * if error occurs, set MDMOSAIC field to BDMOSAIC field
     **/
    MPEFieldGen_multi_sensor(pGeoData , pMPEParams , 
                gageSize, iug, ivg, zg, BDMosaic , 
                ID , umeang , MDMosaic , &errFlag);

    if(errFlag > 1) 
    {
       sprintf( message , "\nerror in multi-sensor field generation -- MDMOSAIC field set to all missing\n");
       printMessage( message, logFile );
    }

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end mdmosaic calculation." , 
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
        if(getAppsDefaults("mpe_mdmosaic_dir", mosaicDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"mpe_mdmosaic_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        first = 0 ;
    }
    
    sprintf(fileName, "MDMOSAIC%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, MDMosaic, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number = %ld "
            "attempting to write file: %s/%s" , 
            irc, mosaicDir, fileName) ;
        printMessage( message, logFile );
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s" , 
                         mosaicDir, fileName) ;
        printMessage( message, logFile );
    }

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end writing fields to flat files." , 
                              currTime) ;
    printMessage( message, logFile );
    
    /**      
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is mdmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "mdmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = MDMosaic[i][j] ;
            }
        }
    }

    releaseMDMosaicMemory(pGeoData) ;
}


void allocMDMosaicMemory(const geo_data_struct * pGeoData)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    MDMosaic = (double **)malloc(rowSize * sizeof(double *)); 
    if(MDMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocMDMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MDMosaic[i] = (double *)malloc(colSize * sizeof(double)); 
        if(MDMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocMDMosaicMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    
}

void releaseMDMosaicMemory(const geo_data_struct * pGeoData) 
{
    int    i ;
    const int rowSize = pGeoData->num_rows;


    if(MDMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(MDMosaic[i] != NULL)
            {
                free(MDMosaic[i]);
                MDMosaic[i] = NULL;
            }
        }
        free(MDMosaic);
        MDMosaic = NULL;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/run_mdmosaic.c,v $";
 static char rcs_id2[] = "$Id: run_mdmosaic.c,v 1.2 2012/07/23 17:00:12 pst Exp $";}
/*  ===================================================  */

}
