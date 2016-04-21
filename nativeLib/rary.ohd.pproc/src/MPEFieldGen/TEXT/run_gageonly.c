/*******************************************************************************
* 
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
*
*********************************************************************************/

#include "mpe_fieldgen.h"
#include "multi_sensor.h"

double ** GMosaic ;

void allocGageonlyMemory(const geo_data_struct * pGeoData) ;
void releaseGageonlyMemory(const geo_data_struct * pGeoData) ;

void MPEFieldGen_runGageonly(const run_date_struct * pRunDate,
                const geo_data_struct * pGeoData,
                mpe_params_struct * pMPEParams,
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

    static char    dateYMD[YYYYMMDDHH_LEN + 1] = {'\0'} ; 
    long int irc ;

    static char    fileName[PATH_LEN] = {'\0'} ;
    static char    mosaicDir[PATH_LEN] = { '\0'} ;
    static int    first = 1 ;
    struct tm * pRunTime = NULL ;

    int i, j ;    

    /**      
     * dateYMD string should be in format: yyyymmddhh
     **/
    memset(dateYMD, '\0', YYYYMMDDHH_LEN + 1);
    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(dateYMD, YYYYMMDDHH_LEN + 1,
        "%Y%m%d%H", pRunTime);

    GMosaic = NULL ;

    int errFlag ;

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin gageonly analysis." , 
                    currTime) ;
    printMessage( message, logFile );        

    /**      
     * Allocates memory for data arrays
     * based on the geo grid size data.
     **/     
    allocGageonlyMemory(pGeoData) ;

    /**      
     * Initialize array
     **/
    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            GMosaic[i][j] = MOSAIC_DEFAULT ;
        }
    }

    if(gageSize < 1)
    {
        sprintf( message , "STATUS: No hourly gages data available"
            " for current hour, "
            "\n\tdefault gageonly to missing value.\n") ;
        printMessage( message, logFile );
    }
    else
    {
        MPEFieldGen_gage_only(pGeoData , pMPEParams, gageSize, iug, ivg, zg ,
            umeang , GMosaic , &errFlag);
    }

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end gageonly analysis." , 
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
        if(getAppsDefaults("rfcwide_gageonly_dir", mosaicDir) == -1)
        {
            sprintf ( message , "Error: Invalid token value"
                " for token \"rfcwide_gageonly_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        first = 0 ;
    }

    sprintf(fileName, "GAGEONLY%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
               FACTOR_PRECIP, replace_missing,
               pMPEParams->user, pRunDate->tRunTime,
               PROC_FLAG, GMosaic, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number = %ld "
            "attempting to write file: %s/%s." , 
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
     * if the qpe_fieldtype is gageonly.
     **/
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

    releaseGageonlyMemory(pGeoData) ;
} /* end MPEFieldGen_runGageonly */


void allocGageonlyMemory(const geo_data_struct * pGeoData)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    GMosaic = (double **)malloc(rowSize * sizeof(double *)); 
    if(GMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocGageonlyMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        GMosaic[i] = (double *)malloc(colSize * sizeof(double)); 
        if(GMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocGageonlyMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    
} /* end allocGageonlyMemory */

void releaseGageonlyMemory(const geo_data_struct * pGeoData) 
{
    int i ;
    const int rowSize = pGeoData->num_rows;

    if(GMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(GMosaic[i] != NULL)
            {
                free(GMosaic[i]);
                GMosaic[i] = NULL;
            }
        }
        free(GMosaic);
        GMosaic = NULL;
    }

} /* end releaseGageonlyMemory */
