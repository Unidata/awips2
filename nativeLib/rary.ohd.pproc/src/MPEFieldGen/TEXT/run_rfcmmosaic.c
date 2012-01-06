/*******************************************************************************
* FILENAME:            run_mmosaic.c
*
* Purpose:
* This function is converted from FORTRAN code: run_mmosaic.f.
* This function computes the mmosaic data.
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
* RMosaic - the raw mosaic of radars.
*
* BMosaic - the bmosaic array.
*
* umeang  - the PRISM data
*
* output variables
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   June 2005    Guoxian Zhou      finish conversion to C Language 
*
*********************************************************************************/

#include "mpe_fieldgen.h"
#include "multi_sensor.h"

static double ** RfcMMosaic = NULL ;

static void allocRfcMMosaicMemory(const geo_data_struct * pGeoData)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    RfcMMosaic = (double **)malloc(rowSize * sizeof(double *)); 
    if(RfcMMosaic == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in allocRfcMMosaicMemory function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        RfcMMosaic[i] = (double *)malloc(colSize * sizeof(double)); 
        if(RfcMMosaic[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in allocRfcMMosaicMemory function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    
}

static void releaseRfcMMosaicMemory(const geo_data_struct * pGeoData) 
{
    int    i ;
    const int rowSize = pGeoData->num_rows;


    if(RfcMMosaic != NULL)
    {
        for(i = 0; i < rowSize; i++)
        {
            if(RfcMMosaic[i] != NULL)
            {
                free(RfcMMosaic[i]);
                RfcMMosaic[i] = NULL;
            }
        }
        free(RfcMMosaic);
        RfcMMosaic = NULL;
    }
}

void runRfcMMosaic(const run_date_struct * pRunDate ,
                   const geo_data_struct * pGeoData ,
                   mpe_params_struct * pMPEParams ,
                   const int gageSize,
                   short * iug ,
                   short * ivg ,
                   float * zg ,
                   int ** ID ,
                   double ** RMosaic ,
                   double ** RfcBMosaic ,
                   double ** umeang ,
                   double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int replace_missing = 0 ;

    static char    mosaicDir[PATH_LEN] = "" ;
    static int    first = 1 ;

    int i, j ;
    int errFlag ;

    static char dateYMD[YYYYMMDDHH_LEN + 1] ; 
    static char fileName[PATH_LEN] = "" ;
    struct tm * pRunTime = NULL ;
    long int irc ;

    RfcMMosaic = NULL ;

    /**      
     * Allocates memory for data arrays
     * based on the geo grid size data.
     **/
    allocRfcMMosaicMemory(pGeoData) ;

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
            RfcMMosaic[i][j] = MOSAIC_DEFAULT ;
        }
    }

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin mmosaic calculation." , 
                             currTime) ;
    printMessage( message, logFile );        

    /**      
     * Multisensor analysis
     * generate MMOSAIC field = BMOSAIC field + gages.
     **/
    MPEFieldGen_multi_sensor(pGeoData , pMPEParams , 
                gageSize, iug, ivg, zg, RfcBMosaic , 
                ID , umeang , RfcMMosaic , &errFlag);

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end mmosaic calculation." , 
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
        if(getAppsDefaults("mpe_rfcmmosaic_dir", mosaicDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"mpe_rfcmmosaic_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
        first = 0 ;
    }
    
    sprintf(fileName, "RFCMMOSAIC%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, RfcMMosaic, &irc) ;

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
     * if the qpe_fieldtype is rfcmmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "rfcmmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = RfcMMosaic[i][j] ;
            }
        }
    }

    releaseRfcMMosaicMemory(pGeoData) ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/run_rfcmmosaic.c,v $";
 static char rcs_id2[] = "$Id: run_rfcmmosaic.c,v 1.1 2007/10/15 12:19:15 dsa Exp lawrence $";}
/*  ===================================================  */

}



