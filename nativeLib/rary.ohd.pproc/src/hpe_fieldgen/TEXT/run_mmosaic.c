/***********************************************************************
* Filename: run_mmosaic.c
*
* Original Author: Unknown. Probably DJ Seo or Jay Breidenbach.
*
* File Creation Date: June 2005
*
* Development Group: HSEB/OHD
*
* Description:
* This function is converted from FORTRAN code: run_mmosaic.f.
* This function computes the mmosaic data.
* 
* Modules:
* runMMosaic
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"
#include "multi_sensor.h"

/***********************************************************************
* Module Name: runMMosaic
*
* Original Author: Unknown. Probably DJ Seo or Jay Breidenbach.
*
* Module Creation Date: June 2005
* 
* Description:
* This function computes the mmosaic
*
* Calling Arguments:
* Name         Input/Output Type             Description
*
* pRunDate     Input        const run_date_struct *  date/time 
* pGeoData     Input        const geo_data_struct *
*                                            global HRAP lowerleft-corner
*                                            bin and dimension and dimension
*                                            of the RFC estimation domain
* pMPEParams   Input/Output mpe_params_struct *
*                                            static parameters
* gageSize     Input       int              the size of gage array.
* iug          Input       short *          the X_coord array of gage data.
* ivg          Input       short *          the Y_coord array of gage data.
* zg           Input       float *          the gage value array.
* ID           Input       int **           the ID array.
* RMosaic      Input       double **        the raw dsp mosaic.
* BMosaic      Input       double **        the mean biased mosaic.
* umeang       Input       double **        the PRISM data.
* QPEMosaic    Output      double **        the best estimate mosaic 
* 
* 
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* initMMosaicArray, freeMMosaicArray, multi_sensor, writeArray
*
* Return Value:
* Type          Description
* None
*
* Error Codes/Exceptions:
* 
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
* Date         Developer         Action
* June 2005    Guoxian Zhou      finish conversion to C Language 
* 01/16/2007   Guoxian Zhou      Modified for empe 
*
***********************************************************************/

static double ** MMosaic = NULL;

static void initMMosaicArray(const geo_data_struct * pGeoData) ;
static void freeMMosaicArray(const geo_data_struct * pGeoData) ;

void runMMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                empe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                int ** ID ,
                double ** RMosaic ,
                double ** BMosaic ,
                double ** umeang ,
                double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    const int replace_missing = 0 ;

    static char   mosaicDir[PATH_LEN] = {'\0'} ;
    static int    first = 1 ;

    int i, j ;
    int errFlag ;

    static char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ; 
    static char fileName[PATH_LEN] = {'\0'} ;
    struct tm * pRunTime = NULL ;
    long int irc ;

    /*      
     * Allocates memory for data arrays
     * based on the geo grid size data.
     */

    initMMosaicArray(pGeoData) ;

    /*      
     * strDateTime string should be in format: yyyymmddHHMM
     */

    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(strDateTime, ANSI_YEARSEC_TIME_LEN + 1,
        "%Y%m%d%H%M", pRunTime);

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin mmosaic calculation." , 
                       currTime) ;
    hpe_fieldgen_printMessage( message );        

    /*      
     * Multisensor analysis
     * generate MMOSAIC field = BMOSAIC field + gages.
     */

    multi_sensor(pGeoData , pMPEParams , 
                gageSize, iug, ivg, zg, BMosaic , 
                ID , umeang , MMosaic , &errFlag);

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end mmosaic calculation." , 
                       currTime) ;
    hpe_fieldgen_printMessage( message );        

    /*
     * write out gridded data in xmrg format to flat files
     */

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin writing fields to flat files." , 
                       currTime) ;
    hpe_fieldgen_printMessage( message );

    if(first == 1)
    {
        if(hpe_fieldgen_getAppsDefaults("hpe_mmosaic_dir", mosaicDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"hpe_mmosaic_dir\"."
                "\n\tProgram exit.") ;
            shutdown( message);
        }
        first = 0 ;
    }
    
    sprintf(fileName, "MMOSAIC%s%sz", pMPEParams->category_name, strDateTime ); 

    writeArray(pGeoData, mosaicDir, fileName,
               FACTOR_PRECIP, replace_missing,
               pMPEParams->user, pRunDate->tRunTime,
               PROC_FLAG, MMosaic, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number = %ld "
            "attempting to write file: %s/%s" , 
            irc, mosaicDir, fileName) ;
        hpe_fieldgen_printMessage( message );
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s" , 
                         mosaicDir, fileName) ;
        hpe_fieldgen_printMessage( message );
    }

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end writing fields to flat files." , 
                       currTime) ;
    hpe_fieldgen_printMessage( message );
    
    /*      
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is mmosaic.
     */

    if(strcmp(pMPEParams->qpe_fieldtype, "mmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = MMosaic[i][j] ;
            }
        }
    }

    freeMMosaicArray(pGeoData) ;
}


static void initMMosaicArray(const geo_data_struct * pGeoData)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    MMosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );
}

static void freeMMosaicArray(const geo_data_struct * pGeoData) 
{
    const int rowSize = pGeoData->num_rows;

    free2DDoubleArray(MMosaic, rowSize );
}
