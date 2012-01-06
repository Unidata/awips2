/***********************************************************************
* Filename: run_mlmosaic.c
*
* Original Author: Unknown. Probably DJ Seo or Jay Breidenbach.
*
* File Creation Date: April 2005
*
* Development Group: HSEB/OHD
*
* Description:
* This function is converted from FORTRAN code: run_mlmosaic.f.
* This function computes the mlmosaic data.
* 
* Modules:
* runMLMosaic
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"
#include "multi_sensor.h"

/***********************************************************************
* Module Name: runMLMosaic
*
* Original Author: Unknown. Probably DJ Seo or Jay Breidenbach.
*
* Module Creation Date: April 2005
* 
* Description:
* This function computes the mlmosaic
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
* LMosaic      Input       double **        the local biased mosaic.
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
* initMLMosaicArray, freeMLMosaicArray, multi_sensor, writeArray
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
* April 2005   Guoxian Zhou      finish conversion to C Language 
* 02/14/2007   Guoxian Zhou      Modified for empe 
*
***********************************************************************/

static double ** MLMosaic = NULL ;

static void initMLMosaicArray(const geo_data_struct * pGeoData) ;
static void freeMLMosaicArray(const geo_data_struct * pGeoData) ;

void runMLMosaic(const run_date_struct * pRunDate ,
                 const geo_data_struct * pGeoData ,
                 empe_params_struct * pMPEParams ,
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

    static char mosaicDir[PATH_LEN] = {'\0'} ;
    static int first = 1 ;

    int i, j ;
    int errFlag ;

    static char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ; 
    static char fileName[PATH_LEN] = {'\0'} ;
    struct tm * pRunTime = NULL ;
    long int irc ;

    /*
     * skip the MLMosaic calculation when
     * the local bias 1hr rerun token is OFF
     * and the run hour is not greater than 1.
     */

    if((pMPEParams->locbias_1hr_rerun != 1) &&
       (pRunDate->hourNum <= 1))
    {
        sprintf( message , "No MLMosaic calculation because "
                           "the local bias 1hr rerun token is OFF "
                           "and the run hour is not greater than 1.") ;
        hpe_fieldgen_printMessage( message );
        
        return;
    }

    /*
     * Allocates memory for data arrays
     * based on the geo grid size data
     * and set to default value.
     */

    initMLMosaicArray(pGeoData) ;

    /*
     * strDateTime string should be in format: yyyymmddHHMM
     */

    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(strDateTime, ANSI_YEARSEC_TIME_LEN + 1,
        "%Y%m%d%H%M", pRunTime);

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin mlmosaic calculation." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message );        

    /*
     * Multisensor analysis
     *
     * generate MLMOSAIC field = LMOSAIC field + gages.
     */

    multi_sensor(pGeoData, pMPEParams, 
                gageSize, iug, ivg, zg, LMosaic,
                ID, umeang, MLMosaic, &errFlag);

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end mlmosaic calculation." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message );

    /*
     * write mlmosaic based on input options.
     */

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin writing fields to flat files." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message );

    if(first == 1)
    {
        if(hpe_fieldgen_getAppsDefaults("hpe_mlmosaic_dir", mosaicDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"hpe_mlmosaic_dir\"."
                "\n\tProgram exit.") ;
            shutdown( message);
        }
        first = 0 ;
    }

    sprintf(fileName, "MLMOSAIC%s%sz", pMPEParams->category_name, strDateTime ); 
    writeArray(pGeoData, mosaicDir, fileName,
               FACTOR_PRECIP, replace_missing,
               pMPEParams->user, pRunDate->tRunTime,
               PROC_FLAG, MLMosaic, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number = %ld "
            "attempting to write file: %s/%s." , 
            irc, mosaicDir, fileName) ;
        hpe_fieldgen_printMessage( message );
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s." , 
                           mosaicDir, fileName) ;
        hpe_fieldgen_printMessage( message );
    }

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time end writing fields to flat files." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message );
    
    /*
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is mlmosaic.
     */

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

    freeMLMosaicArray(pGeoData) ;

}


static void initMLMosaicArray(const geo_data_struct * pGeoData)
{

    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    MLMosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize );

}

static void freeMLMosaicArray(const geo_data_struct * pGeoData) 
{

    const int rowSize = pGeoData->num_rows;

    free2DDoubleArray(MLMosaic, rowSize );

}
