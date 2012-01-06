/***********************************************************************
* Filename: run_lmosaic.c
*
* Original Author: Unknown. Probably DJ Seo or Jay Breidenbach.
*
* File Creation Date: April 2005
*
* Development Group: HSEB/OHD
*
* Description:
* This function is converted from FORTRAN code: run_lmosaic.f.
* This function computes the lmosaic data.
* 
* Modules:
* runLMosaic
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: runLMosaic
*
* Original Author: Unknown. Probably DJ Seo or Jay Breidenbach.
*
* Module Creation Date: April 2005
* 
* Description:
* This function computes the lmosaic
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
* LMosaic      Output      double **        the local biased mosaic.
* QPEMosaic    Output      double **        the best estimate mosaic 
* 
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* initLMosaicArray, freeLMosaicArray, lb_gr_pairs, local_bias, writeArray
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
* 01/30/2007   Guoxian Zhou      Modified for empe 
*
***********************************************************************/

static short  ** locSpan = NULL ;
static double ** tempLocSpan = NULL ;
static double ** locBias = NULL ;

static gage_radar_pair_table_struct * pGageRadarPairTable = NULL ;

void initLMosaicArray(const geo_data_struct * pGeoData, const int gageSize);
void freeLMosaicArray(const geo_data_struct * pGeoData) ;

void runLMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                empe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** RMosaic ,
                double ** LMosaic,
                double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;
    const int replace_missing = 0 ;
    const local_bias_params * pLocalBiasParams = NULL;

    static char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ; 
    char    fileName[PATH_LEN] = {'\0'} ;

    static char mosaicDir[PATH_LEN] = {'\0'} ;
    static char locBiasDir[PATH_LEN] = {'\0'} ;
    static char locSpanDir[PATH_LEN] = {'\0'} ;
    static char stateVarDir[PATH_LEN] = {'\0'} ;
    static int  first = 1 ;

    struct tm * pRunTime = NULL ;
    int ierr = 0 ;
    int year;
    int month;
    int day;
    int hour;
    long int irc ;

    float si_cut = 10.0;

    int i, j ;    

    /*
     * Retrieve the local bias settings.
     */

    pLocalBiasParams = getLocalBiasParams ( );

    initLMosaicArray(pGeoData, gageSize) ;

    /*      
     * strDateTime string should be in format: yyyymmddHHMM
     */

    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(strDateTime, ANSI_YEARSEC_TIME_LEN + 1,
        "%Y%m%d%H%M", pRunTime);

    year = pRunTime->tm_year + 1900;
    month = pRunTime->tm_mon + 1;
    day = pRunTime->tm_mday;
    hour = pRunTime->tm_hour;

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin lmosaic calculation." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message );

    /*
     * Log whether or not the local bias recalculation
     * will be performed.
     */

    if ( pMPEParams->locbias_1hr_rerun == 1 )
    {
        sprintf ( message, "local bias recalculation on rerun = ON" );
    }
    else
    {
        sprintf ( message, "local bias recalculation on rerun = OFF" );
    }
    hpe_fieldgen_printMessage( message);

    /*      
     * local bias calculation for radar
     * if locbias_1hr_rerun = ON, 
     * then recalculate local bias without regard
     * for number of hours
     * if locbias_1hr_rerun = OFF, 
     * then recalculate local bias ONLY if
     * hourNum > 1.
     */

    if ( ( pMPEParams->locbias_1hr_rerun == 1 ) ||
         ( pRunDate->hourNum > 1 ) )
    {
        sprintf ( message, "local bias correction for radar." );
        hpe_fieldgen_printMessage( message);

        /*
         * store positive gage/radar pairs.
         */

        lb_gr_pairs(pMPEParams->ptrRWBiasStat->min_gr_value_bias,
            gageSize, iug, ivg, zg, RMosaic, pGageRadarPairTable) ;

        /*
         * generate LMosaic, locBias and locSpan fields.
         */

        if(first == 1)
        {
            if(hpe_fieldgen_getAppsDefaults("hpe_statevar_dir", stateVarDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"hpe_statevar_dir\"."
                    "\n\tProgram exit.") ;
                shutdown( message);
            }
        }

        local_bias( pRunDate, pGeoData, gageSize, iug, ivg, zg,  
                    pMPEParams, pLocalBiasParams, si_cut, 
                    pGageRadarPairTable, RMosaic, locSpan, locBias,
                    stateVarDir, LMosaic, &ierr) ;

        /*
         * if error occured generating local bias field,
         * then set lmosaic field to rmosaic field.
         */

        if(ierr > 0)
        {
            sprintf ( message , "ERROR: error occurred generating"
                " local bias field - "
                "lmosaic field set to rmosaic field.") ;
            hpe_fieldgen_printMessage( message );

            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                {
                    LMosaic[i][j] = RMosaic[i][j] ;
                }
            }
        } 
    }
    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time end  lmosaic calculation." , 
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
        if(hpe_fieldgen_getAppsDefaults("hpe_lmosaic_dir", mosaicDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"hpe_lmosaic_dir\"."
                "\n\tProgram exit.") ;
            shutdown( message);
        }
    }

    sprintf(fileName, "LMOSAIC%s%sz", pMPEParams->category_name, strDateTime ); 

    writeArray(pGeoData, mosaicDir, fileName, FACTOR_PRECIP, 
               replace_missing, pMPEParams->user, pRunDate->tRunTime,
               PROC_FLAG, LMosaic, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number =%ld "
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

#if APPLY_POLYGON

        /* Apply edit polygons to the avgrmosaic product for
           use in furture products. */

        apply_mpe_polygons ( LMosaic,
                             strDateTime,
                             year,
                             month,
                             day,
                             hour,
                             display_lMosaic,
                             pGeoData,
                             FACTOR_PRECIP,
                             0,
                             0 ); 
#endif

    if(first == 1)
    {
        if(hpe_fieldgen_getAppsDefaults("hpe_locbias_dir", locBiasDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"hpe_locbias_dir\"."
                "\n\tProgram exit.") ;
            shutdown( message);
        }
    }
    
    sprintf(fileName, "LOCBIAS%s%sz", pMPEParams->category_name, strDateTime ); 

    writeArray(pGeoData, locBiasDir, fileName, FACTOR_PRECIP, 
               replace_missing, pMPEParams->user, pRunDate->tRunTime,
               PROC_FLAG, locBias, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number = %ld "
            "attempting to write file: %s/%s." , 
            irc, locBiasDir, fileName) ;
        hpe_fieldgen_printMessage( message );
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s." , 
                locBiasDir, fileName) ;
        hpe_fieldgen_printMessage( message );
    }

    if(first == 1)
    {
        if(hpe_fieldgen_getAppsDefaults("hpe_locspan_dir", locSpanDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"hpe_locspan_dir\"."
                "\n\tProgram exit.") ;
            shutdown( message);
        }
    }

    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            tempLocSpan[i][j] = (double)locSpan[i][j] ;
        }
    }

    sprintf(fileName, "LOCSPAN%s%sz", pMPEParams->category_name, strDateTime ); 
    writeArray(pGeoData, locSpanDir, fileName, 1.0,
               replace_missing, pMPEParams->user, pRunDate->tRunTime,
               PROC_FLAG, tempLocSpan, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number =%ld "
            "attempting to write file: %s/%s." , 
            irc, locSpanDir, fileName) ;
        hpe_fieldgen_printMessage( message );
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s." , 
                         locSpanDir, fileName ) ;
        hpe_fieldgen_printMessage( message );
    }

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time end writing fields to flat files." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message );

    /*
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is lmosaic.
     */

    if(strcmp(pMPEParams->qpe_fieldtype, "lmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = LMosaic[i][j] ;
            }
        }
    }

    first = 0 ;
    freeLMosaicArray(pGeoData) ;    
}


void initLMosaicArray(const geo_data_struct * pGeoData, const int gageSize)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    locSpan = init2DShortArray(SPAN_DEFAULT, rowSize, colSize );
    tempLocSpan = init2DDoubleArray(SPAN_DEFAULT, rowSize, colSize );

    locBias = init2DDoubleArray(BIAS_DEFAULT, rowSize, colSize );

    /*      
     * allocate memory for gage radar pair struct data
     */

    pGageRadarPairTable = 
    (gage_radar_pair_table_struct *)malloc(sizeof(gage_radar_pair_table_struct)); 
    if(pGageRadarPairTable == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in initLMosaicArray function."
            "\n\tProgram exit");
        shutdown( message);
    }
    pGageRadarPairTable->ptrGageRadarPair = 
        (gage_radar_pair_struct *)malloc(gageSize * sizeof(gage_radar_pair_struct)); 
    if(pGageRadarPairTable->ptrGageRadarPair == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in initLMosaicArray function."
            "\n\tProgram exit");
        shutdown( message);
    }
}

void freeLMosaicArray(const geo_data_struct * pGeoData) 
{
    const int rowSize = pGeoData->num_rows ;

    free2DShortArray(locSpan, rowSize );

    free2DDoubleArray(tempLocSpan, rowSize );
    free2DDoubleArray(locBias, rowSize );

    /*      
     * release memory for gage radar pair struct data
     */

    if(pGageRadarPairTable != NULL)
    {
        if(pGageRadarPairTable->ptrGageRadarPair != NULL)
        {
            free(pGageRadarPairTable->ptrGageRadarPair) ;
            pGageRadarPairTable->ptrGageRadarPair = NULL ;
        }

        free(pGageRadarPairTable) ;
        pGageRadarPairTable = NULL ;
    }
}
