/*******************************************************************************
* FILENAME:            run_lmosaic.c
*
* Purpose:
* This function is converted from FORTRAN code: run_lmosaic.f.
* This function computes the lmosaic data.
*
* calling function: main_mpe_fieldgen
* functions called: lb_gr_pairs, local_bias, writeArray
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
* RMosaic - the raw mosaic of radars.
*
* output variables
*
* LMosaic   - the lmosaic array.
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   April 2005   Guoxian Zhou      finish conversion to C Language 
*   March 2007   P Tilles          added logic to not write LMOSAIC field to file
*                                   if not calculated
*                                  removed print out concerning begin/end time file was
*                                   written
*
*********************************************************************************/

#include "mpe_fieldgen.h"

short int ** locSpan = NULL ;
double ** tempLocSpan = NULL ;
double ** locBias = NULL ;
gage_radar_pair_table_struct * pGageRadarPairTable = NULL ;

void allocLMosaicMemory(const geo_data_struct * pGeoData, const int gageSize) ;
void releaseLMosaicMemory(const geo_data_struct * pGeoData) ;

void MPEFieldGen_runLMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
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

    char    dateYMD[YYYYMMDDHH_LEN + 1] = {'\0'} ; 
    char    fileName[PATH_LEN] = {'\0'} ;

    static char    mosaicDir[PATH_LEN] = {'\0'} ;
    static char    locBiasDir[PATH_LEN] = {'\0'} ;
    static char    locSpanDir[PATH_LEN] = {'\0'} ;
    static char    stateVarDir[PATH_LEN] = {'\0'} ;
    static int     first = 1 ;

    struct tm * pRunTime = NULL ;
    int ierr = 0 ;
    int year;
    int month;
    int day;
    int hour;
    long int irc ;

	float si_cut = 10.0;

    int i, j ;    

    locSpan = NULL ;
    locBias = NULL ;
    pGageRadarPairTable = NULL ;
    tempLocSpan = NULL ;

    /* Retrieve the local bias settings. */
    pLocalBiasParams = MPEFieldGen_getLocalBiasParams ( );

    allocLMosaicMemory(pGeoData, gageSize) ;

    /**      
     * dateYMD string should be in format: yyyymmddhh
     **/
    memset(dateYMD, '\0', YYYYMMDDHH_LEN + 1);
    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime(dateYMD, YYYYMMDDHH_LEN + 1,
        "%Y%m%d%H", pRunTime);

    year = pRunTime->tm_year + 1900;
    month = pRunTime->tm_mon + 1;
    day = pRunTime->tm_mday;
    hour = pRunTime->tm_hour;

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin lmosaic calculation." , 
                    currTime) ;
    printMessage( message, logFile );

    /**      
     * Initialize arrays
     **/
    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            LMosaic[i][j] = MOSAIC_DEFAULT ;
            locSpan[i][j] = -1 ;
            locBias[i][j] = -9.0 ;
        }
    }

    /**      
     * Log whether or not the local bias recalculation
     * will be performed.
     **/
    if ( pMPEParams->locbias_1hr_rerun == 1 )
    {
        sprintf ( message, "local bias recalculation on rerun = ON" );
    }
    else
    {
        sprintf ( message, "local bias recalculation on rerun = OFF" );
    }
    printMessage ( message, logFile );

    /**      
     * local bias calculation for radar
     * if locbias_1hr_rerun = ON (= 1), 
     * then recalculate local bias without regard
     * for number of hours
     * if locbias_1hr_rerun = OFF (= 0), 
     * then recalculate local bias ONLY if
     * hourNum > 1.
     **/
    if ( ( pMPEParams->locbias_1hr_rerun == 1 ) ||
         ( pRunDate->hourNum > 1 ) )
    {

        if ( pLocalBiasParams->interp == 2 )
        {
           sprintf ( message, "  search radius = %4.1f km\n", 
                     pLocalBiasParams->dist_cut );
           printMessage ( message, logFile );
        }
     
        sprintf ( message, "  npairs_bias_select for radar = %3.0f",
                           pLocalBiasParams->sel_npr_lb_rad) ;
        printMessage ( message, logFile );

        /**
         * store positive gage/radar pairs.
         **/
        MPEFieldGen_lb_gr_pairs(pMPEParams->ptrRWBiasStat->min_gr_value_bias,
            gageSize, iug, ivg, zg, RMosaic, pGageRadarPairTable) ;

        /**
         * generate LMosaic, locBias and locSpan fields.
         **/
        if(first == 1)
        {
            if(getAppsDefaults("rfcwide_statevar_dir", stateVarDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"rfcwide_statevar_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }

        MPEFieldGen_local_bias( pRunDate, pGeoData, gageSize, iug, ivg, zg,  
                    pMPEParams, pLocalBiasParams, si_cut, 
                    pGageRadarPairTable, RMosaic, locSpan, locBias,
					stateVarDir, LMosaic, &ierr) ;

        /**
         * if error occured generating local bias field,
         * then set lmosaic field to rmosaic field.
         **/
        if(ierr > 0)
        {
            sprintf ( message , "ERROR: error occurred generating"
                " local bias field - "
                "lmosaic field set to rmosaic field.") ;
            printMessage( message, logFile );

            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                {
                    LMosaic[i][j] = RMosaic[i][j] ;
                }
            }
        } 
    }

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time end  lmosaic calculation." , 
                    currTime) ;
    printMessage( message, logFile );

    /*  if LMOSAIC field has not been recalculated, then do not write it to file */

    if ( ( pMPEParams->locbias_1hr_rerun == 1 ) ||
         ( pRunDate->hourNum > 1 ) )
    {

    /**
    * write out gridded data in xmrg format to flat files
    **/

    if(first == 1)
    {
        if(getAppsDefaults("rfcwide_lmosaic_dir", mosaicDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"rfcwide_lmosaic_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }
    
    sprintf(fileName, "LMOSAIC%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName, FACTOR_PRECIP, 
                   replace_missing, pMPEParams->user, pRunDate->tRunTime,
           PROC_FLAG, LMosaic, &irc) ;

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

        /* Apply edit polygons to the avgrmosaic product for
           use in future products. */
        MPEFieldGen_apply_mpe_polygons ( LMosaic,
                             dateYMD,
                             year,
                             month,
                             day,
                             hour,
                             display_lMosaic,
                             pGeoData,
                             FACTOR_PRECIP,
                             0,
		             0	); 
          

    if(first == 1)
    {
        if(getAppsDefaults("rfcwide_locbias_dir", locBiasDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"rfcwide_locbias_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }
    
    sprintf(fileName, "LOCBIAS%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, locBiasDir, fileName, FACTOR_PRECIP, 
                   replace_missing, pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, locBias, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number =%ld "
            "attempting to write file: %s/%s." , 
            irc, locBiasDir, fileName) ;
        printMessage( message, logFile );
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s." , 
                locBiasDir, fileName) ;
        printMessage( message, logFile );
    }

    if(first == 1)
    {
        if(getAppsDefaults("rfcwide_locspan_dir", locSpanDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"rfcwide_locspan_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            tempLocSpan[i][j] = (double)locSpan[i][j] ;
        }
    }

    sprintf(fileName, "LOCSPAN%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, locSpanDir, fileName, 1.0,
                   replace_missing, pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, tempLocSpan, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number =%ld "
            "attempting to write file: %s/%s." , 
            irc, locSpanDir, fileName) ;
        printMessage( message, logFile );
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s." , 
                         locSpanDir, fileName ) ;
        printMessage( message, logFile );
    }

    /**      
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is lmosaic.
     **/
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

    }

    first = 0 ;
    releaseLMosaicMemory(pGeoData) ;  
}


void allocLMosaicMemory(const geo_data_struct * pGeoData, const int gageSize)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**      
     * allocate memory for locSpan variable
     **/
    locSpan = (short int **)malloc(rowSize * sizeof(short int *)); 
    if(locSpan == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runLMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        locSpan[i] = (short int *)malloc(colSize * sizeof(short int)); 
        if(locSpan[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runLMosaic function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    

    tempLocSpan = (double **)malloc(rowSize * sizeof(double *)); 
    if(tempLocSpan == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runLMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        tempLocSpan[i] = (double *)malloc(colSize * sizeof(double)); 
        if(tempLocSpan[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runLMosaic function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    


    /**      
     * allocate memory for locBias variable
     **/
    locBias = (double **)malloc(rowSize * sizeof(double *)); 
    if(locBias == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runLMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        locBias[i] = (double *)malloc(colSize * sizeof(double)); 
        if(locBias[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runLMosaic function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    

    /**      
     * allocate memory for gage radar pair struct data
     **/
    pGageRadarPairTable = 
    (gage_radar_pair_table_struct *)malloc(sizeof(gage_radar_pair_table_struct)); 
    if(pGageRadarPairTable == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in runLMosaic function."
            "\n\tProgram exit");
        shutDownMPE( message, logFile );
    }
    pGageRadarPairTable->ptrGageRadarPair = 
        (gage_radar_pair_struct *)malloc(gageSize * sizeof(gage_radar_pair_struct)); 
    if(pGageRadarPairTable->ptrGageRadarPair == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in get_mean_bias function."
            "\n\tProgram exit");
        shutDownMPE( message, logFile );
    }
}

void releaseLMosaicMemory(const geo_data_struct * pGeoData) 
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;

    for(i = 0; i < rowSize; i++)
    {
        if(locSpan[i] != NULL)
        {
            free(locSpan[i]);
            locSpan[i] = NULL;
        }
    }
    if(locSpan != NULL)
    {
        free(locSpan);
        locSpan = NULL;
    }

    for(i = 0; i < rowSize; i++)
    {
        if(tempLocSpan[i] != NULL)
        {
            free(tempLocSpan[i]);
            tempLocSpan[i] = NULL;
        }
    }
    if(tempLocSpan != NULL)
    {
        free(tempLocSpan);
        tempLocSpan = NULL;
    }

    for(i = 0; i < rowSize; i++)
    {
        if(locBias[i] != NULL)
        {
            free(locBias[i]);
            locBias[i] = NULL;
        }
    }
    if(locBias != NULL)
    {
        free(locBias);
        locBias = NULL;
    }

    /**      
     * release memory for gage radar pair struct data
     **/
    if(pGageRadarPairTable->ptrGageRadarPair != NULL)
    {
        free(pGageRadarPairTable->ptrGageRadarPair) ;
        pGageRadarPairTable->ptrGageRadarPair = NULL ;
    }

    if(pGageRadarPairTable != NULL)
    {
        free(pGageRadarPairTable) ;
        pGageRadarPairTable = NULL ;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEFieldGen/RCS/run_lmosaic.c,v $";
 static char rcs_id2[] = "$Id: run_lmosaic.c,v 1.1 2007/10/15 12:19:15 dsa Exp lawrence $";}
/*  ===================================================  */

}
