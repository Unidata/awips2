/*******************************************************************************
* FILENAME:            run_ldmosaic.c
*
* Purpose:
* This function computes the ldmosaic data.
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
* RDMosaic - the raw mosaic of radars.
*
* output variables
*
* LDMosaic   - the ldmosaic array.
*
* QPEMosaic - the mosaic of radars for best estimate.
*
*
*********************************************************************************/

#include "mpe_fieldgen.h"

short int ** locSpanDP ;
double ** tempLocSpan ;
double ** locBiasDP  ;
gage_radar_pair_table_struct * pGageRadarPairTable  ;

void allocLDMosaicMemory(const geo_data_struct * pGeoData, const int gageSize) ;
void releaseLDMosaicMemory(const geo_data_struct * pGeoData) ;

void runLDMosaic(const run_date_struct * pRunDate ,
                const geo_data_struct * pGeoData ,
                mpe_params_struct * pMPEParams ,
                const int gageSize,
                short * iug ,
                short * ivg ,
                float * zg ,
                double ** RDMosaic ,
                double ** LDMosaic,
                double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;
    const int replace_missing = 0 ;
    const local_bias_params * pLocalBiasParams = NULL;

    char    dateYMD[YYYYMMDDHH_LEN + 1] = {'\0'} ; 
    char    fileName[PATH_LEN] = {'\0'} ;

    static char    mosaicDir[PATH_LEN] = {'\0'} ;
    static char    locBiasDPDir[PATH_LEN] = {'\0'} ;
    static char    locSpanDPDir[PATH_LEN] = {'\0'} ;
    static char    stateVarDPDir[PATH_LEN] = {'\0'} ;
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

    locSpanDP = NULL ;
    locBiasDP = NULL ;
    pGageRadarPairTable = NULL ;
    tempLocSpan = NULL ;

    /* Retrieve the local bias settings. */
    pLocalBiasParams = MPEFieldGen_getLocalBiasParams ( );


    allocLDMosaicMemory(pGeoData, gageSize) ;

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
    sprintf( message , "%s = time begin ldmosaic calculation." , 
                    currTime) ;
    printMessage( message, logFile );

    /**      
     * Initialize arrays
     **/
    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            LDMosaic[i][j] = MOSAIC_DEFAULT ;
            locSpanDP[i][j] = -1 ;
            locBiasDP[i][j] = -9.0 ;
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
            gageSize, iug, ivg, zg, RDMosaic, pGageRadarPairTable) ;

        /**
         * generate LDMosaic, locBiasDP and locSpanDP fields.
         **/
        if(first == 1)
        {
            if(getAppsDefaults("mpe_statevardp_dir", stateVarDPDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"mpe_statevardp_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }

        MPEFieldGen_local_bias( pRunDate, pGeoData, gageSize, iug, ivg, zg,  
                    pMPEParams, pLocalBiasParams, si_cut, 
                    pGageRadarPairTable, RDMosaic, locSpanDP, locBiasDP,
					stateVarDPDir, LDMosaic, &ierr) ;

        /**
         * if error occured generating local bias field,
         * then set ldmosaic field to rdmosaic field.
         **/
        if(ierr > 0)
        {
            sprintf ( message , "ERROR: error occurred generating"
                " local bias field - "
                "ldmosaic field set to rdmosaic field.") ;
            printMessage( message, logFile );

            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                {
                    LDMosaic[i][j] = RDMosaic[i][j] ;
                }
            }
        } 
    } /* if (pMPEParams->locbias_1hr_rerun == 1 || pRunDate->hourNum > 1) */

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time end  ldmosaic calculation." , 
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
        if(getAppsDefaults("mpe_ldmosaic_dir", mosaicDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"mpe_ldmosaic_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }
    
    sprintf(fileName, "LDMOSAIC%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName, FACTOR_PRECIP, 
                   replace_missing, pMPEParams->user, pRunDate->tRunTime,
           PROC_FLAG, LDMosaic, &irc) ;

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
/*
        apply_mpe_polygons ( LDMosaic,
                             dateYMD,
                             year,
                             month,
                             day,
                             hour,
                             display_ldMosaic,
                             pGeoData,
                             FACTOR_PRECIP,
                             0,
		             0	); 
          
*/

    if(first == 1)
    {
        if(getAppsDefaults("mpe_locbiasdp_dir", locBiasDPDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"mpe_locbiasdp_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }
    
    sprintf(fileName, "LOCBIASDP%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, locBiasDPDir, fileName, FACTOR_PRECIP, 
                   replace_missing, pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, locBiasDP, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number =%ld "
            "attempting to write file: %s/%s." , 
            irc, locBiasDPDir, fileName) ;
        printMessage( message, logFile );
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s." , 
                locBiasDPDir, fileName) ;
        printMessage( message, logFile );
    }

    if(first == 1)
    {
        if(getAppsDefaults("mpe_locspandp_dir", locSpanDPDir) == -1)
        {
            sprintf ( message , "ERROR: Invalid token value"
                " for token \"mpe_locspandp_dir\"."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            tempLocSpan[i][j] = (double)locSpanDP[i][j] ;
        }
    }

    sprintf(fileName, "LOCSPANDP%sz", dateYMD ); 
    MPEFieldGen_writeArray(pGeoData, locSpanDPDir, fileName, 1.0,
                   replace_missing, pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, tempLocSpan, &irc) ;

    if(irc != 0)
    {
        sprintf( message , "ERROR: error number =%ld "
            "attempting to write file: %s/%s." , 
            irc, locSpanDPDir, fileName) ;
        printMessage( message, logFile );
    }
    else
    {
        sprintf( message , "STATUS: file written to: %s/%s." , 
                         locSpanDPDir, fileName ) ;
        printMessage( message, logFile );
    }

    /**      
     * fill in the "best estimate" mosaic
     * if the qpe_fieldtype is ldmosaic.
     **/
    if(strcmp(pMPEParams->qpe_fieldtype, "ldmosaic") == 0)
    {
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                QPEMosaic[i][j] = LDMosaic[i][j] ;
            }
        }
    }

    }

    first = 0 ;
    releaseLDMosaicMemory(pGeoData) ;  
} /* end runLDMosaic */


void allocLDMosaicMemory(const geo_data_struct * pGeoData, const int gageSize)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**      
     * allocate memory for locSpanDP variable
     **/
    locSpanDP = (short int **)malloc(rowSize * sizeof(short int *)); 
    if(locSpanDP == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runLDMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        locSpanDP[i] = (short int *)malloc(colSize * sizeof(short int)); 
        if(locSpanDP[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runLDMosaic function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    

    tempLocSpan = (double **)malloc(rowSize * sizeof(double *)); 
    if(tempLocSpan == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runLDMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        tempLocSpan[i] = (double *)malloc(colSize * sizeof(double)); 
        if(tempLocSpan[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runLDMosaic function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    


    /**      
     * allocate memory for locBiasDP variable
     **/
    locBiasDP = (double **)malloc(rowSize * sizeof(double *)); 
    if(locBiasDP == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runLDMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        locBiasDP[i] = (double *)malloc(colSize * sizeof(double)); 
        if(locBiasDP[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runLDMosaic function."
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
            " in runLDMosaic function."
            "\n\tProgram exit");
        shutDownMPE( message, logFile );
    }
    pGageRadarPairTable->ptrGageRadarPair = 
        (gage_radar_pair_struct *)malloc(gageSize * sizeof(gage_radar_pair_struct)); 
    if(pGageRadarPairTable->ptrGageRadarPair == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in runLDMosaic function."
            "\n\tProgram exit");
        shutDownMPE( message, logFile );
    }
} /* end allocLDMosaicMemory */

void releaseLDMosaicMemory(const geo_data_struct * pGeoData) 
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;

    for(i = 0; i < rowSize; i++)
    {
        if(locSpanDP[i] != NULL)
        {
            free(locSpanDP[i]);
            locSpanDP[i] = NULL;
        }
    }
    if(locSpanDP != NULL)
    {
        free(locSpanDP);
        locSpanDP = NULL;
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
        if(locBiasDP[i] != NULL)
        {
            free(locBiasDP[i]);
            locBiasDP[i] = NULL;
        }
    }
    if(locBiasDP != NULL)
    {
        free(locBiasDP);
        locBiasDP = NULL;
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

} /* end releaseLDMosaicMemory */
