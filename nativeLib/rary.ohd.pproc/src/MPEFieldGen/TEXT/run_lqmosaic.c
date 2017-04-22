/*******************************************************************************
* FILENAME:            run_lqmosaic.c
*
* Purpose:
* This function computes the lqmosaic data.
*
* calling function: main_mpe_fieldgen
* functions called: lb_gr_pairs, localBias, writeArray
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
* QMosaic - the raw Q2 mosaic field
*
* output variables
*
* LQMosaic   - the lqosaic array.
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   August 2008  P Tilles          initial version
*
*********************************************************************************/

#include "mpe_fieldgen.h"

void MPEFieldGen_local_bias ( const run_date_struct * pRunDate,
                  const geo_data_struct * pGeoData,
                  const int gageSize,
                  short * iug ,
                  short * ivg ,
                  float * zg ,
                  mpe_params_struct * pMPEParams,
                  const local_bias_params * pLocalBiasParams,
                  float si_cut,
                  gage_radar_pair_table_struct * pGageRadarPair,
                  double ** RMosaic,
                  short int ** local_span,
                  double ** local_bias,
                  const char * dirname,
                  double ** lmosaic,
                  int * ierr ) ;
short int ** local_span = NULL ;
double ** temp_local_span = NULL ;
double ** localBias = NULL ;
gage_radar_pair_table_struct * pGRPairTable = NULL ;

void allocLQMosaicMemory(const geo_data_struct * pGeoData, const int gageSize) ;
void releaseLQMosaicMemory(const geo_data_struct * pGeoData) ;

void runLQMosaic(const run_date_struct * pRunDate ,
		         const geo_data_struct * pGeoData ,
                 mpe_params_struct * pMPEParams ,
                 const int gageSize,
                 short * iug ,
                 short * ivg ,
                 float * zg ,
                 double ** QMosaic ,
                 double ** LQMosaic,
                 double ** QPEMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;
    const int replace_missing = 0 ;
    const local_bias_params * pLocalBiasParams = NULL;

    char    dateYMD[YYYYMMDDHH_LEN + 1] = {'\0'} ;
    char    fileName[PATH_LEN] = {'\0'} ;

    static char    mosaicDir[PATH_LEN] = {'\0'} ;
    static char    localBiasDir[PATH_LEN] = {'\0'} ;
    static char    local_spanDir[PATH_LEN] = {'\0'} ;
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

    local_span = NULL ;
    localBias = NULL ;
    pGRPairTable = NULL ;
    temp_local_span = NULL ;

    /* Retrieve the local bias settings. */
    pLocalBiasParams = getLocalBiasParams ( );

/*    releaseLQMosaicMemory(pGeoData) ;  */
    allocLQMosaicMemory(pGeoData, gageSize) ;

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
    sprintf( message , "%s = time begin lqmosaic calculation." ,
                        currTime) ;
    printMessage( message, logFile );

    /**
    * Initialize arrays
    **/
    for(i = 0; i < rowSize; i ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            LQMosaic[i][j] = MOSAIC_DEFAULT;
            local_span[i][j] = -1 ;
            localBias[i][j] = -9.0 ;
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
           gageSize, iug, ivg, zg, QMosaic, pGRPairTable) ;

       /**
        * generate LQMosaic, localBias and local_span fields.
        **/
       if(first == 1)
       {
           if(getAppsDefaults("mpe_q2_statevar_dir", stateVarDir) == -1)
           {
               sprintf ( message , "ERROR: Invalid token value"
                   " for token \"mpe_q2_statevar_dir\"."
                   "\n\tProgram exit.") ;
               shutDownMPE( message, logFile );
           }
       }

       MPEFieldGen_local_bias( pRunDate, pGeoData, gageSize, iug, ivg, zg,
                   pMPEParams, pLocalBiasParams, si_cut,
                            pGRPairTable, QMosaic, local_span, localBias,
                                       stateVarDir, LQMosaic, &ierr) ;


       /**
        * if error occured generating local bias field,
        * then set lqosaic field to qmosaic field.
        **/
       if(ierr > 0)
       {
           sprintf ( message , "ERROR: error occurred generating"
               " Q2 local bias field - "
               "lqmosaic field set to qmosaic field.") ;
           printMessage( message, logFile );

           for(i = 0; i < rowSize; i ++)
           {
               for(j = 0; j < colSize; j ++)
               {
                   LQMosaic[i][j] = QMosaic[i][j] ;
               }
           }
       }
   }


    getCurrentTime(currTime) ;
    sprintf( message , "%s = time end  lqmosaic calculation." ,
                           currTime) ;
    printMessage( message, logFile );

   /*  if LQMOSAIC field has not been recalculated, then do not write it to file */

    if ( ( pMPEParams->locbias_1hr_rerun == 1 ) ||
                 ( pRunDate->hourNum > 1 ) )
    {

    /**
     *write out gridded data in xmrg format to flat files
    **/

        if(first == 1)
        {
           if(getAppsDefaults("mpe_lqmosaic_dir", mosaicDir) == -1)
           {
              sprintf ( message , "ERROR: Invalid token value"
                      " for token \"mpe_lqmosaic_dir\"."
                       "\n\tProgram exit.") ;
              shutDownMPE( message, logFile );
           }
        }

        sprintf(fileName, "LQMOSAIC%sz", dateYMD );
        MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName, FACTOR_PRECIP,
                          replace_missing, pMPEParams->user, pRunDate->tRunTime,
                  PROC_FLAG, LQMosaic, &irc) ;

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
        * if the qpe_fieldtype is lqmosaic.
        **/
        if(strcmp(pMPEParams->qpe_fieldtype, "lqmosaic") == 0)
        {
           for(i = 0; i < rowSize; i ++)
           {
             for(j = 0; j < colSize; j ++)
             {
                 QPEMosaic[i][j] = LQMosaic[i][j] ;
             }
           }
         }

         }

        first = 0 ;
        releaseLQMosaicMemory(pGeoData) ;
}


void allocLQMosaicMemory(const geo_data_struct * pGeoData, const int gageSize)
{
	int    i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**
     * allocate memory for local_span variable
     **/
    local_span = (short int **)malloc(rowSize * sizeof(short int *));
    if(local_span == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runLQMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        local_span[i] = (short int *)malloc(colSize * sizeof(short int));
        if(local_span[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runLQMosaic function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

    temp_local_span = (double **)malloc(rowSize * sizeof(double *));
    if(temp_local_span == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runLQMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        temp_local_span[i] = (double *)malloc(colSize * sizeof(double));
        if(temp_local_span[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runLQMosaic function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }


    /**
     * allocate memory for localBias variable
     **/
    localBias = (double **)malloc(rowSize * sizeof(double *));
    if(localBias == NULL)


    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runLQMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        localBias[i] = (double *)malloc(colSize * sizeof(double));
        if(localBias[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runLQMosaic function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

    /**
     * allocate memory for gage radar pair struct data
     **/
    pGRPairTable =
    (gage_radar_pair_table_struct *)malloc(sizeof(gage_radar_pair_table_struct));
    if(pGRPairTable == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in runLQMosaic function."
            "\n\tProgram exit");
        shutDownMPE( message, logFile );
    }
    pGRPairTable->ptrGageRadarPair =
        (gage_radar_pair_struct *)malloc(gageSize * sizeof(gage_radar_pair_struct));
    if(pGRPairTable->ptrGageRadarPair == NULL)
    {
        sprintf ( message , "ERROR: Memory allocation failure"
            " in get_mean_bias function."
            "\n\tProgram exit");
        shutDownMPE( message, logFile );
    }
}

void releaseLQMosaicMemory(const geo_data_struct * pGeoData)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;

    for(i = 0; i < rowSize; i++)
    {
        if(local_span[i] != NULL)
        {
            free(local_span[i]);
            local_span[i] = NULL;
        }


    }
    if(local_span != NULL)
    {
        free(local_span);
        local_span = NULL;
    }

    for(i = 0; i < rowSize; i++)
    {
        if(temp_local_span[i] != NULL)
        {
            free(temp_local_span[i]);
            temp_local_span[i] = NULL;
        }
    }
    if(temp_local_span != NULL)
    {
        free(temp_local_span);
        temp_local_span = NULL;
    }

    for(i = 0; i < rowSize; i++)
    {
        if(localBias[i] != NULL)
        {
            free(localBias[i]);
            localBias[i] = NULL;
        }
    }
    if(localBias != NULL)
    {
        free(localBias);
        localBias = NULL;
    }

    /**
     * release memory for gage radar pair struct data
     **/
    if(pGRPairTable->ptrGageRadarPair != NULL)
    {
        free(pGRPairTable->ptrGageRadarPair) ;
        pGRPairTable->ptrGageRadarPair = NULL ;
    }

    if(pGRPairTable != NULL)
    {
        free(pGRPairTable) ;
        pGRPairTable = NULL ;
    }

    /*  ==============  Statements containing RCS keywords:  */
    {static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc_lib/src/MPEFieldGen/RCS/run_lqmosaic.c,v $";
     static char rcs_id2[] = "$Id: run_lqmosaic.c,v 1.1 2009/07/27 17:58:12 pst Exp $";}
    /*  ===================================================  */

}



