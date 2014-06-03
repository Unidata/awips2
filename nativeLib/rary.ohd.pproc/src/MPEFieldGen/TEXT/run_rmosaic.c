/*******************************************************************************
* FILENAME:  run_rmosaic.c
*
* Purpose:
* This function is converted from FORTRAN code: run_rmosaic.f.
* This function computes the raw mosaic data
*
* calling function: main_mpe_fieldgen
* functions called: readRadarLoc, readRadarResult ,
*                readRadarData, readMisc, getMeanBias,
*                writeRadarResult, createMosaic,
*                checkMultisensorQC, writeArray
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
* pGageTable - array of gage data
*
* pQCGageTable - copy of the gage data for multisensor qc check.
*
*
* output variables
*
* meanFieldBias - the mean field bias value computed for each radar.
*
* ID - the mosaic id array of radars.
*
* RMosaic - the mosaic of radars.
*
* QPEMosaic - the mosaic of radars for best estimate.
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*                C. R. Kondragunta Original FORTRAN code
*   March 2005   Guoxian Zhou      convert to C Language
*
*********************************************************************************/

#include "mpe_fieldgen.h"
#include "mpe_field_names.h"
#include "BinarySearch.h"

short  ** MPEFieldGen_radarMiscBins = NULL ;
double ** MHeight ;
double ** tempID ;


// Added by Ram for the average and max mosaic calculation
// ---------------------------
extern double  ** MPEFieldGen_MaxMosaic;
extern double  ** MPEFieldGen_AvgMosaic;
extern int     ** MPEFieldGen_AvgMosaicNumRadars;
extern double  ** MPEFieldGen_P3Mosaic;
// --------------------------


extern void MPEFieldGen_set_best_estimate_p3();

extern void MPEFieldGen_unset_best_estimate_p3();


float radar [ NUM_DPA_COLS] [ NUM_DPA_COLS ] ;

void allocRMosaicMemory(const geo_data_struct * pGeoData) ;
void releaseRMosaicMemory(const geo_data_struct * pGeoData) ;

static int compare_radar_id ( void * search_value,
                              void * array_value ) ;

void MPEFieldGen_runRMosaic(const run_date_struct * pRunDate,
                const geo_data_struct * pGeoData,
                mpe_params_struct * pMPEParams,
                const radarLoc_table_struct * pRadarLocTable ,
                const gage_table_struct * pGageTable,
                const gage_table_struct * pGageTableP3,
                const gage_table_struct * pQCGageTable,
                double * meanFieldBias,
                int     ** ID,
                double ** RMosaic,
                double ** QPEMosaic,
                int    *  blnMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;
    const int replace_missing = 0 ;

    static char datetime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    static char dateYMD[YYYYMMDDHH_LEN + 1] = {'\0'} ;
    static char radarID[ RADAR_ID_LEN + 1] = {'\0'} ;
    register int  i = 0, j, k, l;
    int  gageRadarPairNum ;    /* number of positive gage/radar pairs */

    long int    irc ;

    int radarAvailFlag ;
    int blnEditBias ;
    int blnIgnoreRadar ;
    register int year, month, day, hour;

    double editBiasValue = 0.0 ;
    double memSpanBias ;
    double meanBias = 0.0 ;

    static char fileName[PATH_LEN] = {'\0'} ;
    static char heightDir[PATH_LEN] = {'\0'} ;
    static char indexDir[PATH_LEN] = {'\0'} ;
    static char mosaicDir[PATH_LEN] = {'\0'} ;
    static char avgRMosaicDir[PATH_LEN] = {'\0'} ;
    static char maxRMosaicDir[PATH_LEN] = {'\0'} ;

    static int first = 1 ;
    struct tm * pRunTime = NULL;

    MHeight = NULL ;
    tempID = NULL ;

    pMPEParams->radar_avail_num = 0 ;

    // Additional check added by Ram to see if any of the Radar mosaics are
    // to be calculated. if not we abort here
    // ---------------------

    if(blnMosaic[rmosaic] == 0 && blnMosaic[avgrmosaic] == 0 && blnMosaic[maxrmosaic] == 0)
    {
        sprintf ( message , "\nErroneous call to RMosaic\n");
        printMessage( message, logFile );
        return;
    }
    // ---------------------


    /**
     * Allocates memory for data arrays
     * based on the geo grid size data.
     **/
    allocRMosaicMemory(pGeoData) ;

    /**
     * dateYMD string should be in format: yyyymmddhh
     **/

    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime ( datetime, ANSI_YEARSEC_TIME_LEN + 1,
        "%Y-%m-%d %H:00:00", pRunTime ) ;
    strftime(dateYMD, YYYYMMDDHH_LEN + 1,
        "%Y%m%d%H", pRunTime);

    year = pRunTime->tm_year + 1900;
    month = pRunTime->tm_mon + 1;
    day = pRunTime->tm_mday;
    hour = pRunTime->tm_hour;

    /**
     * Initialize arrays
     **/


    //unset_best_estimate_p3();

    for(k = 0; k < rowSize; k ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            RMosaic[k][j] = MOSAIC_DEFAULT ;
            MPEFieldGen_AvgMosaic[k][j] = MOSAIC_DEFAULT ;
            MPEFieldGen_MaxMosaic[k][j] = MOSAIC_DEFAULT ;
	    MPEFieldGen_AvgMosaicNumRadars[k][j] = 0 ;
            MHeight[k][j] = MOSAIC_DEFAULT ;
            tempID[k][j] = 0.0 ;
            ID[k][j] = 0 ;
	    MPEFieldGen_P3Mosaic[k][j] = MOSAIC_DEFAULT;
        }
    }

    for(k = 0; k < NUM_DPA_COLS; k++)
    {
        for(j = 0; j < NUM_DPA_COLS; j++)
        {
            radar[k][j] = RADAR_DEFAULT ;
        }
    }

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin RMOSAIC calculation." ,
                    currTime) ;
    printMessage( message, logFile );

    radar_result_struct * pRadarResult = NULL;
    short radar_count ;

    pRadarResult = (radar_result_struct *)
        malloc(pRadarLocTable->radarNum * sizeof(radar_result_struct));
    if(pRadarResult == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runRMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    for(i = 0; i < pRadarLocTable->radarNum; i++ )
    {
        memset(&(pRadarResult[i].radID), '\0', RADAR_ID_LEN + 1) ;
        pRadarResult[i].edit_bias = 0;
        pRadarResult[i].ignore_dpa_radar = 0;
        pRadarResult[i].bias = 0.0 ;
    }

    /**
     * Read the edit bias flag, edited bias, ignore radar flag
     * from RWRadarResult table
     **/
    MPEFieldGen_readRadarResult (datetime, pRadarResult, &radar_count, &irc) ;
    if (irc < 0)
    {
        sprintf ( message , "ERROR: Database error #%ld attempting to "
            "select record from RWRadarResult table.", irc) ;
        printMessage( message, logFile );
    }

    /* Read the misbin information.  Do this only once per MPE Fieldgen Run. */
//    if ( first == 1 )
    {
       MPEFieldGen_readMisc ( pRadarLocTable, pMPEParams->os, MPEFieldGen_radarMiscBins ) ;
    }

    /**
     * begin loop on radars
     **/
    for(i = 0; i < pRadarLocTable->radarNum; i++ )
    {
        meanFieldBias[i] = 0.0 ;

        strcpy(radarID, pRadarLocTable->ptrRadarLocRecords[i].radarID) ;

        sprintf ( message , "\n******* radar: %s *******", radarID);
        printMessage( message, logFile );

        /**
         * initialize edit bias flag, ignore radar flag
         **/
        blnEditBias    = 0 ;
        blnIgnoreRadar = 0 ;
        editBiasValue  = 0.0 ;

        radar_result_struct * pRadarInfo = NULL ;

        pRadarInfo = (radar_result_struct *)
            binary_search ( pRadarResult, radarID, radar_count,
                sizeof ( radar_result_struct), compare_radar_id );

        /* load in the radar info */
        if ( pRadarInfo != NULL )
        {
            editBiasValue   = pRadarInfo->bias;
            blnEditBias       = pRadarInfo->edit_bias;
            blnIgnoreRadar = pRadarInfo->ignore_dpa_radar;
        }

        /**
         * Read in gridded radar data
         * count number of available radars
         * an "ignored" radar is considered "not available"
         **/
        readRadarData(radarID, datetime, pMPEParams->dpa_wind,
            blnIgnoreRadar, radar, &radarAvailFlag) ;

        if(radarAvailFlag > 0)
            pMPEParams->radar_avail_num ++ ;

        /**
         * calculate mean field bias if it is set to "ON"
         **/
        if(pMPEParams->blnMeanFieldBias == 1)
        {
            /**
             * calculate the mean field bias value
             * for each radar and sAve it to meanFieldBias[i]
             **/
           MPEFieldGen_getMeanBias(&(pRadarLocTable->ptrRadarLocRecords[i]),
                    datetime, ( void * ) MPEFieldGen_radarMiscBins [i], radar,
                    pGeoData, pGageTable, pMPEParams,
                    &meanBias, &memSpanBias, &gageRadarPairNum ) ;

            meanFieldBias[i] = meanBias ;

            /**
             * if blnEditBias = 1, then use edited bias value
             **/
            if(blnEditBias == 1)
                meanFieldBias[i] = editBiasValue ;

            /**
             * write information to the rwradarresult table
             **/
            MPEFieldGen_writeRadarResult(radarID, datetime, gageRadarPairNum,
                radarAvailFlag, meanFieldBias[i], memSpanBias, &irc) ;

            if(irc != 0)
            {
                sprintf ( message , "Database error #%ld attempting to "
                            "write record to RWRadarResult table.\n"
                            "Program exit", irc);
                shutDownMPE( message, logFile );
            }

            if(blnEditBias == 1)
            {
                sprintf ( message , "Edited bias value = %4.2f used.",
                    editBiasValue);
                printMessage( message, logFile );
            }
        }

        /**
         * mosaicking algorithm
         **/
        MPEFieldGen_createMosaic(&(pRadarLocTable->ptrRadarLocRecords[i]),
                    radar, (void * )MPEFieldGen_radarMiscBins[i], i+1, pGeoData,
                    RMosaic, MHeight, ID,
                    MPEFieldGen_MaxMosaic, MPEFieldGen_AvgMosaic, MPEFieldGen_AvgMosaicNumRadars,blnMosaic ) ;
    }
    /* end loop on radars  */

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end RMOSAIC calculation." ,
                    currTime) ;
    printMessage( message, logFile );



    // Added by Ram for the average and max mosaic calculations

    // at this time we have the sum for each hrap grid bin from the create_mosaic.c
    // file for the AvgMosaic array. also for the corresponding grid bins we have
    // the number of radars contributing to it in the AvgMosaicNumRadar array.
    // so in the following code we divide each gridbin value by the num radars to
    // get the final average mosaics.
    // we already have the final product in the MaxMosaic array at this time.
    // ---------------------------
    if(blnMosaic[avgrmosaic] == 1)
    {
        for(k=0;k<rowSize;k++)
        {
            for(l=0;l<colSize;l++)
            {
                if(MPEFieldGen_AvgMosaicNumRadars[k][l] > 0)
                {
                    if(MPEFieldGen_AvgMosaic[k][l] != MOSAIC_DEFAULT)
                    {
                        MPEFieldGen_AvgMosaic[k][l] /= MPEFieldGen_AvgMosaicNumRadars[k][l];
                    }
                    else
                    {
                        sprintf(message, "grid cell calculation error for cell [%d][%d] = %lf in the avg mosaic\n",k,l,MPEFieldGen_AvgMosaic[k][l]);
                        printMessage(message,logFile);
                    }
                }
                else
                {
                    if(MPEFieldGen_AvgMosaic[k][l] != MOSAIC_DEFAULT)
                    {
                        sprintf(message, "grid cell calculation error for cell [%d][%d] = %lf in the avg mosaic\n",k,l,MPEFieldGen_AvgMosaic[k][l]);
                        printMessage(message,logFile);
                    }
                }
            }
        }
        // ----------------------------
        // at this time we have the average and max mosaics in the AvgMosaic and MaxMosaic arrays!
        // now we have to write these arrays to xmrg files which is done in the following code.
        if(first == 1)
        {
            if(getAppsDefaults("rfcwide_avg_rmosaic_dir", avgRMosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"rfcwide_ave_rmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }

        sprintf(fileName, "AVGRMOSAIC%sz", dateYMD );

        MPEFieldGen_writeArray( pGeoData, avgRMosaicDir, fileName,
                    FACTOR_PRECIP, replace_missing,
                    pMPEParams->user, pRunDate->tRunTime,
                    PROC_FLAG, MPEFieldGen_AvgMosaic, &irc ) ;

        /* Apply edit polygons to the avgrmosaic product for
           use in furture products. */
        MPEFieldGen_apply_mpe_polygons ( MPEFieldGen_AvgMosaic,
                             dateYMD,
                             year,
                             month,
                             day,
                             hour,
                             display_avgrMosaic,
                             pGeoData,
                             FACTOR_PRECIP,
                             0,
		             0);


    }//end blnMosaic condition

    if(blnMosaic[maxrmosaic] == 1)
    {
        if(first == 1)
        {
            if(getAppsDefaults("rfcwide_max_rmosaic_dir", maxRMosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"rfcwide_max_rmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }

        sprintf(fileName, "MAXRMOSAIC%sz", dateYMD );
        MPEFieldGen_writeArray(pGeoData, maxRMosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, MPEFieldGen_MaxMosaic, &irc) ;

        /* Apply edit polygons to the avgrmosaic product for
           use in furture products. */
        MPEFieldGen_apply_mpe_polygons ( MPEFieldGen_MaxMosaic,
                             dateYMD,
                             year,
                             month,
                             day,
                             hour,
                             display_maxrMosaic,
                             pGeoData,
                             FACTOR_PRECIP,
                             0,
		             0	);

    }// end of blnMosaic condition
    // -------------------------------------------------
    // average and max mosaic xmrg's written to file.

    /**
     * Gage QC - Point Check using Radar Mosaic data.
     **/

    if(blnMosaic[rmosaic] == 1)
    {
        if(pMPEParams->gage_qc == 1)
        {
            getCurrentTime(currTime) ;
            sprintf( message , "%s = time begin Gage QC multisensor check." ,
                            currTime) ;
            printMessage( message, logFile );

            if(pGageTable->pseudoGageNum == pGageTable->totalGageNum)
            {
                sprintf( message , "STATUS: No hourly gages data available,"
                    " skip Multisensor Check." ) ;
                printMessage( message, logFile );
            }
            else
            {
                MPEFieldGen_checkMultisensorQC(datetime , RMosaic ,
                    pGeoData, pQCGageTable) ;
            }
            getCurrentTime(currTime) ;
            sprintf( message , "%s = time end Gage QC multisensor check." ,
                            currTime) ;
            printMessage( message, logFile );
        }
    }//end blnMosaic condition

    /**
     * write out gridded data in xmrg format to flat files
     **/
    if(blnMosaic[rmosaic] == 1)
    {
        getCurrentTime(currTime) ;
        sprintf( message , "%s = time begin writing RMOSAIC fields to flat files.",
                        currTime) ;
        printMessage( message, logFile );

        if(first == 1)
        {
            if(getAppsDefaults("rfcwide_rmosaic_dir", mosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"rfcwide_rmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }

        sprintf(fileName, "RMOSAIC%sz", dateYMD );
        MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName, FACTOR_PRECIP,
                   replace_missing, pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, RMosaic, &irc) ;

        if(irc != 0)
        {
            sprintf( message , "ERROR: error number = %ld"
                " attempting to write file: %s/%s." ,
                irc, mosaicDir, fileName) ;
            printMessage( message, logFile );
        }
        else
        {
            sprintf( message , "STATUS: file written to: %s/%s" ,
                    mosaicDir, fileName) ;
            printMessage( message, logFile );
        }

            /* Apply edit polygons to the avgrmosaic product for
               use in future products. */
            MPEFieldGen_apply_mpe_polygons ( RMosaic,
                                 dateYMD,
                                 year,
                                 month,
                                 day,
                                 hour,
                                 display_rMosaic,
                                 pGeoData,
                                 FACTOR_PRECIP,
                                 0,
			         0 );

        if(first == 1)
        {
            if(getAppsDefaults("rfcwide_height_dir", heightDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"rfcwide_height_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }

        sprintf(fileName, "HEIGHT%sz", dateYMD );
        MPEFieldGen_writeArray(pGeoData, heightDir, fileName, FACTOR_OTHER,
                   replace_missing, pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, MHeight, &irc) ;

        if(irc != 0)
        {
            sprintf( message , "ERROR: error number = %ld"
                " attempting to write file: %s/%s." ,
                irc, heightDir, fileName) ;
            printMessage( message, logFile );
        }
        else
        {
            sprintf( message , "STATUS: file written to: %s/%s" , heightDir,
                     fileName) ;
            printMessage( message, logFile );
        }

        if(first == 1)
        {
            if(getAppsDefaults("rfcwide_index_dir", indexDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"rfcwide_index_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }

        sprintf(fileName, "INDEX%sz", dateYMD );

        /**
         * fill in tempID array
         **/
        for(i = 0; i < rowSize; i ++)
        {
            for(j = 0; j < colSize; j ++)
            {
                tempID[i][j] = (double)ID[i][j] ;
            }
        }

        MPEFieldGen_writeArray(pGeoData, indexDir, fileName, FACTOR_OTHER,
                   replace_missing, pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, tempID, &irc) ;

        if(irc != 0)
        {
            sprintf( message , "ERROR: error number = %ld"
                " attempting to write file: %s/%s." ,
                irc, indexDir, fileName) ;
            printMessage( message, logFile );
        }
        else
        {
            sprintf( message, "STATUS: file written to: %s/%s" , indexDir,
                     fileName) ;
            printMessage( message, logFile );
        }

        getCurrentTime(currTime) ;
        sprintf( message, "%s = time   end writing RMOSAIC fields to flat files." ,
                        currTime) ;
        printMessage( message, logFile );

        /**
         * fill in the "best estimate" mosaic
         * if the qpe_fieldtype is rmosaic.
         **/
        if(strcmp(pMPEParams->qpe_fieldtype, "rmosaic") == 0)
        {
            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                {
                    QPEMosaic[i][j] = RMosaic[i][j] ;
                }
            }
        }
        if(strcmp(pMPEParams->qpe_fieldtype, "avgrmosaic") == 0)
        {
            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                {
                    QPEMosaic[i][j] = MPEFieldGen_AvgMosaic[i][j] ;
                }
            }
        }
        if(strcmp(pMPEParams->qpe_fieldtype, "maxrmosaic") == 0)
        {
            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                {
                    QPEMosaic[i][j] = MPEFieldGen_MaxMosaic[i][j] ;
                }
            }
        }
        if(strcmp(pMPEParams->qpe_fieldtype, "p3lmosaic") == 0)
        {
            MPEFieldGen_set_best_estimate_p3();
        }
    }//end blnMosaic condition

    releaseRMosaicMemory(pGeoData) ;

    if(pRadarResult != NULL)
    {
        free(pRadarResult);
        pRadarResult = NULL;
    }

    first = 0 ;
}

void allocRMosaicMemory(const geo_data_struct * pGeoData)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    /**
     * allocate memory for MHeight variable
     **/
    MHeight = (double **)malloc(rowSize * sizeof(double *));
    if(MHeight == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runRMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MHeight[i] = (double *)malloc(colSize * sizeof(double));
        if(MHeight[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runRMosaic function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

    /**
     * allocate memory for temp ID (double data type) variable
     **/
    tempID = (double **)malloc(rowSize * sizeof(double *));
    if(tempID == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runRMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        tempID[i] = (double *)malloc(colSize * sizeof(double));
        if(tempID[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runRMosaic function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }

}

void releaseRMosaicMemory(const geo_data_struct * pGeoData)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;

    for(i = 0; i < rowSize; i++)
    {
        if(MHeight[i] != NULL)
        {
            free(MHeight[i]);
            MHeight[i] = NULL;
        }
    }
    if(MHeight != NULL)
    {
        free(MHeight);
        MHeight = NULL;
    }

    for(i = 0; i < rowSize; i++)
    {
        if(tempID[i] != NULL)
        {
            free(tempID[i]);
            tempID[i] = NULL;
        }
    }
    if(tempID != NULL)
    {
        free(tempID);
        tempID = NULL;
    }
}


/**********************************************************************
   compare_radar_id()

   Compare function used for the binary search of the id list
   done when checking if the location is a valid one.

   ********************************************************************/
static int compare_radar_id ( void * search_value,
                              void * array_value )
{
   /* declare and define the values for two variables mapped
      against the input arguments */
   char * pSearchValue = ( char * ) search_value ;
   radar_result_struct * pArrayValue = ( radar_result_struct * ) array_value ;

   /* return the result of the simple string comparison */
   return ( strcmp ( pSearchValue , pArrayValue->radID) ) ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
