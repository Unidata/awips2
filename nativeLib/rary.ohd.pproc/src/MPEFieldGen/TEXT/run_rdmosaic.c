/*******************************************************************************
* FILENAME:  run_rdmosaic.c
*
* Purpose:
* This function computes the radar only mosaic field using dual-pol radar products
* (where available)
*
* calling function: main_mpe_fieldgen
* functions called: readRadarLoc, readRadarResult ,
*                readDPRadarData, readMisc, getMeanBiasDP, 
*                writeDAARadarResult, createRDMosaic, 
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
* meanFieldBias - MFB value used for each radar in the BDMOSAIC field calculation
*               - this is a combination of SP and DP values
*               - array is initially defined for the RMosaic field generation
*               - if radar has DAA product, then use DAA MFB value
*
* RDMosaic - the mosaic of radars.
*
* QPEMosaic - the mosaic of radars for best estimate.
*
*********************************************************************************/

#include "mpe_fieldgen.h"
#include "mpe_field_names.h"
#include "BinarySearch.h"


double ** MHeight ;
extern short  ** MPEFieldGen_radarMiscBins ;

// Added by Ram for the average and max mosaic calculation
// ---------------------------
extern double  ** MPEFieldGen_MaxRDMosaic;
extern double  ** MPEFieldGen_AvgRDMosaic;
extern int     ** MPEFieldGen_AvgRDMosaicNumRadars;
extern double  ** MPEFieldGen_P3Mosaic;
// --------------------------

extern void set_best_estimate_p3();

extern void unset_best_estimate_p3();


float radar [ NUM_DPA_COLS] [ NUM_DPA_COLS ] ;

void allocMHeightMemory(const geo_data_struct * pGeoData) ;
void releaseMHeightMemory(const geo_data_struct * pGeoData) ;

static int compare_radar_id ( void * search_value,
                              void * array_value ) ;

void runRDMosaic(const run_date_struct * pRunDate,
                const geo_data_struct * pGeoData,
                mpe_params_struct * pMPEParams,
                const radarLoc_table_struct * pRadarLocTable ,
                const gage_table_struct * pGageTable,
                const gage_table_struct * pGageTableP3,
                const gage_table_struct * pQCGageTable,
                double * meanFieldBias,
                int    ** IDDP,
                double ** RDMosaic,
                double ** QPEMosaic,
                int    *  blnMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;
    const int replace_missing = 0 ;

    static char datetime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    static char dateYMD[YYYYMMDDHH_LEN + 1] = {'\0'} ; 
    static char radarID[ RADAR_ID_LEN + 1] = {'\0'} ;
    register int  i = 0, ii, j, k, l; 
    int  gageRadarPairNum ;    /* number of positive gage/radar pairs */

    long int    irc ;

    int radarAvailFlag ;
    short daa_avail_flag ;
    int blnEditBias ;
    int blnIgnoreDPARadar, blnIgnoreDAARadar, ignoreFlag ;
    register int year, month, day, hour;

    double editBiasValue = 0.0 ;
    double memSpanBias ;
    double meanBias = 1.0 ;
    
    static char fileName[PATH_LEN] = {'\0'} ;
  //  static char heightDir[PATH_LEN] = {'\0'} ;
    static char indexDir[PATH_LEN] = {'\0'} ;
    static char mosaicDir[PATH_LEN] = {'\0'} ;
    static char avgRDMosaicDir[PATH_LEN] = {'\0'} ;
    static char maxRDMosaicDir[PATH_LEN] = {'\0'} ;

    static int first = 1 ;
    struct tm * pRunTime = NULL;

    float radarMFB [NUM_DPA_ROWS ][ NUM_DPA_COLS ];

    pMPEParams->radar_avail_num = 0 ;

    // Additional check added by Ram to see if any of the Radar mosaics are 
    // to be calculated. if not we abort here
    // ---------------------

    if(blnMosaic[rdmosaic] == 0 && blnMosaic[avgrdmosaic] == 0 && blnMosaic[maxrdmosaic] == 0)
    {
        sprintf ( message , "\nErroneous call to runRDMosaic\n");
        printMessage( message, logFile );
        return;
    }
    // ---------------------


    /**      
     * Allocates memory for MHeight array
     **/
    allocMHeightMemory(pGeoData) ;

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
    
    
    for(k = 0; k < rowSize; k ++)
    {
        for(j = 0; j < colSize; j ++)
        {
            RDMosaic[k][j] = MOSAIC_DEFAULT ;
            MPEFieldGen_AvgRDMosaic[k][j] = MOSAIC_DEFAULT ;
            MPEFieldGen_MaxRDMosaic[k][j] = MOSAIC_DEFAULT ;
            MPEFieldGen_AvgRDMosaicNumRadars[k][j] = 0 ;
            MHeight[k][j] = MOSAIC_DEFAULT ;
            IDDP[k][j] = 0 ;
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
    sprintf( message , "%s = time begin RDMOSAIC calculation." , 
                    currTime) ;
    printMessage( message, logFile );        

    radar_result_struct * pRadarResult = NULL;
    short radar_count ;

    pRadarResult = (radar_result_struct *) 
        malloc(pRadarLocTable->radarNum * sizeof(radar_result_struct)); 
    if(pRadarResult == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runRDMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }

    for(i = 0; i < pRadarLocTable->radarNum; i++ )
    {
        memset(&(pRadarResult[i].radID), '\0', RADAR_ID_LEN + 1) ;
        pRadarResult[i].edit_bias = 0;
        pRadarResult[i].ignore_dpa_radar = 0;
        pRadarResult[i].ignore_daa_radar = 0;
        pRadarResult[i].bias = 0.0 ;
    }

    /**      
     * Read the edit bias flag, edited bias, ignore radar flag
     * from DAARadarResult table 
     **/
    MPEFieldGen_readDAARadarResult (datetime, pRadarResult, &radar_count, &irc) ;
    if (irc < 0)
    {
        sprintf ( message , "ERROR: Database error #%ld attempting to "
            "select record from DAARadarResult table.", irc) ;
        printMessage( message, logFile );        
    }

    sprintf ( message , "\n******* RDMOSAIC radars *******");
    printMessage( message, logFile );        

    /**      
     * begin loop on radars
     **/
    for(i = 0; i < pRadarLocTable->radarNum; i++ )
    {
        strcpy(radarID, pRadarLocTable->ptrRadarLocRecords[i].radarID) ;

        sprintf ( message , "\n******* radar: %s *******", radarID);
        printMessage( message, logFile );        

        /**      
         * initialize edit bias flag, ignore radar flag
         **/
        blnEditBias    = 0 ;
        blnIgnoreDPARadar = 0 ;
        blnIgnoreDAARadar = 0 ;
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
            blnIgnoreDAARadar = pRadarInfo->ignore_daa_radar;
        }

        /*-----------------------------------------------------------------------------*/
        /*  read ignore_radar flag for the DPA radar product from RWRadarResult table  */
        /*  this is needed by the RDMosaic field generation                            */
        /*-----------------------------------------------------------------------------*/

        readIgnoreDPARadar(radarID, datetime, &ignoreFlag, &irc);
        if(irc == 0) 
        {
           blnIgnoreDPARadar = ignoreFlag; 
        }

        /**      
         * Read in gridded radar data (dual-pol or single-pol)
         * an "ignored" radar is considered "not available"
         **/
        readRDMosaicRadars(radarID, datetime,
            pMPEParams->dpa_wind, pMPEParams->daa_wind, pMPEParams->daa_min_coverage_dur, 
            blnIgnoreDPARadar, blnIgnoreDAARadar, radar, &radarAvailFlag, &daa_avail_flag) ;


        if(radarAvailFlag > 0)
            pMPEParams->radar_avail_num ++ ;

         /*--------------------------------------------------------------------------------*/
         /* calculate mean field bias for DP radars if required by fields in generate list */
         /* note that some radars in the RDMOSAIC field may be SP radars                   */
         /*--------------------------------------------------------------------------------*/
        if(pMPEParams->blnMeanFieldBiasDP == 1)
        {

           if(daa_avail_flag == 0)
           {
              /*---------------------------------------------------*/
              /* DAA product not available                         */
              /* use all missing field for MFB calculation         */
              /* if DAA product is NOT available but DPA product is*/
              /*  available, then must reset radarAvailFlag to 0   */
              /*---------------------------------------------------*/

              radarAvailFlag = 0;

              for( k = 0; k < NUM_DPA_COLS; k++)
              {

                 for( j = 0; j < NUM_DPA_COLS; j++)
                 {
                    radarMFB[k][j] = RADAR_DEFAULT ;
                 }

              }
           
           }
           else
           {

              /*---------------------------------------------------*/
              /* DAA product available                             */
              /*---------------------------------------------------*/
              for( k = 0; k < NUM_DPA_COLS; k++)
              {

                 for( j = 0; j < NUM_DPA_COLS; j++)
                 {
                    radarMFB[k][j] = radar[k][j];
                 }

              }
           }

           getMeanBiasDP(&(pRadarLocTable->ptrRadarLocRecords[i]),
                   datetime, ( void * ) MPEFieldGen_radarMiscBins [i], radarMFB,
                   pGeoData, pGageTable, pMPEParams,
                   &meanBias, &memSpanBias, &gageRadarPairNum ) ;

           /*----------------------------------------------------------------------------*/
           /*  meanFieldBias array is initially defined for the RMosaic field generation */
           /*  if radar has DAA product, then substitute the DAA MFB value               */
           /*----------------------------------------------------------------------------*/
           if(daa_avail_flag == 1)
           {
              meanFieldBias[i] = meanBias ;

              /* if blnEditBias = 1, then use edited bias value */
              if(blnEditBias == 1)
              {
                 meanFieldBias[i] = editBiasValue ;
                 sprintf ( message , "Edited MFB value used.");
                 printMessage( message, logFile );        
              }
           }


           sprintf ( message , "MFB value used = %4.2f", meanFieldBias[i]);
           printMessage( message, logFile );        

           /* following statement preserves the edited MFB value for case of radar ignored */

           if(blnEditBias == 1) meanBias = editBiasValue ;

           writeDAARadarResult(radarID, datetime, gageRadarPairNum, 
               radarAvailFlag, meanBias, memSpanBias, &irc) ;

           if(irc != 0)
           {
               sprintf ( message , "Database error #%ld attempting to "
                           "write record to DAARadarResult table.\n"
                           "Program exit", irc);
               shutDownMPE( message, logFile );
           }

        } /* if(pMPEParams->blnMeanFieldBiasDP == 1) */

        /**
         * mosaicking algorithm
         **/

        createRDMosaic(&(pRadarLocTable->ptrRadarLocRecords[i]),
                    radar, (void * )MPEFieldGen_radarMiscBins[i], i+1, pGeoData,
                    RDMosaic, MHeight, IDDP, 
                    MPEFieldGen_MaxRDMosaic, MPEFieldGen_AvgRDMosaic, MPEFieldGen_AvgRDMosaicNumRadars,blnMosaic ) ;

    }
    /* end loop on radars  */

    getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end RDMOSAIC calculation." , 
                    currTime) ;
    printMessage( message, logFile );        
    
    
    
    // at this time we have the sum for each hrap grid bin from the create_mosaic.c
    // file for the AvgMosaic array. also for the corresponding grid bins we have
    // the number of radars contributing to it in the AvgRDMosaicNumRadar array.
    // so in the following code we divide each gridbin value by the num radars to
    // get the final average mosaics.
    // we already have the final product in the MaxMosaic array at this time.
    // AvgRDMosaicNumRadars is the sum of SP and DP radars
    // ---------------------------
    if(blnMosaic[avgrdmosaic] == 1)
    {
        for(k=0;k<rowSize;k++)
        {
            for(l=0;l<colSize;l++)
            {
                if(MPEFieldGen_AvgRDMosaicNumRadars[k][l] > 0)
                {
                    if(MPEFieldGen_AvgRDMosaic[k][l] != MOSAIC_DEFAULT)
                    {
                        MPEFieldGen_AvgRDMosaic[k][l] /= MPEFieldGen_AvgRDMosaicNumRadars[k][l];
                    }
                    else
                    {
                        sprintf(message, "grid cell calculation error for cell [%d][%d] = %lf in avgrdmosaic calc (1)\n",
                                k,l,MPEFieldGen_AvgRDMosaic[k][l]);
                        printMessage(message,logFile);
                    }
                }
                else
                {
                    if(MPEFieldGen_AvgRDMosaic[k][l] != MOSAIC_DEFAULT)
                    {
                        sprintf(message, "grid cell calculation error for cell [%d][%d] = %lf in avgrdmosaic calc (2)\n",
                                k,l,MPEFieldGen_AvgRDMosaic[k][l]);
                        printMessage(message,logFile);
                    }
                }
            }
        }
        // ----------------------------
        // write these arrays to xmrg files 
        if(first == 1)
        {
            if(getAppsDefaults("mpe_avgrdmosaic_dir", avgRDMosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"mpe_avgrdmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }

        sprintf(fileName, "AVGRDMOSAIC%sz", dateYMD );

        MPEFieldGen_writeArray( pGeoData, avgRDMosaicDir, fileName,
                    FACTOR_PRECIP, replace_missing,
                    pMPEParams->user, pRunDate->tRunTime,
                    PROC_FLAG, MPEFieldGen_AvgRDMosaic, &irc ) ;

        // Apply edit polygons to the avgrdmosaic product for
        // use in furture products.
        apply_mpe_polygons ( MPEFieldGen_AvgRDMosaic,
                             dateYMD,
                             year,
                             month,
                             day,
                             hour,
                             display_avgrdMosaic,
                             pGeoData,
                             FACTOR_PRECIP,
                             0,
		             0); 

    }//end avgrdmosaic calculation
    
    if(blnMosaic[maxrdmosaic] == 1)
    {
        if(first == 1)
        {
            if(getAppsDefaults("mpe_maxrdmosaic_dir", maxRDMosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"mpe_maxrdmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }

        sprintf(fileName, "MAXRDMOSAIC%sz", dateYMD );
        MPEFieldGen_writeArray(pGeoData, maxRDMosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, MPEFieldGen_MaxRDMosaic, &irc) ;

        // Apply edit polygons to the maxrmosaic product for
        // use in furture products.

        apply_mpe_polygons ( MPEFieldGen_MaxRDMosaic,
                             dateYMD,
                             year,
                             month,
                             day,
                             hour,
                             display_maxrdMosaic,
                             pGeoData,
                             FACTOR_PRECIP,
                             0,
		             0	); 
          
    }// end of maxrdmosaic calculation
    // -------------------------------------------------

    /**
     * Gage QC - Point Check using Radar Mosaic data.
     **/

/*
    if(blnMosaic[rdmosaic] == 1)
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
                checkMultisensorQC(datetime , RDMosaic ,
                    pGeoData, pQCGageTable) ;
            }
            getCurrentTime(currTime) ;
            sprintf( message , "%s = time end Gage QC multisensor check." , 
                            currTime) ;
            printMessage( message, logFile );
        }
    }

*/
    /**
     * write out gridded data in xmrg format to flat file
     **/
    if(blnMosaic[rdmosaic] == 1)
    {

        getCurrentTime(currTime) ;
        sprintf( message, "%s = time begin writing RDMOSAIC fields to flat files." , 
                        currTime) ;
        printMessage( message, logFile );

        if(first == 1)
        {
            if(getAppsDefaults("mpe_rdmosaic_dir", mosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"mpe_rdmosaic_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }

        sprintf(fileName, "RDMOSAIC%sz", dateYMD ); 
        MPEFieldGen_writeArray(pGeoData, mosaicDir, fileName, FACTOR_PRECIP, 
                   replace_missing, pMPEParams->user, pRunDate->tRunTime, 
                   PROC_FLAG, RDMosaic, &irc) ;

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

            /* Apply edit polygons to the rdmosaic product for
               use in future products. */
            apply_mpe_polygons ( RDMosaic,
                                 dateYMD,
                                 year,
                                 month,
                                 day,
                                 hour,
                                 display_rdMosaic,
                                 pGeoData,
                                 FACTOR_PRECIP,
                                 0,
			         0 ); 
/*

        if(first == 1)
        {
            if(getAppsDefaults("rfcwide_daaheight_dir", heightDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"rfcwide_daaheight_dir\"."
                    "\n\tProgram exit.") ;
                shutDownMPE( message, logFile );
            }
        }

        sprintf(fileName, "DAAHEIGHT%sz", dateYMD ); 
        MPEFieldGen_writeArray(pGeoData, heightDir, fileName, FACTOR_OTHER, 
                   replace_missing, pMPEParams->user, pRunDate->tRunTime,
                   PROC_FLAG, MheightDP, &irc) ;

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

        getCurrentTime(currTime) ;
        sprintf( message, "%s = time   end writing RDMOSAIC fields to flat files." , 
                        currTime) ;
        printMessage( message, logFile );
*/

        /**      
         * fill in the "best estimate" mosaic
         * if the qpe_fieldtype is rmosaic.
         **/
        if(strcmp(pMPEParams->qpe_fieldtype, "rdmosaic") == 0)
        {
            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                {
                    QPEMosaic[i][j] = RDMosaic[i][j] ;
                }
            }
        }
        if(strcmp(pMPEParams->qpe_fieldtype, "avgrdmosaic") == 0)
        {
            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                {
                    QPEMosaic[i][j] = MPEFieldGen_AvgRDMosaic[i][j] ;
                }
            }
        }
        if(strcmp(pMPEParams->qpe_fieldtype, "maxrdmosaic") == 0)
        {
            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                {
                    QPEMosaic[i][j] = MPEFieldGen_MaxRDMosaic[i][j] ;
                }
            }
        }
        if(strcmp(pMPEParams->qpe_fieldtype, "p3lmosaic") == 0)
        {
            set_best_estimate_p3();    
        }
    }    

    releaseMHeightMemory(pGeoData) ;

    if(pRadarResult != NULL)
    {
        free(pRadarResult);
        pRadarResult = NULL;
    }

    first = 0 ;
}

void allocMHeightMemory(const geo_data_struct * pGeoData)
{
    int    i ;
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;
    
    //allocate memory for MHeight variable
    MHeight = (double **)malloc(rowSize * sizeof(double *)); 
    if(MHeight == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runRDMosaic function."
            "\n\tProgram exit.") ;
        shutDownMPE( message, logFile );
    }
    for(i = 0; i < rowSize; i++)
    {
        MHeight[i] = (double *)malloc(colSize * sizeof(double)); 
        if(MHeight[i] == NULL)
        {
            sprintf ( message , "ERROR: memory allocation failure"
                " in runRDMosaic function."
                "\n\tProgram exit.") ;
            shutDownMPE( message, logFile );
        }
    }    

}

void releaseMHeightMemory(const geo_data_struct * pGeoData) 
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
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc_lib/src/MPEFieldGen/RCS/run_rdmosaic.c,v $";
 static char rcs_id2[] = "$Id: run_rdmosaic.c,v 1.11 2012/09/13 18:33:59 pst Exp $";}
/*  ===================================================  */
}
