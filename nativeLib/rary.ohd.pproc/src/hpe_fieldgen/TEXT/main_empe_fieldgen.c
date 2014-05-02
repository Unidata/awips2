#include "empe_fieldgen.h"
#include "get_empe_product_state.h"
#include "p3.h"

run_date_struct * ptrRunDate ;

geo_data_struct * ptrGeoData ;

empe_params_struct * ptrEMPEParams ;

gage_table_struct ** ptrGageTable;

gage_table_struct ** ptrGageTableP3;

/*
 * A copy of the gage value array for the multisensor qc
 */

gage_table_struct ** ptrQCGageTable;

radarLoc_table_struct * ptrRadarLocTable;

char * Mosaics[num_mosaics] = {"dhrmosaic", "bdhrmosaic", "ermosaic",
                               "avgermosaic", "maxermosaic", "gageonly",
                               "ebmosaic","lmosaic", "mmosaic", "mlmosaic",
                               "lsatpre", "p3lmosaic"};

double ** BaseRMosaic = NULL;

double ** RMosaic = NULL ;

double ** DHRMosaic = NULL ;

double ** BMosaic = NULL ;

double ** LMosaic = NULL ;

/*
 * Added by Ram for the average and max mosaic calculation
 * structure to hold the max mosaic values throughout the program run
 */

double  ** MaxMosaic = NULL;

/*
 * structure to hold the average mosaic values
 */

double  ** AvgMosaic = NULL;

/*
 * structure to hold the number of radars contributed to a hrap grid bin
 * this will be used to calculate the average mosaic.
 */

int     ** AvgMosaicNumRadars = NULL;

/*
 * variable for the p3 lmosaic calculation
 */

double  ** P3Mosaic = NULL;

int p3_return_value = -1;

int readradartriangles_once_for_all_hours = 0;
int ** ID = NULL ;

/* This two dimensional array hold the "best estimate" mosaic */
double ** QPEMosaic = NULL ;

 /* prism data */

double ** umeang = NULL ;

 /* radar bean height data */

double ** RadarBeamHeight = NULL ;

short  ** radarMiscBins = NULL ;

/* The local bias satellite precipitation array. */

double ** LSatpre = NULL ;

/* mean field bias array */

double * meanFieldBias = NULL ;

extern void free_tr();

/* dual pol product switch, default is "on" */

int dualpol_on_flag = 1;
int dualpol_used = 0;

int hpe_fieldgen_main(int argc, const char ** argv)
{
    enum DisplayFieldData radar_display_type = display_erMosaic;
    mosaicType indexMosaic;
    mosaicType indexBaseRadar = ermosaic;

    int i, j, k, status;
    int blnMosaic[num_mosaics];
    int blnGetPrism = 0 ;
    time_t tmpTime, start_time, end_time;
    static char strTempTime[50] = {'\0'} ;
    int flag , mosaicLength ;
    int radar_processed;
    int empe_base_radar_len = BESTFIELD_LEN;
    int verbose = VERBOSE ;
    char cemr[3] = {'\0'} ;        /* char    month of run */
    struct tm * pRunTime = NULL ;
    char datetime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    int blnOverwrite = 1 ; /* 0 = overwrite best xmrg; 1 = don't overwrite */

    const char * HPE_DUALPOL_ON_TOKEN = "hpe_dualpol_on";
    char dualpol_on[TOKEN_LEN] = {'\0'};
    short * iug = NULL;
    short * ivg = NULL;
    float * zg = NULL;
    int gageSize = 0;
    int gageSizeP3 = 0;

    int isTopHour = 0;

    logFile = NULL;

    ptrRunDate = NULL;
    ptrGeoData = NULL;
    ptrEMPEParams = NULL;
    ptrRadarLocTable = NULL ;
    ptrGageTable = NULL ;
    ptrGageTableP3 = NULL ;
    ptrQCGageTable = NULL ;

    /* determin if dual pol product can be retrived */
    
    getAppsDefaults(HPE_DUALPOL_ON_TOKEN, dualpol_on);
    if (strcmp(toLowerCase(dualpol_on), "yes") != 0 )
      dualpol_on_flag = 0;
    else
      dualpol_on_flag = 1;  
            
    time(&start_time);    

    /*
     * allocates memory for global struct data and initialization.
     */

    hpe_fieldgen_constructor() ;

    /*
     * Verify the argument list
     * and parse the values into variable ptrRunDate.
     */

    hpe_fieldgen_parseArgs(argc, argv, ptrRunDate) ;

    /*
     * open the log file.
     */

    hpeOpenLogFile(ptrRunDate->tRunTime);

    time(&tmpTime);
    strftime(strTempTime, 50, "%Y-%m-%d %X %Z", gmtime(&tmpTime));
    sprintf ( message , "\n\n\tHPE Precip Processing -- %s\n", strTempTime) ;
    printLogMessage(message);
    sprintf ( message , "\t\tVersion OB14.3.1 -- March 08, 2014 \n") ;
    printMessage( message, logFile );

/*
    sprintf( message , "STATUS: HPE Fieldgen is running for %d hour(s)\n",
             ptrRunDate->hourNum) ;
    printMessage( message);
*/

    /*
     * retrieve the value of the base radar product.
     */

    get_empe_base_radar ( &verbose, ptrEMPEParams->base_radar_mosaic,
                         &empe_base_radar_len, &status );

    if ( status != 0 )
    {
        sprintf ( message, "Error occurred when retrieving base radar "
                           "mosaic.\n\tProgram Exit." );
        shutdown( message );
    }

    hpe_fieldgen_toLowerCase(ptrEMPEParams->base_radar_mosaic);

    if ( strcmp ( ptrEMPEParams->base_radar_mosaic, "ermosaic" ) == 0 )
    {
        indexBaseRadar = ermosaic;
        radar_display_type = display_erMosaic;
    }
    else if ( strcmp ( ptrEMPEParams->base_radar_mosaic, "avgermosaic" ) == 0 )
    {
        indexBaseRadar = avgermosaic;
        radar_display_type = display_avgerMosaic;
    }
    else if ( strcmp ( ptrEMPEParams->base_radar_mosaic, "maxermosaic" ) == 0 )
    {
        indexBaseRadar = maxermosaic;
        radar_display_type = display_maxerMosaic;
    }
    else
    {
        sprintf ( message, "Error: Unrecognized base radar mosaic %s.\n"
                         "\tProgram Exit.",
                         ptrEMPEParams->base_radar_mosaic );
        shutdown( message );
    }

    sprintf ( message, "STATUS: Using %s as the base radar mosaic...",
              ptrEMPEParams->base_radar_mosaic );
    hpe_fieldgen_printMessage( message );

    /*
     * read status for qpe generate types.
     * and flag indicates if need calculate mean field bias.
     */

    sprintf( message , "STATUS: loading qpe generate types...") ;
    hpe_fieldgen_printMessage( message);

    ptrEMPEParams->blnMeanFieldBias = 0;
    ptrEMPEParams->blnDHRMeanFieldBias = 0;

    for( indexMosaic = dhrmosaic; indexMosaic < num_mosaics; indexMosaic++)
    {
        blnMosaic[indexMosaic] = 0;
        mosaicLength = strlen(Mosaics[indexMosaic]) ;
        get_empe_product_state(Mosaics[indexMosaic], &mosaicLength,
                    &verbose, &flag, &status);
        if(status != 0)
        {
            sprintf ( message , "Error(s) occur when set generate type: %s."
                    "\n\tProgram exit." , Mosaics[indexMosaic]) ;
            shutdown( message );
        }

        blnMosaic[indexMosaic] = flag ;

        if(blnMosaic[indexMosaic] == 1)
        {
            sprintf ( message , "\tgenerate type: \"%s\" is set \"ON\" ",
                Mosaics[indexMosaic]) ;
            hpe_fieldgen_printMessage( message);
        }
        else
        {
            sprintf ( message , "\tgenerate type: \"%s\" is set \"OFF\" ",
                Mosaics[indexMosaic]) ;
            hpe_fieldgen_printMessage( message);
        }

        /*
         * Need calculate the mean field bias
         * when ebmosaic or mmosaic is ON
         */

        if((indexMosaic == ebmosaic)
             && blnMosaic[indexMosaic] == 1)
        {
            ptrEMPEParams->blnMeanFieldBias = 1;
        }

        /*
         * Need compute the dhrmosaic and bdhrmosaic
         * when bdhrmosaic is ON
         */

        if((indexMosaic == bdhrmosaic)
             && blnMosaic[indexMosaic] == 1)
        {
            blnMosaic[dhrmosaic] = 1;
            ptrEMPEParams->blnDHRMeanFieldBias = 1;
        }

        /*
         * check if the prism data needs to be read in.
         * when mmosiac or mlmosaic or gageonly is ON
         */

        if((indexMosaic == mmosaic ||
            indexMosaic == mlmosaic ||
            indexMosaic == gaugeonly )
             && blnMosaic[indexMosaic] == 1)
            blnGetPrism = 1;
    }

    /*
     * allocate memory and Initialize the struct data
     * which are determined by the run time data.
     */

    hpe_fieldgen_constructorByRunTime() ;

    /*
     * read in static parameters from Apps_defaults files.
     */

    readParams(ptrEMPEParams) ;

    /*
     * read in geo grid data.
     */

    readGeoData(ptrEMPEParams->hrap_grid_factor, ptrGeoData) ;

    /*
     * read in the overlays
     */

    rfcw_load_static (ptrEMPEParams->hrap_grid_factor, & status );

    if ( status != 0 )
    {
       sprintf ( message, "Error occurred when reading overlay data.\n" );
       hpe_fieldgen_printMessage( message );
    }
    ptrEMPEParams->irc_load_stat = status;

    /*
     * allocate memory and Initialize the struct data
     * which are determined by the geographic grid data.
     */

    hpe_fieldgen_constructorByGeodata(blnMosaic) ;

    /*
     * Open the database.
     */

    if ( (status = OpenDbms ( ptrEMPEParams->db_name )) != 0 )
    {
        sprintf ( message , "Error(s) occur during opening database %s."
                    " SQLCODE: %ld\n\tProgram exit.",
                    ptrEMPEParams->db_name , SQLCODE ) ;
        shutdown( message );
    }

    /*
     * read static parameters from database for running mpe_fieldgen.
     */

    readDBParams(ptrEMPEParams) ;

    /*
     * write static data to log file.
     */

    writeParams(ptrEMPEParams) ;

    /*
     * read in gage data for entire area and whole time range
     * only when the DSP duration is 60 minutes and
     * the run time is the top hour.
     */

    pRunTime = gmtime(&(ptrRunDate->tRunTime));

    if( (ptrEMPEParams->dsp_duration == 60) &&
        (pRunTime->tm_min == 0) )
    {
        isTopHour = 1;
    }

    if(isTopHour == 1)
    {
        readGageData(ptrRunDate, ptrEMPEParams, ptrGeoData,
            ptrGageTable, ptrGageTableP3, ptrQCGageTable);
    }

    /*
     * read in radar identifiers, lat/lon and heights
     * store in radarloc struct variable
     */

    readRadarLoc(ptrRadarLocTable) ;

    /*
     * allocate memory and Initialize the mean bias struct data
     * which is determined by the radarLoc number.
     */

    constructorForMeanBias(ptrRadarLocTable->radarNum) ;

    /*
     * build the category name based on dsp_duration
     */

    buildCategoryName(ptrEMPEParams->dsp_duration, ptrEMPEParams->category_name);

    /*
     * for multiple hours case:
     * set the first run time
     * to the earliest time.
     */

    ptrRunDate->tRunTime -= (ptrRunDate->hourNum - 1) * SECONDS_PER_HOUR;

    /*
     * load the radar bean height array
     */

    loadRadarBeamHeight(RadarBeamHeight,
                        NUM_DPA_ROWS * ptrEMPEParams->hrap_grid_factor,
                        NUM_DPA_COLS * ptrEMPEParams->hrap_grid_factor);

    for( i = 0; i < ptrRunDate->hourNum; i++)
    {
        radar_processed = 0;
        ptrEMPEParams->sat_avail = 0;
        ptrEMPEParams->build_neighbor_list = 0 ;

        gageSize = ptrGageTable[i]->totalGageNum ;
        gageSizeP3 = ptrGageTableP3[i]->totalGageNum ;

        if( isTopHour == 1 )
        {
            iug = init1DShortArray(ZERO_CONSTANT, gageSize);
            ivg = init1DShortArray(ZERO_CONSTANT, gageSize);
             zg = init1DFloatArray(MOSAIC_DEFAULT, gageSize);

            for(j = 0; j < gageSize; j ++)
            {
                iug[j] = ptrGageTable[i]->ptrGageRecords[j].hrap_x ;
                ivg[j] = ptrGageTable[i]->ptrGageRecords[j].hrap_y ;
                 zg[j] = ptrGageTable[i]->ptrGageRecords[j].gageValue ;
            }
        }

        pRunTime = gmtime(&(ptrRunDate->tRunTime));
        sprintf(cemr, "%02d", (pRunTime->tm_mon + 1));

        strftime ( datetime, ANSI_YEARSEC_TIME_LEN + 1,
                "%Y-%m-%d %H:%M:00", pRunTime ) ;

        hpe_fieldgen_getCurrentTime(currTime) ;
        sprintf( message , "\n%s = time begin HPE fieldgen MOSAIC generation for: %s." ,
                        currTime, datetime) ;
        hpe_fieldgen_printMessage( message);

        /*
         * read in prism data if required.
         */

        if(blnGetPrism == 1)
        {
            get_climate(ptrEMPEParams->os , ptrGeoData->num_rows,
                ptrGeoData->num_cols, cemr, umeang) ;
        }

        /*
         * for this hour, check the autosave flag in the rwresult table.
         * this will indicate to subsequent modules whether or not to
         * overwrite the best estimate qpe.
         */

        int irfclen = strlen(ptrEMPEParams->rfc_name);
        int idatelen = strlen(datetime);
        check_autosave(ptrEMPEParams->rfc_name, &irfclen,
            datetime, &idatelen, &blnOverwrite) ;

        /*
         * load the mean bias data for each radar ID
         * if need compute ebmosaic and/or bdhrmosaic.
         */

/*        if( (ptrEMPEParams->blnDHRMeanFieldBias == 1) ||
            (ptrEMPEParams->blnMeanFieldBias == 1) )*/
	
	/*if (ptrEMPEParams->blnDHRMeanFieldBias == 1)    	    
        {
            readMeanBias(ptrRunDate,
                         ptrRadarLocTable,
                         ptrEMPEParams,
                         meanFieldBias ); 
			 	 
        }*/

        /*
         * run mosaic functions based on mosaic status value.
         *
         * there are 4 products that are fully tested and delivered for ob83:
         * dhrmosaic, bdhrmosaic, ermosaic and ebmosaic
         *
         * and there are 5 more products are developed but not fully tested
         * and therefore not delivered in ob83.
         * The function calls are commented out in ob83.
         * -- gzhou 09/2007
         */

        for( indexMosaic = dhrmosaic; indexMosaic < num_mosaics; indexMosaic++)
        {
            if(blnMosaic[indexMosaic] == 0)
            {
                continue;
            }

            switch(indexMosaic)
            {
                case dhrmosaic :
                    runDHRMosaic(ptrRunDate,
                                 ptrGeoData,
                                 ptrEMPEParams,
                                 ptrRadarLocTable,
				                 meanFieldBias,
                                 RadarBeamHeight, 
                                 ID, 
                                 DHRMosaic, 
                                 QPEMosaic) ;
                    break ;

                case bdhrmosaic :
                    runBDHRMosaic(ptrRunDate,
                                  ptrGeoData,
                                  ptrEMPEParams,
                                  meanFieldBias,
                                  ID,
                                  DHRMosaic,
                                  QPEMosaic) ;
                    break ;

                case avgermosaic:
                case maxermosaic:
                case ermosaic :
                    if ( radar_processed == 0 )
                    {
                        runERMosaic(ptrRunDate,
                                   ptrGeoData,
                                   ptrEMPEParams,
                                   ptrRadarLocTable,
                                   ptrGageTable[i],
                                   ptrGageTableP3[i],
                                   ptrQCGageTable[i],
                                   meanFieldBias,
                                   RadarBeamHeight,
                                   ID,
                                   RMosaic,
                                   QPEMosaic,
                                   blnMosaic) ;

                        /*
                         * Assign the base radar mosaic.  This radar mosaic
                         * will be used as the base for all of the radar
                         * derived MPE products.
                         */

                        switch ( indexBaseRadar )
                        {
                           case ermosaic:
                              BaseRMosaic = RMosaic;
                              break;

                           case avgermosaic:
                              BaseRMosaic = AvgMosaic;
                              break;

                           case maxermosaic:
                              BaseRMosaic = MaxMosaic;
                              break;

                           default:
                              sprintf ( message, "Error: Unrecognized base "
                                                 "radar mosaic index.\n\t"
                                                 "Program Exit." );
                              shutdown ( message );
                              break;
                        }

                        /*
                         * delete zero gage values where radar says that
                         * it is raining note that this will delete good
                         * gage values in ap and virga but most of the
                         * time will delete bad values
                         */

                        if( ptrEMPEParams->del_gage_zeros == 1 )
                        {
                            deleteZeros( &gageSize, iug, ivg, zg,
                                             BaseRMosaic) ;
                        }
                    }

                    radar_processed = 1;
                    break ;

                case ebmosaic :
                    runEBMosaic(ptrRunDate,
                               ptrGeoData,
                               ptrEMPEParams,
                               meanFieldBias,
                               ID,
                               BaseRMosaic,
                               BMosaic,
                               QPEMosaic) ;
                   break ;

                case p3lmosaic :
                    sprintf ( message , "STATUS: P3LMosaic product is "
                                        "not available for current version.");
                    hpe_fieldgen_printMessage( message);

/*
                    p3_return_value = runP3LMosaic ( ptrRunDate,
                                                     ptrEMPEParams,
                                                     ptrRadarLocTable,
                                                     ptrGageTableP3[i],
                                                     ptrGeoData,
                                                     radar_display_type,
                                                     P3Mosaic,
                                                     BaseRMosaic,
                                                     QPEMosaic);

                    if(p3_return_value == -1)
                    {
                        sprintf ( message , "oops!...Problem calculating P3"
                                  " Mosaic..."
                                  " seems like there is no gage data\n");
                        printMessage( message);
                    }
                    readradartriangles_once_for_all_hours = 1;
*/
                    break;

                case gaugeonly :

                    /*
                     * The gageonly can be calculated
                     * only when the DSP duration is 60 minutes and
                     * the run time is the top of the hour.
                     */

                    sprintf ( message , "STATUS: Gageonly product is "
                                        "not available for current version.");
                    hpe_fieldgen_printMessage( message);
/*
                    if( isTopHour == 1 )
                    {
                         runGageonly(ptrRunDate,
                                     ptrGeoData,
                                     ptrEMPEParams,
                                     gageSize, iug, ivg, zg,
                                     umeang,
                                     QPEMosaic) ;
                    }
                    else
                    {
                        sprintf ( message , "STATUS: Gageonly could not be "
                                            "calculated due to "
                                            "it is at non-top hour or "
                                            "DSP duration is not 60 minutes.");
                        printMessage( message);
                    }
*/
                    break ;

                case lmosaic :

                    /*
                     * The lmosaic can be calculated
                     * only when the DSP duration is 60 minutes and
                     * the run time is the top of the hour.
                     */

                    sprintf ( message , "STATUS: LMosaic product is "
                                        "not available for current version.");
                    hpe_fieldgen_printMessage( message);
/*
                    if( isTopHour == 1 )
                    {
                        runLMosaic( ptrRunDate,
                                    ptrGeoData,
                                    ptrEMPEParams,
                                    gageSize, iug, ivg, zg,
                                    BaseRMosaic,
                                    LMosaic,
                                    QPEMosaic) ;
                    }
                    else
                    {
                        sprintf ( message , "STATUS: LMosaic could not be "
                                            "calculated due to "
                                            "it is at non-top hour or "
                                            "DSP duration is not 60 minutes.");
                        printMessage( message);
                    }
*/
                    break ;

                case mmosaic :

                    /*
                     * The mmosaic can be calculated
                     * only when the DSP duration is 60 minutes and
                     * the run time is the top of the hour.
                     */

                    sprintf ( message , "STATUS: MMosaic product is "
                                        "not available for current version.");
                    hpe_fieldgen_printMessage( message);
/*
                    if( isTopHour == 1 )
                    {
                        runMMosaic(ptrRunDate,
                                   ptrGeoData,
                                   ptrEMPEParams,
                                   gageSize, iug, ivg, zg,
                                   ID,
                                   BaseRMosaic ,
                                   BMosaic ,
                                   umeang ,
                                   QPEMosaic) ;
                    }
                    else
                    {
                        sprintf ( message , "STATUS: MMosaic could not be "
                                            "calculated due to "
                                            "it is at non-top hour or "
                                            "DSP duration is not 60 minutes.");
                        printMessage( message);
                    }
*/
                    break ;

                case mlmosaic :

                    /*
                     * The mlmosaic can be calculated
                     * only when the DSP duration is 60 minutes and
                     * the run time is the top of the hour.
                     */

                    sprintf ( message , "STATUS: MLMosaic product is "
                                        "not available for current version.");
                    hpe_fieldgen_printMessage( message);
/*
                    if( isTopHour == 1 )
                    {
                        runMLMosaic( ptrRunDate,
                                     ptrGeoData,
                                     ptrEMPEParams,
                                     gageSize, iug, ivg, zg,
                                     ID,
                                     BaseRMosaic ,
                                     LMosaic ,
                                     umeang ,
                                     QPEMosaic) ;
                    }
                    else
                    {
                        sprintf ( message , "STATUS: MLMosaic could not be "
                                            "calculated due to "
                                            "it is at non-top hour or "
                                            "DSP duration is not 60 minutes.");
                        printMessage( message);
                    }
*/
                    break ;

                case lsatpre :

                    /*
                     * The lsatpre can be calculated
                     * only when the DSP duration is 60 minutes and
                     * the run time is the top of the hour.
                     */

                    sprintf ( message , "STATUS: LSatpre product is "
                                        "not available for current version.");
                    hpe_fieldgen_printMessage( message);
/*
                    if( isTopHour == 1 )
                    {
                        runLSatpre ( ptrRunDate,
                                     ptrGeoData,
                                     ptrEMPEParams,
                                     gageSize, iug, ivg, zg,
                                     BaseRMosaic,
                                     LSatpre,
                                     QPEMosaic );
                    }
                    else
                    {
                        sprintf ( message , "STATUS: LSatpre could not be "
                                            "calculated due to "
                                            "it is at non-top hour or "
                                            "DSP duration is not 60 minutes.");
                        printMessage( message);
                    }
*/
                    break ;

                default:
                    sprintf ( message , "ERROR: Unknown mosaic type!");
                    hpe_fieldgen_printMessage( message);
            }
        }

        /*
         * Write out the "best estimate" data.
         * Be sure to apply any polygons drawn on this product.
         *
         * need not write out qpe product for empe
         *   -- gzhou 05-2007
         */

        blnOverwrite = 1 ;
        if( blnOverwrite == 0)
        {
            writeQPE(ptrRunDate, ptrEMPEParams, ptrGeoData ,
                ptrGageTable[i]->totalGageNum,
                ptrGageTableP3[i]->totalGageNum, QPEMosaic ) ;
        }

        hpe_fieldgen_getCurrentTime(currTime) ;
        sprintf( message , "%s = time end HPE Fieldgen MOSAIC generation for: %s.\n" ,
                        currTime, datetime) ;
        hpe_fieldgen_printMessage( message);

        if( isTopHour == 1 )
        {
            free1DShortArray( iug );
            free1DShortArray( ivg );
            free1DFloatArray( zg );
        }

        /*
         * Modify the run time to one hour greater than the current run time.
         */

        ptrRunDate->tRunTime += SECONDS_PER_HOUR;
    }

    free_tr();
    free_mem();

    /*
     * releases memory for global struct data.
     */

    hpe_fieldgen_destructor() ;

    time(&end_time);

    sprintf ( message , "STATUS: Program exit normally"
            " with elapse time: %ld second(s)",
            (end_time - start_time));
    hpe_fieldgen_printMessage( message);

    /*
     * close db connection and free memory of global variables.
     */

    hpeDeleteLogFile ( );
    CloseDbms();

    exit(0);

}
