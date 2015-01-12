#include "mpe_fieldgen.h"
#include "get_mpe_product_state.h"
#include "p3.h"
#include "CodeTimer.h"
#include "wr2perflog.h"

codetimer timer1;
codetimer timer2;

codetimer perflog_table_timer;

run_date_struct * ptrRunDate ;

geo_data_struct * ptrGeoData ;

mpe_params_struct * ptrMPEParams ;

gage_table_struct ** ptrGageTable;

gage_table_struct ** ptrGageTableP3;

/* A copy of the gage value array for the multisensor qc */
gage_table_struct ** ptrQCGageTable;

radarLoc_table_struct * ptrRadarLocTable;

char * MPEFieldGen_Mosaics[num_mosaics] = {"rmosaic", "avgrmosaic", "maxrmosaic",
                               "gageonly", "bmosaic","lmosaic",
                               "mmosaic", "mlmosaic", "satpre", "lsatpre", "p3lmosaic",
                               "srmosaic", "sgmosaic", "srgmosaic",
                               "qmosaic", "lqmosaic", "mlqmosaic",
                               "rdmosaic", "avgrdmosaic", "maxrdmosaic",
                               "bdmosaic", "ldmosaic", "mdmosaic", "mldmosaic",
                               "srdmosaic", "srdgmosaic", 
                               "rfcmosaic", "rfcbmosaic", "rfcmmosaic"};

double ** MPEFieldGen_BaseRMosaic = NULL;
double ** MPEFieldGen_BaseRDMosaic = NULL;


double ** MPEFieldGen_RMosaic = NULL ;
double ** MPEFieldGen_RDMosaic = NULL ;

double ** MPEFieldGen_QMosaic = NULL ;

double ** MPEFieldGen_BMosaic = NULL ;
double ** MPEFieldGen_BDMosaic = NULL ;

// Added by Bryon L on 5/16/2007.
//
double ** RfcBMosaic = NULL;

double ** MPEFieldGen_LMosaic = NULL ;
double ** MPEFieldGen_LDMosaic = NULL ;


double ** MPEFieldGen_LQMosaic = NULL ;

double ** SRMosaic = NULL ;
double ** SRDMosaic = NULL ;

// Added by Ram for the average and max mosaic calculation
// ------------
// structure to hold the max mosaic values throughout the program run
double  ** MPEFieldGen_MaxMosaic = NULL;
double  ** MPEFieldGen_MaxRDMosaic = NULL;


// structure to hold the average mosaic values
double  ** MPEFieldGen_AvgMosaic = NULL;
double  ** MPEFieldGen_AvgRDMosaic = NULL;

// structure to hold the number of radars contributed to a hrap grid bin
// this will be used to calculate the average mosaic.
int     ** MPEFieldGen_AvgMosaicNumRadars = NULL;
int     ** MPEFieldGen_AvgRDMosaicNumRadars = NULL;

// -----------
// variable for the p3 lmosaic calculation
double  ** MPEFieldGen_P3Mosaic = NULL;

int MPEFieldGen_p3_return_value = -1;
// --------------


int ** MPEFieldGen_ID = NULL ;
int ** MPEFieldGen_IDDP = NULL ;
int ** Q2ID = NULL ;

/*This two dimensional array hold the "best estimate" mosaic*/
double ** MPEFieldGen_QPEMosaic = NULL ;

 /* prism data */
double ** MPEFieldGen_umeang = NULL ;

/* The local bias satellite precipitation array. */
double ** MPEFieldGen_LSatpre = NULL ;

/* mean field bias array */
double * MPEFieldGen_meanFieldBias = NULL ;

extern void MPEFieldGen_free_tr();

int mpe_editor_call = 0;

void set_mpe_editor_call()
{
   mpe_editor_call = 1;
}

void unset_mpe_editor_call()
{
   mpe_editor_call = 0;
}

void main_mpe_fieldgen_for_calls_from_editor(int num_args, char ** args)
{
    enum DisplayFieldData radar_display_type = display_rMosaic;
    mosaicType indexMosaic;
    mosaicType indexBaseRadar = rmosaic;
    mosaicType indexBaseRadarDP = rdmosaic;

    int i, j, status;
    int blnMosaic[num_mosaics];
    int blnGetPrism = 0 ;
    time_t tmpTime, start_time, end_time;
    static char strTempTime[50] ;
    long irc;
    int flag , mosaicLength ;
    int radar_processed, radardp_processed;
    int mpe_base_radar_len = BESTFIELD_LEN;
    int overwrite_flag;
    int verbose = VERBOSE ;
    static char mpe_base_radar_mosaic [ BESTFIELD_LEN ] = {'\0'};
    static char mpe_base_radardp_mosaic [ BESTFIELD_LEN ] = {'\0'};
    struct tm * pRunTime = NULL ;
    char datetime[ANSI_YEARSEC_TIME_LEN + 1] ;
    int ioverwrt = 1 ; /* 0 = overwrite best xmrg; 1 = don't overwrite */

    short iug[GAGE_NUMBER], ivg[GAGE_NUMBER];
    float zg[GAGE_NUMBER];
    short iugd[GAGE_NUMBER], ivgd[GAGE_NUMBER];
    float zgd[GAGE_NUMBER];
    int gageSize, gageSizeDP; 
    int gageSizeP3;

    static int readradartri_error;

    int ret = 1;

    int var = 0;

    logFile = NULL;

    ptrRunDate = NULL;
    ptrGeoData = NULL;
    ptrMPEParams = NULL;
    ptrRadarLocTable = NULL ;
    ptrGageTable = NULL ;
    ptrGageTableP3 = NULL ;
    ptrQCGageTable = NULL ;

//    init_timer ( & timer1 );
//    init_timer ( & timer2 );
//    start_timer ( & timer1 );

    init_timer(&perflog_table_timer);
    start_timer(&perflog_table_timer);


    for(i = 0; i < GAGE_NUMBER; i ++)
    {
        iug[i] = 0 ;
        ivg[i] = 0 ;
         zg[i] = 0.0 ;
     
    /* Added by PaulT */
        iugd[i] = 0 ;
        ivgd[i] = 0 ;
         zgd[i] = 0.0 ;
    }


    time(&start_time);

    /********************************************************************
     * allocates memory for global struct data and initialization.
     **/


    constructor() ;

    /***************************************************
     * Verify the argument list
     * and parse the values into variable ptrRunDate.
     **/


    parseArgs(num_args, args, ptrRunDate) ;

    /************************
     * open the log file.
     **/


    mpe_fg_openLogFile(ptrRunDate->tRunTime, ptrRunDate->hourNum);

    time(&tmpTime);

    strftime(strTempTime, 50, "%Y-%m-%d %X %Z", gmtime(&tmpTime));
    sprintf ( message , "\t\tMPE Precip Processing -- %s\n", strTempTime) ;
    printMessage( message, logFile );

    sprintf ( message , "\t\tVersion ob9e -- May 01, 2012 (dual-pol)\n\n") ;
    printMessage( message, logFile );

    sprintf( message , "STATUS: Processing %d hour(s)\n",
             ptrRunDate->hourNum) ;
    printMessage( message, logFile );


    /*******************************************************
     * retrieve the value of the base radar product.
     **/
    get_mpe_base_radar ( &verbose, mpe_base_radar_mosaic,
                         &mpe_base_radar_len, &status );

    if ( status != 0 )
    {
       sprintf ( message, "Error occurred when retrieving base radar "
                          "mosaic.\n\tProgram Exit." );
       shutDownMPE ( message, logFile );
    }

    status = strcmp ( mpe_base_radar_mosaic, "RMOSAIC" );

    if ( status == 0 )
    {
       indexBaseRadar = rmosaic;
       radar_display_type = display_rMosaic;
    }
    else
    {
       status = strcmp ( mpe_base_radar_mosaic, "AVGRMOSAIC" );

       if ( status == 0 )
       {
          indexBaseRadar = avgrmosaic;
          radar_display_type = display_avgrMosaic;
       }
       else
       {
         status = strcmp ( mpe_base_radar_mosaic, "MAXRMOSAIC" );

         if ( status == 0 )
         {
            indexBaseRadar = maxrmosaic;
            radar_display_type = display_maxrMosaic;
         }
         else
         {
            sprintf ( message, "Error: Unrecognized base radar mosaic %s.\n"
                               "\tProgram Exit.", mpe_base_radar_mosaic );
            shutDownMPE ( message, logFile );
         }
       }
    }

    sprintf ( message, "STATUS:  Using %s as the base radar mosaic...",
              mpe_base_radar_mosaic );
    printMessage ( message, logFile );

    /*******************************************************
     * retrieve the value of the base radar product for rdmosaic.
     **/
    mpe_base_radar_len = BESTFIELD_LEN;
    get_mpe_base_radardp ( &verbose, mpe_base_radardp_mosaic, 
                         &mpe_base_radar_len, &status );

    if ( status != 0 )
    {
       sprintf ( message, "Error occurred when retrieving base radardp "
                          "mosaic.\n\tProgram Exit." );
       shutDownMPE ( message, logFile );
    }
   
    status = strcmp ( mpe_base_radardp_mosaic, "RDMOSAIC" );

    if ( status == 0 )
    {
       indexBaseRadarDP = rdmosaic;
       radar_display_type = display_rdMosaic;
    }
    else
    {
       status = strcmp ( mpe_base_radardp_mosaic, "AVGRDMOSAIC" ); 

       if ( status == 0 )
       {
          indexBaseRadarDP = avgrdmosaic;
          radar_display_type = display_avgrdMosaic;
       }
       else
       {
         status = strcmp ( mpe_base_radardp_mosaic, "MAXRDMOSAIC" );
 
         if ( status == 0 )
         {
            indexBaseRadarDP = maxrdmosaic;
            radar_display_type = display_maxrdMosaic;
         }
         else
         {
               sprintf ( message, "Error: Unrecognized base radardp mosaic %s.\n"
                                  "\tProgram Exit.", mpe_base_radardp_mosaic );
               shutDownMPE ( message, logFile );
          }
       }
    }

    sprintf ( message, "STATUS:  Using %s as the base radardp mosaic...",
              mpe_base_radardp_mosaic );
    printMessage ( message, logFile );

    /********************************************************
     * read status for qpe generate types.
     * and flag indicates if need calculate mean field bias.
     **/
    sprintf( message , "STATUS: loading qpe generate types...") ;
    printMessage( message, logFile );

    ptrMPEParams->blnMeanFieldBias = 0;
    ptrMPEParams->blnMeanFieldBiasDP = 0;

    for( indexMosaic = rmosaic; indexMosaic < num_mosaics; indexMosaic++)
    {
        blnMosaic[indexMosaic] = 0;
        mosaicLength = strlen(MPEFieldGen_Mosaics[indexMosaic]) ;
        get_mpe_product_state(MPEFieldGen_Mosaics[indexMosaic], &mosaicLength,
                    &verbose, &flag, &status);
        if(status != 0)
        {
            sprintf ( message , "Error(s) occur when set generate type: %s."
                    "\n\tProgram exit." , MPEFieldGen_Mosaics[indexMosaic]) ;
            shutDownMPE( message, logFile );
        }

        blnMosaic[indexMosaic] = flag ;

        if(blnMosaic[indexMosaic] == 1)
        {
            sprintf ( message , "\tgenerate type: \"%s\" is set \"ON\" ",
                MPEFieldGen_Mosaics[indexMosaic]) ;
            printMessage( message, logFile );
        }
        else
        {
            sprintf ( message , "\tgenerate type: \"%s\" is set \"OFF\" ",
                MPEFieldGen_Mosaics[indexMosaic]) ;
            printMessage( message, logFile );
        }

        /***************************************
         * Need calculate the mean field bias
         * when bmosiac or mmosaic is ON
         **/
        if((indexMosaic == bmosaic)
             && blnMosaic[indexMosaic] == 1)
            ptrMPEParams->blnMeanFieldBias = 1;

        if((indexMosaic == bdmosaic)
             && blnMosaic[indexMosaic] == 1)
            ptrMPEParams->blnMeanFieldBiasDP = 1;

        /***************************************
         * check if the prism data needs to be read in.
         * when mmosiac or mlmosaic or gageonly is ON
         **/
        if((indexMosaic == mmosaic ||
            indexMosaic == mlmosaic ||
            indexMosaic == mlqmosaic ||
            indexMosaic == mdmosaic ||
            indexMosaic == gaugeonly )
             && blnMosaic[indexMosaic] == 1)
            blnGetPrism = 1;
    }

    /*********************************************************
     * allocate memory and Initialize the struct data
     * which are determined by the run time data.
     **/
    constructorByRunTime() ;

    /***************************
     * read in geo grid data.
     **/
    MPEFieldGen_readGeoData(ptrGeoData) ;

    /***************************
     * read in the overlays
     **/
    MPEFieldGen_rfcw_load_static ( & status );

    if ( status != 0 )
    {
       sprintf ( message, "Error occurred when reading overlay data.\n" );
       printMessage ( message, logFile );
    }

     /*********************************************************
     * allocate memory and Initialize the struct data
     * which are determined by the geographic grid data.
     **/
    constructorByGeodata(blnMosaic) ;

    /*****************************************************
     * read in static parameters from Apps_defaults files.
     **/
    MPEFieldGen_readParams(ptrMPEParams) ;

    /***********************
     * Open the database.
     **/

    if(!mpe_editor_call)
    {
       if ( (status = OpenDbms ( ptrMPEParams->db_name )) != 0 )
       {
          sprintf ( message , "Error(s) occur during opening database %s."
                    " SQLCODE: %ld\n\tProgram exit.",
                    ptrMPEParams->db_name , SQLCODE ) ;
          shutDownMPE( message, logFile );
       }
    }

    /********************************************************************
     * read static parameters from database for running mpe_fieldgen.
     **/
    MPEFieldGen_readDBParams(ptrMPEParams) ;

    /**********************************
     * write static data to log file.
     **/
    MPEFieldGen_writeParams(ptrMPEParams) ;

    /************************************************************
     * read in gage data for entire area and whole time range.
     * print list of all gages available in this hour
     * remove multiple gages in the same HRAP bin (after print of list)
     **/


    MPEFieldGen_readGageData(ptrRunDate, ptrMPEParams, ptrGeoData,
        ptrGageTable, ptrGageTableP3, ptrQCGageTable);

    /**
     * read in radar identifiers, lat/lon and heights
     * store in radarloc struct variable
     **/
    MPEFieldGen_readRadarLoc(ptrRadarLocTable) ;
    /*********************************************************
     * allocate memory and Initialize the struct data
     * which are determined by the radarLoc number.
     **/
    constructorByRadarLoc(ptrRadarLocTable->radarNum) ;

    /**
     * for multiple hours case:
     * set the first run time
     * to the earliest time.
     **/
    ptrRunDate->tRunTime -= (ptrRunDate->hourNum - 1) * SECONDS_PER_HOUR;

    for( i=0; i < ptrRunDate->hourNum; i++)
    {
        radar_processed = 0;
        radardp_processed = 0;
        ptrMPEParams->sat_avail = 0;
	     ptrMPEParams->build_neighbor_list_SP = 0 ;
	     ptrMPEParams->build_neighbor_list_DP = 0 ;

		gageSize = ptrGageTable[i]->totalGageNum ;
	   gageSizeDP = gageSize ;
		gageSizeP3 = ptrGageTableP3[i]->totalGageNum ;

    	for(j = 0; j < gageSize; j ++)
    	{
        	iug[j] = ptrGageTable[i]->ptrGageRecords[j].hrap_x ;
        	ivg[j] = ptrGageTable[i]->ptrGageRecords[j].hrap_y ;
        	 zg[j] = ptrGageTable[i]->ptrGageRecords[j].gageValue ;

         /* Added by PaulT */
        	iugd[j] = ptrGageTable[i]->ptrGageRecords[j].hrap_x ;
        	ivgd[j] = ptrGageTable[i]->ptrGageRecords[j].hrap_y ;
        	 zgd[j] = ptrGageTable[i]->ptrGageRecords[j].gageValue ;
    	}


        pRunTime = gmtime(&(ptrRunDate->tRunTime));

        strftime ( datetime, ANSI_YEARSEC_TIME_LEN + 1,
                "%Y-%m-%d %H:00:00", pRunTime ) ;

        /************************************************************
         * read in prism data if required.
         **/
        if(blnGetPrism == 1)
        {
            MPEFieldGen_get_climate(ptrMPEParams->os , ptrGeoData->num_rows,
                ptrGeoData->num_cols, pRunTime->tm_mon, MPEFieldGen_umeang) ;
       }

        /**
        * for this hour, check the autosave flag in the rwresult table.
        * this will indicate to subsequent modules whether or not to
        * overwrite the best estimate qpe.
        **/
        int irfclen = strlen(ptrMPEParams->rfc_name);
        int idatelen = strlen(datetime);
        MPEFieldGen_check_autosave(ptrMPEParams->rfc_name, &irfclen,
            datetime, &idatelen, &ioverwrt) ;


	/********************************************************************
         * run mosaic functions based on mosaic status value.
         **/

        for( indexMosaic = rmosaic; indexMosaic < num_mosaics; indexMosaic++)
        {
            if(blnMosaic[indexMosaic] == 0)
                continue;

            switch(indexMosaic)
            {
                case avgrmosaic:
                case maxrmosaic:
                case avgrdmosaic:
                case maxrdmosaic:
                case rmosaic :
                    if ( radar_processed == 0 )
                    {

                        MPEFieldGen_runRMosaic(ptrRunDate,
                                   ptrGeoData,
                                   ptrMPEParams,
                                   ptrRadarLocTable,
                                   ptrGageTable[i],
                                   ptrGageTableP3[i],
                                   ptrQCGageTable[i],
                                   MPEFieldGen_meanFieldBias,
                                   MPEFieldGen_ID,
                                   MPEFieldGen_RMosaic,
                                   MPEFieldGen_QPEMosaic,
                                   blnMosaic) ;

                        /**
                          * Assign the base radar mosaic.  This radar mosaic
                          * will be used as the base for all of the radar
                          * derived MPE products.
                          **/
                        switch ( indexBaseRadar )
                        {
                           case rmosaic:
                              MPEFieldGen_BaseRMosaic = MPEFieldGen_RMosaic;
                              break;

                           case avgrmosaic:
                              MPEFieldGen_BaseRMosaic = MPEFieldGen_AvgMosaic;
                              break;

                           case maxrmosaic:
                              MPEFieldGen_BaseRMosaic = MPEFieldGen_MaxMosaic;
                              break;

                           default:
                              sprintf ( message, "Error: Unrecognized base "
                                                 "radar mosaic index.\n\t"
                                                 "Program Exit." );
                              shutDownMPE ( message, logFile );
                              break;
                        }

                        /**
                          * delete zero gage values where radar says that
                          * it is raining note that this will delete good
                          * gage values in ap and virga but most of the
                          * time will delete bad values
                          **/
                		if( ptrMPEParams->del_gage_zeros == 1 )
                		{

                    		MPEFieldGen_deleteZeros( &gageSize, iug, ivg, zg,
                                             MPEFieldGen_BaseRMosaic) ;

                		}


                    }

                    radar_processed = 1;
                    break ;

                case p3lmosaic :

/*   //read the radar triangles and the hrap grid bin radar mosaicked values.
   if (readradartriangles_once_for_all_runs != 1)
   {
      readradartri_error = 0;
      ret = readradartriangles ();
      if(ret == -1) readradartri_error = 1;
      readradartriangles_once_for_all_runs = 1;
   }
*/
//start_timer ( & timer2 );

                    MPEFieldGen_p3_return_value = MPEFieldGen_runP3LMosaic ( ptrRunDate,
                                                     ptrMPEParams,
                                                     ptrRadarLocTable,
                                                     ptrGageTableP3[i],
                                                     ptrGeoData,
                                                     radar_display_type,
                                                     MPEFieldGen_P3Mosaic,
                                                     MPEFieldGen_BaseRMosaic,
                                                     MPEFieldGen_QPEMosaic);

//stop_timer_and_print_elapsed_time ( & timer2, "Elapsed Time for p3lmosaic", stdout );

                    if(MPEFieldGen_p3_return_value == -1)
                    {
                        sprintf ( message , "Problem calculating P3 Mosaic - "
                                  " could be caused by no gage data or error reading input files\n");
                        printMessage( message, logFile );

                        getCurrentTime ( currTime );
                        sprintf ( message, "%s = time end P3LMOSAIC calc\n", currTime );
                        printMessage ( message, logFile );
                    }
                    break;

                case gaugeonly :

	                  ptrMPEParams->polarizationType = SinglePol ;
	                  MPEFieldGen_runGageonly(ptrRunDate,
                                 ptrGeoData,
                                 ptrMPEParams,
                                 gageSize, iug, ivg, zg,
                                 MPEFieldGen_umeang,
                                 MPEFieldGen_QPEMosaic) ;

                    break ;

                case qmosaic :
                     runQMosaic ( ptrRunDate,
                                  ptrGeoData,
                                  ptrMPEParams,
                                  MPEFieldGen_QMosaic,
                                  MPEFieldGen_QPEMosaic) ;
                     break;

                case bmosaic :


                    runBMosaic(ptrRunDate,
                               ptrGeoData,
                               ptrMPEParams,
                               MPEFieldGen_meanFieldBias,
                               MPEFieldGen_ID,
                               MPEFieldGen_BaseRMosaic,
                               MPEFieldGen_BMosaic,
                               MPEFieldGen_QPEMosaic) ;

                   break ;

                case lmosaic :

	                 ptrMPEParams->polarizationType = SinglePol ;
	                 MPEFieldGen_runLMosaic( ptrRunDate,
                                ptrGeoData,
                                ptrMPEParams,
                                gageSize, iug, ivg, zg,
                                MPEFieldGen_BaseRMosaic,
                                MPEFieldGen_LMosaic,
                                MPEFieldGen_QPEMosaic) ;


                    break ;

                case ldmosaic :
	                 ptrMPEParams->polarizationType = SinglePol ;
                    runLDMosaic( ptrRunDate, 
                                ptrGeoData, 
                                ptrMPEParams,
                                gageSizeDP, iugd, ivgd, zgd,
                                MPEFieldGen_BaseRDMosaic, 
                                MPEFieldGen_LDMosaic, 
                                MPEFieldGen_QPEMosaic) ;
                    break ;

                case lqmosaic :
	                ptrMPEParams->polarizationType = SinglePol ;
                   runLQMosaic( ptrRunDate,
                                ptrGeoData,
                                ptrMPEParams,
                                gageSize, iug, ivg, zg,
                                MPEFieldGen_QMosaic,
                                MPEFieldGen_LQMosaic,
                                MPEFieldGen_QPEMosaic) ;
                    break ;

                case mmosaic :

	                 ptrMPEParams->polarizationType = SinglePol ;
	                 MPEFieldGen_runMMosaic(ptrRunDate,
                               ptrGeoData,
                               ptrMPEParams,
                               gageSize, iug, ivg, zg,
                               MPEFieldGen_ID,
                               MPEFieldGen_BaseRMosaic ,
                               MPEFieldGen_BMosaic ,
                               MPEFieldGen_umeang ,
                               MPEFieldGen_QPEMosaic) ;

                   break ;

                case mlmosaic :
	                 ptrMPEParams->polarizationType = SinglePol ;
	                 MPEFieldGen_runMLMosaic( ptrRunDate,
                                 ptrGeoData,
                                 ptrMPEParams,
                                 gageSize, iug, ivg, zg,
                                 MPEFieldGen_ID,
                                 MPEFieldGen_BaseRMosaic ,
                                 MPEFieldGen_LMosaic ,
                                 MPEFieldGen_umeang ,
                                 MPEFieldGen_QPEMosaic) ;
                    break ;

                case mldmosaic :
	                 ptrMPEParams->polarizationType = DualPol ;
                    runMLDMosaic( ptrRunDate, 
                                 ptrGeoData, 
                                 ptrMPEParams,
                                 gageSizeDP, iugd, ivgd, zgd,
                                 MPEFieldGen_ID,
                                 MPEFieldGen_BaseRDMosaic ,
                                 MPEFieldGen_LDMosaic ,
                                 MPEFieldGen_umeang ,
                                 MPEFieldGen_QPEMosaic) ;
                    break ;

                case mlqmosaic :
	                 ptrMPEParams->polarizationType = SinglePol ;
                    runMLQMosaic( ptrRunDate,
                                  ptrGeoData,
                                  ptrMPEParams,
                                  gageSize, iug, ivg, zg,
                                  Q2ID,
                                  MPEFieldGen_QMosaic ,
                                  MPEFieldGen_LQMosaic ,
                                  MPEFieldGen_umeang ,
                                  MPEFieldGen_QPEMosaic) ;

                    break ;

                case satpre:

                    /* The Satellite Precipitation Field is not generated
                       by MPE Fieldgen. It is created from the D2D
                       AutoSpe product. It is saved as the QPE Mosaic if
                       the mpe_qpe_fieldtype token is set to it. */
                     runSatpre ( ptrRunDate,
                                 ptrGeoData,
                                 ptrMPEParams,
                                 MPEFieldGen_QPEMosaic );

                     break;

                case lsatpre :
                	sprintf( message , "STATUS: BEFORE calling MPEFieldGen_runLSatpre in main \n") ;
                	printMessage( message, logFile );

                	ptrMPEParams->polarizationType = SinglePol ;
                     MPEFieldGen_runLSatpre ( ptrRunDate,
                                  ptrGeoData,
                                  ptrMPEParams,
                                  gageSize, iug, ivg, zg,
                                  MPEFieldGen_BaseRMosaic,
                                  MPEFieldGen_LSatpre,
                                  MPEFieldGen_QPEMosaic );
                    break ;

                case srmosaic :
                    runSRMosaic( ptrRunDate,
                                 ptrGeoData,
                                 ptrMPEParams,
                                 MPEFieldGen_ID,
                                 MPEFieldGen_LSatpre,
                                 MPEFieldGen_LMosaic,
                                 MPEFieldGen_QPEMosaic) ;
                    break ;

                case sgmosaic :
                    /* call to deleteZero function (if requested) uses single pol radar */
	                 ptrMPEParams->polarizationType = SinglePol ;
                    runSGMosaic( ptrRunDate,
                                 ptrGeoData,
                                 ptrMPEParams,
                                 gageSize, iug, ivg, zg,
                                 MPEFieldGen_LSatpre ,
                                 MPEFieldGen_umeang ,
                                 MPEFieldGen_QPEMosaic) ;
                    break ;

                case srgmosaic :
	                 ptrMPEParams->polarizationType = SinglePol ;
                    runSRGMosaic( ptrRunDate,
                                  ptrGeoData,
                                  ptrMPEParams,
                                  gageSize, iug, ivg, zg,
                                  MPEFieldGen_umeang ,
                                  MPEFieldGen_QPEMosaic) ;
                    break ;

                case srdmosaic :
                    runSRDMosaic( ptrRunDate,
                                 ptrGeoData,
                                 ptrMPEParams,
                                 MPEFieldGen_IDDP,
                                 MPEFieldGen_LSatpre,
                                 MPEFieldGen_LDMosaic,
                                 MPEFieldGen_QPEMosaic) ;
                    break ;


                case srdgmosaic :
	            ptrMPEParams->polarizationType = DualPol ;

                    runSRDGMosaic( ptrRunDate,
                                  ptrGeoData,
                                  ptrMPEParams,
                                  gageSizeDP, iugd, ivgd, zgd,
                                  MPEFieldGen_umeang ,
                                  MPEFieldGen_QPEMosaic) ;
                    break ;

                case rdmosaic :
                    if ( radardp_processed == 0 )
                    {

                    	runRDMosaic(ptrRunDate,
                                   ptrGeoData, 
                                   ptrMPEParams,
                                   ptrRadarLocTable, 
                                   ptrGageTable[i], 
                                   ptrGageTableP3[i], 
                                   ptrQCGageTable[i],
                                   MPEFieldGen_meanFieldBias,
                                   MPEFieldGen_IDDP,
                                   MPEFieldGen_RDMosaic,
                                   MPEFieldGen_QPEMosaic,
                                   blnMosaic) ;

                    	/**
                          * Assign the base radar mosaic.  This radar mosaic
                          * will be used as the base for all of the radar
                          * derived DP MPE products.
                          **/
                        switch ( indexBaseRadarDP )
                        {
                           case rdmosaic:
                        	  MPEFieldGen_BaseRDMosaic = MPEFieldGen_RDMosaic;
                              break;

                           case avgrdmosaic:
                        	  MPEFieldGen_BaseRDMosaic = MPEFieldGen_AvgRDMosaic;
                              break;

                           case maxrdmosaic:
                        	  MPEFieldGen_BaseRDMosaic = MPEFieldGen_MaxRDMosaic;
                              break;

                           default:
                              sprintf ( message, "Error: Unrecognized base "
                                                 "radar dp mosaic index.\n\t"
                                                 "Program Exit." );
                              shutDownMPE ( message, logFile );
                              break;
                        }
                        
                        /**      
                          * delete zero gage values where radar says that 
                          * it is raining note that this will delete good 
                          * gage values in ap and virga but most of the 
                          * time will delete bad values
                          **/
                		if( ptrMPEParams->del_gage_zeros == 1 )
                    		deleteZeros( &gageSizeDP, iugd, ivgd, zgd, 
                    				MPEFieldGen_BaseRDMosaic) ;
                    }

                    radardp_processed = 1;  
                    break ;
                case bdmosaic :

                	runBDMosaic(ptrRunDate,
                               ptrGeoData, 
                               ptrMPEParams,
                               MPEFieldGen_meanFieldBias,
                               MPEFieldGen_IDDP,
                               MPEFieldGen_BaseRDMosaic,
                               MPEFieldGen_BDMosaic,
                               MPEFieldGen_QPEMosaic) ;

                   break ;

                case mdmosaic :
	                 ptrMPEParams->polarizationType = DualPol ;
                    runMDMosaic(ptrRunDate, 
                               ptrGeoData, 
                               ptrMPEParams,
                               gageSizeDP, iugd, ivgd, zgd,
                               MPEFieldGen_IDDP,
                               MPEFieldGen_BaseRDMosaic ,
                               MPEFieldGen_BDMosaic ,
                               MPEFieldGen_umeang ,
                               MPEFieldGen_QPEMosaic) ;
                    break ;

                case rfcmosaic :

                    /* The rfc mosaic is not generated by MPE Fieldgen.
                       It is created  by the gen areal qpe. Just save it as
                       the QPE Mosaic if the mpe_qpe_fieldtype token is set
                       to it. */
                    runRfcMosaic ( ptrRunDate,
                                   ptrGeoData,
                                   ptrMPEParams,
                                   MPEFieldGen_QPEMosaic) ;
                    break;

                case rfcbmosaic :

                    runRfcBMosaic(ptrRunDate,
                                  ptrGeoData,
                                  ptrMPEParams,
                                  ptrRadarLocTable,
                                  MPEFieldGen_meanFieldBias,
                                  MPEFieldGen_ID,
                                  MPEFieldGen_BaseRMosaic,
                                  RfcBMosaic,
                                  MPEFieldGen_QPEMosaic) ;
                    break;

                case rfcmmosaic :
	                 ptrMPEParams->polarizationType = SinglePol ;

                    /* note: BMOSAIC generated outside of this local MPE - may not jibe with iug,ivg,zg gage list */
                    runRfcMMosaic ( ptrRunDate,
                                    ptrGeoData,
                                    ptrMPEParams,
                                    gageSize,
                                    iug,
                                    ivg,
                                    zg,
                                    MPEFieldGen_ID,
                                    MPEFieldGen_BaseRMosaic,
                                    RfcBMosaic,
                                    MPEFieldGen_umeang,
                                    MPEFieldGen_QPEMosaic );
                    break ;

                default:
                    sprintf ( message , "ERROR: Unknown mosaic type!");
                    printMessage( message, logFile );
                    break;

            } /* end switch-case */

        } /* end for indexMosaic loop */

        /*************************************
         * Write out the "best estimate" data.
         * Be sure to apply any polygons drawn on this product.
         **/
        if( ioverwrt == 0)
            MPEFieldGen_writeQPE(ptrRunDate, ptrMPEParams, ptrGeoData ,
                ptrGageTable[i]->totalGageNum, ptrGageTableP3[i]->totalGageNum,MPEFieldGen_QPEMosaic ) ;
        else
        {
            sprintf( message , "STATUS: auto_save = F, can not write out best estimate data and image data.\n") ;
            printMessage( message, logFile );
        }

        /**
         * write record to RWResult table
         * return overwrite_flag flag
         *    = 0 -- no previous record OR previous record
         *            found with auto_save=T
         *        -- write xmrg file
         *    = 1 -- previous record found with auto_save=F
         *        -- do not write xmrg file
         *
         * update the number of radars available and the bias overwrite
         * flag in the rwresult table.
         **/
        MPEFieldGen_writeRWResult ( ptrMPEParams->rfc_name,
                        datetime,
                        ptrGageTable[i]->totalGageNum,
                        ptrMPEParams->sat_avail,
                        ptrMPEParams->radar_avail_num,
                        ptrMPEParams->qpe_fieldtype,
                        &overwrite_flag,
                        &irc);

        if(irc != 0)
        {
           overwrite_flag = 0 ;
           sprintf ( message , "Database error #%ld -- attempting to "
                               " write record to RWResult table.", irc ) ;
           printMessage( message, logFile );
        }

        if(overwrite_flag != 0)
        {
           sprintf ( message , "qpe/xmrg file not written due to "
                               "presence of manually saved version.");
           printMessage( message, logFile );
        }

        /******************************************************************
         * Modify the run time to one hour great than the current run time.
         **/
        ptrRunDate->tRunTime += SECONDS_PER_HOUR;

    } /* end for run hour loop */

    MPEFieldGen_free_tr();

   stop_timer(&perflog_table_timer);
   wr2perflog(&var, perflog_table_timer.elapsed_time, &var, &var);

   /*******************************************
     * releases memory for global struct data.
     **/
    //destructor() ;

    time(&end_time);

    sprintf ( message , "STATUS: Program exit normally"
            " with elapse time: %ld second(s)",
            (end_time - start_time));
    printMessage( message, logFile );

    /********************************************************************
     * close db connection and free memory of global variables.
     **/
    deleteLogFiles ( );

    if(!mpe_editor_call)
    {
       CloseDbms();
    }

    //stop_timer_and_print_elapsed_time ( & timer1, "Elapsed Time for mpe_fieldgen", stdout );

    return;

} /* end main_mpe_fieldgen_for_calls_from_editor */
