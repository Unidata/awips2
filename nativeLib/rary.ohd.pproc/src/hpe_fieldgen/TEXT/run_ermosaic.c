/***********************************************************************
* Filename: run_ermosaic.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: 08/21/2006
*
* Development Group: HSEB/OHD
*
* Description:
* compute the raw DSP radar mosaic data
* 
* Modules:
* runERMosaic
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: runERMosaic
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: 08/21/2006
* 
* Description:
* This function computes the raw DSP radar mosaic data.
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
* pRadarLocRecord Input     const radarLoc_table_struct *
*                                            info from radarLoc table
* meanFieldBias Output      double **        the mean field bias value
*                                            computed for each radar.
*
* ID           Output       int **           the mosaic id array of radars.
*
* RMosaic      Output       double **        the raw dsp mosaic.
* QPEMosaic    Output       double **        the best estimate mosaic 
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* readRadarLoc, readRadarResult , readMisc, getMeanBias, 
* writeRadarResult, createMosaic, checkMultisensorQC, writeArray
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
* Date        Developer         Action
*             C. R. Kondragunta Original FORTRAN code  
* March 2005  Guoxian Zhou      convert to C Language 
* 9/06/2006   Guoxian Zhou      Modify for empe dsp mosaic 
* 12/2012     Jingtao Deng      Modify for adding dual pol producat DSA
*                               first try to find the DSA product from dsaradar
*                               table and calculate the 1 hour precipitation. if
*                               not able to get DSA product, then use DSP product
***********************************************************************/

extern short  ** radarMiscBins ;

static double ** MHeight = NULL ;
static double ** tempID = NULL ;
static float  ** origRadar = NULL;
static float  ** currRadar = NULL ;
static short  ** hrapMiscBins = NULL ;
static short  ** currMiscBins = NULL ;


// Added by Ram for the average and max mosaic calculation
// ---------------------------
extern double  ** MaxMosaic;
extern double  ** AvgMosaic;
extern int     ** AvgMosaicNumRadars;
extern double  ** P3Mosaic;
// --------------------------


extern int dualpol_used;
extern int dualpol_on_flag;
extern void set_best_estimate_p3();

static void initMosaicArray(const geo_data_struct * pGeoData,
                            const int grid_rows,
                            const int grid_cols) ;

static void freeMosaicArray(const geo_data_struct * pGeoData,
                            const int grid_rows) ;

void runERMosaic(const run_date_struct * pRunDate,
                const geo_data_struct * pGeoData,
                empe_params_struct * pEMPEParams,
                const radarLoc_table_struct * pRadarLocTable ,
                const gage_table_struct * pGageTable,
                const gage_table_struct * pGageTableP3,
                const gage_table_struct * pQCGageTable,
                double * meanFieldBias,
                double ** RadarBeamHeight,
                int    ** ID,
                double ** RMosaic,
                double ** QPEMosaic,
                int    *  blnMosaic)
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;
    const int replace_missing = 0 ;

    const int GRID_ROWS = NUM_DPA_ROWS * pEMPEParams->hrap_grid_factor ;
    const int GRID_COLS = NUM_DPA_COLS * pEMPEParams->hrap_grid_factor ;

    const char * MOSAIC_DIR_TOKEN  = "hpe_ermosaic_dir";
    const char * AVG_ERMOSAIC_DIR_TOKEN  = "hpe_avg_ermosaic_dir";
    const char * MAX_ERMOSAIC_DIR_TOKEN  = "hpe_max_ermosaic_dir";

    const char * SAVE_HEIGHT_TOKEN = "hpe_save_dspheight";
    const char * HEIGHT_DIR_TOKEN = "hpe_dspheight_dir";
    const char * SAVE_INDEX_TOKEN  = "hpe_save_dspindex";
    const char * INDEX_DIR_TOKEN  = "hpe_dspindex_dir";

    const char * SAVE_GRIB_TOKEN = "ermosaic_save_grib";

    const char * SAVE_NETCDF_TOKEN = "ermosaic_save_netcdf";
    const char * NETCDF_DIR_TOKEN = "ermosaic_netcdf_dir";
    const char * NETCDF_ID_TOKEN = "ermosaic_netcdf_id";

    const char * SAVE_GIF_TOKEN = "ermosaic_save_gif";
    const char * GIF_DIR_TOKEN = "ermosaic_gif_dir";
    const char * GIF_ID_TOKEN = "ermosaic_gif_id";

    const char * SAVE_JPEG_TOKEN = "ermosaic_save_jpeg";

    double ** pMosaic = NULL;

    char saveflag[TOKEN_LEN]= {'\0'};

    static char datetime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    static char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ; 
    static char prdDateTime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    static char datehour[ANSI_YEARSEC_TIME_LEN + 1]  = {'\0'} ;
    static char radarID[ RADAR_ID_LEN + 1] = {'\0'} ;
    register int  i = 0, j, k, l; 
    int  gageRadarPairNum = 0 ;    /* number of positive gage/radar pairs */

    long int    irc ;

    int radarAvailFlag = 0;
    int dp_blnEditBias, sp_blnEditBias ;
    int dp_blnIgnoreRadar, sp_blnIgnoreRadar ;
    register int year, month, day, hour;

    double dp_editBiasValue = 0.0, sp_editBiasValue= 0.0 ;
    double memSpanBias ;
    double meanBias = 0.0 ;

    static char fileName[PATH_LEN] = {'\0'} ;
    static char ermosaic_fileName[PRESET_DESCR_LEN] = {'\0'} ;
    static char heightDir[PATH_LEN] = {'\0'} ;
    static char indexDir[PATH_LEN] = {'\0'} ;
    static char mosaicDir[PATH_LEN] = {'\0'} ;
    static char avgRMosaicDir[PATH_LEN] = {'\0'} ;
    static char maxRMosaicDir[PATH_LEN] = {'\0'} ;

    static int first = 1 ;
    struct tm * pRunTime = NULL;

    short pps_radar_count, dp_radar_count ;
    int index ;
        
    int  dualpol_meanbias_flag[MAX_RADAR_NUM];
    int  sp_meanbias_flag[MAX_RADAR_NUM];
    int  dualpol_data_avail[MAX_RADAR_NUM];
    
    /* initialize no dualpol MFB and no dual pol product avialble for each radar */
    for (i = 0; i < MAX_RADAR_NUM; i++)
    {
       dualpol_meanbias_flag[i] = 0;
       sp_meanbias_flag[i] = 0;
       dualpol_data_avail[i] = 0;
    }
    
    pEMPEParams->radar_avail_num = 0 ;

    // Additional check added by Ram to see if any of the Radar mosaics are 
    // to be calculated. if not we abort here
    // ---------------------

    if(blnMosaic[ermosaic] == 0 &&
       blnMosaic[avgermosaic] == 0 &&
       blnMosaic[maxermosaic] == 0)
    {
        sprintf ( message , "\nErroneous call to ERMosaic\n");
        printLogMessage( message );
        return;
    }

    /*      
     * Allocates memory for data arrays
     * based on the geo grid size data.
     */

    initMosaicArray(pGeoData, GRID_ROWS, GRID_COLS) ;

    /*      
     * strDateTime string should be in format: yyyymmddHHMM
     */

    pRunTime = gmtime(&(pRunDate->tRunTime)) ;
    strftime ( datetime, ANSI_YEARSEC_TIME_LEN + 1,
        "%Y-%m-%d %H:%M:00", pRunTime ) ;
    strftime ( datehour, ANSI_YEARSEC_TIME_LEN + 1,
        "%Y-%m-%d %H:00:00", pRunTime ) ;
    strftime(strDateTime, ANSI_YEARSEC_TIME_LEN + 1,
        "%Y%m%d%H%M", pRunTime);
    strftime(prdDateTime, ANSI_YEARSEC_TIME_LEN + 1,
            "%Y-%m-%d %H:%M:%S", pRunTime);

    year = pRunTime->tm_year + 1900;
    month = pRunTime->tm_mon + 1;
    day = pRunTime->tm_mday;
    hour = pRunTime->tm_hour;

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time begin ERMOSAIC calculation." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message );

    sprintf ( message , "STATUS: In runERMosaic - Dual pol product flag is %d.", dualpol_on_flag) ;
    printLogMessage( message );

    /* for single pol product which use rwradarresult table */
    radar_result_struct * pRadarResult = NULL;

    pRadarResult = (radar_result_struct *) 
        malloc(pRadarLocTable->radarNum * sizeof(radar_result_struct)); 
    if(pRadarResult == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runRMosaic function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }
    
    /* for dual pol proudcts which use daaradarresult table */    
    radar_result_struct * pDAARadarResult = NULL;
    
    pDAARadarResult = (radar_result_struct *) 
        malloc(pRadarLocTable->radarNum * sizeof(radar_result_struct)); 
    if(pDAARadarResult == NULL)
    {
        sprintf ( message , "ERROR: memory allocation failure"
            " in runRMosaic function."
            "\n\tProgram exit.") ;
        shutdown( message );
    }

    for(i = 0; i < pRadarLocTable->radarNum; i++ )
    {
        memset(pRadarResult[i].radID, '\0', RADAR_ID_LEN + 1) ;
        pRadarResult[i].edit_bias = 0;
        pRadarResult[i].ignore_radar = 0;
        pRadarResult[i].bias = 0.0 ;
	
	    memset(pDAARadarResult[i].radID, '\0', RADAR_ID_LEN + 1) ;
        pDAARadarResult[i].edit_bias = 0;
        pDAARadarResult[i].ignore_radar = 0;
        pDAARadarResult[i].bias = 0.0 ;
    }

    /*      
     * Read the edit bias flag, edited bias, ignore radar flag
     * from RWRadarResult table for the top hour
     */

       /* when dualpol_on_flag is set as zero, no any dual pol products will be used even
      though they are available. Only single pol radar producats are used. This keeps the old
      functionality */
      if (dualpol_on_flag == 0)
      {
        readRadarResult (datehour, pRadarResult, &pps_radar_count, sp_meanbias_flag, &irc) ; /* for single pol */
        if (irc < 0)
        {
           sprintf ( message , "ERROR: In runERMosaic - Database error #%ld attempting to "
                    "select record from RWRadarResult table.", irc) ;
           printLogMessage( message );
        }
	
      }
      /* Able to use dual pol products if available for this radar, if not available for this radar, use the single
         pol product */ 
      else
      {
          readDAARadarResult (datehour, pDAARadarResult, &dp_radar_count, dualpol_meanbias_flag, &irc) ; /* for dual pol */
    
          if (irc < 0 || dp_radar_count <= 0)
          {
              if (irc < 0)
	          {
                  sprintf ( message , "ERROR: In runERMosaic - Database error #%ld attempting to "
                            "select record from DAARadarResult table. Try RWRadarResult table.", irc) ;
                  hpe_fieldgen_printMessage( message );        
              }
	
	          else if (dp_radar_count <= 0)
              {
                  sprintf ( message , "STATUS: in RunERMosaic - No data found for radar "
                            "from DAARadarResult table,  then select data from RWRadarResult table.") ;
                  hpe_fieldgen_printMessage( message );
              }
          } 		
          readRadarResult (datehour, pRadarResult, &pps_radar_count, sp_meanbias_flag, &irc) ; /* for single pol */
          if (irc < 0)
          {
              sprintf ( message , "ERROR: in RunERMosaic -  Database error #%ld attempting to "
                       "select record from RWRadarResult table.", irc) ;
              printLogMessage( message );        
          }
      
    }    
    /*
     * Read the misbin information if the token hpe_load_misbin is ON,
     * otherwise the misbin will be default value(1).
     * Do this only once per EMPE_fieldgen run.
     */

    if ( radarMiscBins == NULL )
    {
        radarMiscBins = init2DShortArray(MISBIN_DEFAULT,
                          pRadarLocTable->radarNum,
                          NUM_DPA_ELEMENTS );

        if(pEMPEParams->load_misbin == 1)
        {
            readMisc ( pRadarLocTable, radarMiscBins ) ;
        }
    }
    
    /*      
     * begin loop on radars
     */

    for(i = 0; i < pRadarLocTable->radarNum; i++ )
    {
        meanFieldBias[i] = 0.0 ;

        strcpy(radarID, pRadarLocTable->ptrRadarLocRecords[i].radarID) ;

        sprintf ( message , "\n******* radar: %s *******", radarID);
        hpe_fieldgen_printMessage( message );

        /*
         * initialize edit bias flag, ignore radar flag
         */

        dp_blnEditBias    = 0 ;
        dp_blnIgnoreRadar = 0 ;
        dp_editBiasValue  = 0.0 ;
	    sp_blnEditBias    = 0 ;
        sp_blnIgnoreRadar = 0 ;
        sp_editBiasValue  = 0.0 ;

        radar_result_struct * dp_pRadarInfo = NULL ;
	    radar_result_struct * sp_pRadarInfo = NULL ;

        /* Try dual-pol first from DAARadarResult table first, not available, then use 
           single-pol RWRadarResult table */
	   
        if (dp_radar_count > 0)
        {
	        dp_pRadarInfo = (radar_result_struct *)
                         binary_search ( pDAARadarResult, radarID, dp_radar_count,
                       sizeof ( radar_result_struct), compare_radar_id );
   	        if ( dp_pRadarInfo != NULL )
            {
               dp_editBiasValue  = dp_pRadarInfo->bias;
               dp_blnEditBias    = dp_pRadarInfo->edit_bias;
               dp_blnIgnoreRadar = dp_pRadarInfo->ignore_radar;
            }
        }

	    sp_pRadarInfo = (radar_result_struct *)
                      binary_search ( pRadarResult, radarID, pps_radar_count,
                      sizeof ( radar_result_struct), compare_radar_id );
        if ( sp_pRadarInfo != NULL )
        {
           sp_editBiasValue  = sp_pRadarInfo->bias;
           sp_blnEditBias    = sp_pRadarInfo->edit_bias;
           sp_blnIgnoreRadar = sp_pRadarInfo->ignore_radar;
        }	      

        /*
         * Read in gridded radar data
         * count number of available radars
         * an "ignored" radar is considered "not available"
         */
	 
	 /* read the DSA gridded data first, if not available, then use DSP data */	 	
	 if (dualpol_on_flag == 1)
	 {
            readDSARadar(radarID, datetime, pEMPEParams->dsp_window,
                      pEMPEParams->dsp_duration, dp_blnIgnoreRadar,
                      origRadar, &radarAvailFlag);

            if (radarAvailFlag > 0)
	        {
	           sprintf ( message , "STATUS: In runERMosaic - dual pol product DSA is used for radar %s", radarID);
               hpe_fieldgen_printMessage( message );
	           dualpol_data_avail[i] = 1;
	     
	         }
	         else
	         {    		    
                 readDSPRadar(radarID, datetime, pEMPEParams->dsp_window,
                              pEMPEParams->dsp_duration, sp_blnIgnoreRadar,
                              origRadar, &radarAvailFlag);	     

	             dualpol_data_avail[i] = 0;
	      
	             if (radarAvailFlag > 0)
	             {
	                sprintf ( message , "STATUS: In runERMosaic - single pol product DSP is used for radar %s", radarID);
                    hpe_fieldgen_printMessage( message );
	              }
	         } 
     }
	 else        
     {
         readDSPRadar(radarID, datetime, pEMPEParams->dsp_window,
                     pEMPEParams->dsp_duration, sp_blnIgnoreRadar,
                     origRadar, &radarAvailFlag);
            
	     dualpol_data_avail[i] = 0;
	    
         if (radarAvailFlag > 0)
         {
	         sprintf ( message , "STATUS: In runERMosaic - single pol product DSP is used for radar %s", radarID);
             hpe_fieldgen_printMessage( message );
	     }
	}  
	  	    

        if(radarAvailFlag > 0)
        {
            pEMPEParams->radar_avail_num ++ ;
        }
         sprintf ( message , "STATUS: In runERMosaic - the dualpol_data_avail flag is %d for radarID %s", dualpol_data_avail[i], radarID);
         hpe_fieldgen_printMessage( message );

        /*
         * Convert radar array to current hrap grid
         */

        convertFloatArray(NUM_DSP_ROWS,
                          NUM_DSP_COLS,
                          origRadar,
                          RADAR_DEFAULT,
                          GRID_ROWS,
                          GRID_COLS,
                          currRadar );

        /*
         * pick up the misbin for current running radar and
         * convert it to current hrap grid
         */

        for(j = 0; j < NUM_DPA_ROWS; j ++)
        {
            for(k = 0; k < NUM_DPA_COLS; k ++)
            {
                index = j * NUM_DPA_COLS + k;
                hrapMiscBins[j][k] = radarMiscBins[i][index];                
            }
        }

        convertShortArray(NUM_DPA_ROWS,
                          NUM_DPA_COLS,
                          hrapMiscBins,
                          MISBIN_DEFAULT,
                          GRID_ROWS,
                          GRID_COLS,
                          currMiscBins );

        /*      
         * get mean field bias if it is set to "ON"
         */

        if(pEMPEParams->blnMeanFieldBias == 1)
        {

            /*      
             * load the mean field bias value 
             * and save it to meanFieldBias[i]
             * use DAABiasDyn table to load mean field bias for dual pol product, if not available
	         * use RWBiasDyn table to load mean field bias for PPS product 
             */

           getMeanBias(&(pRadarLocTable->ptrRadarLocRecords[i]),
                    datetime, GRID_ROWS, GRID_COLS,
                    currMiscBins, currRadar,
                    pGeoData, pGageTable, pEMPEParams, dualpol_data_avail[i],
                    &meanBias, &memSpanBias, &gageRadarPairNum ) ;

           meanFieldBias[i] = meanBias ;

            /*
             * if blnEditBias = 1, then use edited bias value
             */

            if(dualpol_data_avail[i] == 1)
	        {
               if(dp_blnEditBias == 1)
               {
                  meanFieldBias[i] = dp_editBiasValue ;
		          sprintf ( message , "STATUS: in runERMosaic - Edited bias value = %4.2f used.",
                            dp_editBiasValue);
                  hpe_fieldgen_printMessage( message );  
               }
           }
	       else
	       {
	           if(sp_blnEditBias == 1)
               {
                  meanFieldBias[i] = sp_editBiasValue ;
		          sprintf ( message , "STATUS: in runERMosaic - Edited bias value = %4.2f used.",
                            sp_editBiasValue);
                  hpe_fieldgen_printMessage( message );  
               }	   
	       }
          
           
        }   /*end of ptrEMPEParams->blnMeanFieldBias == 1 */
		   
	    sprintf( message , "STATUS: In runERMosaic - MFB is %f for radid %s." , 
                 meanFieldBias[i], radarID) ;
        hpe_fieldgen_printMessage( message );
	    	    
        /*
         * mosaicking algorithm
         */

        createMosaic(&(pRadarLocTable->ptrRadarLocRecords[i]),
                    GRID_ROWS, GRID_COLS, 
                    currRadar, currMiscBins, pGeoData, i+1,
                    RadarBeamHeight, RMosaic, MHeight, ID, 
                    MaxMosaic, AvgMosaic, AvgMosaicNumRadars, blnMosaic );
    }
    /* end loop on radars  */

    hpe_fieldgen_getCurrentTime(currTime) ;
    sprintf( message , "%s = time   end ERMOSAIC calculation." , 
                    currTime) ;
    hpe_fieldgen_printMessage( message );

    /*
     * Added by Ram for the average and max ermosaic calculations
     * 
     * at this time we have the sum for each quarter hrap grid bin from
     * the create_ermosaic.c file for the AvgERMosaic array.
     * also for the corresponding grid bins we havethe number of
     * radars contributing to it in the AvgERMosaicNumRadar array.
     * so in the following code we divide each gridbin value by 
     * the num radars to get the final average mosaics.
     * we already have the final product in the MaxERMosaic array
     * at this time.
     */

    if(blnMosaic[avgermosaic] == 1)
    {
        for(k=0;k<rowSize;k++)
        {
            for(l=0;l<colSize;l++)
            {
                if(AvgMosaicNumRadars[k][l] > 0)
                {
                    if(AvgMosaic[k][l] >= 0.0)
                    {
                        AvgMosaic[k][l] /= AvgMosaicNumRadars[k][l];
                    }
                    else
                    {
                        sprintf(message, "grid cell calculation error"
                               " for cell [%d][%d] = %lf in the avg mosaic.",
                               k, l, AvgMosaic[k][l]);
                        hpe_fieldgen_printMessage( message );
                    }
                }
                else
                {
                    if(AvgMosaic[k][l] >= 0.0)
                    {
                        sprintf(message, "grid cell calculation error"
                                " for cell [%d][%d] = %lf in the avg mosaic.",
                                k, l, AvgMosaic[k][l]);
                        hpe_fieldgen_printMessage( message );
                    }
                }
            }
        }

        if(first == 1)
        {
            if(hpe_fieldgen_getAppsDefaults(AVG_ERMOSAIC_DIR_TOKEN, avgRMosaicDir)==-1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"%s\".\n\tProgram exit.",
                     AVG_ERMOSAIC_DIR_TOKEN) ;
                shutdown( message );
            }
        }

        sprintf(fileName, "AVGRMOSAIC%s%sz",
                pEMPEParams->category_name, strDateTime );
    
        writeArray( pGeoData, avgRMosaicDir, fileName,
                    FACTOR_PRECIP, replace_missing,
                    pEMPEParams->user, pRunDate->tRunTime,
                    DSP_PROC_FLAG, AvgMosaic, &irc ) ;

        if(irc != 0)
        {
            sprintf( message , "ERROR: error number = %ld"
                " attempting to write file: %s/%s" , 
                irc, avgRMosaicDir, fileName) ;
            printLogMessage( message );
        }
        else
        {
            sprintf( message , "STATUS: file written to: %s/%s" , 
                    avgRMosaicDir, fileName) ;
            printLogMessage( message );
        }

#if APPLY_POLYGON

        /* Apply edit polygons to the avgrmosaic product for
           use in furture products. */
        apply_mpe_polygons ( AvgMosaic,
                             strDateTime,
                             year,
                             month,
                             day,
                             hour,
                             display_avgerMosaic,
                             pGeoData,
                             FACTOR_PRECIP,
                             0,
                             0); 
#endif

    }//end blnMosaic condition
    
    if(blnMosaic[maxermosaic] == 1)
    {
        if(first == 1)
        {
            if(hpe_fieldgen_getAppsDefaults(MAX_ERMOSAIC_DIR_TOKEN, maxRMosaicDir)==-1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"%s\".\n\tProgram exit.",
                    MAX_ERMOSAIC_DIR_TOKEN) ;
                shutdown( message );
            }
        }

        sprintf(fileName, "MAXRMOSAIC%s%sz",
                pEMPEParams->category_name, strDateTime );
        writeArray(pGeoData, maxRMosaicDir, fileName,
                   FACTOR_PRECIP, replace_missing,
                   pEMPEParams->user, pRunDate->tRunTime,
                   DSP_PROC_FLAG, MaxMosaic, &irc) ;

        if(irc != 0)
        {
            sprintf( message , "ERROR: error number = %ld"
                " attempting to write file: %s/%s" , 
                irc, maxRMosaicDir, fileName) ;
            printLogMessage( message );
        }
        else
        {
            sprintf( message , "STATUS: file written to: %s/%s" , 
                    maxRMosaicDir, fileName) ;
            printLogMessage( message );
        }

#if APPLY_POLYGON

        /* Apply edit polygons to the avgrmosaic product for
           use in furture products. */
        apply_mpe_polygons ( MaxMosaic,
                             strDateTime,
                             year,
                             month,
                             day,
                             hour,
                             display_maxerMosaic,
                             pGeoData,
                             FACTOR_PRECIP,
                             0,
                             0    ); 
#endif

    }// end of blnMosaic condition
    // -------------------------------------------------
    // average and max mosaic xmrg's written to file.

    /*
     * Gage QC - Point Check using Radar Mosaic data.
     */

    if(blnMosaic[ermosaic] == 1)
    {
        if(pEMPEParams->gage_qc == 1)
        {
            hpe_fieldgen_getCurrentTime(currTime) ;
            sprintf( message , "%s = time begin Gage QC multisensor check." , 
                            currTime) ;
            hpe_fieldgen_printMessage( message );        

            if(pGageTable->pseudoGageNum == pGageTable->totalGageNum)
            {
                sprintf( message , "STATUS: No hourly gages data available,"
                    " skip Multisensor Check." ) ;
                hpe_fieldgen_printMessage( message );
            }
            else
            {
                checkMultisensorQC(datetime , RMosaic ,
                    pGeoData, pQCGageTable) ;
            }
            hpe_fieldgen_getCurrentTime(currTime) ;
            sprintf( message , "%s = time end Gage QC multisensor check." , 
                            currTime) ;
            hpe_fieldgen_printMessage( message );
        }
    }//end blnMosaic condition

    /*
     * write out gridded data in xmrg format to flat files
     */

    if(blnMosaic[ermosaic] == 1)
    {
        hpe_fieldgen_getCurrentTime(currTime) ;
        sprintf( message , "%s = time begin writing ERMOSAIC"
                           " fields to flat files.", currTime) ;
        hpe_fieldgen_printMessage( message );

        if(first == 1)
        {
            if(hpe_fieldgen_getAppsDefaults(MOSAIC_DIR_TOKEN, mosaicDir) == -1)
            {
                sprintf ( message , "ERROR: Invalid token value"
                    " for token \"%s\".\n\tProgram exit.",
                    MOSAIC_DIR_TOKEN) ;
                shutdown( message );
            }
        }

        sprintf(fileName, "ERMOSAIC%s%sz",
                pEMPEParams->category_name, strDateTime ); 
        sprintf(ermosaic_fileName, "ERMOSAIC%s%sz",
                pEMPEParams->category_name, strDateTime ); 
        writeArray(pGeoData, mosaicDir, fileName, FACTOR_PRECIP, 
                   replace_missing, pEMPEParams->user, pRunDate->tRunTime, 
                   DSP_PROC_FLAG, RMosaic, &irc) ;

        if(irc != 0)
        {
            sprintf( message , "ERROR: error number = %ld"
                " attempting to write file: %s/%s" , 
                irc, mosaicDir, fileName) ;
            printLogMessage( message );
        }
        else
        {
            sprintf( message , "STATUS: file written to: %s/%s" , 
                    mosaicDir, fileName) ;
            printLogMessage( message );
        }

#if APPLY_POLYGON

            /* Apply edit polygons to the avgrmosaic product for
               use in future products. */
            apply_mpe_polygons ( RMosaic,
                                 strDateTime,
                                 year,
                                 month,
                                 day,
                                 hour,
                                 display_erMosaic,
                                 pGeoData,
                                 FACTOR_PRECIP,
                                 0,
                                 0 ); 
#endif

        hpe_fieldgen_getAppsDefaults(SAVE_HEIGHT_TOKEN, saveflag);

        if(strcmp(hpe_fieldgen_toLowerCase(saveflag), "save") == 0)
        {
            if(first == 1)
            {
                if(hpe_fieldgen_getAppsDefaults(HEIGHT_DIR_TOKEN, heightDir) == -1)
                {
                    sprintf ( message , "ERROR: Invalid token value"
                        " for token \"%s\".\n\tProgram exit.",
                        HEIGHT_DIR_TOKEN) ;
                    shutdown( message );
                }
            }
            
            sprintf(fileName, "DSPHEIGHT%s%sz",
                    pEMPEParams->category_name, strDateTime ); 
            writeArray(pGeoData, heightDir, fileName, FACTOR_OTHER, 
                       replace_missing, pEMPEParams->user, pRunDate->tRunTime,
                       DSP_PROC_FLAG, MHeight, &irc) ;
    
            if(irc != 0)
            {
                sprintf( message , "ERROR: error number = %ld"
                    " attempting to write file: %s/%s." , 
                    irc, heightDir, fileName) ;
                printLogMessage( message );
            }
            else
            {
                sprintf( message , "STATUS: file written to: %s/%s." , 
                         heightDir, fileName) ;
                printLogMessage( message );
            }
        }

        hpe_fieldgen_getAppsDefaults(SAVE_INDEX_TOKEN, saveflag);

        if(strcmp(hpe_fieldgen_toLowerCase(saveflag), "save") == 0)
        {
            if(first == 1)
            {
                if(hpe_fieldgen_getAppsDefaults(INDEX_DIR_TOKEN, indexDir) == -1)
                {
                    sprintf ( message , "ERROR: Invalid token value"
                        " for token \"%s\".\n\tProgram exit.",
                        INDEX_DIR_TOKEN) ;
                    shutdown( message );
                }
            }
    
            sprintf(fileName, "DSPINDEX%s%sz",
                    pEMPEParams->category_name, strDateTime ); 
    
            /*      
             * fill in tempID array
             */
    
            for(i = 0; i < rowSize; i ++)
            {
                for(j = 0; j < colSize; j ++)
                {
                    tempID[i][j] = (double)ID[i][j] ;
                }
            }
    
            writeArray(pGeoData, indexDir, fileName, FACTOR_OTHER, 
                       replace_missing, pEMPEParams->user, pRunDate->tRunTime,
                       DSP_PROC_FLAG, tempID, &irc) ;
    
            if(irc != 0)
            {
                sprintf( message , "ERROR: error number = %ld"
                    " attempting to write file: %s/%s." , 
                    irc, indexDir, fileName) ;
                printLogMessage( message );
            }
            else
            {
                sprintf( message, "STATUS: file written to: %s/%s.", indexDir,
                         fileName) ;
                printLogMessage( message );
            }
        }

        if(strcmp(pEMPEParams->base_radar_mosaic, "ermosaic") == 0)
        {
            sprintf(fileName, "ERMOSAIC%s%sz",
                     pEMPEParams->category_name, strDateTime );
            pMosaic = RMosaic;
        }
        else if(strcmp(pEMPEParams->base_radar_mosaic, "avgermosaic") == 0)
        {
            sprintf(fileName, "AVGRMOSAIC%s%sz",
                    pEMPEParams->category_name, strDateTime );
            pMosaic = AvgMosaic;
        }
        else if(strcmp(pEMPEParams->base_radar_mosaic, "maxermosaic") == 0)
        {
            sprintf(fileName, "MAXRMOSAIC%s%sz",
                     pEMPEParams->category_name, strDateTime );
            pMosaic = MaxMosaic;
        }

        writeFormattedXMRG(pEMPEParams, pGeoData, 
                 mosaicDir, fileName, DSP_PROC_FLAG, 
                 SAVE_GRIB_TOKEN,
                 SAVE_GIF_TOKEN, GIF_DIR_TOKEN, GIF_ID_TOKEN,
                 SAVE_NETCDF_TOKEN, NETCDF_DIR_TOKEN, NETCDF_ID_TOKEN,
                 SAVE_JPEG_TOKEN,
                 pMosaic);


        hpe_fieldgen_getCurrentTime(currTime) ;
        sprintf( message, "%s = time end writing ERMOSAIC fields to flat files.",
                        currTime) ;
        hpe_fieldgen_printMessage( message );

    }//end blnMosaic condition   
    
    /* check  dual_pol_flag */   
    if (dualpol_on_flag == 0)
      dualpol_used = 0;
    else
    {
       for (i = 0; i < pRadarLocTable->radarNum; i++)
       {
          if (dualpol_data_avail[i] != 0)
          {
	     dualpol_used = 1;
	    
	     /* if dualpol data is available, however no record found in the DAARadarResult table */
	     if (dualpol_meanbias_flag[i] == 0)
	        meanFieldBias[i] = 1.0;
		
	     break;	
	  }
	  else
	     dualpol_used = 0;     	          
       }    
    }  
    
    sprintf ( message , "\nSTATUS:  In ERMOSAIC, the flag dualpol_used = %d\n", dualpol_used);
    printLogMessage( message );
    
    sprintf ( message , "\nSTATUS:  In ERMOSAIC, insert/update HPERadarResult table");
    printLogMessage( message );
    
    if(blnMosaic[ermosaic] == 1)
       wrtodb_HPERadarResult(ermosaic_fileName,  prdDateTime, pEMPEParams, dualpol_used);
    
    sprintf ( message , "\nSTATUS:  In ERMOSAIC, complete insert/update HPERadarResult table");
    printLogMessage( message );
       
    freeMosaicArray(pGeoData, GRID_ROWS) ;

    if(pRadarResult != NULL)
    {
        free(pRadarResult);
        pRadarResult = NULL;
    }
    if (pDAARadarResult != NULL)
    {
        free(pDAARadarResult);
        pDAARadarResult = NULL;
    }
    

    first = 0 ;
}

static void initMosaicArray(const geo_data_struct * pGeoData,
                             const int radar_rows,
                             const int radar_cols) 
{
    const int rowSize = pGeoData->num_rows ;
    const int colSize = pGeoData->num_cols ;

    MHeight = init2DDoubleArray(HEIGHT_DEFAULT, rowSize, colSize );

    tempID = init2DDoubleArray(ID_DEFAULT, rowSize, colSize );

    origRadar = init2DFloatArray(RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);

    currRadar = init2DFloatArray(RADAR_DEFAULT, radar_rows, radar_cols);

    hrapMiscBins = init2DShortArray(MISBIN_DEFAULT, NUM_DPA_ROWS, NUM_DPA_COLS);

    currMiscBins = init2DShortArray(MISBIN_DEFAULT, radar_rows, radar_cols);
}

static void freeMosaicArray(const geo_data_struct * pGeoData,
                                 const int radar_rows) 
{
    const int rowSize = pGeoData->num_rows ;

    free2DDoubleArray(MHeight, rowSize );
                      
    free2DDoubleArray(tempID, rowSize );

    free2DFloatArray(origRadar, NUM_DSP_ROWS );

    free2DFloatArray(currRadar, radar_rows );

    free2DShortArray(hrapMiscBins, NUM_DPA_ROWS );

    free2DShortArray(currMiscBins, radar_rows );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
