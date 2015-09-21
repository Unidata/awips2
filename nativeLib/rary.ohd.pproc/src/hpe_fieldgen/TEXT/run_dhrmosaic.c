/***********************************************************************
 * Filename: run_dhrmosaic.c
 *
 * Original Author: Guoxian Zhou
 *
 * File Creation Date: 08/13/2006
 *
 * Development Group: HSEB/OHD
 *
 * Description:
 * compute the raw DHR mosaic data
 *
 * Modules:
 * runDHRMosaic
 *
 ***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

/***********************************************************************
 * Module Name: createDHRMosaic
 *
 * Original Author: Guoxian Zhou
 *
 * Module Creation Date: 08/13/2006
 *
 * Description:
 *   This function computes the raw DHR mosaic data.
 *
 * calling function: main_empe_fieldgen
 * functions called: readRadarResult ,
 *                readDHRData, readMisc,
 *                createDHRMosaic, writeArray
 *
 * Calling Arguments:
 * Name         Input/Output Type             Description
 *
 * pRunDate     Input        run_date_struct* date/time
 * pGeoData     Input        geo_data_struct* global HRAP lowerleft-corner
 *                                            bin and dimension and dimension
 *                                            of the RFC estimation domain
 * pMPEParams   Input        mpe_params_struct*     static parameters
 * pRadarLocRecord Input     radarLoc_table_struct* info from radarLoc table
 * DHRMosaic    Output       double **        the DHR radar mosaic product
 * QPEMosaic    Output       double **        the mosaic of radars
 *                                            for best estimate
 *
 *
 * Required
 * None
 *
 * Required Files/Databases:
 * None
 *
 * Non System Routines Called:
 *
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
 * Date        Developer     Action
 * 8/13/2006   Guoxian Zhou  Build operational version
 * 07/2013     Jingtao Deng  Modify for adding dual pol producat DPR
 *                           first try to find the DPR product from dprradar
 *                           table and calculate the rain rate. if
 *                           not able to get DPR product, then use DHR product
 * 02/2015     JingtaoD      A2 OB14.4.1 DR#17123 - HPE Bias Source field
 * 09/2015     JingtaoD      A2 OB16.2.1 DR#17860 - HPE DualPol Mean field bias
 ***********************************************************************/

extern short ** radarMiscBins;

static double ** tempID = NULL;
static double ** DHRHeight = NULL;

static float ** origRadar = NULL;
static short ** hrapMiscBins = NULL;
static float ** currRadar = NULL;
static short ** currMiscBins = NULL;

extern int dualpol_used;
extern int dualpol_on_flag;

static void initMosaicArray(const geo_data_struct * pGeoData,
        const int grid_rows, const int grid_cols);
static void freeMosaicArray(const geo_data_struct * pGeoData,
        const int grid_rows);

void runDHRMosaic(const run_date_struct * pRunDate,
        const geo_data_struct * pGeoData, empe_params_struct * pEMPEParams,
        const radarLoc_table_struct * pRadarLocTable,
        const gage_table_struct * pGageTable, double * meanFieldBias,
        double ** RadarBeamHeight, int ** ID, double ** Mosaic,
        double ** QPEMosaic) {
    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;
    const int replace_missing = 0;

    const char * MOSAIC_DIR_TOKEN = "hpe_dhrmosaic_dir";
    const char * SAVE_HEIGHT_TOKEN = "hpe_save_dhrheight";
    const char * HEIGHT_DIR_TOKEN = "hpe_dhrheight_dir";
    const char * SAVE_INDEX_TOKEN = "hpe_save_dhrindex";
    const char * INDEX_DIR_TOKEN = "hpe_dhrindex_dir";

    const char * SAVE_GRIB_TOKEN = "dhrmosaic_save_grib";

    const char * SAVE_NETCDF_TOKEN = "dhrmosaic_save_netcdf";
    const char * NETCDF_DIR_TOKEN = "dhrmosaic_netcdf_dir";
    const char * NETCDF_ID_TOKEN = "dhrmosaic_netcdf_id";

    const char * SAVE_GIF_TOKEN = "dhrmosaic_save_gif";
    const char * GIF_DIR_TOKEN = "dhrmosaic_gif_dir";
    const char * GIF_ID_TOKEN = "dhrmosaic_gif_id";

    const char * SAVE_JPEG_TOKEN = "dhrmosaic_save_jpeg";

    char saveflag[TOKEN_LEN] = { '\0' };

    const int RADAR_ROWS = NUM_DPA_ROWS * pEMPEParams->hrap_grid_factor;
    const int RADAR_COLS = NUM_DPA_COLS * pEMPEParams->hrap_grid_factor;

    static char datetime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    static char datehour[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    static char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    static char prdDateTime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    static char radarID[RADAR_ID_LEN + 1] = { '\0' };
    register int i, j, k;

    long int irc;
    register int year, month, day, hour;

    int radarAvailFlag = 0;
    int dp_blnEditBias, sp_blnEditBias;
    double dp_editBiasValue, sp_editBiasValue;
    int dp_ignoreRadarFlag, sp_ignoreRadarFlag;

    static char fileName[PATH_LEN] = { '\0' };
    static char dhrmosaic_fileName[PRESET_DESCR_LEN] = { '\0' };
    static char heightDir[PATH_LEN] = { '\0' };
    static char indexDir[PATH_LEN] = { '\0' };
    static char mosaicDir[PATH_LEN] = { '\0' };

    static int first = 1;
    struct tm * pRunTime = NULL;

    short pps_radar_count = 0, dp_radar_count = 0;
    int index;
    double meanBias = 0.0;
    double memSpanBias;
    int gageRadarPairNum = 0; /* number of positive gage/radar pairs */
    int dualpol_meanbias_flag[MAX_RADAR_NUM];
    int sp_meanbias_flag[MAX_RADAR_NUM];
    int dualpol_data_avail[MAX_RADAR_NUM];
    int nobias_flag;

    /* for DHR product, there is no bias applied */
    nobias_flag = 1;

    /* initialize no dualpol MFB and no dual pol product avialble for each radar */
    for (i = 0; i < MAX_RADAR_NUM; i++) {
        dualpol_meanbias_flag[i] = 0;
        sp_meanbias_flag[i] = 0;
        dualpol_data_avail[i] = 0;
    }

    pEMPEParams->radar_avail_num = 0;

    /*
     * Allocates memory for data arrays
     * based on the geo grid size data.
     */

    initMosaicArray(pGeoData, RADAR_ROWS, RADAR_COLS);

    /*
     * strDateTime string should be in format: yyyymmddHHMM
     */

    pRunTime = gmtime(&(pRunDate->tRunTime));
    strftime(datetime, ANSI_YEARSEC_TIME_LEN + 1, "%Y-%m-%d %H:%M:00",
            pRunTime);
    strftime(datehour, ANSI_YEARSEC_TIME_LEN + 1, "%Y-%m-%d %H:00:00",
            pRunTime);
    strftime(strDateTime, ANSI_YEARSEC_TIME_LEN + 1, "%Y%m%d%H%M", pRunTime);
    strftime(prdDateTime, ANSI_YEARSEC_TIME_LEN + 1, "%Y-%m-%d %H:%M:%S",
            pRunTime);

    year = pRunTime->tm_year + 1900;
    month = pRunTime->tm_mon + 1;
    day = pRunTime->tm_mday;
    hour = pRunTime->tm_hour;

    hpe_fieldgen_getCurrentTime(currTime);
    sprintf(message, "%s = time begin DHRMOSAIC calculation.", currTime);
    hpe_fieldgen_printMessage(message);

    radar_result_struct * pRadarResult = NULL;

    pRadarResult = (radar_result_struct *) malloc(
            pRadarLocTable->radarNum * sizeof(radar_result_struct));
    if (pRadarResult == NULL) {
        sprintf(message, "ERROR: memory allocation failure"
                " in runDHRMosaic function."
                "\n\tProgram exit.");
        shutdown(message);
    }

    /* for dual pol proudcts which use daaradarresult table */
    radar_result_struct * pDAARadarResult = NULL;

    pDAARadarResult = (radar_result_struct *) malloc(
            pRadarLocTable->radarNum * sizeof(radar_result_struct));
    if (pDAARadarResult == NULL) {
        sprintf(message, "ERROR: memory allocation failure"
                " in runDHRMosaic function."
                "\n\tProgram exit.");
        shutdown(message);
    }

    for (i = 0; i < pRadarLocTable->radarNum; i++) {
        memset(pRadarResult[i].radID, '\0', RADAR_ID_LEN + 1);
        pRadarResult[i].ignore_radar = 0;
        pRadarResult[i].edit_bias = 0;
        pRadarResult[i].bias = 0.0;
        memset(pDAARadarResult[i].radID, '\0', RADAR_ID_LEN + 1);
        pDAARadarResult[i].ignore_radar = 0;
        pDAARadarResult[i].edit_bias = 0;
        pDAARadarResult[i].bias = 0.0;
    }

    /*
     * Read from RWRadarResult table for SinglePol or DAARadarResult table for DualPol
     */
    if (dualpol_on_flag == 0) {
        readRadarResult(datetime, pRadarResult, &pps_radar_count,
                sp_meanbias_flag, &irc);
        if (irc < 0) {
            sprintf(message,
                    "ERROR:in runDHRMosaic - Database error #%ld attempting to "
                            "select record from RWRadarResult table.", irc);
            printLogMessage(message);
        }
    } else {
        readDAARadarResult(datehour, pDAARadarResult, &dp_radar_count,
                dualpol_meanbias_flag, &irc); /* for dual pol */

        if (irc < 0) {
            sprintf(message,
                    "ERROR:in runDHRMosaic - Database error #%ld attempting to "
                            "select record from DAARadarResult table.", irc);
            hpe_fieldgen_printMessage(message);
        }
    }

    /*
     * Read the misbin information if the token hpe_load_misbin is ON,
     * otherwise the misbin will be default value(1).
     */

    if (radarMiscBins == NULL) {
        radarMiscBins = init2DShortArray(MISBIN_DEFAULT,
                pRadarLocTable->radarNum, NUM_DPA_ELEMENTS);

        if (pEMPEParams->load_misbin == 1) {
            readMisc(pRadarLocTable, radarMiscBins);
        }
    }

    /*
     * begin loop on radars
     */

    for (i = 0; i < pRadarLocTable->radarNum; i++) {

        strcpy(radarID, pRadarLocTable->ptrRadarLocRecords[i].radarID);

        sprintf(message, "\n******* radar: %s *******", radarID);
        hpe_fieldgen_printMessage(message);

        radar_result_struct * dp_pRadarInfo = NULL;
        radar_result_struct * sp_pRadarInfo = NULL;

        /*
         * initialize edit bias flag, ignore radar flag
         */

        dp_blnEditBias = 0;
        dp_editBiasValue = 0.0;
        dp_ignoreRadarFlag = 0;
        sp_blnEditBias = 0;
        sp_editBiasValue = 0.0;
        sp_ignoreRadarFlag = 0;

        if (pDAARadarResult != NULL) {
            dp_pRadarInfo = (radar_result_struct *) binary_search(
                    pDAARadarResult, radarID, dp_radar_count,
                    sizeof(radar_result_struct), compare_radar_id);
            if (dp_pRadarInfo != NULL) {
                dp_ignoreRadarFlag = dp_pRadarInfo->ignore_radar;
                dp_editBiasValue = dp_pRadarInfo->bias;
                dp_blnEditBias = dp_pRadarInfo->edit_bias;
            }
        }

        if (pRadarResult != NULL) {
            sp_pRadarInfo = (radar_result_struct *) binary_search(pRadarResult,
                    radarID, pps_radar_count, sizeof(radar_result_struct),
                    compare_radar_id);
            if (sp_pRadarInfo != NULL) {
                sp_ignoreRadarFlag = sp_pRadarInfo->ignore_radar;
                sp_editBiasValue = sp_pRadarInfo->bias;
                sp_blnEditBias = sp_pRadarInfo->edit_bias;
            }
        }

        /*
         * Read in gridded radar data
         * count number of available radars
         * an "ignored" radar is considered "not available"
         */

        if (dualpol_on_flag == 1) {
            readDPRData(radarID, datetime, pEMPEParams->dhr_window,
                    dp_ignoreRadarFlag, origRadar, &radarAvailFlag);

            if (radarAvailFlag > 0) {
                sprintf(message,
                        "STATUS:In runDHRMosaic - dual pol product DPR is used for radar %s",
                        radarID);
                hpe_fieldgen_printMessage(message);
                dualpol_data_avail[i] = 1;
            }
        } else {
            readDHRData(radarID, datetime, pEMPEParams->dhr_window,
                    sp_ignoreRadarFlag, origRadar, &radarAvailFlag);

            if (radarAvailFlag > 0) {
                sprintf(message,
                        "STATUS:In runDHRMosaic - single pol product DHR is used for radar %s",
                        radarID);
                hpe_fieldgen_printMessage(message);
                dualpol_data_avail[i] = 0;
            }
        }

        if (radarAvailFlag > 0) {
            pEMPEParams->radar_avail_num++;
        }

        /*
         * Convert radar array to current hrap grid
         */

        convertFloatArray(NUM_DHR_ROWS, NUM_DHR_COLS, origRadar, RADAR_DEFAULT,
                RADAR_ROWS, RADAR_COLS, currRadar);

        /*
         * pick up the misbin for current running radar and
         * convert it to current hrap grid
         */

        for (j = 0; j < NUM_DPA_ROWS; j++) {
            for (k = 0; k < NUM_DPA_COLS; k++) {
                index = j * NUM_DPA_COLS + k;
                hrapMiscBins[j][k] = radarMiscBins[i][index];
            }
        }

        convertShortArray(NUM_DPA_ROWS, NUM_DPA_COLS, hrapMiscBins,
                MISBIN_DEFAULT, RADAR_ROWS, RADAR_COLS, currMiscBins);

        /*
         * get mean field bias if it is set to "ON"
         */
        if (pEMPEParams->blnDHRMeanFieldBias == 1) {
            getMeanBias(&(pRadarLocTable->ptrRadarLocRecords[i]), datetime,
                    RADAR_ROWS, RADAR_COLS, currMiscBins, currRadar, pGeoData,
                    pGageTable, pEMPEParams, dualpol_data_avail[i], &meanBias,
                    &memSpanBias, &gageRadarPairNum);

            meanFieldBias[i] = meanBias;

            if (dualpol_on_flag == 1 && dp_blnEditBias == 1) {
                sprintf(message,
                        "STATUS:In runDHRMosaic - Edited Bias is used for radar %s",
                        radarID);
                hpe_fieldgen_printMessage(message);
                meanFieldBias[i] = dp_editBiasValue;
            } else if (dualpol_on_flag == 0 && sp_blnEditBias == 1) {
                sprintf(message,
                        "STATUS:In runDHRMosaic - Edited Bias is used for radar %s",
                        radarID);
                hpe_fieldgen_printMessage(message);
                meanFieldBias[i] = sp_editBiasValue;
            }

        } /*end of pEMPEParams->blnMeanFieldBias == 1 */

        sprintf(message, "\nSTATUS:In runDHRMosaic - MFB is %f for radid %s.",
                meanFieldBias[i], radarID);
        hpe_fieldgen_printMessage(message);

        /*
         * mosaicking algorithm
         */

        createDHRMosaic(&(pRadarLocTable->ptrRadarLocRecords[i]), RADAR_ROWS,
                RADAR_COLS, currRadar, currMiscBins, pGeoData, i + 1,
                RadarBeamHeight, DHRHeight, ID, Mosaic);
    }

    hpe_fieldgen_getCurrentTime(currTime);
    sprintf(message, "\n%s = time   end DHRMOSAIC calculation.", currTime);
    hpe_fieldgen_printMessage(message);

    /*
     * write out gridded data in xmrg format to flat files
     */

    hpe_fieldgen_getCurrentTime(currTime);
    sprintf(message, "%s = time begin writing DHRMOSAIC fields to flat files.",
            currTime);
    hpe_fieldgen_printMessage(message);

    if (first == 1) {
        if (hpe_fieldgen_getAppsDefaults(MOSAIC_DIR_TOKEN, mosaicDir) == -1) {
            sprintf(message, "ERROR: Invalid token value"
                    " for token \"%s\".\n\tProgram exit.", MOSAIC_DIR_TOKEN);
            shutdown(message);
        }
    }

    sprintf(dhrmosaic_fileName, "DHRMOSAIC%sz", strDateTime);
    sprintf(fileName, "DHRMOSAIC%sz", strDateTime);
    writeArray(pGeoData, mosaicDir, fileName, FACTOR_PRECIP, replace_missing,
            pEMPEParams->user, pRunDate->tRunTime, DHR_PROC_FLAG, Mosaic, &irc);

    if (irc != 0) {
        sprintf(message, "ERROR: error number = %ld"
                " attempting to write file: %s/%s", irc, mosaicDir, fileName);
        printLogMessage(message);
    } else {
        sprintf(message, "STATUS: file written to: %s/%s", mosaicDir, fileName);
        printLogMessage(message);

        writeFormattedXMRG(pEMPEParams, pGeoData, mosaicDir, fileName,
                DHR_PROC_FLAG, SAVE_GRIB_TOKEN, SAVE_GIF_TOKEN, GIF_DIR_TOKEN,
                GIF_ID_TOKEN, SAVE_NETCDF_TOKEN, NETCDF_DIR_TOKEN,
                NETCDF_ID_TOKEN, SAVE_JPEG_TOKEN, Mosaic);
    }

#if APPLY_POLYGON

    /*
     * Apply edit polygons to the DHRMosaic product
     * for use in future products.
     */

    apply_mpe_polygons ( (void * )&Mosaic,
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

    if (strcmp(hpe_fieldgen_toLowerCase(saveflag), "save") == 0) {
        if (first == 1) {
            if (hpe_fieldgen_getAppsDefaults(HEIGHT_DIR_TOKEN, heightDir)
                    == -1) {
                sprintf(message, "ERROR: Invalid token value"
                        " for token \"%s\".\n\tProgram exit.",
                        HEIGHT_DIR_TOKEN);
                shutdown(message);
            }
        }

        sprintf(fileName, "DHRHEIGHT%sz", strDateTime);
        writeArray(pGeoData, heightDir, fileName, FACTOR_OTHER, replace_missing,
                pEMPEParams->user, pRunDate->tRunTime, DHR_PROC_FLAG, DHRHeight,
                &irc);

        if (irc != 0) {
            sprintf(message, "ERROR: error number = %ld"
                    " attempting to write file: %s/%s", irc, heightDir,
                    fileName);
            printLogMessage(message);
        } else {
            sprintf(message, "STATUS: file written to: %s/%s", heightDir,
                    fileName);
            printLogMessage(message);
        }
    }

    hpe_fieldgen_getAppsDefaults(SAVE_INDEX_TOKEN, saveflag);

    if (strcmp(hpe_fieldgen_toLowerCase(saveflag), "save") == 0) {
        if (first == 1) {
            if (hpe_fieldgen_getAppsDefaults(INDEX_DIR_TOKEN, indexDir) == -1) {
                sprintf(message, "ERROR: Invalid token value"
                        " for token \"%s\".\n\tProgram exit.", INDEX_DIR_TOKEN);
                shutdown(message);
            }
        }

        sprintf(fileName, "DHRINDEX%sz", strDateTime);

        /*
         *  * fill in tempID array
         */

        for (i = 0; i < rowSize; i++) {
            for (j = 0; j < colSize; j++) {
                tempID[i][j] = (double) ID[i][j];
            }
        }

        writeArray(pGeoData, indexDir, fileName, FACTOR_OTHER, replace_missing,
                pEMPEParams->user, pRunDate->tRunTime, DHR_PROC_FLAG, tempID,
                &irc);

        if (irc != 0) {
            sprintf(message, "ERROR: error number = %ld"
                    " attempting to write file: %s/%s", irc, indexDir,
                    fileName);
            printLogMessage(message);
        } else {
            sprintf(message, "STATUS: file written to: %s/%s", indexDir,
                    fileName);
            printLogMessage(message);
        }
    }

    hpe_fieldgen_getCurrentTime(currTime);
    sprintf(message, "%s = time   end writing DHRMOSAIC fields to flat files.",
            currTime);
    printLogMessage(message);

    sprintf(message,
            "\nSTATUS:In DHRMOSAIC - insert/update HPERadarResult table");
    printLogMessage(message);

    wrtodb_HPERadarResult(dhrmosaic_fileName, prdDateTime, pEMPEParams,
            dualpol_used, nobias_flag);

    sprintf(message,
            "\nSTATUS:In DHRMOSAIC - complete insert/update HPERadarResult table");
    printLogMessage(message);

    /*
     * run the nowcast
     */
    if (pEMPEParams->blnRunNowcast == 1) {
        const char * mosaicID = "DHR";
        runNowcast(pGeoData, pRunDate->tRunTime, mosaicID, pEMPEParams,
                mosaicDir, Mosaic, dualpol_used);
    }

    freeMosaicArray(pGeoData, RADAR_ROWS);

    if (pRadarResult != NULL) {
        free(pRadarResult);
        pRadarResult = NULL;
    }
    if (pDAARadarResult != NULL) {
        free(pDAARadarResult);
        pDAARadarResult = NULL;
    }

    first = 0;
}

static void initMosaicArray(const geo_data_struct * pGeoData,
        const int radar_rows, const int radar_cols) {

    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;

    DHRHeight = init2DDoubleArray(HEIGHT_DEFAULT, rowSize, colSize);

    tempID = init2DDoubleArray(ID_DEFAULT, rowSize, colSize);

    origRadar = init2DFloatArray(RADAR_DEFAULT, NUM_DHR_ROWS, NUM_DHR_COLS);

    currRadar = init2DFloatArray(RADAR_DEFAULT, radar_rows, radar_cols);

    hrapMiscBins = init2DShortArray(MISBIN_DEFAULT, NUM_DPA_ROWS, NUM_DPA_COLS);

    currMiscBins = init2DShortArray(MISBIN_DEFAULT, radar_rows, radar_cols);
}

static void freeMosaicArray(const geo_data_struct * pGeoData,
        const int radar_rows) {
    const int rowSize = pGeoData->num_rows;

    free2DDoubleArray(DHRHeight, rowSize);

    free2DDoubleArray(tempID, rowSize);

    free2DFloatArray(origRadar, NUM_DHR_ROWS);

    free2DFloatArray(currRadar, radar_rows);

    free2DShortArray(hrapMiscBins, NUM_DPA_ROWS);

    free2DShortArray(currMiscBins, radar_rows);
}

