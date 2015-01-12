/***********************************************************************
 * Filename: project.c
 *
 * Original Author: 
 *
 * File Creation Date: 
 *
 * Development Group: HSEB/OHD
 *
 * Description:
 * calculates the projected accum and projected variance.
 *          write out the rain rate forecast in 15 minutes and the 
 *          hourly rain total forecast. 
 * 
 * Modules:
 * project
 *
 ***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"
#include "save_empe_grib.h"

#define DEFAULT_SMOOTH_METHOD 1
#define DEFAULT_RUNFREQ 5

/***********************************************************************
 * Module Name: project
 *
 * Original Author: 
 *
 * Module Creation Date: 
 * 
 * Description:
 *   This function runs calculates the projected accum and projected variance.
 *          write out the rain rate forecast in 15 minutes and the 
 *          hourly rain total forecast. 
 *
 * calling function: runProjection
 * functions called: 
 *
 * Calling Arguments:
 * Name         Input/Output Type             Description
 *
 * tRunTime     Input        time_t           run time 
 * pGeoData     Input        geo_data_struct* global HRAP lowerleft-corner
 *                                            bin and dimension and dimension
 *                                            of the RFC estimation domain
 * mosaicID     Input        char *           the mosaic type 
 * pEMPEParams  Input        empe_params_struct*
 *                                            static parameters
 * pProjectionParams
 *              Input        projection_params_struct *
 *                                            static projection parameters
 * ibins        Input        int **           bin array for radar coverage
 * pVelocityX   Input        double **        velocity array every box
 * pVelocityY   Input        double **        velocity array every box
 * growth       Input        double **        growth/decay rates every fifth box (mm/hr)
 * pObservedVar Input        double **        observed error variance
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
 * MODIFICATION HISTORY:
 *	DATE 		PROGRAMMER  	DESCRIPTION/REASON
 *	Jul. 1987  	Paul Tilles 	initial version  	
 *	Jun. 14, 1999 R. Fulton		fixed error in equation for mtx in ael
 *	Feb. 2001  	R. Fulton		updated to incorporate echo growth/decay
 *	Apr. 2001  	R. Fulton		added new subroutine "ratesmooth" to
 *							implement the bellon and zawadski (1994)
 *							spatial smoothing logic as an option
 *	Jul. 2001  	R. Fulton		added mxpra limiting of projected rates
 *	Aug. 11, 2006 Shucai Guan 	finish conversion to C Language
 *  Mar., 2008  Guoxian Zhou    finish first operational version
 *
 ***********************************************************************/

static void writeGrib(char * fileName, char * procFlag)
{
    char fname_grib[FNAME_LEN] = { '\0' };
    char fname_xmrg[PATH_LEN] = { '\0' };
    char * pGribCommand= NULL;

    sprintf(fname_xmrg, "%s", fileName);
    sprintf(fname_grib, "%s.grib", fname_xmrg);

    pGribCommand = save_hpe_grib(fname_xmrg, fname_grib, procFlag);

    if (pGribCommand != NULL)
    {
        sprintf(message, "STATUS: process_hpe_grib_files script called "
            "using command '%s'.", pGribCommand);
        hpe_fieldgen_printMessage(message);
        free(pGribCommand);
        pGribCommand = NULL;
    }

}

void project(const geo_data_struct * pGeoData, const char * mosaicID,
        const empe_params_struct * pEMPEParams, const time_t tRunTime,
        const projection_params_struct * pProjectionParams,
        const int projectionStep, const double delt,
        const double maxProjectedRate, int ** ibins, double ** velocityX,
        double ** velocityY, double ** growth, int ** pCount, double ** rmean,
        double ** resid, double ** pObservedVar, const double varres,
        double projParam, const int radar_data_source)
{

    const char * SAVE_RATE_GRIB_TOKEN = "hpe_rate_save_grib";
    const char * SAVE_BRATE_GRIB_TOKEN = "hpe_brate_save_grib";
    const char * SAVE_TP1H_GRIB_TOKEN = "hpe_tp1h_save_grib";
    const char * SAVE_BTP1H_GRIB_TOKEN = "hpe_btp1h_save_grib";
    const char * SAVE_4KM_TP1H_GRIB_TOKEN = "hpe_4km_tp1h_save_grib";
    const char * SAVE_4KM_BTP1H_GRIB_TOKEN = "hpe_4km_btp1h_save_grib";
    const char * HPE_NOWCAST_DIR_TOKEN = "hpe_nowcast_dir";
    const char * HPE_SMOOTH_METHOD_TOKEN = "hpe_smooth_method";
    const char * HPE_RUNFREQ_TOKEN = "hpe_runfreq";

    char saveflag[TOKEN_LEN]= { '\0' };
    int saveRateFlag = 0;
    int saveTP1HFlag = 0;
    int save4kmTP1HFlag = 0;

    char fileName[PATH_LEN] = { '\0' };

    char strDateTime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    char prdDateTime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };

    const char * rateFilePrefix = "PRT";
    const char * biasedRateFilePrefix = "BPRT";
    const char * accFilePrefix = "ACC";
    const char * biasedAccFilePrefix = "BACC";

    static char outputDir[PATH_LEN] = { '\0' };
    char procFlag[10] = { '\0' };
    char strTokenValue[10] = { '\0' };

    static int first = 1;

    const int replace_missing = 0;
    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;

    double ** pMovedObservedVar = init2DDoubleArray(0, rowSize, colSize);

    double ** pHourlyRate = init2DDoubleArray(0, rowSize, colSize);
    double ** pProjectedVar = init2DDoubleArray(0, rowSize, colSize);
    double ** pHourlyAccum = init2DDoubleArray(0, rowSize, colSize);

    double ** pProjectedRate = init2DDoubleArray(0, rowSize, colSize);
    double ** pMovedInstantRate = init2DDoubleArray(0, rowSize, colSize);

    double sum, tsum, prtim, ppmult, growthamt, x;
    int i, j, iprj, isFilled;

    long irc;

    int origRowSize = pGeoData->num_rows;
    int origColSize = pGeoData->num_cols;

    static int isSmooth= DEFAULT_SMOOTH_METHOD;
    static int runfreq= DEFAULT_RUNFREQ;
    int isGrowth = pProjectionParams->isGrowth;

    /*
     * strDateTime string should be in format: yyyymmddHHMM
     */

    struct tm * pRunTime = gmtime(&tRunTime) ;
    strftime(strDateTime, ANSI_YEARSEC_TIME_LEN + 1, "%Y%m%d%H%M", pRunTime);
    strftime(prdDateTime, ANSI_YEARSEC_TIME_LEN + 1,
                            "%Y-%m-%d %H:%M:%S", pRunTime);

    if (strstr(mosaicID, "BDHR") != NULL)
    {
        strcpy(procFlag, BDHR_PROC_FLAG);
        hpe_fieldgen_getAppsDefaults(SAVE_BRATE_GRIB_TOKEN, saveflag) ;
        if (strcmp(toLowerCase(saveflag), "save") == 0)
        {
            saveRateFlag = 1;
        }

        hpe_fieldgen_getAppsDefaults(SAVE_BTP1H_GRIB_TOKEN, saveflag) ;
        if (strcmp(toLowerCase(saveflag), "save") == 0)
        {
            saveTP1HFlag = 1;
        }

        hpe_fieldgen_getAppsDefaults(SAVE_4KM_BTP1H_GRIB_TOKEN, saveflag) ;
        if (strcmp(toLowerCase(saveflag), "save") == 0)
        {
            save4kmTP1HFlag = 1;
        }
    } else
    {
        strcpy(procFlag, DHR_PROC_FLAG);
        hpe_fieldgen_getAppsDefaults(SAVE_RATE_GRIB_TOKEN, saveflag) ;
        if (strcmp(toLowerCase(saveflag), "save") == 0)
        {
            saveRateFlag = 1;
        }

        hpe_fieldgen_getAppsDefaults(SAVE_TP1H_GRIB_TOKEN, saveflag) ;
        if (strcmp(toLowerCase(saveflag), "save") == 0)
        {
            saveTP1HFlag = 1;
        }

        hpe_fieldgen_getAppsDefaults(SAVE_4KM_TP1H_GRIB_TOKEN, saveflag) ;
        if (strcmp(toLowerCase(saveflag), "save") == 0)
        {
            save4kmTP1HFlag = 1;
        }
    }

    if (first == 1)
    {
        if (hpe_fieldgen_getAppsDefaults(HPE_NOWCAST_DIR_TOKEN, outputDir) == -1)
        {
            sprintf(message, "ERROR: Invalid token value"
                " for token \"%s\".\n\tProgram exit.", HPE_NOWCAST_DIR_TOKEN) ;
            shutdown(message);
        }

        if (hpe_fieldgen_getAppsDefaults(HPE_SMOOTH_METHOD_TOKEN, strTokenValue) == -1)
        {
            sprintf(message, "WARNING: Invalid token value"
                " for token \"%s\".\nDefault to %d.", HPE_SMOOTH_METHOD_TOKEN,
                    isSmooth) ;
            printLogMessage(message);
        } else
        {
            int value = atoi(strTokenValue);
            if (value >= 0 && value <= 2)
            {
                isSmooth = value;
            }

            sprintf(message, "STATUS: Set token \"%s\" to %d.",
                    HPE_SMOOTH_METHOD_TOKEN, isSmooth) ;
            hpe_fieldgen_printMessage(message);
        }

        if (hpe_fieldgen_getAppsDefaults(HPE_RUNFREQ_TOKEN, strTokenValue) == -1)
        {
            sprintf(message, "WARNING: Invalid token value"
                " for token \"%s\".\nDefault to %d.", HPE_RUNFREQ_TOKEN,
                    runfreq) ;
            printLogMessage(message);
        } else
        {
            int value = atoi(strTokenValue);
            if (value > 0 && value <= 15)
            {
                runfreq = value;
            }

            sprintf(message, "STATUS: Set token \"%s\" to %d.",
                    HPE_RUNFREQ_TOKEN, runfreq) ;
            hpe_fieldgen_printMessage(message);
        }

    }

    geo_data_struct tmpGeoData;

    tmpGeoData.hrap_x = pGeoData->hrap_x * pEMPEParams->hrap_grid_factor;
    tmpGeoData.hrap_y = pGeoData->hrap_y * pEMPEParams->hrap_grid_factor;
    tmpGeoData.num_cols = pGeoData->num_cols * pEMPEParams->hrap_grid_factor;
    tmpGeoData.num_rows = pGeoData->num_rows * pEMPEParams->hrap_grid_factor;

    int newRowSize = tmpGeoData.num_rows;
    int newColSize = tmpGeoData.num_cols;

    double ** p1kmArray = init2DDoubleArray(0, newRowSize, newColSize);
    double ** p1kmSarray = init2DDoubleArray(0, newRowSize, newColSize);

    /*
     * if user specifies no smoothing (smoothflg=0)
     * then set pp to 1.0 which	overwrites the value
     * computed previously in pparm subroutine
     */

    if (isSmooth == 0)
    {
        projParam = 1.0;
    }

    /*
     * compute projection error due to residual process.
     * equals sum of all terms in residual covariances.
     * fill all terms in prvar with this value.
     * 
     * sum below is mtx in the ael
     */

    sum = 0.0;

    for (i = 0; i < projectionStep; i++)
    {
        tsum = 0.0;
        for (j = 0; j < i; j++)
        {
            tsum += pow(projParam, 2 * j);
        }
        sum += tsum;
    }

    sum *= varres;

    /*
     * fill the initial projected rate residual variance array
     * initailize projected accumulation to 0.0
     */

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            if (ibins[i][j] == 0)
            {
                continue;
            }

            pProjectedVar[i][j] = sum;
        }
    }

    /*
     * loop through projection time steps
     */
    int k, ii, jj;

    k=1;

    for (iprj = 1; iprj <= projectionStep; iprj ++)
    {
        /*
         * prtim is in hours
         */

        prtim = iprj * delt;

        fill(pProjectedRate, rowSize, colSize, 0.0);

        if (isSmooth == 0 || isSmooth == 1)
        {
            ppmult = pow(projParam, iprj);

            /*
             * compute projected rates in stationary coordinates,
             * accounting for growth/decay if the flag is set
             */

            for (i = 0; i < rowSize; i++)
            {
                for (j = 0; j < colSize; j++)
                {
                    if (ibins[i][j] == 0)
                    {
                        continue;
                    }

                    if (isGrowth != 0)
                    {
                        growthamt = growth[i][j] * prtim;
                    } else
                    {
                        growthamt = 0.0;
                    }

                    pProjectedRate[i][j] = rmean[i][j] + ppmult * resid[i][j]
                            + growthamt;

                    /*
                     * constrain projected rainrates to fall
                     * within (0,mxpra) in case growth or
                     * decay accounting causes unrealistic rates
                     */

                    if (pProjectedRate[i][j] < 0.0)
                    {
                        pProjectedRate[i][j] = 0.0;
                    }

                    if (pProjectedRate[i][j] > maxProjectedRate)
                    {
                        pProjectedRate[i][j] = maxProjectedRate;
                    }

                    if ((rmean[i][j] + resid[i][j]) <= 0.0)
                    {
                        pProjectedRate[i][j] = 0.0;
                    }
                }
            }
        } else
        {
            /*
             * compute the projected rates in stationary coordinates
             * using the bellon and zawadski (1994) smoothing technique
             */

            ratesmooth(rowSize, colSize, isGrowth, prtim, maxProjectedRate,
                    pProjectionParams->lamda, pProjectionParams->kappa, ibins,
                    rmean, resid, growth, pProjectedRate);

        }

        /*
         * move projected rates (temp1) and observed residual variance
         * of rates	(errvar) by storm velocity.
         * temp1 moved into temp2; errvar moved into temp3
         */

        move2(rowSize, colSize, pProjectedRate, pObservedVar, pCount, ibins,
                velocityX, velocityY, prtim, pMovedInstantRate,
                pMovedObservedVar);

        /*
         * fill holes in the projected/moved rain rate array
         */

        fill_hole(rowSize, colSize, ibins, pMovedInstantRate);

        /*
         * fill holes in the projected/moved rain rate array a second time
         * to fill even more holes
         */

        fill_hole(rowSize, colSize, ibins, pMovedInstantRate);

        /*
         * add on moved projected rates and moved observed rate resid.
         * variances for this timestep to the total rates and variances
         * for the entire forecast time period.
         * fill holes prior to summing if necessary.
         * note that we are summing rainrates here and will later multiply
         * by the fixed	timestep in order to get accumulations.
         * note that the projected error variances of rain rate remain
         * unchanged for each future time step,
         * unlike the projected rain rates.
         */

        for (i = 0; i < rowSize; i++)
        {
            for (j = 0; j < colSize; j++)
            {
                if (ibins[i][j] == 0)
                {
                    continue;
                }

                if (pCount[i][j] > 0)
                {
                    pHourlyAccum[i][j] += pMovedInstantRate[i][j];
                    pProjectedVar[i][j] += pMovedObservedVar[i][j];
                } else
                {
                    fillHole(rowSize, colSize, i, j, pMovedInstantRate, pCount,
                            &x, &isFilled);

                    if (isFilled == 1)
                    {
                        pMovedInstantRate[i][j] = x;
                        pHourlyAccum[i][j] += pMovedInstantRate[i][j];
                    }

                    fillHole(rowSize, colSize, i, j, pMovedObservedVar, pCount,
                            &x, &isFilled);

                    if (isFilled == 1)
                    {
                        pProjectedVar[i][j] += x;
                    }
                }
            }
        }

        /*
         *  Write out the projected rates at the time steps
         *  nearest 15 minute intervals
         */

        if ((prtim >= (0.25 * k - delt / 2)) && (prtim < (0.25 * k + delt/ 2)))
        {

            if (strstr(mosaicID, "BDHR") != NULL)
            {
                sprintf(procFlag, "%s%02d  ", biasedRateFilePrefix, k * 15);
                sprintf(fileName, "%sM%d%sz", biasedRateFilePrefix, k * 15,
                        strDateTime);
            } else
            {
                sprintf(procFlag, "%s%02d   ", rateFilePrefix, k * 15);
                sprintf(fileName, "%sM%d%sz", rateFilePrefix, k * 15,
                        strDateTime);
            }

            /*
             * expand the output array to 1km grid
             */
            convertDoubleArray(origRowSize, origColSize, pMovedInstantRate, 0,
                    newRowSize, newColSize, p1kmArray);

            for (i = 0; i < newRowSize; i++)
            {
                for (j = 0; j < newColSize; j++)
                {
                    if (i==0 && j==0)
                    {
                        p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                                + p1kmArray [i][j+1])/3.0;
                    } else if (i==(newRowSize-1) && j==0)
                    {
                        p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i-1][j]
                                + p1kmArray [i][j+1])/3.0;
                    } else if (i==0 && j==(newColSize-1))
                    {
                        p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                                + p1kmArray [i][j-1])/3.0;
                    } else if (i==(newRowSize-1) && j==(newColSize-1))
                    {
                        p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i-1][j]
                                + p1kmArray [i][j-1])/3.0;
                    } else if (i==0)
                    {
                        p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                                + p1kmArray [i][j-1] + p1kmArray[i][j+1])/4.0;
                    } else if (j==0)
                    {
                        p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                                + p1kmArray [i-1][j] + p1kmArray[i][j+1])/4.0;
                    } else if (i==(newRowSize-1))
                    {
                        p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i-1][j]
                                + p1kmArray [i][j-1] + p1kmArray[i][j+1])/4.0;
                    } else if (j==(newColSize-1))
                    {
                        p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                                + p1kmArray [i-1][j] + p1kmArray[i][j-1])/4.0;
                    } else
                    {
                        p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                                + p1kmArray [i][j-1] + p1kmArray[i][j+1]
                                + p1kmArray[i-1][j])/5.0;
                    }
                }
            }

            writeArray(&tmpGeoData, outputDir, fileName, FACTOR_PRECIP,
                    replace_missing, pEMPEParams->user, tRunTime, procFlag,
                    p1kmSarray, &irc) ;

            if (irc != 0)
            {
                sprintf(message, "ERROR: error number = %ld"
                    " attempting to write file: %s/%s", irc, outputDir,
                        fileName) ;
                printLogMessage(message);
            } else
            {
                sprintf(message, "STATUS: file written to: %s/%s", outputDir,
                        fileName) ;
                printLogMessage(message);

                sprintf ( message , "\nSTATUS:  In Project, insert/update 1km mosaic nowcast into HPERadarResult table");
                printLogMessage( message );

                wrtodb_HPERadarResult(fileName, prdDateTime, pEMPEParams, radar_data_source);

            }

            /*
             * if saveRateFlag is found as "ON",
             * then create and save grib image.
             */

            if (saveRateFlag == 1)
            {
                writeGrib(fileName, procFlag);
            }

            k++;

        }
    }

    /*
     * multiply by the projection time step interval to obtain total projected
     * rainfall for the forecast period (this constant was pulled out of the
     * earlier summation for computational efficiency).
     * 
     * write out the hourly total projected rainfall.
     */

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            if (ibins[i][j] == 0)
            {
                continue;
            }

            pHourlyAccum[i][j] *= delt;
        }
    }

    /*
     * output the 4km mosaic product for top hour
     */

    if (pRunTime->tm_min >= 0 && pRunTime->tm_min < runfreq)
    {
        if (strstr(mosaicID, "BDHR") != NULL)
        {
            sprintf(procFlag, "rfcwide ");
            sprintf(fileName, "%s4kmH%sz", biasedAccFilePrefix, strDateTime);
        } else
        {
            sprintf(procFlag, "rfcwide ");
            sprintf(fileName, "%s4kmH%sz", accFilePrefix, strDateTime);
        }

        writeArray(pGeoData, outputDir, fileName, FACTOR_PRECIP,
                replace_missing, pEMPEParams->user, tRunTime, procFlag,
                pHourlyAccum, &irc);

        if (irc != 0)
        {
            sprintf(message, "ERROR: error number = %ld"
                " attempting to write file: %s/%s", irc, outputDir, fileName) ;
            printLogMessage(message);
        } else
        {
            sprintf(message, "STATUS: Output top-hour 4km mosaic product.\n"
                "\tfile written to: %s/%s", outputDir, fileName) ;
            printLogMessage(message);

            sprintf ( message , "\nSTATUS:  In Project, insert/update 4km mosaic nowcast into HPERadarResult table");
            printLogMessage( message );

            wrtodb_HPERadarResult(fileName,  prdDateTime, pEMPEParams, radar_data_source);
        }

    }

    /*
     * if save4kmTP1HFlag is found as "ON",
     * then create and save grib image.
     */

    if (save4kmTP1HFlag == 1)
    {
        writeGrib(fileName, procFlag);
    }

    /*
     * output the 1km grid array
     */

    if (strstr(mosaicID, "BDHR") != NULL)
    {
        sprintf(procFlag, "BTP1H   ");
        sprintf(fileName, "%sH%sz", biasedAccFilePrefix, strDateTime);
    } else
    {
        sprintf(procFlag, "TP1H    ");
        sprintf(fileName, "%sH%sz", accFilePrefix, strDateTime);
    }

    /*
     * expand the output array to 1km grid
     */

    convertDoubleArray(origRowSize, origColSize, pHourlyAccum, 0, newRowSize,
            newColSize, p1kmArray);

    for (i = 0; i < newRowSize; i++)
    {
        for (j = 0; j < newColSize; j++)
        {
            if (i==0 && j==0)
            {
                p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                        + p1kmArray [i][j+1])/3.0;
            } else if (i==(newRowSize-1) && j==0)
            {
                p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i-1][j]
                        + p1kmArray [i][j+1])/3.0;
            } else if (i==0 && j==(newColSize-1))
            {
                p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                        + p1kmArray [i][j-1])/3.0;
            } else if (i==(newRowSize-1) && j==(newColSize-1))
            {
                p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i-1][j]
                        + p1kmArray [i][j-1])/3.0;
            } else if (i==0)
            {
                p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                        + p1kmArray [i][j-1] + p1kmArray[i][j+1])/4.0;
            } else if (j==0)
            {
                p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                        + p1kmArray [i-1][j] + p1kmArray[i][j+1])/4.0;
            } else if (i==(newRowSize-1))
            {
                p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i-1][j]
                        + p1kmArray [i][j-1] + p1kmArray[i][j+1])/4.0;
            } else if (j==(newColSize-1))
            {
                p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                        + p1kmArray [i-1][j] + p1kmArray[i][j-1])/4.0;
            } else
            {
                p1kmSarray[i][j] = (p1kmArray[i][j] + p1kmArray[i+1][j]
                        + p1kmArray [i][j-1] + p1kmArray[i][j+1]
                        + p1kmArray[i-1][j])/5.0;
            }
        }
    }

    writeArray(&tmpGeoData, outputDir, fileName, FACTOR_PRECIP,
            replace_missing, pEMPEParams->user, tRunTime, procFlag, p1kmSarray,
            &irc) ;

    if (irc != 0)
    {
        sprintf(message, "ERROR: error number = %ld"
            " attempting to write file: %s/%s", irc, outputDir, fileName) ;
        printLogMessage(message);
    } else
    {
        sprintf(message, "STATUS: file written to: %s/%s", outputDir, fileName) ;
        printLogMessage(message);

        sprintf ( message , "\nSTATUS:  In Project, insert/update 1km mosaic nowcast into HPERadarResult table");
        printLogMessage( message );

        wrtodb_HPERadarResult(fileName,  prdDateTime, pEMPEParams, radar_data_source);
    }

    /*
     * if saveTP1HFlag is found as "ON",
     * then create and save grib image.
     */

    if (saveTP1HFlag == 1)
    {
        writeGrib(fileName, procFlag);
    }

    free2DDoubleArray(p1kmArray, newRowSize);
    free2DDoubleArray(p1kmSarray, newRowSize);

    first = 0;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/project.c,v $";
 static char rcs_id2[] = "$Id: project.c,v 1.5 2008/12/04 21:38:47 gzhou Exp $";}
/*  ===================================================  */

}
