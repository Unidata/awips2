/***********************************************************************
 * Filename: run_nowcast.c
 *
 * Original Author: Guoxian Zhou
 *
 * File Creation Date: 01/13/2008
 *
 * Development Group: HSEB/OHD
 *
 * Description:
 * nowcast based on the DHR mosaic data
 * 
 * Modules:
 * runNowcast
 *
 ***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

#define    DEFAULT_MEANVELOCITY_DIRECTION   45.0
#define    DEFAULT_MEANVELOCITY_SPEED       20.0
#define    DEFAULT_MEANVELOCITY_COUNT       25
#define    HRAP_POLE_I                      401
#define    HRAP_POLE_J                      1601
#define    COS280                           0.1736482
#define    SIN280                          -0.9848078
#define    PI 3.14159265

/***********************************************************************
 * Module Name: runNowcast
 *
 * Original Author: Guoxian Zhou
 *
 * Module Creation Date: 01/13/2008
 * 
 * Description:
 *   This function nowcasts based on the DHR mosaic data.
 *
 * calling function: runDHRMosaic, runBDHRMosaic
 * functions called: predetfun, getPreviousFilename, runProjection
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
 * pInputDir    Input        char *           the directory of mosaic xmrg files
 * currMosaic   Input        double **        current DHR/BDHR mosaic product
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
 * 1/13/2008   Guoxian Zhou  Build operational version 
 *
 ***********************************************************************/

int useFakeMean;
double fakeUMean;
double fakeVMean;

static projection_params_struct * pProjectionParams= NULL;

void initProjectionParams();

void runNowcast(const geo_data_struct * pGeoData, const time_t tRunTime,
        const char * mosaicID, const empe_params_struct * pEMPEParams,
        const char * pInputDir, double ** currMosaic,
        const int  radar_data_source)
{
    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;

    const char * USE_MEANVELOCITY_TOKEN = "hpn_use_meanvelocity";
    const char * MEANVELOCITY_DIRECTION_TOKEN = "hpn_meanvelocity_direction";
    const char * MEANVELOCITY_SPEED_TOKEN = "hpn_meanvelocity_speed";

    static char useMeanvelocity[4] = "off";
    static double meanvelocityDirection= DEFAULT_MEANVELOCITY_DIRECTION;
    static double meanvelocitySpeed= DEFAULT_MEANVELOCITY_SPEED;

    static int first = 0;

    char tokenValue[10] = { '\0' };

    double factor= FACTOR_PRECIP;
    double uvel, vvel, numi, numj, dfp;
    int irc = 0;

    char prevMosaicFilename[FILE_LEN] = { '\0' };
    char prevMosaicFilePath[FILE_LEN] = { '\0' };
    int timeDiff;

    double ** prevMosaic = init2DDoubleArray(MOSAIC_DEFAULT, rowSize, colSize);
    int origRowSize = pGeoData->num_rows;
    int origColSize = pGeoData->num_cols;
    
    double swlat, swlon, selat, selon, se_x, se_y, dist_x, dist_y;

    Velocity * pVelocity = (Velocity *) malloc(sizeof(Velocity));

    sprintf(message, "STATUS: Starting nowcast...\n") ;
    hpe_fieldgen_printMessage(message);

    /*
     * create the new geo grid for conversion to 4km
     */
    geo_data_struct tmpGeoData;

    tmpGeoData.hrap_x = pGeoData->hrap_x / pEMPEParams->hrap_grid_factor;
    tmpGeoData.hrap_y = pGeoData->hrap_y / pEMPEParams->hrap_grid_factor;
    tmpGeoData.num_cols = pGeoData->num_cols / pEMPEParams->hrap_grid_factor;
    tmpGeoData.num_rows = pGeoData->num_rows / pEMPEParams->hrap_grid_factor;

    int newRowSize = tmpGeoData.num_rows;
    int newColSize = tmpGeoData.num_cols;
    
    se_x = (double) tmpGeoData.hrap_x + newColSize;
    se_y = (double) tmpGeoData.hrap_y;
    
    

    double ** p4kmCurrMosaic = init2DDoubleArray(MOSAIC_DEFAULT, newRowSize,
            newColSize);

    double ** p4kmPrevMosaic = init2DDoubleArray(MOSAIC_DEFAULT, newRowSize,
            newColSize);

    /*
     * diminish the current mosaic array to 4km grid
     */
    convertDoubleArray(origRowSize, origColSize, currMosaic, MOSAIC_DEFAULT,
            newRowSize, newColSize, p4kmCurrMosaic);

    /*
     * read constants for projection algorithm
     */

    if (pProjectionParams == NULL)
    {
        initProjectionParams();
        readProjectionParams(pProjectionParams);
    }

    /*
     * compute the area of echo whose intensity is greater than an input
     * threshold and compare it with an input echo area minimum threshold.
     * exit the nowcast if the area of echo is less than the echo area
     * minimum threshold.
     */

    predetfun(rowSize, colSize, pProjectionParams->minPDFRainrateThreshold,
            pProjectionParams->minPDFAreaThreshold, currMosaic, &irc);

    if (irc == 1)
    {
        sprintf(message,
                "STATUS: the area of echo is less than the threshold.\n"
                    "\tQuit nowcast.") ;
        hpe_fieldgen_printMessage(message);
        return;
    }

    /*
     * find mosaic file 10 - 20 minutes ago.
     * No projection if this file does not exist.
     */

    time_t tPrevRunTime = tRunTime - 15 * 60;
    getPreviousFilename(pInputDir, tPrevRunTime, &timeDiff, prevMosaicFilename);

    if (strlen(prevMosaicFilename) > 0)
    {
        sprintf(prevMosaicFilePath, "%s/%s", pInputDir, prevMosaicFilename);
        int lenfn = strlen(prevMosaicFilePath);
        sprintf(message, "DEBUG: previous file path -- %s\n",
                prevMosaicFilePath) ;
        hpe_fieldgen_printMessage(message);

        readxmrg(pEMPEParams->os, rowSize, colSize, prevMosaicFilePath, lenfn,
                factor, prevMosaic, &irc) ;

        if (irc != 0)
        {
            sprintf(message, "ERROR: failure to load file -- %s\n"
                "\tQuit nowcast.", prevMosaicFilename) ;
            hpe_fieldgen_printMessage(message);

            return;
        }
    } else
    {
        sprintf(message, "STATUS: There is no record 15 minutes ago.\n") ;
        hpe_fieldgen_printMessage(message);

        if (first == 0)
        {
            if ( (hpe_fieldgen_getAppsDefaults(USE_MEANVELOCITY_TOKEN, useMeanvelocity)
                    != -1) && strcmp(toLowerCase(useMeanvelocity), "on") == 0)
            {
                sprintf(message, "STATUS: Token \"%s\" is \"ON\".",
                        USE_MEANVELOCITY_TOKEN) ;
                hpe_fieldgen_printMessage(message);

                useFakeMean = 1;

            } else if ( (hpe_fieldgen_getAppsDefaults(USE_MEANVELOCITY_TOKEN,
                    useMeanvelocity) != -1) && strcmp(
                    toLowerCase(useMeanvelocity), "off") == 0)
            {
                sprintf(message, "STATUS: Token \"%s\" is \"OFF\".",
                        USE_MEANVELOCITY_TOKEN) ;
                hpe_fieldgen_printMessage(message);

                useFakeMean = 0;

            } else
            {
                sprintf(message, "WARN: Invalid token value"
                    " for token \"%s\".\n\tDefault to \"OFF\".",
                        USE_MEANVELOCITY_TOKEN) ;
                hpe_fieldgen_printMessage(message);

                useFakeMean = 0;
            }

            if (hpe_fieldgen_getAppsDefaults(MEANVELOCITY_DIRECTION_TOKEN, tokenValue) == -1)
            {
                sprintf(message, "WARN: Invalid token value"
                    " for token \"%s\".\n\tDefault to %f.",
                        MEANVELOCITY_DIRECTION_TOKEN, 
                        DEFAULT_MEANVELOCITY_DIRECTION) ;
                hpe_fieldgen_printMessage(message);
            } else
            {
                meanvelocityDirection = atof(tokenValue);

                sprintf(message, "STATUS: Token \"%s\" is set to %f.",
                        MEANVELOCITY_DIRECTION_TOKEN, meanvelocityDirection) ;
                hpe_fieldgen_printMessage(message);
            }

            if (hpe_fieldgen_getAppsDefaults(MEANVELOCITY_SPEED_TOKEN, tokenValue) == -1)
            {
                sprintf(message, "WARN: Invalid token value"
                    " for token \"%s\".\n\tDefault to %f.",
                        MEANVELOCITY_SPEED_TOKEN, DEFAULT_MEANVELOCITY_SPEED) ;
                hpe_fieldgen_printMessage(message);
            } else
            {
                meanvelocitySpeed = atof(tokenValue);

                sprintf(message, "STATUS: Token \"%s\" is set to %f.",
                        MEANVELOCITY_SPEED_TOKEN, meanvelocitySpeed) ;
                hpe_fieldgen_printMessage(message);
            }
	    
	   /* First do some gross error checking on the direction since this is a user token.
	      
	      Next, transform the compass direction to a polar direction so the u and v can
	      be computed properly.
	      
	      Note that for HPN purposes, the final u and v are actually reversed with u in the y
	      direction and v in the x direction
	   */

	    if (meanvelocityDirection >= 360)
	       meanvelocityDirection -= 360;
	    if (meanvelocityDirection < 0 )
	       meanvelocityDirection += 360;
	       
	    if (meanvelocityDirection >= 0 && meanvelocityDirection <= 270)
	       meanvelocityDirection = 90 - meanvelocityDirection;
	    else
	       meanvelocityDirection = 450 - meanvelocityDirection;


            double radian = meanvelocityDirection * PI / 180.0;

            /* Unfortunately, one cannot just use the value above as it is relative to the HRAP 
	       area and not to the bearing angles.  Therefore, must determine whether the HRAP area
	       is skewed and then determine the skewed angle.  While the method below may not be
	       advisable for higher latitudes and also for places off the normal HRAP grid (i.e. Hawaii),
	       it should work for most sites.  Probably turn this token off for those sites.
	       
	       First, find the lat and lon of the SW and SE HRAP points.
	       Then, if necessary, subtract the lats and lons multiply by approx distance in km.
	       The atan2 function with give the skew angle in radians.  Add that to the user
	       token value to adjust the HRAP relative direction to the desired bearing angle.
	    */
	    
	    hpe_fieldgen_hrap_to_latlon(tmpGeoData.hrap_x,tmpGeoData.hrap_y, &swlon, &swlat);
	    hpe_fieldgen_hrap_to_latlon(se_x, se_y, &selon, &selat);
	    
	    if (swlat != selat)   /* if these are the same then there's no skew angle */
	    {
	    
	       dist_y = (swlat - selat) * 111.0;  /* approx degree of lat distance in km */
	    
	       dist_x = (swlon - selon) * 111.0;
	    
	       double radian_diff = atan2(dist_y, dist_x);
	    
	       radian += radian_diff;
	    
	       double newdir = radian * 180.0/ PI;
	    
		    
	    }
	    
	    fakeUMean = meanvelocitySpeed * sin(radian) / 2.49;
            fakeVMean = meanvelocitySpeed * cos(radian) / 2.49;
	    
 
            first = 1;
        }

        /*
         * save the velocity to the database.
         * gzhou 11-12-2008
         */

        if (useFakeMean == 1)
        {
            dtime_t dtCurrent;

            timet_to_yearsec_dt(tRunTime, &dtCurrent);

            strcpy(pVelocity->mosaicid, mosaicID);
            pVelocity->createtime = dtCurrent;
            pVelocity->count = DEFAULT_MEANVELOCITY_COUNT;
            pVelocity->umean = fakeUMean;
            pVelocity->vmean = fakeVMean;

            InsertOrUpdateVelocity(pVelocity);
        }

        return;
    }

    /*
     * diminish the mosaic array to 4km grid
     */

    convertDoubleArray(origRowSize, origColSize, prevMosaic, MOSAIC_DEFAULT,
            newRowSize, newColSize, p4kmPrevMosaic);

    runProjection(&tmpGeoData, tRunTime, mosaicID, pEMPEParams,
            pProjectionParams, timeDiff, p4kmCurrMosaic, p4kmPrevMosaic, radar_data_source);

    free2DDoubleArray(prevMosaic, rowSize);
    free2DDoubleArray(p4kmCurrMosaic, newRowSize);
    free2DDoubleArray(p4kmPrevMosaic, newRowSize);

    sprintf(message, "STATUS: Exiting nowcast...\n") ;
    hpe_fieldgen_printMessage(message);

}

void initProjectionParams()
{
    /*
     * Allocate and initialize the projection parameter struct.
     */

    pProjectionParams
            = (projection_params_struct * ) malloc(sizeof(projection_params_struct));
    if (pProjectionParams == NULL)
    {
        sprintf(message, "ERROR: memory allocation failure"
            " in initProjectionParams function."
            "\n\tProgram exit.") ;
        shutdown(message);

    }

    memset(pProjectionParams, '\0', sizeof(projection_params_struct));

}

void freeProjectionParams()
{
    if (pProjectionParams != NULL)
    {
        free(pProjectionParams);
        pProjectionParams = NULL;
    }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/run_nowcast.c,v $";
 static char rcs_id2[] = "$Id: run_nowcast.c,v 1.3 2008/12/04 21:48:28 gzhou Exp $";}
/*  ===================================================  */

}

