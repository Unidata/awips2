/***********************************************************************
 * Filename: read_DSA_radar.c
 *
 * Original Author: Guoxian Zhou
 *
 * File Creation Date: September 07, 2006
 *
 * Development Group: OHD/HSEM
 *
 * Description:
 * Contains routine for creating rainfall at product targert time
 * with user-specified duration from decoded DSA radar.
 * 
 * Modules:
 * readDSARadar
 *
 ***********************************************************************/

/* Include files, definitions, globals go here. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DSAAdapt.h"
#include "DSARadar.h"
#include "empe_fieldgen.h"

/***********************************************************************
 * Module Name: readDSARadar
 *
 * Original Author: Guoxian Zhou
 *
 * Module Creation Date: September 07, 2006
 *
 * Description:
 * Contains routine for creating rainfall at product targert time
 * with user-specified duration from decoded DSA radar.
 * 
 * Calling Arguments:
 * Name          Input/Output Type          Description
 * radarID       Input        const char *  radar id
 * datetime      Input        const char *  date and time of current run
 * dsp_window    Input        const int     DSA product search window value
 * dsp_duration  Input        const int     DSA product duration value
 * ignoreRadarFlag
 *              Input        const int     ignore radar flag
 *                                         = 0 -- use radar (default)
 *                                         = 1 -- ignore radar
 * radar        Output       float **      two-dimensional array of radar data
 * radarAvailFlag
 *              Output       int *         radar availability flag
 *                                         = 0 -- no radar data available
 *                                         = 1 -- radar data available
 * 
 * Required
 * None
 *
 * Required Files/Databases:
 * None
 * 
 * calling function: runDSAERMosaic
 * 
 * Non System Routines Called:
 * GetDsaAdapt, GetDSARadar, readDecodedDSA
 *
 * Return Value:
 * Type          Description
 * void
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
 * Date         Developer     Action
 * 09/07/2006   Guoxian Zhou  First version
 * 03/13/2007   Guoxian Zhou  remove the raw mean bias from the radar record.
 * 06/26/2007   Guoxian Zhou  Using the latest record before the run time 
 *                            as the last record when there is no record
 *                            after the run time.
 * 02/2012      Jingtao Deng  Modify to DSA dual pol product
 ***********************************************************************/

static float ** ptrPrevRadar = NULL;
static float ** ptrPostRadar = NULL;
static float ** ptrFirstRadar = NULL;
static float ** ptrLastRadar = NULL;

static void allocDSAMemory();
static void freeDSAMemory();

static void getDSAParams(const char * radid, const time_t first_obstime,
                const time_t last_obstime, double * mean_bias,
                double * min_scale_factor);

static void buildFirstRadarRecord(const time_t run_time, float ** pPrevRadar,
        const time_t prev_begin_time, const time_t prev_end_time,
        float ** pPostRadar, const time_t post_begin_time,
        const time_t post_end_time, float ** pFirstRadar, time_t * begin_time,
        time_t * end_time);

static void buildLastRadarRecord(const time_t run_time, float ** pPrevRadar,
        const time_t prev_begin_time, const time_t prev_end_time,
        float ** pPostRadar, const time_t post_begin_time,
        const time_t post_end_time, const int useLatestRadar,
        float ** pLastRadar, time_t * begin_time, time_t * end_time);

void readDSARadar(const char * radid, const char * datetime,
        const int dsp_window, const int dsp_duration,
        const int ignoreRadarFlag, float ** radar, int * radarAvailFlag)
{
    const int window_t = dsp_window * 60;
    const int duration_t = dsp_duration * 60;

    char where [ 256 ] = { '\0' };

    char str_datetime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    char str_start_runtime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    char str_end_runtime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };

    char prev_filename[FNAME_LEN] = { '\0' };
    char post_filename[FNAME_LEN] = { '\0' };

    time_t start_runtime, end_runtime;

    time_t first_begin_time, first_end_time;
    time_t last_begin_time, last_end_time;

    time_t obstime_t;

    time_t prev_begin_time = -1;
    time_t prev_end_time = -1;

    time_t post_begin_time = -1;
    time_t post_end_time = -1;

    int status;

    int i, j;

    int useLatestRadar = 0;
    int foundLaterRecord = 0;

    DSARadar * pDSARadarHead = NULL;
    DSARadar * pDSARadarNode = NULL;

    time_t offset_time_t, prev_diff, post_diff;
    int gap_percent;

    /*
     * Initialize the parameters.
     */

    *radarAvailFlag = 0;
    
     /*
     * malloc memory and initialize the radar array
     */

    fill2DFloatArray(radar, RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);

    allocDSAMemory();

    /*
     * if ignore radar flag set ON, 
     * return all missing values
     */

    if (ignoreRadarFlag == 1)
    {
        sprintf(message, "STATUS: Dual Pol DSA radar product marked as ignored.Stop.") ;
        hpe_fieldgen_printMessage(message);

        return;
    }
   
    strcpy(str_datetime, datetime);
    yearsec_ansi_to_timet(str_datetime, &end_runtime) ;

    start_runtime = end_runtime - duration_t;

    /*
     * Start to search DSA and build the first DSA record
     */

    prev_diff = window_t;
    post_diff = window_t;

    timet_to_yearsec_ansi((start_runtime - window_t), str_start_runtime);
    timet_to_yearsec_ansi((start_runtime + window_t), str_end_runtime);

    /*
     * search for record(s) within the dsp_window range of start_runtime
     */

    sprintf(where, "WHERE radid = '%s' AND obstime >= '%s' "
            "AND obstime <= '%s' ORDER BY obstime", radid, str_start_runtime,
            str_end_runtime) ;

    pDSARadarHead = GetDSARadar(where);

    if (pDSARadarHead == NULL)
    {

        /*
         * there is no record found.
         */

        sprintf(message, "STATUS: no dual pol DSA radar data for current run time.") ;
        hpe_fieldgen_printMessage(message);

        freeDSAMemory() ;
        return;
    } else
    {
        pDSARadarNode = ( DSARadar * ) ListFirst(&pDSARadarHead->list) ;
    }

    while (pDSARadarNode != NULL)
    {
        yearsec_dt_to_timet(pDSARadarNode->obstime, &obstime_t);
        offset_time_t = obstime_t - start_runtime;

        if (offset_time_t == 0)
        {
            /*
             * the record has the same datetime value
             * as the running datatime
             */

            yearsec_dt_to_timet(pDSARadarNode->begin_time, &prev_begin_time);
            yearsec_dt_to_timet(pDSARadarNode->end_time, &prev_end_time);
            strncpy(prev_filename, pDSARadarNode->grid_filename, 19);

            yearsec_dt_to_timet(pDSARadarNode->begin_time, &post_begin_time);
            yearsec_dt_to_timet(pDSARadarNode->end_time, &post_end_time);
            strncpy(post_filename, pDSARadarNode->grid_filename, 19);
	    
	    /*sprintf(message, "STATUS: Build the 1st record - The dual pol DSA record has same datetime as the running datetime."
                              "prev_filename is %s and post_filename is %s.", prev_filename,
                               post_filename) ;
            hpe_fieldgen_printMessage(message);*/

            break;
        } else if (offset_time_t < 0)
        {
            /*
             * the datetime of the record is less than
             * the running datatime.
             */

            if (abs(offset_time_t) < prev_diff)
            {
                yearsec_dt_to_timet(pDSARadarNode->begin_time, &prev_begin_time);
                yearsec_dt_to_timet(pDSARadarNode->end_time, &prev_end_time);
                strncpy(prev_filename, pDSARadarNode->grid_filename, 19);
		
		        /*sprintf(message, "STATUS: Build the 1st record - The dual pol DSA record datetime is less than the running datetime."
                                  "prev_filename is %s.", prev_filename) ;
                hpe_fieldgen_printMessage(message);*/

                prev_diff = abs(offset_time_t) ;
            }
            pDSARadarNode = ( DSARadar * ) ListNext( &pDSARadarNode->node) ;
            continue;
        } else if (offset_time_t > 0)
        {
            /*
             * the datetime of the record is great than
             * the running datatime.
             */

            if (offset_time_t < post_diff)
            {
                yearsec_dt_to_timet(pDSARadarNode->begin_time, &post_begin_time);
                yearsec_dt_to_timet(pDSARadarNode->end_time, &post_end_time);
                strncpy(post_filename, pDSARadarNode->grid_filename, 19);

                /*sprintf(message, "STATUS: Build the 1st record - The dual pol DSA record datetime is greater than the running datetime."
                                  "post_filename is %s.", post_filename) ;
                hpe_fieldgen_printMessage(message);*/
                
                post_diff = offset_time_t;
            }
            break;
        }
    }

    /*
     * load the radar data for the previous record.
     */

    if (strlen(prev_filename) > 0)
    {
        readDecodedDSA(prev_filename, ptrPrevRadar, &status);

        if (status != 0)
        {
            /*sprintf(message, "ERROR: Build the 1st record - #%d encountered reading"
                    " dual pol DSA radar file as prev_filename = %s -- missing data substituted.", status,
                    prev_filename) ;
            hpe_fieldgen_printMessage(message);*/
            *radarAvailFlag = 0;

            pDSARadarNode = NULL;
            if (pDSARadarHead != NULL)
            {
                FreeDSARadar(pDSARadarHead);
                pDSARadarHead = NULL;
            }

            freeDSAMemory() ;

            return;
        }
    } else
    {
        /*sprintf(message, "ERROR: Build the 1st record - no dual pol DSA radar file as prev_filename: %s", prev_filename) ;
        hpe_fieldgen_printMessage(message);*/
        *radarAvailFlag = 0;

        pDSARadarNode = NULL;
        if (pDSARadarHead != NULL)
        {
            FreeDSARadar(pDSARadarHead);
            pDSARadarHead = NULL;
        }

        freeDSAMemory() ;

        return;

    }

    /*
     * load the radar data for the post record.
     */

    if (strlen(post_filename) > 0)
    {
        readDecodedDSA(post_filename, ptrPostRadar, &status);

        if (status != 0)
        {
            /*sprintf(message, "ERROR: Build the 1st record - #%d encountered reading"
                   " dual pol DSA radar file as post_filename = %s -- missing data substituted.", status,
                    post_filename) ;*/
            hpe_fieldgen_printMessage(message);
            *radarAvailFlag = 0;

            pDSARadarNode = NULL;
            if (pDSARadarHead != NULL)
            {
                FreeDSARadar(pDSARadarHead);
                pDSARadarHead = NULL;
            }

            freeDSAMemory() ;

            return;
        }

    } else
    {
        /*sprintf(message, "ERROR: Build the 1st record - no dual pol DSA radar file as post_filename: %s", post_filename) ;
        hpe_fieldgen_printMessage(message);*/
        *radarAvailFlag = 0;

        pDSARadarNode = NULL;
        if (pDSARadarHead != NULL)
        {
            FreeDSARadar(pDSARadarHead);
            pDSARadarHead = NULL;
        }

        freeDSAMemory() ;

        return;
    }

    buildFirstRadarRecord(start_runtime, ptrPrevRadar, prev_begin_time,
            prev_end_time, ptrPostRadar, post_begin_time, post_end_time,
            ptrFirstRadar, &first_begin_time, &first_end_time);

    if (first_end_time == -999)
    {
        /*sprintf(message, "STATUS: can not build the first DSA record") ;
        hpe_fieldgen_printMessage(message);*/
        *radarAvailFlag = 0;

        pDSARadarNode = NULL;
        if (pDSARadarHead != NULL)
        {
            FreeDSARadar(pDSARadarHead);
            pDSARadarHead = NULL;
        }

        freeDSAMemory() ;

        return;
    }

    /*
     * Start to serach DSA and build the last DSA record
     */

    prev_diff = window_t;
    post_diff = window_t;

    timet_to_yearsec_ansi((end_runtime - window_t), str_start_runtime);
    timet_to_yearsec_ansi((end_runtime + window_t), str_end_runtime);

    /*
     * search for record(s) within the dsp_window range of end_runtime
     */

    sprintf(where, "WHERE radid = '%s' AND obstime >= '%s' "
           "AND obstime <= '%s' ORDER BY obstime", radid, str_start_runtime,
            str_end_runtime) ;

    pDSARadarNode = NULL;
    if (pDSARadarHead != NULL)
    {
        FreeDSARadar(pDSARadarHead);
        pDSARadarHead = NULL;
    }

    pDSARadarHead = GetDSARadar(where);

    if (pDSARadarHead == NULL)
    {

        /*
         * there is no record found.
         */

        sprintf(message, "STATUS: no dual pol DSA radar data for current run time.") ;
        hpe_fieldgen_printMessage(message);

        freeDSAMemory() ;

        return;
    } else
    {
        pDSARadarNode = ( DSARadar * ) ListFirst(&pDSARadarHead->list) ;
    }

    while (pDSARadarNode != NULL)
    {
        yearsec_dt_to_timet(pDSARadarNode->obstime, &obstime_t);
        offset_time_t = obstime_t - end_runtime;

        if (offset_time_t == 0)
        {
            /*
             * the record has the same datetime value
             * as the running datatime
             */

            yearsec_dt_to_timet(pDSARadarNode->begin_time, &prev_begin_time);
            yearsec_dt_to_timet(pDSARadarNode->end_time, &prev_end_time);
            strncpy(prev_filename, pDSARadarNode->grid_filename, 19);

            yearsec_dt_to_timet(pDSARadarNode->begin_time, &post_begin_time);
            yearsec_dt_to_timet(pDSARadarNode->end_time, &post_end_time);
            strncpy(post_filename, pDSARadarNode->grid_filename, 19);
	    
	        /*sprintf(message, "STATUS: Build the 2nd record - The dual pol DSA record has same datetime as the running datetime."
                              "prev_filename is %s and post_filename is %s.", prev_filename,
                               post_filename) ;
            hpe_fieldgen_printMessage(message);*/

            break;
        } else if (offset_time_t < 0)
        {
            /*
             * the datetime of the record is less than
             * the running datatime.
             */

            if (abs(offset_time_t) < prev_diff)
            {
                yearsec_dt_to_timet(pDSARadarNode->begin_time, &prev_begin_time);
                yearsec_dt_to_timet(pDSARadarNode->end_time, &prev_end_time);
                strncpy(prev_filename, pDSARadarNode->grid_filename, 19);
		
		        /*sprintf(message, "STATUS: Build the 2nd record - The dual pol DSA record datetime is less than the running datetime."
                                  "prev_filename is %s.", prev_filename) ;
                hpe_fieldgen_printMessage(message);*/

                prev_diff = abs(offset_time_t) ;
            }
            pDSARadarNode = ( DSARadar * ) ListNext( &pDSARadarNode->node);
            continue;
        } else if (offset_time_t > 0)
        {
            /*
             * found record after the run time,
             * build the last record by interpolation,
             */

            foundLaterRecord = 1;

            /*
             * the datetime of the record is great than
             * the running datatime.
             */

            if (offset_time_t < post_diff)
            {
                yearsec_dt_to_timet(pDSARadarNode->begin_time, &post_begin_time);
                yearsec_dt_to_timet(pDSARadarNode->end_time, &post_end_time);
                strncpy(post_filename, pDSARadarNode->grid_filename, 19);
		
		        /*sprintf(message, "STATUS: Build the 2nd record - The dual pol DSA record datetime is greater than the running datetime."
                                  "post_filename is %s.", post_filename) ;
                hpe_fieldgen_printMessage(message);*/
                
                post_diff = offset_time_t;
            }
            break;
        }
    }

    /*
     * load the radar data for the previous record.
     */

    if (strlen(prev_filename) > 0)
    {
        readDecodedDSA(prev_filename, ptrPrevRadar, &status);

        if (status != 0)
        {
            /*sprintf(message, "ERROR: Build the 2nd record - #%d encountered reading"
                    " dual pol DSA radar file as prev_filename = %s -- missing data substituted.", status,
                    prev_filename) ;
            hpe_fieldgen_printMessage(message);*/

            fill2DFloatArray(radar, RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);
            *radarAvailFlag = 0;

            pDSARadarNode = NULL;
            if (pDSARadarHead != NULL)
            {
                FreeDSARadar(pDSARadarHead);
                pDSARadarHead = NULL;
            }
            freeDSAMemory() ;

            return;
        }
    } else
    {
        /*sprintf(message, "STATUS: Build the 2nd record - no dual pol DSA radar file as prev_filename: %s", prev_filename) ;
        hpe_fieldgen_printMessage(message);*/
        fill2DFloatArray(radar, RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);
        *radarAvailFlag = 0;

        pDSARadarNode = NULL;
        if (pDSARadarHead != NULL)
        {
            FreeDSARadar(pDSARadarHead);
            pDSARadarHead = NULL;
        }

        freeDSAMemory() ;

        return;
    }

    /*
     * there is no record after the run time and
     * the obstime of latest record before the run time
     * is less than the default lag time,
     * use the latest record as the last record.
     * 
     * -- gzhou 06/2007 
     * 
     */

    if ( (foundLaterRecord == 0) && (prev_diff < RADAR_LAG_TIME))
    {
        /*sprintf(message, "STATUS: no dual pol DSA radar data after the run time. "
               "Use the latest record before the run time.");
        hpe_fieldgen_printMessage(message);*/

        useLatestRadar = 1;
    }

    if (useLatestRadar == 0)
    {

        /*
         * load the radar data for the post record.
         */

        if (strlen(post_filename) > 0)
        {
            readDecodedDSA(post_filename, ptrPostRadar, &status);

            if (status != 0)
            {
                /*sprintf(message, "ERROR: Build the 2nd record - #%d encountered reading"
                       " dual pol DSA radar file as post_filename = %s -- missing data substituted.", status,
                        post_filename) ;
                hpe_fieldgen_printMessage(message);*/
                fill2DFloatArray(radar, RADAR_DEFAULT, NUM_DSP_ROWS,
                        NUM_DSP_COLS);
                *radarAvailFlag = 0;

                pDSARadarNode = NULL;
                if (pDSARadarHead != NULL)
                {
                    FreeDSARadar(pDSARadarHead);
                    pDSARadarHead = NULL;
                }

                freeDSAMemory() ;

                return;
            }
        } else
        {
            /*sprintf(message, "STATUS: Build the 2nd record - no dual pol DSA radar file as post_filename: %s", post_filename) ;
            hpe_fieldgen_printMessage(message);*/
            fill2DFloatArray(radar, RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);

            pDSARadarNode = NULL;
            if (pDSARadarHead != NULL)
            {
                FreeDSARadar(pDSARadarHead);
                pDSARadarHead = NULL;
            }
            freeDSAMemory() ;

            *radarAvailFlag = 0;
            return;
        }
    }

    buildLastRadarRecord(end_runtime, ptrPrevRadar, prev_begin_time,
            prev_end_time, ptrPostRadar, post_begin_time, post_end_time,
            useLatestRadar, ptrLastRadar, &last_begin_time, &last_end_time);

    if (last_end_time == -999)
    {
        /*sprintf(message, "STATUS: can not build the last DSA record") ;
        hpe_fieldgen_printMessage(message);*/
        fill2DFloatArray(radar, RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);
        *radarAvailFlag = 0;

        pDSARadarNode = NULL;
        if (pDSARadarHead != NULL)
        {
            FreeDSARadar(pDSARadarHead);
            pDSARadarHead = NULL;
        }

        freeDSAMemory() ;

        return;
    }

    /* 
     * Calculate the raw mean bias and get the max scale factor
     * based on the start runtime and the end runtime.
     * 
     * remove the raw mean bias from the radar data.
     */

    double raw_mean_bias;
    double min_scale_factor;

    getDSAParams(radid, start_runtime, end_runtime, &raw_mean_bias,
            &min_scale_factor);

    sprintf(
            message,
            "STATUS: loaded minimum scale factor from the DSARadar table is: %f ",
            min_scale_factor) ;
    hpe_fieldgen_printMessage(message);

    sprintf(message,
            "STATUS: removing the raw mean bias %f from the DSA record...",
            raw_mean_bias) ;
    hpe_fieldgen_printMessage(message);

    /* 
     * Start to do differencing of the last DSA record
     * and the first DSA record.
     * If they have same begin time, it's same rain event and simply do a 
     * substraction. Otherwise, search all records between them.
     * 
     * remove the raw bias value if applied. 
     */

    if (last_begin_time == first_begin_time)
    {
        for (i = 0; i < NUM_DSP_ROWS; i++)
        {
            for (j = 0; j < NUM_DSP_COLS; j++)
            {
                if ( (ptrLastRadar[i][j] >= 0.0)
                        && (ptrFirstRadar[i][j] >= 0.0))
                {
                    radar[i][j] = (ptrLastRadar[i][j] - ptrFirstRadar[i][j]);

                    /*
                     * Added by gzhou 04-01-2008
                     */

                    if (fabs(radar[i][j]) <= (1.0/min_scale_factor) * 0.01 * 25.4)
                    {
                        radar[i][j] = 0.0;
                    }
                }
            }
        }

        *radarAvailFlag = 1;

        if (fabs(raw_mean_bias - 1.0) > 0.001)
        {
            for (i = 0; i < NUM_DSP_ROWS; i++)
            {
                for (j = 0; j < NUM_DSP_COLS; j++)
                {
                    if (radar[i][j] > 0)
                    {
                        radar[i][j] /= raw_mean_bias;
                    }
                }
            }
        }

        pDSARadarNode = NULL;
        if (pDSARadarHead != NULL)
        {
            FreeDSARadar(pDSARadarHead);
            pDSARadarHead = NULL;
        }
        freeDSAMemory() ;

        return;
    } else if (last_begin_time < first_begin_time)
    {
        /*sprintf(message,
                "ERROR: can not build this radar rainfall due to begin time"
                    " of the last DSA record is less than the first record.") ;
        hpe_fieldgen_printMessage(message);*/
        fill2DFloatArray(radar, RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);

        *radarAvailFlag = 0;

        pDSARadarNode = NULL;
        if (pDSARadarHead != NULL)
        {
            FreeDSARadar(pDSARadarHead);
            pDSARadarHead = NULL;
        }

        freeDSAMemory() ;

        return;
    }

    /*
     * search for record(s) within the dsp_duration range
     */

    timet_to_yearsec_ansi(start_runtime, str_start_runtime);
    timet_to_yearsec_ansi(end_runtime, str_end_runtime);

    sprintf(where, "WHERE radid = '%s' AND obstime >= '%s' "
           "AND obstime <= '%s' ORDER BY obstime", radid, str_start_runtime,
            str_end_runtime) ;

    pDSARadarNode = NULL;
    if (pDSARadarHead != NULL)
    {
        FreeDSARadar(pDSARadarHead);
        pDSARadarHead = NULL;
    }

    pDSARadarHead = GetDSARadar(where);

    if (pDSARadarHead == NULL)
    {
        /*
         * there is no record found. Something is wrong.
         */

        sprintf(message, "STATUS: no dual pol DSA radar data in duration time.") ;
        hpe_fieldgen_printMessage(message);
        fill2DFloatArray(radar, RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);

        *radarAvailFlag = 0;
        freeDSAMemory() ;

        return;
    } else
    {
        pDSARadarNode = ( DSARadar * ) ListFirst(&pDSARadarHead->list) ;
    }

    prev_begin_time = first_begin_time;
    prev_end_time = first_begin_time;

    while (pDSARadarNode != NULL)
    {
        yearsec_dt_to_timet(pDSARadarNode->obstime, &obstime_t);
        yearsec_dt_to_timet(pDSARadarNode->begin_time, &post_begin_time);
        yearsec_dt_to_timet(pDSARadarNode->end_time, &post_end_time);
        strncpy(post_filename, pDSARadarNode->grid_filename, 19);

        if (post_begin_time == prev_begin_time) /* Same rain event */
        {
            /*
             * Do nothing, just go to next DSA.
             */
        } else /*Rain stopped or new rain event started */
        {
            /*
             * same rain event as the first DSA record
             */

            if (prev_begin_time == first_begin_time)
            {
                if (strlen(prev_filename) > 0)
                {
                    readDecodedDSA(prev_filename, ptrPrevRadar, &status);

                    if (status != 0)
                    {
                        sprintf(message, "ERROR: #%d encountered reading"
                            " radar file = %s -- missing data substituted.",
                                status, post_filename) ;
                        hpe_fieldgen_printMessage(message);
                        fill2DFloatArray(radar, RADAR_DEFAULT, NUM_DSP_ROWS,
                                NUM_DSP_COLS);
                        *radarAvailFlag = 0;

                        pDSARadarNode = NULL;
                        if (pDSARadarHead != NULL)
                        {
                            FreeDSARadar(pDSARadarHead);
                            pDSARadarHead = NULL;
                        }

                        freeDSAMemory() ;
                        return;
                    }
                } else
                {
                    sprintf(message, "ERROR: no dual pol DSA radar file as prev_filename: %s", prev_filename) ;
                    hpe_fieldgen_printMessage(message);
                    *radarAvailFlag = 0;
                    fill2DFloatArray(radar, RADAR_DEFAULT, NUM_DSP_ROWS,
                            NUM_DSP_COLS);

                    pDSARadarNode = NULL;
                    if (pDSARadarHead != NULL)
                    {
                        FreeDSARadar(pDSARadarHead);
                        pDSARadarHead = NULL;
                    }

                    freeDSAMemory() ;

                    return;
                }

                for (i = 0; i < NUM_DSP_ROWS; i++)
                {
                    for (j = 0; j < NUM_DSP_COLS; j++)
                    {
                        if ( (ptrPrevRadar[i][j] >= 0.0)
                                && (ptrFirstRadar[i][j] >= 0.0))
                        {
                            radar[i][j] = (ptrPrevRadar[i][j]
                                    - ptrFirstRadar[i][j]);				                               			    

                        }
                    }
                }
            }

            /*
             * prev_begin_time != first_begin_time, no rain or another
             * rain event in the middle
             */

            else
            {
                /*
                 * no rain or new rain event just started
                 */

                if (prev_begin_time == prev_end_time)
                {
                    /*
                     * Do nothing, just go to next DSA.
                     */
                } else
                {
                    /*
                     * middle rain event ends. Add the rainfall of this middle
                     * rain event to total rainfall.
                     */

                    if (post_begin_time == post_end_time)
                    {
                        if (strlen(prev_filename) > 0)
                        {
                            readDecodedDSA(prev_filename, ptrPrevRadar, &status);

                            if (status != 0)
                            {

                                sprintf(message,
                                        "ERROR: #%d encountered reading"
                                            " radar file = %s"
                                            " -- missing data substituted.",
                                        status, post_filename) ;
                                hpe_fieldgen_printMessage(message);
                                *radarAvailFlag = 0;
                                fill2DFloatArray(radar, RADAR_DEFAULT,
                                        NUM_DSP_ROWS, NUM_DSP_COLS);

                                pDSARadarNode = NULL;
                                if (pDSARadarHead != NULL)
                                {
                                    FreeDSARadar(pDSARadarHead);
                                    pDSARadarHead = NULL;
                                }

                                freeDSAMemory() ;

                                return;
                            }
                        } else
                        {
                            sprintf(message, "ERROR: no dual pol radar file as prev_filename: %s",
                                    prev_filename);
                            hpe_fieldgen_printMessage(message);
                            *radarAvailFlag = 0;
                            fill2DFloatArray(radar, RADAR_DEFAULT,
                                    NUM_DSP_ROWS, NUM_DSP_COLS);

                            pDSARadarNode = NULL;
                            if (pDSARadarHead != NULL)
                            {
                                FreeDSARadar(pDSARadarHead);
                                pDSARadarHead = NULL;
                            }

                            freeDSAMemory() ;

                            return;
                        }

                        for (i = 0; i < NUM_DSP_ROWS; i++)
                        {
                            for (j = 0; j < NUM_DSP_COLS; j++)
                            {
                                if ( (ptrPrevRadar[i][j] >= 0.0)
                                        && (radar[i][j] >= 0.0))
                                {
                                    radar[i][j] += ptrPrevRadar[i][j];				    				    
                                }
                            }
                        }

                    } else /* there are lost DSA records, check the gap time. */
                    {
                        gap_percent = 100 * (post_end_time - prev_end_time)
                                / duration_t;
                        if ((gap_percent < 0) || (gap_percent > 10))
                        {
                            sprintf(message, "STATUS: "
                                "gap time is more than 10%% of duration\n") ;
                            hpe_fieldgen_printMessage(message);
                            *radarAvailFlag = 0;
                            fill2DFloatArray(radar, RADAR_DEFAULT,
                                    NUM_DSP_ROWS, NUM_DSP_COLS);

                            pDSARadarNode = NULL;
                            if (pDSARadarHead != NULL)
                            {
                                FreeDSARadar(pDSARadarHead);
                                pDSARadarHead = NULL;
                            }

                            freeDSAMemory() ;

                            return;
                        } else
                        {
                            if (strlen(prev_filename) > 0)
                            {
                                readDecodedDSA(prev_filename, ptrPrevRadar,
                                        &status);

                                if (status != 0)
                                {
                                    sprintf(
                                            message,
                                            "ERROR: #%d encountered reading"
                                                " radar file = %s"
                                                " -- missing data substituted.",
                                            status, post_filename) ;
                                    hpe_fieldgen_printMessage(message);
                                    *radarAvailFlag = 0;
                                    fill2DFloatArray(radar, RADAR_DEFAULT,
                                            NUM_DSP_ROWS, NUM_DSP_COLS);

                                    pDSARadarNode = NULL;
                                    if (pDSARadarHead != NULL)
                                    {
                                        FreeDSARadar(pDSARadarHead);
                                        pDSARadarHead = NULL;
                                    }

                                    freeDSAMemory() ;

                                    return;
                                }

                            } else
                            {
                                sprintf(message, "ERROR: no dual pol DSA radar file as prev_filename: %s",
                                        prev_filename);
                                hpe_fieldgen_printMessage(message);
                                *radarAvailFlag = 0;
                                fill2DFloatArray(radar, RADAR_DEFAULT,
                                        NUM_DSP_ROWS, NUM_DSP_COLS);

                                pDSARadarNode = NULL;
                                if (pDSARadarHead != NULL)
                                {
                                    FreeDSARadar(pDSARadarHead);
                                    pDSARadarHead = NULL;
                                }

                                freeDSAMemory() ;

                                return;
                            }

                            for (i = 0; i < NUM_DSP_ROWS; i++)
                            {
                                for (j = 0; j < NUM_DSP_COLS; j++)
                                {
                                    if ( (ptrPrevRadar[i][j] >= 0.0)
                                            && (radar[i][j] >= 0.0))
                                    {
                                        radar[i][j] += ptrPrevRadar[i][j];										
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        /*
         * Check if the DSA with same begin_time of the last DSA record
         * is found. If it is, no more searching is needed. Otherwise, go to 
         * next dsa record. 
         */

        if (post_begin_time == last_begin_time)
        {
            for (i = 0; i < NUM_DSP_ROWS; i++)
            {
                for (j = 0; j < NUM_DSP_COLS; j++)
                {
                    if ( (ptrLastRadar[i][j] >= 0.0) && (radar[i][j] >= 0.0))
                    {
                        radar[i][j] += ptrLastRadar[i][j];						
                    }
                }
            }

            /*
             * Added by gzhou 04-01-2008
             */
            for (i = 0; i < NUM_DSP_ROWS; i++)
            {
                for (j = 0; j < NUM_DSP_COLS; j++)
                {
                    if (fabs(radar[i][j]) <= (1.0/min_scale_factor) * 0.01 * 25.4)
                    {
                        radar[i][j] = 0.0;
                    }
                }
            }

            if (fabs(raw_mean_bias - 1.0) > 0.001)
            {
                for (i = 0; i < NUM_DSP_ROWS; i++)
                {
                    for (j = 0; j < NUM_DSP_COLS; j++)
                    {
                        if (radar[i][j] > 0)
                        {
                            radar[i][j] /= raw_mean_bias;
                        }
                    }
                }
            }

            pDSARadarNode = NULL;
            if (pDSARadarHead != NULL)
            {
                FreeDSARadar(pDSARadarHead);
                pDSARadarHead = NULL;
            }

            freeDSAMemory() ;

            *radarAvailFlag = 1;

            return;
        } else
        {
            prev_begin_time = post_begin_time;
            prev_end_time = post_end_time;
            strncpy(prev_filename, post_filename, 19);

            pDSARadarNode = ( DSARadar * ) ListNext( &pDSARadarNode->node) ;
            continue;
        }
    }
}

static void allocDSAMemory()
{

    ptrPrevRadar = init2DFloatArray(RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);

    ptrPostRadar = init2DFloatArray(RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);

    ptrFirstRadar= init2DFloatArray(RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);

    ptrLastRadar = init2DFloatArray(RADAR_DEFAULT, NUM_DSP_ROWS, NUM_DSP_COLS);
}

static void freeDSAMemory()
{

    free2DFloatArray(ptrPrevRadar, NUM_DSP_ROWS);

    free2DFloatArray(ptrPostRadar, NUM_DSP_ROWS);

    free2DFloatArray(ptrFirstRadar, NUM_DSP_ROWS);

    free2DFloatArray(ptrLastRadar, NUM_DSP_ROWS);
}

static void getDSAParams(const char * radid, const time_t first_obstime,
        const time_t last_obstime, double * mean_bias, double * min_scale_factor)
{
    char firstDateTime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    char lastDateTime[ANSI_YEARSEC_TIME_LEN + 1] = { '\0' };
    char where[100] = { '\0' };

    DSAAdapt * pDSAAdaptHead = NULL;
    DSAAdapt * pDSAAdaptNode = NULL;

    DSARadar * pDSARadarHead = NULL;
    DSARadar * pDSARadarNode = NULL;

    *mean_bias = 0.0;   
    *min_scale_factor = 9999;

    int count = 0;

    timet_to_yearsec_ansi(first_obstime, firstDateTime);
    timet_to_yearsec_ansi(last_obstime, lastDateTime);

    sprintf(where, "WHERE radid='%s' AND obstime>='%s' AND obstime<='%s'"
            " ORDER BY obstime ASC", radid, firstDateTime, lastDateTime) ;

    /*
     * search for records between the first obstime and the last obstime
     * from DSAAdapt table
     */

    pDSAAdaptHead = GetDSAAdapt(where);

    if (pDSAAdaptHead == NULL)
    {

        /*
         * there is no record found.
         */

        sprintf(message, "STATUS: No DSA radar adapt data between %s and %s.",
                firstDateTime, lastDateTime) ;
        hpe_fieldgen_printMessage(message);

        *mean_bias = 1.0;
        return;
    } else
    {
        pDSAAdaptNode = ( DSAAdapt * ) ListFirst(&pDSAAdaptHead->list) ;
    }

    /*
     * search for records between the first obstime and the last obstime
     * from DSARadar table
     */

    pDSARadarHead = GetDSARadar(where);

    if (pDSARadarHead == NULL)
    {

        /*
         * There is no record found.
         * Should not enter here.
         */

        sprintf(message, "STATUS: No DSA radar data between %s and %s.",
                firstDateTime, lastDateTime) ;
        hpe_fieldgen_printMessage(message);

        pDSAAdaptNode = NULL;
        if (pDSAAdaptHead != NULL)
        {
            FreeDSAAdapt(pDSAAdaptHead) ;
            pDSAAdaptHead = NULL;
        }
        *mean_bias = 1.0;
        return;
    } else
    {
        pDSARadarNode = ( DSARadar * ) ListFirst(&pDSARadarHead->list) ;
    }

    while ( (pDSAAdaptNode != NULL ) && (pDSARadarNode != NULL ))
    {
        if (pDSARadarNode->obstime == pDSAAdaptNode->obstime)
        {
           /* if (pDSARadarNode->scale > *max_scale_factor)*/
	    if (pDSARadarNode->scale < *min_scale_factor)
            {
                *min_scale_factor = pDSARadarNode->scale;
            }

          /*  if ( (pDSAAdaptNode->bias_applied[0] == 'T')
                    || (pDSAAdaptNode->bias_applied[0] == 't'))
            {
                *mean_bias += (double)pDSARadarNode->mean_field_bias / 100.0;
            } else
            {*/
                *mean_bias += 1.0;
		
       /*     } */
            count ++;
        } else
        {
            sprintf(message, "ERROR: DSA radar record does not match "
                "the DSA Adapt record.") ;
            hpe_fieldgen_printMessage(message);
            count = 0;
            break;
        }

        pDSARadarNode = ( DSARadar * ) ListNext( &pDSARadarNode->node) ;
        pDSAAdaptNode = ( DSAAdapt * ) ListNext( &pDSAAdaptNode->node) ;
    }
   
    
    if (count > 0)
    {
        *mean_bias /= count;

        if (*mean_bias < 0.001)
        {
            *mean_bias = 1.0;
        }
        sprintf(message, "STATUS: DSA raw mean bias: %f "
                "calculated from %d record(s).", *mean_bias, count) ;
        hpe_fieldgen_printMessage(message);
    } else
    {
        *mean_bias = 1.0;
    }

    pDSAAdaptNode = NULL;
    if (pDSAAdaptHead != NULL)
    {
        FreeDSAAdapt(pDSAAdaptHead) ;
        pDSAAdaptHead = NULL;
    }

    pDSARadarNode = NULL;
    if (pDSARadarHead != NULL)
    {
        FreeDSARadar(pDSARadarHead) ;
        pDSARadarHead = NULL;
    }

    return;
}

static void buildFirstRadarRecord(const time_t run_time, float ** pPrevRadar,
        const time_t prev_begin_time, const time_t prev_end_time,
        float ** pPostRadar, const time_t post_begin_time,
        const time_t post_end_time, float ** pFirstRadar, time_t * begin_time,
        time_t * end_time)
{
    int i, j;

    float prev_weight, post_weight;

    *begin_time = -1;
    *end_time = -1;

    /*
     * build the radar array by interpolation
     * when the previous/post radar records
     * have the same start time.
     * 
     * Set the begin time of this built DSA array as that of post record,
     * and the end time as the run time.
     */

    if (prev_begin_time == post_begin_time)
    {
        if (post_end_time != prev_end_time)
        {

            post_weight = (float)(run_time - prev_end_time)
                    / (float)(post_end_time - prev_end_time) ;

            prev_weight = 1.0 - post_weight;

            for (i = 0; i < NUM_DSP_ROWS; i++)
            {
                for (j = 0; j < NUM_DSP_COLS; j++)
                {
                    if ( (pPrevRadar[i][j] >= 0.0) && (pPostRadar[i][j] >= 0.0))
                    {
                        pFirstRadar[i][j] = prev_weight * pPrevRadar[i][j]
                                + post_weight * pPostRadar[i][j];
                    }
                }
            }
        } else /* find a DSA at start_runtime  */
        {
            for (i = 0; i < NUM_DSP_ROWS; i++)
            {
                for (j = 0; j < NUM_DSP_COLS; j++)
                {
                    if (pPostRadar[i][j] >= 0.0)
                    {
                        pFirstRadar[i][j] = pPostRadar[i][j];
                    }
                }
            }
        }

        *begin_time = post_begin_time;
        *end_time = run_time;
    } else if (prev_begin_time < post_begin_time)
    {
        /*
         * the prev record and the post record have different begin time, and
         * the begin time of the post record is great than the previous record.
         * 
         * If the begin time of the post record is great than the run time,
         * this means no rain and fill the array with 0.0.
         * Otherwise, fill the array with the interpolation
         * part from the post record if the rain just started;
         * Fill the array with 0.0 if no rain.
         * Set the begin time of this built DSA array as that of post record,
         * and the end time as the run time.
         * 
         */

        if (post_begin_time >= run_time) /* no rain */
        {
            for (i = 0; i < NUM_DSP_ROWS; i++)
            {
                for (j = 0; j < NUM_DSP_COLS; j++)
                {
                    if (pPostRadar[i][j] >= 0.0)
                    {
                        pFirstRadar[i][j] = 0.0;
                    }
                }
            }
        } else
        {
            if (post_begin_time != post_end_time) /* rain just started */
            {
                post_weight = (run_time - post_begin_time) / (post_end_time
                        - post_begin_time);

                for (i = 0; i < NUM_DSP_ROWS; i++)
                {
                    for (j = 0; j < NUM_DSP_COLS; j++)
                    {
                        if (pPostRadar[i][j] >= 0.0)
                        {
                            pFirstRadar[i][j] = post_weight * pPostRadar[i][j];
                        }
                    }
                }
            } else /* no rain */
            {
                for (i = 0; i < NUM_DSP_ROWS; i++)
                {
                    for (j = 0; j < NUM_DSP_COLS; j++)
                    {
                        if (pPostRadar[i][j] >= 0.0)
                        {
                            pFirstRadar[i][j] = 0.0;
                        }
                    }
                }
            }
        }

        *begin_time = post_begin_time;
        *end_time = run_time;
    } else
    {
        /*
         * the prev record and the post record have different begin time, and
         * the begin time of the post record is less than the begin time of 
         * prev record, it's wrong. Set end tim to -999.
         */

        *end_time = -999;
    }
}

static void buildLastRadarRecord(const time_t run_time, float ** pPrevRadar,
        const time_t prev_begin_time, const time_t prev_end_time,
        float ** pPostRadar, const time_t post_begin_time,
        const time_t post_end_time, const int useLatestRadar,
        float ** pLastRadar, time_t * begin_time, time_t * end_time)
{
    int i, j;

    float prev_weight, post_weight;

    /*
     * If there is no record after the run time,
     * use the latest record before the run time
     * as the last record.
     * 
     * Set the begin time of this built DSA array as that of previous record,
     * and the end time as the run time.
     * -- gzhou 06/2007
     */

    if (useLatestRadar == 1)
    {
        for (i = 0; i < NUM_DSP_ROWS; i++)
        {
            for (j = 0; j < NUM_DSP_COLS; j++)
            {
                if (pPrevRadar[i][j] >= 0.0)
                {
                    pLastRadar[i][j] = pPrevRadar[i][j];
                }
            }
        }

        *begin_time = prev_begin_time;
        *end_time = prev_end_time;
        return;
    }

    *begin_time = -1;
    *end_time = -1;

    /*
     * build the radar array by interpolation
     * when the previous/post radar records
     * have the same start time.
     * 
     * Set the begin time of this built DSA array as that of previous record,
     * and the end time as the run time.
     */

    if (prev_begin_time == post_begin_time)
    {
        if (post_end_time != prev_end_time)
        {
            post_weight = (float)(run_time - prev_end_time)
                    / (float)(post_end_time - prev_end_time) ;

            prev_weight = 1.0 - post_weight;

            for (i = 0; i < NUM_DSP_ROWS; i++)
            {
                for (j = 0; j < NUM_DSP_COLS; j++)
                {
                    if ( (pPrevRadar[i][j] >= 0.0) && (pPostRadar[i][j] >= 0.0))
                    {
                        pLastRadar[i][j] = prev_weight * pPrevRadar[i][j]
                                + post_weight * pPostRadar[i][j];
                    }
                }
            }
        } else /* find a DSA at end_runtime  */
        {
            for (i = 0; i < NUM_DSP_ROWS; i++)
            {
                for (j = 0; j < NUM_DSP_COLS; j++)
                {
                    if (pPrevRadar[i][j] >= 0.0)
                    {
                        pLastRadar[i][j] = pPrevRadar[i][j];
                    }
                }
            }
        }

        *begin_time = prev_begin_time;
        *end_time = run_time;

    } else if (prev_begin_time < post_begin_time)
    {
        /*
         * The prev record and the post record have different begin time, and
         * the begin time of the post record is great than that of previous
         * record.
         * 
         * If the begin time of the post record is great than the run time, 
         * rain just ended and fill the array with prev record.
         * Otherwise, fill the array with prev record or plus the interpolation
         * part from the post record.
         * 
         * Set the begin time of this DSA array as that of previous record,
         * and the end time as the run time.
         * 
         */

        if (post_begin_time >= run_time) /* rain just ended */
        {
            for (i = 0; i < NUM_DSP_ROWS; i++)
            {
                for (j = 0; j < NUM_DSP_COLS; j++)
                {
                    if (pPrevRadar[i][j] >= 0.0)
                    {
                        pLastRadar[i][j] = pPrevRadar[i][j];
                    }
                }
            }
        } else
        {
            if (post_begin_time != post_end_time) /* new rain just started */
            {
                post_weight = (run_time - post_begin_time) / (post_end_time
                        - post_begin_time);

                for (i = 0; i < NUM_DSP_ROWS; i++)
                {
                    for (j = 0; j < NUM_DSP_COLS; j++)
                    {
                        if (pPrevRadar[i][j] >= 0.0)
                        {
                            pLastRadar[i][j] = pPrevRadar[i][j] + post_weight
                                    * pPostRadar[i][j];
                        }
                    }
                }
            } else /* rain just ended */
            {
                for (i = 0; i < NUM_DSP_ROWS; i++)
                {
                    for (j = 0; j < NUM_DSP_COLS; j++)
                    {
                        if (pPrevRadar[i][j] >= 0.0)
                        {
                            pLastRadar[i][j] = pPrevRadar[i][j];
                        }
                    }
                }
            }
        }

        *begin_time = prev_begin_time;
        *end_time = run_time;
    } else
    {
        /*
         * the prev record and the post record have
         * different begin time, and
         * the begin time of the post record is
         * less than the begin time of prev record,
         * it's wrong. Set end tim to -999.
         */

        *end_time = -999;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc/src/hpe_fieldgen/RCS/read_dsa_radar.c,v $";
 static char rcs_id2[] = "$Id: read_dsa_radar.c,v 1.3 2012/11/20 21:44:53 deng Exp $";}
/*  ===================================================  */

}

