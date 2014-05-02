/***********************************************************************
* Filename: read_dpr_data.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: July 26, 2006
*
* Development Group: OHD/HSEB
*
* Description:
* Contains routine for creating rain rate at product target time
* from decoded dpr radar data.
* 
*
* Modules:
* readDHRData
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include "empe_fieldgen.h"
#define ACC_MIN 0.01

/***********************************************************************
* Module Name: readDPRData
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: July 26, 2006
*
* Description:
*   This subroutine creates rain rate at product target time by interpolating 
*   two nearby DHR or using latest DHR.
*
* calling function: runDHRMosaic
*
* Calling Arguments:
* Name         Input/Output Type          Description
* radarID      Input        const char *  radar id
* datetime     Input        const char *  date and time of current run
* dhr_wind     Input        const int     dhr search window value
* ignoreRadarFlag
*              Input        const int     ignore radar flag
*                                         = 0 -- use radar (default)
*                                         = 1 -- ignore radar
* radar        Output       float **      two-dimensional array of radar data
* radarAvailFlag
*              Output       int *         radar availability flag
*                                         = 0 -- no radar data available
*                                         = 1 -- radar data available
*                                               (some values > 0.0)
*                                         = 2 -- radar data available
*                                                (all values = 0.0)
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* readDHRRadar, readDecodedDHR, interpolateArray,
* init2DFloatArray
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
* 07/27/2006   Guoxian Zhou  First version
* 12/08/2006   Guoxian Zhou  Using dynamic memory allocation
*                            for radar arrays.  
* 06/26/2006   Guoxian Zhou  Using the latest record before the run time 
*                            when there is no record after the run time 
***********************************************************************/

void readDPRData(const char * radarID,
                 const char * datetime,
                 const int    dhr_wind, 
                 const int    ignoreRadarFlag,
                 float ** radar,
                 int *    radarAvailFlag)
{
    char    filename[FNAME_LEN] = {'\0'} ;
    int     i, j ;
    int     status = 0 ;

    double prev_bias, post_bias;
    char prev_filename[FNAME_LEN] = {'\0'} ;
    char post_filename[FNAME_LEN] = {'\0'} ;
    int prev_offset, post_offset, dhr_wind_sec;

    float ** prev_radar = NULL;
    float ** post_radar = NULL;

    int tmp_rows;
    int tmp_cols;
    float missingValue = RADAR_DEFAULT;
    float prev_weight;
    float post_weight;

    *radarAvailFlag = 0 ;
    if (dhr_wind <= 0)
    {
       dhr_wind_sec = 600; /* 10 minutes */
    }
    else
    {
       dhr_wind_sec = 60 * dhr_wind;
    }
    
    /*
     * initialize the radar array
     */

    fill2DFloatArray(radar, RADAR_DEFAULT, NUM_DHR_ROWS, NUM_DHR_COLS);

    prev_radar = init2DFloatArray(RADAR_DEFAULT, NUM_DHR_ROWS, NUM_DHR_COLS);
    post_radar = init2DFloatArray(RADAR_DEFAULT, NUM_DHR_ROWS, NUM_DHR_COLS);

    /*
     * if ignore radar flag set ON, 
     * return all missing values
     */

    if(ignoreRadarFlag == 1)
    {
        sprintf ( message , "STATUS: Dual Pol DPR radar product marked as ignored. Stop.") ;
        hpe_fieldgen_printMessage( message);

        return ;
    }

    /*
     * read record from DPRRadar table
     */

    readDPRRadar(radarID, datetime, dhr_wind,
                 &prev_bias, &post_bias,
                 prev_filename, post_filename,
                 &prev_offset, &post_offset, &status) ;

    if( status == 100 )
    {
        sprintf ( message , "STATUS: no DPR radar data for current run time.") ;
        hpe_fieldgen_printMessage( message);
    }
    else if(status == 0)
    {
        if( (prev_offset == 0) && (post_offset == 0) )
        {
            *radarAvailFlag = 1 ;

            /*
             * find a record with the same datetime 
             * as the running datetime.
             * 
             * Read decoded dpr file.
             */

            readDecodedDPR(prev_filename, prev_radar, &status);

            if(status != 0)
            {
                sprintf ( message , "ERROR: #%d encountered reading radar file"
                                    " = %s -- missing data substituted.",
                                    status, prev_filename ) ;
                hpe_fieldgen_printMessage( message);
            }
            else
            {
                for( i = 0; i < NUM_DHR_ROWS; i++)
                {
                    for( j = 0; j < NUM_DHR_COLS; j++)
                    {
                        if(prev_radar[i][j] < 0.0 )
                        {
                            radar[i][j] = RADAR_DEFAULT ;
                        }
                        else if(prev_radar[i][j] < ACC_MIN)
                        {
                            radar[i][j] = 0.0 ;
                        }
                        else
                        {
                            radar[i][j] = prev_radar[i][j] ;
                        }
                    }
                }
            }
        }
        else if( (prev_offset < INT_MAX) &&
                 (post_offset < INT_MAX))
        {
            *radarAvailFlag = 1 ;

            /*
             * find two records before/after
             * the running datetime.
             * 
             * Need load two radar products
             * and then interpolate. 
             * 
             * Read decoded dhr file.
             */

            readDecodedDPR(prev_filename, prev_radar, &status);
    
            if(status != 0)
            {
                sprintf ( message , "ERROR: #%d encountered reading radar file"
                            " = %s -- missing data substituted.",
                            status, prev_filename ) ;
                hpe_fieldgen_printMessage( message);
                return;
            }

            readDecodedDPR(post_filename, post_radar, &status);

            if(status != 0)
            {
                sprintf ( message , "ERROR: #%d encountered reading radar file"
                            " = %s -- missing data substituted.",
                            status, post_filename ) ;
                hpe_fieldgen_printMessage( message);
                return;
            }

            tmp_rows = NUM_DHR_ROWS;
            tmp_cols = NUM_DHR_COLS;
            prev_weight = (float)post_offset;
            post_weight = (float)prev_offset;

            interpolateArray(tmp_rows, tmp_cols, prev_radar, 
                            post_radar, missingValue, 
                            prev_weight, post_weight,
                            radar);
        }
        else if( (prev_offset < dhr_wind_sec) &&
                 (post_offset == INT_MAX))
        {
            sprintf ( message , "STATUS: no radar data after the run time.\n"
                                "Using the latest record before the run time "
                                "within %d minutes, filename %s.\n",
                                dhr_wind, prev_filename);
            hpe_fieldgen_printMessage( message);

            *radarAvailFlag = 1 ;

            /*
             * find a record with the latest datetime 
             * before the running datetime and within the time window.
             * 
             * Read decoded dpr file.
             */

            readDecodedDPR(prev_filename, prev_radar, &status);

            if(status != 0)
            {
                sprintf ( message , "ERROR: #%d encountered reading radar file"
                                    " = %s -- missing data substituted.",
                                    status, filename ) ;
                hpe_fieldgen_printMessage( message);
            }
            else
            {
                for( i = 0; i < NUM_DHR_ROWS; i++)
                {
                    for( j = 0; j < NUM_DHR_COLS; j++)
                    {
                        if(prev_radar[i][j] < 0.0 )
                        {
                            radar[i][j] = RADAR_DEFAULT ;
                        }
                        else if(prev_radar[i][j] < ACC_MIN)
                        {
                            radar[i][j] = 0.0 ;
                        }
                        else
                        {
                            radar[i][j] = prev_radar[i][j] ;
                        }
                    }
                }
            }
        }
        else
        {
            sprintf ( message , "STATUS: no radar data "
                                "within current run time range.") ;
            hpe_fieldgen_printMessage( message);
        }
    }

    free2DFloatArray(prev_radar, NUM_DHR_ROWS );
    free2DFloatArray(post_radar, NUM_DHR_ROWS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc/src/hpe_fieldgen/RCS/read_dpr_data.c,v $";
 static char rcs_id2[] = "$Id: read_dpr_data.c,v 1.2 2012/09/24 19:52:40 deng Exp $";}
/*  ===================================================  */

}
