/*******************************************************************************
* FILENAME:            read_radar_data.c
*
* Purpose:
* This function is converted from FORTRAN code: getradar.f.
* it reads the hourly radar precip accumulation array
* and converts all values from dba to mm.
*
* calling function: run_rmosaic
* functions called: read_dparadar, read_stage1_decoded_
*
* input variables
*
* datetime - date and time of current run
*          - minutes and seconds set to 00
*
* dpa_wind -  parameters
*
* ignoreDPARadar - Ignore DPA Radar Product Flag
*                - the DPA Radar product can be set to "ignored" via the 4-panel "single-site" window   
*                = 0 -- ignore flag not set (use radar) (default)
*                = 1 -- ignore flag set via 4-panel window
*
* output variables
*
* radar - two-dimensional array of radar data
*
* radarAvailFlag - radar availability flag
*     = 0 -- no radar data available
*     = 1 -- radar data available (some values > 0.0)
*     = 2 -- radar data available (all values = 0.0)
*
*
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   March 2005   Guoxian Zhou      finish conversion to C Language 
**
********************************************************************************
*/

#include "mpe_fieldgen.h"
#define ACC_MIN 0.01

void readRadarData(const char * radarID,
                const char * datetime,
                const int    dpa_wind, 
                const int    ignoreDPARadarFlag,
                float radar [ ] [ NUM_DPA_COLS ] ,
                int *    radarAvailFlag)
{
    char    fname[FNAME_LEN] = "" ;
    int     i, j ;
    int     itim ;
    double  bias, xmax ;
    long int irc = 0 ;
    
    /**
     * fname = filename of dpa product read
     *
     * the dpa product is coded as a value between 0 and 255.   0 and 255
     * are special values. when converting from level (0-255) to dba, a 
     * level of "0" is set to -98 dba.  -98 dba = 1.58489e-10 mm of 
     * accumulation.   this small positive value must be set to zero.
     * the acc_min theshold is used to check for values less than the 
     * minimum detectible value so that they can be set to zero.
     **/

    *radarAvailFlag = 0 ;

    /**
     * if ignore DPA radar flag set ON, 
     * return all missing values
     **/
    if(ignoreDPARadarFlag == 1)
    {
        for( i = 0; i < NUM_DPA_COLS; i++)
        {
            for( j = 0; j < NUM_DPA_COLS; j++)
                radar[i][j] = RADAR_DEFAULT ;
        }
        sprintf ( message , "STATUS: SP radar product marked as ignored.") ;
        printMessage(message, logFile);
        return ;
    }

    /**
     * read record from DPARadar table
     **/
    readDPARadar(radarID, datetime, dpa_wind, &xmax, &bias, 
        fname, &itim, &irc) ;

    if((irc == 100) || (xmax == -99.0))
    {
        sprintf ( message , "STATUS: no radar data for current hour.") ;
        printMessage(message, logFile);

        for( i = 0; i < NUM_DPA_COLS; i++)
        {
            for( j = 0; j < NUM_DPA_COLS; j++)
                radar[i][j] = RADAR_DEFAULT ;
        }
    }
    else if(irc != 0)
    {
        sprintf ( message , "ERROR: database error #%ld "
            "attempting select from DPARadar table.", irc ) ;
        printMessage(message, logFile);

        if(itim != 0)
        {
            sprintf ( message , "STATUS: no top-of-hour product found -- %d, "
                "product used instead.", itim ) ;
            printMessage(message, logFile);
        }
    }
    else if(xmax > 0.0)
    {
        *radarAvailFlag = 1 ;
        sprintf ( message , "Maximum radar value = %7.2f mm", xmax ) ;
        printMessage(message, logFile);

        if(itim != 0)
        {
            sprintf ( message , "STATUS: no top-of-hour product found -- %d, "
                "product used instead.", itim ) ;
            printMessage(message, logFile);
        }

        /**
         * Read decoded stage1 file AND CONVERT FROM DBA TO MM
         * function read_stage1_decoded converts from dba to mm
         * a radar value < 0.0 is the "out-of-range" value
         * and is considered missing
         **/

        int len = strlen(fname) ;
        int ierr = 0 ;

        read_stage1_decoded_(fname, &len, radar, &ierr) ;

        if(ierr != 0)
        {
            sprintf ( message , "ERROR: #%d encountered reading radar file = %s"
                        " -- missing data substituted.", ierr, fname ) ;
            printMessage(message, logFile);

            for( i = 0; i < NUM_DPA_COLS; i++)
            {
                for( j = 0; j < NUM_DPA_COLS; j++)
                    radar[i][j] = RADAR_DEFAULT ;
            }
        }
        else
        {
            for( i = 0; i < NUM_DPA_COLS; i++)
            {
                for( j = 0; j < NUM_DPA_COLS; j++)
                {
                    if(radar[i][j] < ACC_MIN)
                        radar[i][j] = 0.0 ;
                }
            }
        }
    }
    else if(xmax == 0.0)
    {
        *radarAvailFlag = 2 ;

        sprintf ( message , "STATUS: radar data all zero for current hour.") ;
        printMessage(message, logFile);

        if(itim != 0)
        {
            sprintf ( message , "STATUS: no top-of-hour product found -- %d, "
                "product used instead.", itim ) ;
            printMessage(message, logFile);
        }

        for( i = 0; i < NUM_DPA_COLS; i++)
        {
            for( j = 0; j < NUM_DPA_COLS; j++)
                radar[i][j] = 0.0 ;
        }
    }
}
