/***********************************************************************
* Filename: read_dprradar.c
*
* Original Author: Guoxian Zhou
*
* File Creation Date: July 26, 2006
*
* Development Group: OHD
*
* Description:
*   This searches the DPRRadar table for the
*    file name of gridded data file and maximum
*    value (from data) from a radar product
* 
* Modules:
* readDPRRadar
*
***********************************************************************/

/* Include files, definitions, globals go here. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DPRRadar.h"
#include "empe_fieldgen.h"

/***********************************************************************
* Module Name: readDPRRadar
*
* Original Author: Guoxian Zhou
*
* Module Creation Date: July 26, 2006
*
* Description:
*   This subroutine searches the DPRRadar table for the
*    file name of gridded data file and maximum
*    value (from data) from a radar product
* 
* Calling Arguments:
* Name          Input/Output Type          Description
* radid         Input        const char *  radar id
* datetime      Input        const char *  run time string
* dhr_window    Input        const int     dhr product search window value
* prev_bias     Output       double *      previous mean field bias value
* post_bias     Output       double *      post mean field bias value
* prev_filename Output       char *        previous DPR file name
* post_filename Output       char *        post DPR file name
* prev_offset   Output       int *         previous time offset value
* post_offset   Output       int *         post time offset value
* status        Output       int *         File read status
*
* Required
* None
*
* Required Files/Databases:
* None
*
* calling function: readDPRData
*
* Non System Routines Called:
* GetDPRRadar, yearsec_dt_to_timet
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
* Date        Developer     Action
* 7/26/2006   Guoxian Zhou  First version
* 05/2012     Jingtao Deng  Modify to DPR dual pol product
***********************************************************************/

void readDPRRadar(const char * radid, 
                  const char * datetime, 
                  const int dhr_window,
                  double * prev_bias,
                  double * post_bias,
                  char * prev_filename,
                  char * post_filename,
                  int * prev_offset,
                  int * post_offset,
                  int * status)
{

    char where [ 256 ] = {'\0'};

    char str_datetime[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    char start_time[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;
    char end_time[ANSI_YEARSEC_TIME_LEN + 1] = {'\0'} ;

    time_t curr_time_t, start_time_t, end_time_t;
    time_t obstime_t, offset_time_t, prev_diff, post_diff;
    
    DPRRadar * pDPRRadarHead = NULL ;
    DPRRadar * pDPRRadarNode = NULL ;

    /*
     * Initialize the parameters.
     */

    strcpy ( prev_filename, "" );
    strcpy ( post_filename, "" );
    *prev_bias = 1.0;
    *post_bias = 1.0;
    *prev_offset = INT_MAX;
    *post_offset = INT_MAX;
    *status = 0;

    strcpy(str_datetime, datetime);
    yearsec_ansi_to_timet(str_datetime, &curr_time_t) ;

    start_time_t = curr_time_t - dhr_window * 60;
    end_time_t   = curr_time_t + dhr_window * 60;

    prev_diff = INT_MAX ;
    post_diff = INT_MAX ;

    timet_to_yearsec_ansi(start_time_t, start_time);
    timet_to_yearsec_ansi(end_time_t, end_time);

    /*
     * search for record(s) within the dhr_window range
     */

    sprintf ( where, "WHERE radid = '%s' AND obstime >= '%s' "
                     "AND obstime <= '%s' ORDER BY obstime",
                     radid, start_time, end_time) ;

    pDPRRadarHead = GetDPRRadar ( where );

    if ( pDPRRadarHead != NULL )
    {
        pDPRRadarNode = ( DPRRadar * ) ListFirst(&pDPRRadarHead->list ) ; 
    }
    else
    {
        /*
         * there is no record found.
         */

        *status = 100;
        return;
    }

    while ( pDPRRadarNode != NULL )
    {
        yearsec_dt_to_timet(pDPRRadarNode->obstime, &obstime_t);
        offset_time_t = obstime_t - curr_time_t;

        if(offset_time_t == 0)
        {
            /*
             * the record has the same datetime value
             * as the running datatime
             */

            *prev_bias = pDPRRadarNode->mean_field_bias;
            *prev_offset = 0; 
            strncpy ( prev_filename, pDPRRadarNode->grid_filename, 19);

            *post_bias = pDPRRadarNode->mean_field_bias;
            *post_offset = 0; 
            strncpy ( post_filename, pDPRRadarNode->grid_filename, 19);

            return;            
        }
        else if( offset_time_t < 0 )
        {
            /*
             * the datetime of the record is less than
             * the running datatime.
             */

            if(abs(offset_time_t) < prev_diff)
            {
                *prev_bias   = pDPRRadarNode->mean_field_bias;
                *prev_offset = (int)(abs(offset_time_t)); 
                strncpy ( prev_filename, pDPRRadarNode->grid_filename, 19);

                prev_diff = abs(offset_time_t) ;
            }
        }
        else if( offset_time_t > 0 )
        {
            /*
             * the datetime of the record is great than
             * the running datatime.
             */

            if(abs(offset_time_t) < post_diff)
            {
                *post_bias   = pDPRRadarNode->mean_field_bias;
                *post_offset = (int)(abs(offset_time_t)); 
                strncpy ( post_filename, pDPRRadarNode->grid_filename, 19);

                post_diff = abs(offset_time_t) ;
            }
        }

        pDPRRadarNode = ( DPRRadar * ) ListNext( &pDPRRadarNode->node );
    }

    if ( pDPRRadarHead != NULL )
    {
        FreeDPRRadar ( pDPRRadarHead );
        pDPRRadarHead = NULL; 
    }
    
     if ( pDPRRadarNode != NULL )
    {
        FreeDPRRadar ( pDPRRadarNode );
        pDPRRadarNode = NULL; 
    }
    return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob9e/ohd/pproc/src/hpe_fieldgen/RCS/read_dpr_radar.c,v $";
 static char rcs_id2[] = "$Id: read_dpr_radar.c,v 1.1 2012/09/12 18:06:12 deng Exp $";}
/*  ===================================================  */

}
