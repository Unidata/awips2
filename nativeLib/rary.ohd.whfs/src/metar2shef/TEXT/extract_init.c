/*-------------------------------------------------------------------

   TITLE:    extract_init                                                 
                                                         
   PURPOSE:  To initialize buffers.                        
                                                                     
   AUTHOR:   David G. Brandon, CBRFC, Salt Lake City, UT             
                                                                     
   VERSION:  
         1.0   NOV 1995
               original version 
         1.1   APR 17 96 DGB
               Include buffer_ibuf[50].
        
---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "mtr.h"
#include "global_external.h"

extern int begin_msg_ptr, begin_remarks_ptr;

void extract_init()
{
    int i, n;

    /*  array locations and uses */
    /*
        1-3  id
        5    time
        14   12 hour max temp
        15   12 hour min temp
        16   snow depth
        17   current precip type
        18   river stage
        34   sea level pressure
        35   temperature
        36   dew point
        38   wind direction
        39   wind speed
        40   wind gust
        41   altimeter
        50   pressure characteristic
        51   pressure tendency
        52   3 or 6 hour precip
        57   24 hour precip
        58   24 max temp
        59   24 min temp
        60   hourly precip
        61   precip accumulator
        62   water equivalent
        63   sunshine
        64   6 hr max temp
        65   6 hr min temp
     */

        begin_msg_ptr = 0;
        begin_remarks_ptr = 0;
        memset(databuf_.record,0,sizeof(databuf_.record));
        memset(databuf_.main,0,sizeof(databuf_.main));
        memset(databuf_.remarks,0,sizeof(databuf_.remarks));
        memset(databuf_.sdata,0,sizeof(databuf_.sdata));

	for( i = 1; i <= 65; i++ )
		{
		buffer_.ibuf[i] = -999;	/* MISSING FOR INTEGERS */
		}
	for( n = 1; n <= 4; n++ )
		{
		buffer_.ibuf[n] = 32;	/* ASCII BLANKS */
		}
	for( n = 24; n <= 33; n++ )
		{
		buffer_.ibuf[n] = 32;
		}
	for( n = 44; n <= 49; n++ )
		{
		buffer_.ibuf[n] = 32;
		}
	for( n = 10; n <= 18; n += 4 )
		{
		buffer_.ibuf[n] = 32;
		buffer_.ibuf[n + 1] = 0;
		buffer_.ibuf[n + 2] = 0;
		buffer_.ibuf[n + 3] = 32;
		}
    buffer_.ibuf[23] = -999;
    buffer_.ibuf[14] = -999;
    buffer_.ibuf[15] = -999;
    buffer_.ibuf[16] = -999;	
    buffer_.ibuf[17] = -999;
    buffer_.ibuf[41] = -999;
    buffer_.ibuf[50] = -999;                      /* dgb:04/17/96 */
    buffer_.ibuf[57] = -999;

    buffer_.ibufchar[0] = -999; /* river stage readings */
    buffer_.ibufchar[1] = -999; /* surface visibility   */
}
