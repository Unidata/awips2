/*******************************************************************************
* FILENAME:           mpe_write_xmrg.h
* DESCRIPTION:        Write to an xmrg format file.
*
* ORIGINAL AUTHOR:    Guoxian Zhou
* CREATION DATE:      May 26, 2006 
* ORGANIZATION:       HSEB/OHD
* MACHINE:            Linux
* MODIFICATION HISTORY:
*      DATE         PROGRAMMER        DESCRIPTION/REASON
* May 26, 2006      Guoxian Zhou      Original Coding
* Sep 2007          P Tilles          changed MAX_VALUE to 32767
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DbmsUtils.h"
#include "GeneralUtil.h"
#include "HourlyPP.h"
#include "HourlyPC.h"
#include "precip_total.h"
#include "time_defs.h"

#ifndef MPE_WRITE_XMRG_H
#define MPE_WRITE_XMRG_H

#define    MAX_VALUE        32767
#define    VERNUM           1.0

typedef struct
{
    /* The HRAP x-coordinate. */
    int hrap_x;

    /* The HRAP y-coordinate. .*/
    int hrap_y;

    /* The number of columns.*/
    int num_cols;
    
    /* The number of rows.*/
    int num_rows;
    
} geo_data_struct; 

void writeXmrg( const geo_data_struct * pGeoData ,
           const char * filename ,
           const char * user ,
           const time_t tRunTime ,
           const char * proc_flag ,
           const short * out_mosaic ,
           size_t record_length,
           long int * irc) ;

#endif /* #ifndef MPE_WRITE_XMRG_H */
