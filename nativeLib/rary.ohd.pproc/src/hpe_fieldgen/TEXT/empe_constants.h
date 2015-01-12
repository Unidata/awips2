/*******************************************************************************
* FILENAME:              empe_constants.h
*
* DESCRIPTION:         This file contains constants
*                      for the empe_fieldgen main function.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         January 11, 2005
* ORGANIZATION:          HSEB / OHD
* MACHINE:               Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*   07/2013      JingtaoD          MAX_RADAR_NUM
*
********************************************************************************
*/

#ifndef EMPE_CONSTANTS_H
#define EMPE_CONSTANTS_H

/*--------------------------------*/
/*  definition of constants       */
/*--------------------------------*/

#define TOKEN_LEN        64
#define TOKEN_VALUE_LEN  512
#define FNAME_LEN        128
#define PATH_LEN         256
#define FILE_LEN         512
#define MESSAGE_LEN      512
#define XMRGDTFORM_LEN     3
#define YYYYMMDDHH_LEN    10
#define HHMMSS_LEN         8

#define GAGE_NUMBER         2500
#define GAGE_NUMBER_P3      2500

#define RADAR_NUMBER        500
#define RADAR_LAG_TIME      2 * 60
#define NEIGHBOR_NUMBER     250
/* The number of available memory spans. */
#define NUM_MEMORY_SPANS    10
#define MOSAIC_TYPE_LEN     10

#define MOSAIC_DEFAULT      -9.0
#define RADAR_DEFAULT       -9.0
#define RANGE_CHECK_DEFAULT -999.0
#define HEIGHT_DEFAULT      100000.0
#define ID_DEFAULT          0
#define MISBIN_DEFAULT      1
#define SATPRE_DEFAULT      -999.0
#define GAGE_DEFAULT        0.0
#define SPAN_DEFAULT        -1
#define BIAS_DEFAULT        -9.0
#define DHR_WINDOW_DEFAULT  10
#define DSP_WINDOW_DEFAULT  10
#define DSP_DURATION_DEFAULT  60

#define ZERO_CONSTANT       0
#define ONE_CONSTANT        1

#define MAXGAGS             10
#define NBMAX               400
#define NBMAXP1             NBMAX + 1
#define NDATX               40000
#define NIND                1000
#define NX1                 2500

#define VERBOSE             0

/**
  * REPLACE_MISSING = replace missing values with 0.0 flag
  *    = 0 -- do not replace missing values (default)
  *    = 1 -- replace missing values (values < 0.0) with 0.0
  **/

#define    REPLACE_MISSING  1

#define    VERNUM           1.0
#define    MAX_VALUE        9998

/* multiplier when changing from float to int array */

#define    FACTOR_PRECIP    100.0

#define    FACTOR_OTHER     1.0

#define    PROC_FLAG        "rfcwide "
#define    DHR_PROC_FLAG    "DHR     "
#define    DSP_PROC_FLAG    "DSP     "
#define    BDHR_PROC_FLAG   "BDHR    "
#define    BDSP_PROC_FLAG   "BDSP    "
#define    RATE_PROC_FLAG   "RATE    "
#define    BRATE_PROC_FLAG  "BRATE   "

#define    RADIUS          100.0

#define    NUM_DHR_COLS    524
#define    NUM_DHR_ROWS    524
#define    NUM_DHR_ELEMENTS    NUM_DHR_ROWS * NUM_DHR_COLS

#define    NUM_DSP_COLS    524
#define    NUM_DSP_ROWS    524
#define    NUM_DSP_ELEMENTS    NUM_DSP_ROWS * NUM_DSP_COLS

#define    APPLY_POLYGON   0 
#define    MAX_RADAR_NUM   600

#endif /* #ifndef EMPE_CONSTANTS_H */
