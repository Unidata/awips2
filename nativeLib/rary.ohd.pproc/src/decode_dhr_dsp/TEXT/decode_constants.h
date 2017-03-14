/*******************************************************************************
* FILENAME:              decode_constants.h
*
* DESCRIPTION:         This file contains constants for the decoding program.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         July 10, 2006
* ORGANIZATION:          HSEB / OHD
* MACHINE:               Dell-Redhat Linux
*
* MODIFICATION HISTORY:
* Date        Developer     Action
* 7/10/2006   Guoxian Zhou  First version
*
********************************************************************************
*/

#ifndef DECODE_CONSTANTS_H
#define DECODE_CONSTANTS_H

#define TRUE                   1
#define FALSE                  0
#define LEVEL_OUT_OF_RANGE     255
#define FILEPATH_LEN           256
#define PATH_LEN               128
#define FNAME_LEN              128

#define    NUM_LEVEL_DBZ       256

#define    MAX_AZIMUTH         360
#define    MAX_RANGE           230

#define    MAX_IHRAP           524
#define    MAX_JHRAP           524

#define    NUM_ROW             524
#define    NUM_COL             524

#define    BEYOND_RANGE        -99
#define    BEYOND_GRID         -77

#define    GRID_COORD_I        433.0
#define    GRID_COORD_J        433.0
#define    ANGLE_THRESH        9.81E-6

#define    R2KO                249.6348607
#define    PRIME               105.0
#define    B_CON               0.025
#define    IKA                 40
#define    KA                  40.0
#define    OFFSET              263
#define    DEFAULT_RADAR_1KM   -0.9 
#define    MAX_RATE            999.0 
#define    MIN_RATE            -999.0 

#define    CONST               135.0
#define    EARTH_RADIUS        6380.0

#define FLOAT_MISSING          -999.0

#endif /* #ifndef CONSTANTS_H */
