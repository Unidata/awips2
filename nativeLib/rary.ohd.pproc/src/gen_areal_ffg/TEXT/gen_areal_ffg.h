#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef GEN_AREAL_FFG_H
#define GEN_AREAL_FFG_H

/*--------------------------------*/
/*  definition of     variables   */
/*--------------------------------*/

/*   variables used in function ffconv  */

#define lower_thres  201
#define upper_thres  253
#define mfactor 6
#define miss_value_float -99.
#define fill_value_float -98.


/*  version number variables */

char * genarealffg_name; // [ ] =  "Gen_Areal_FFG" ;
char * genarealffg_ver;// [ ] = "OB8.1" ;
char * genarealffg_date;// [ ] ="March 26, 2007" ;

/*  fill value for unsigned char    */

#define fill_value_uchar 0

/*  validtime (int format) of latest gridded FFG data from all RFCs  */

int latest_validtime;

/*  lookback time limit (hours)  */
/*  read from .Apps_defaults and transformed from char to int  */

int lookback_limit;

/*  minimum areal coverage       */
/*  read from .Apps_defaults and transformed from char to float  */

float min_coverage;

/*  hsa id read from Admin table */

char  hsa[4];

/*-----------------------------*/
/*  definition of     arrays   */
/*-----------------------------*/

unsigned char   *rfc_ffg_uchar;      /* gridded FFG array read from netcdf file */

float          **mosaic_ffg_float;   /* mosaic array type float */

unsigned char   *mosaic_ffg_uchar;   /* mosaic array type unsigned char */

/*  array containing FFG durations  */

int ffg_durations[6];

/*  array containing RFC names for mosaic  */

char rfc_names[4][6];

/*  array containing full pathnames of gridded FFG files  */

char **filenames_array;

/*---------------------------------------*/
/*  function prototypes for OFS routines */
/*---------------------------------------*/

int get_apps_defaults();


#endif
