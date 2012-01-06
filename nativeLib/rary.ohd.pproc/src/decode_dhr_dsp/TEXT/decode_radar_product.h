/*******************************************************************************
* FILENAME:            decode_radar_product.h
*
* DESCRIPTION:         This file contains constants, global varaibles and 
*                      function prototypes for the radar product decoding program.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         July 03, 2006
* ORGANIZATION:          HSEB / OHD
* MACHINE:               Dell-Redhat Linux
* 
* MODIFICATION HISTORY:
* Date        Developer     Action
* 7/03/2006   Guoxian Zhou  First version
* 
********************************************************************************
*/

#ifndef DECODE_RADAR_PRODUCT_H
#define DECODE_RADAR_PRODUCT_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "decode_constants.h"
#include "decode_conversion.h"
#include "decode_dhr.h"
#include "decode_dsp.h"
#include "GeneralUtil.h"

/*
 * definition of data type
 */

typedef enum {unknown = 0, dhr, dsp}productType ;

/*
 * function prototypes
 */

int  decode_dhr_dsp_get_radid_from_product (const char *filename, char * radid);
int  decode_dhr_dsp_get_radid_from_filename(const char *filename, char * radid);
int  decode_dhr_dsp_check_radid(const char * radid);
char *decode_dhr_dsp_convertJulianDate(short);

int get_adapt(int *num_param, float params[45],
              char param38[2], FILE *fp) ;

#endif /* #ifndef DECODE_RADAR_PRODUCT_H */
