/*******************************************************************************
* FILENAME:              decode_dhr.h
*
* DESCRIPTION:         This file contains constants, global varaibles and 
*                      function prototypes for the DHR decoding program.
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

#ifndef DECODE_DHR_H
#define DECODE_DHR_H

#include "GetOS.h"

/*--------------------------*/
/*  function prototypes     */
/*--------------------------*/

void decodeDHR(const char * radid,
               const char *raw_filename,
               const int write_to_db,
               OperSys os) ;

void wrtodb_DHRRadar(char * obstime,          const char * radid, 
                     const short volcovpat,   const short opermode,
                     const float dbz_min,     const float dbz_inc,
                     const float dbz_cnt,     const short j_date, 
                     const short j_time,      const short mean_field_bias, 
                     const short sample_size, const char *grid_filename);

void wrtodb_DHRAdapt(char * obstime,
                     const char * radid, 
                     const float params[45],
                     const char param46[2]);

void write_decoded_dhr(short int ** dbz_array, const char* file_name,
                       const float dbz_min, const float dbz_inc,
                       const float dbz_cnt, const float zrmult,
                       const float zrexp,  const float mxpra,
                       float bias, const short int bias_flag,
                       const short int end_date, const short int end_time,
                       const short int opermode, const int radar_lat,
                       const int radar_lon, int* ier);


#endif /* #ifndef DECODE_DHR_H */
