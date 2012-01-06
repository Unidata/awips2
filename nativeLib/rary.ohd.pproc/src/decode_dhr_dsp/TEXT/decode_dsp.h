/*******************************************************************************
* FILENAME:            decode_dsp.h
*
* DESCRIPTION:         This file contains constants, global varaibles and 
*                      function prototypes for the DSP decoding program.
*
* ORIGINAL AUTHOR:       Guoxian Zhou
* CREATION DATE:         July 19, 2006
* ORGANIZATION:          HSEB / OHD
* MACHINE:               Dell-Redhat Linux
*
* MODIFICATION HISTORY:
* Date        Developer     Action
* 7/19/2006   Guoxian Zhou  First version
*
********************************************************************************
*/

#ifndef DECODE_DSP_H
#define DECODE_DSP_H

#include "GetOS.h"

/*--------------------------*/
/*  function prototypes     */
/*--------------------------*/

void decodeDSP(const char * radid,
               const char *raw_filename,
               const int write_to_db,
               OperSys os) ;

void wrtodb_DSPRadar(char *obstime,            const char *radid, 
                     const short volcovpat,    const short opermode,
                     const float minval,       const float maxval,
                     const float num_data_lev, const float scale_factor, 
                     char *begin_time,         char *end_time,  
                     const short j_beg_date,   const short j_beg_time,  
                     const short j_end_date,   const short j_end_time,
                     const short mean_field_bias, 
                     const short sample_size, const char *grid_filename);

void wrtodb_DSPAdapt(char * obstime,
                     const char * radid, 
                     const float params[45],
                     const char param46[2]);

void write_decoded_dsp(short int ** ptrDBZ, const char* filename,
                        const short int scale_factor, 
                        const short int opermode,
                        const short int beg_date, const short int beg_time,
                        const short int end_date, const short int end_time,
                        const int radar_lat, const int radar_lon, 
                        int* ier);
						
#endif /* #ifndef DECODE_DSP_H */
