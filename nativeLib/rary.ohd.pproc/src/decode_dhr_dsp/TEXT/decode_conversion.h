/***********************************************************************
* Filename: decode_conversion.h
*
* Original Author: Feng Ding
*
* File Creation Date: July 2006
*
* Development Group: OHD / HSMB
*
* Description:
*  header file for building lookup table of coversion between quarterly HRAP and 
*  radar polar coordinate systems. 
* 
***********************************************************************/
#ifndef DECODE_CONVERSION_H
#define DECODE_CONVERSION_H

#include <math.h>

extern int radar_to_quarter_hrap_i[MAX_RANGE][MAX_AZIMUTH];
extern int radar_to_quarter_hrap_j[MAX_RANGE][MAX_AZIMUTH];
extern int quarter_hrap_to_radar_azimuth[MAX_IHRAP][MAX_JHRAP];
extern int quarter_hrap_to_radar_range[MAX_IHRAP][MAX_JHRAP];

void build_lookup_table(const int rad_lat, const int rad_lon);
void find_holes(const double LS, const double LAMDA);

void quarterhrap_to_az_range(const double LS, const double LAMDAS, 
                             const int LFM_I, const int LFM_J,
                             double *THETA_CIJ, double *RG_IJ);

#endif /* #ifndef DECODE_CONVERSION_H */
