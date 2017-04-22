/*
 * packgrib.h
 *
 *  Created on: BEFORE OB83
 *  Author: ohd
 */
/*

    packgrib.h

    function prototypes for packgrid_.c file

*/
#ifndef PACKGRIB_H
#define PACKGRIB_H

void setBits(size_t *buf,size_t loc,size_t off,size_t bits);

#if defined(LINUX) || defined(linux)
void p_swap4(size_t *buf,size_t length);
#endif

int iround(double val);

size_t real2ibm(double native_real);

void packIS(int *grib_lbl,size_t *out_buf,size_t *out_buf_len,size_t *grib_length);


void packPDS(int *grib_lbl,char *pds_ext,size_t *pds_ext_length,size_t *out_buf,
             size_t *out_buf_len,size_t *grib_length,size_t *off,unsigned char *pds_flag);

void packGDS(int *grib_lbl,size_t *out_buf,size_t *out_buf_len,size_t *grib_length,size_t *off);

void packBDS(int *grib_lbl,float *gridpoints,size_t *gi_len,float miss_val,
             size_t *out_buf,size_t *out_buf_len,size_t *grib_length,size_t off,
	     unsigned char *pds_flag);

void packEND(size_t *out_buf,size_t *out_buf_len,size_t *grib_length);

int packgrib(int *grib_lbl,char *pds_ext,size_t *pds_ext_length,float *gridpoints,
             size_t *gi_len,float *miss_val,size_t *out_buf,size_t *out_buf_len,
	     size_t *grib_length);

#endif /* #ifndef PACKGRIB_H */
