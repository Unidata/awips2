/*****************************************************************************
 * degrib2.h
 *
 * DESCRIPTION
 *    This file contains the main driver routines to call the unpack grib2
 * library functions.  It also contains the code needed to figure out the
 * dimensions of the arrays before calling the FORTRAN library.
 *
 * HISTORY
 *    9/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef DEGRIB2_H
#define DEGRIB2_H

#include <stdio.h>
/* Include type.h for uChar and sChar */
#include "degrib_inc/type.h"
#include "degrib_inc/meta.h"

/* The IS_dataType is used to organize and allocate all the arrays that the
 * unpack library uses. */
typedef struct {
   sInt4 ns[8];         /* Size of each section in bytes. */
   sInt4 *is[8];        /* Section data as long ints. */
   sInt4 nd2x3;         /* Nx * Ny */
/*  Float4 *ain;  */    /* Size = Nd2x3. Holds the unpacked array if float. */
   sInt4 *iain;         /* Size = Nd2x3. Holds the unpacked array if int. */
   sInt4 *ib;           /* Size = Nd2x3. Hold and bitmasks.. */
   sInt4 nidat;         /* Size of section 2 data if int. */
   sInt4 *idat;         /* Section 2 data if int */
   sInt4 nrdat;         /* Size of section 2 data if float. */
   Float4 *rdat;        /* Section 2 data if float. */
   sInt4 *ipack;        /* The grib2 message in MSB as a long int (input) */
   sInt4 ipackLen;      /* The length of ipack. */
   sInt4 nd5;           /* Size of current GRIB message rounded up to the
                         * nearest long int. nd5 <= ipackLen */
} IS_dataType;

void IS_Init (IS_dataType *is);
void IS_Free (IS_dataType *is);

/*
 * WMO_HEADER_LEN should be 19 + 21 + 21 (+19?) bytes for the first header
 * WMO_SECOND_LEN should be 21 (+19?) bytes for subsequent ones.
 * WMO_ORIG_LEN was 21, so I "grandfathered that in, in ReadSECT0.
 * GRIB_LIMIT how many bytes to search for the GRIB message before giving up.
 */
#define WMO_HEADER_LEN 80
#define WMO_SECOND_LEN 40
#define WMO_HEADER_ORIG_LEN 21
#define GRIB_LIMIT 300

#define SECT0LEN_WORD 4
/* Possible error messages left in errSprintf() */
int ReadSECT0 (FILE * fp, int expect, int expect2, char *wmo,
               long int sect0[SECT0LEN_WORD], long int *gribLen,
               long int *wmoLen, int *version, long int limit);

/* Possible error messages left in errSprintf() */
int ReadGrib2Record (FILE * fp, sChar f_unit, double **Grib_Data,
                     long int *grib_DataLen, grib_MetaData * meta,
                     IS_dataType * IS, int subgNum, double majEarth,
                     double minEarth, int simpVer, long int * f_endMsg,
                     LatLon *lwlf, LatLon *uprt);

/* Possible error messages left in errSprintf() */
int FindGRIBMsg (FILE * fp, int msg, long int *offset);

#endif
