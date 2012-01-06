/*****************************************************************************
 * scan.h
 *
 * DESCRIPTION
 *    This file contains the code that is used to assist with handling the
 * possible scan values of the grid.
 *
 * HISTORY
 *    9/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef SCAN_H
#define SCAN_H

#ifndef GRIB2BIT_ENUM
#define GRIB2BIT_ENUM
/* See rule (8) bit 1 is most significant, bit 8 least significant. */
enum {GRIB2BIT_1=128, GRIB2BIT_2=64, GRIB2BIT_3=32, GRIB2BIT_4=16,
      GRIB2BIT_5=8, GRIB2BIT_6=4, GRIB2BIT_7=2, GRIB2BIT_8=1};
#endif

void XY2ScanIndex (long int *Row, long int x, long int y, int scan,
                   long int Nx, long int Ny);

void ScanIndex2XY (long int row, long int *X, long int *Y, int scan,
                   long int Nx, long int Ny);

#endif
