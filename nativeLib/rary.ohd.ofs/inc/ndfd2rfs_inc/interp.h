/*****************************************************************************
 * interp.h
 *
 * DESCRIPTION
 *    This file contains the routines used to interpolate the grid from the
 * NDFD grid to a simple geographic (lat/lon) grid.  It then stores the
 * results to a .flt file, and creates the associated .prj, and .hdr files.
 *    Note: this file takes advantage of write.c for the .prj / .hdr files
 *
 * HISTORY
 *    10/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef INTERP_H
#define INTERP_H

#include "meta.h"
#include "mymapf.h"
#include "type.h"

double BiLinearCompute (double *grib_Data, myMaparam * map, double lat,
                        double lon, double missing, sInt4 Nx, sInt4 Ny,
                        uChar missManage, double missSec);

/* Possible error messages left in errSprintf() */
int gribInterpFloat (const char *Filename, double *grib_Data,
                     grib_MetaData * meta, gridAttribType * attrib,
                     uChar scan, sChar f_MSB, sChar decimal, sChar f_GrADS,
                     sChar f_SimpleWx, sChar f_interp);

#endif
