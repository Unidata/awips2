/*****************************************************************************
 * write.h
 *
 * DESCRIPTION
 *    This file contains all the routines used to write the grid out to disk.
 * Currently this includes writing to a .flt file, or writing to a .shp file.
 * Associated with the .flt file are a .prj, .hdr, and .ave files.
 * Associated with the .shp file are a .dbf, and .shx file.
 *
 * HISTORY
 *    9/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef WRITE_H
#define WRITE_H

#include <stdio.h>
#include "degrib_inc/meta.h"
#include "degrib_inc/userparse.h"

/* Possible error messages left in errSprintf() */
int gribWriteEsriHdr (FILE * fp, gdsType * gds, gridAttribType * attrib,
                      double Dx, double Dy, double orient, sChar f_MSB,
                      sChar decimal);

/* Possible error messages left in errSprintf() */
/*
int gribWriteEsriPrj (FILE * fp, gdsType * gds, double orient);
*/

/* Possible error messages left in errSprintf() */
int gribWriteEsriAve (char *filename, gdsType * gds, double orient);

void gribWriteGradsCTL (char *CLTFile, char *DataFile, grib_MetaData * meta,
                        gdsType * gds, uChar scan, sChar f_MSB, double unDef,
                        uChar f_unDef);

/* Possible error messages left in errSprintf() */
int WriteGradsCube (char *filename, double *grib_Data, grib_MetaData * meta,
                    gridAttribType * attrib, uChar scan, sChar f_MSB,
                    sChar decimal, sInt4 *offset, sChar f_delete);

/* Possible error messages left in errSprintf() */
int gribWriteFloat (const char *Filename, double *grib_Data,
                    grib_MetaData * meta, gridAttribType * attrib,
                    uChar scan, sChar f_MSB, sChar decimal, sChar f_GrADS,
                    sChar f_SimpleWx);

/* Possible error messages left in errSprintf() */
int gribWriteShp (const char *Filename, double *grib_Data,
                  grib_MetaData * meta, sChar f_poly, sChar f_nMissing,
                  sChar decimal, sChar LatLon_Decimal);

/* Possible error messages left in errSprintf() */
int gribWriteCsv (FILE * out_fp, double *grib_Data, grib_MetaData * meta,
                  sChar decimal, char *separator, char *logName,
                  sChar f_WxParse, sChar f_NoMissing, sChar LatLon_Decimal);

int gribWriteNetCDF (char *filename, double *grib_Data, grib_MetaData * meta,
                     sChar f_NetCDF, sChar decimal, sChar LatLon_Decimal);

#endif
