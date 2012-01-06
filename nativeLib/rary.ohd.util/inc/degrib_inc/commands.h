/*****************************************************************************
 * commands.h
 *
 * DESCRIPTION
 *    This file contains the code for the main command type options.  This is
 * mostly a consolidation of what was in "cstart.c" and the "tcldegrib.c".
 *
 * HISTORY
 *   8/2003 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef COMMANDS_H
#define COMMANDS_H
#include "degrib_inc/userparse.h"
#include "degrib_inc/meta.h"
#include "degrib_inc/degrib2.h"

int Grib2Convert (userType * usr, FILE * grib_fp, IS_dataType *is,
                  grib_MetaData *meta);

int GetOutputName (userType * usr, grib_MetaData * meta, char **buffer,
                   int *buffLen);

int MainConvert (userType * usr, IS_dataType * is, grib_MetaData * meta,
                 double *Data, long int DataLen, int f_unit, int f_fstMsg);

int DegribIt (userType * usr);

#endif
