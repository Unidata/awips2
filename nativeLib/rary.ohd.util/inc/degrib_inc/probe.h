/*****************************************************************************
 * probe.h
 *
 * DESCRIPTION
 *    This file contains the code that is called by cstart.c and tcldegrib.c
 * to handle the "Probe" or "-P" command.
 *
 * HISTORY
 *   12/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef PROBE_H
#define PROBE_H

#include "degrib_inc/userparse.h"
#include "degrib_inc/meta.h"
#include "degrib_inc/degrib2.h"

int ReadPntFile (char *pntFile, Point ** pnts, int *NumPnts, char ***labels);

int GRIB2Probe(userType * usr, IS_dataType *is, grib_MetaData *meta);

#endif
