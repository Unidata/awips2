#define PROGRAM_VERSION "1.53"
#define PROGRAM_DATE "06/18/2004"
/*****************************************************************************
 * userparse.h
 *
 * DESCRIPTION
 *    This file contains the code that is common to cstart.c and tcldegrib.c
 * when parsing the options provided by the user.
 *
 * HISTORY
 *    9/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef USERPARSE_H
#define USERPARSE_H

/* Include type.h for uChar and sChar */
#include "degrib_inc/type.h"

enum {
   CMD_INVENTORY, CMD_CONVERT, CMD_PROBE, CMD_VERSION, CMD_DATA,
   CMD_DATAPROBE, CMD_DATACONVERT, CMD_CALC
};

/* A structure containing the user's choices. */
typedef struct {
   char *cfgName;       /* cfgName = -cfg */
   char *inName;        /* inName = -in */
   sChar f_Command;     /* See CMD_* Enumeration.  */

/* sChar f_Inventory; */  /* f_Inventory = -I */
/* sChar f_Convert; */    /* f_Convert = -C */
/* sChar f_Probe; */      /* f_Probe = -P */
/* sChar f_Version; */    /* f_Version = -V */
/* sChar f_Data; */       /* f_Data = -Data */

   sChar f_Flt;         /* f_Flt = -Flt */
   sChar f_Met;         /* f_Met = -nMet */
   sChar f_IS0;         /* f_IS0 */
   sChar f_Shp;         /* f_Shp = -Shp */
   sChar f_Csv;         /* f_Csv = -Csv */
   sChar f_Grib2;       /* f_Grib2 = -Grib2 */
   sChar f_Cube;        /* f_Cube = -Cube */
   sChar f_Append;      /* f_Append = -Append */
	sChar f_poly;        /* Create polygon .shp or point .shp files? */
   sChar f_nMissing;    /* Don't store missing values in .shp files. */
   int msgNum;          /* msgNum = -msg (1..n) (0 means all messages). */
   int subgNum;         /* which subgrid in the message (0..m-1) */
   sChar f_unit;        /* f_unit = 0 -Unit n || 1 -Unit e || 2 -Unit m */
   sChar decimal;       /* How many decimals to round to. (default 3) */
   sChar LatLon_Decimal; /* How many decimals to round Lat/Lons (default 6) */
   char *nameStyle;     /* nameStyle = -nameStyle */
   char *namePath;      /* directory to store nameStyle files in. */
   char *outName;       /* outName = -out or NULL (has 3 letter extension.) */
   uChar f_stdout;      /* true if outName is "stdout" */
   char *logName;       /* logName = -log or NULL (for error messages.) */
   sChar f_interp;      /* f_interp = -Interp */
   sChar f_GrADS;       /* If we should create GrADS control file for .flt. */
   sChar f_NetCDF;      /* 0 for no NetCDF or integer for version of NetCDF
                         * to create (currently only have 1). */
   sChar f_SimpleWx;    /* If we should simplify the .flt file using the NDFD
                         * Weather table. */
   sChar f_SimpleVer;   /* Which version of the simple NDFD Weather table to
                         * use. (1 is 6/2003) (2 is 1/2004) (default 2) */
   sChar f_revFlt;      /* f_revFlt = -revFlt */
   sChar f_MSB;         /* f_MSB = -MSB */
   char *pntFile;       /* pntFile = -pntFile */
   sChar f_pntStyle;    /* f_pntStyle = -pntStyle */
        /*  0 = Elem, Unit, refTime, valTime, (Value at Lat/lon 1), ... */
        /*  1 = Stn Name or (lat,lon), Elem[Unit], refTime, valTime, value */
   char *separator;     /* The separator to use for -P output. */
   sChar f_WxParse;     /* -WxParse option.  0 == ugly string,
                         * 1 == English Translation, 2 == -SimpleWx code */
   Point pnt;           /* pnt = -pnt option. */
   LatLon lwlf;         /* lower left corner (cookie slicing) -lwlf */
   LatLon uprt;         /* upper right corner (cookie slicing) -uprt */
   sChar f_cellsAll;    /* 0 => lat/lon pnts, 1 => cells in pnts,
                         * 2 => all Cells. */
   sChar f_surface;     /* 0 => no surface info,
                         * 1 => short form of surface name
                         * 2 => long form of surface name */
   sChar f_nLabel;      /* 1 => Ignore pntFile label, 0 => use pntFile label*/ 
   double majEarth;     /* -radEarth if < 6000 ignore, otherwise use this to
                         * override the radEarth in the GRIB1 or GRIB2
                         * message.  Needed because NCEP lied.  They use
                         * 6371.2 but in GRIB1 could only state 6367.47. */
   double minEarth;     /* -minEarth if < 6000 ignore, otherwise use this to
                         * override the minEarth in the GRIB1 or GRIB2
                         * message. */
   char *indexFile;     /* indexFile = -Index option or NULL */
   sChar f_Print;       /* Print option (for diagnosis of indexFile). */
   char *Asc2Flx_File;  /* Convert ASCII file to flx index file. */
   char *matchElem;     /* During Probe, entries have to match this element. */
   double matchRefTime; /* During Probe, entries have to match this refTime. */
   double matchValidTime; /* During Probe, entries match this validTime. */
} userType;

void UserInit (userType *usr);

void UserFree (userType *usr);

/* Possible error messages left in errSprintf() */
int UserValidate (userType *usr);

char * Grib2About (void);

#endif
