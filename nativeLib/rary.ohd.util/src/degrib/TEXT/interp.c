/*****************************************************************************
 * interp.c
 *
 * DESCRIPTION
 *    This file contains the routines used to interpolate the grid from the
 * NDFD grid to a simple geographic (lat/lon) grid.  It then stores the
 * results to a .flt file, and creates the associated .prj, and .hdr files.
 *    Note: this file takes advantage of write.c for the .prj / .hdr files
 *
 * HISTORY
 *  10/2002 Arthur Taylor (MDL / RSIS): Created.
 *  12/2002 Rici Yu, Fangyu Chi, Mark Armstrong, & Tim Boyer
 *          (RY,FC,MA,&TB): Code Review 2.
 *
 * NOTES
 * 1) Not sure if given a lat/lon grid cmapf would work.
 *****************************************************************************
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "degrib_inc/mymapf.h"
#include "degrib_inc/tendian.h"
#include "degrib_inc/write.h"
#include "degrib_inc/interp.h"
#include "degrib_inc/myerror.h"
#include "degrib_inc/scan.h"
#include "degrib_inc/myassert.h"

extern double POWERS_ONE[];

/*****************************************************************************
 * BiLinearBorder() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Performs a bi-linear interpolation to the given lat/lon.  It assumes:
 * 1) That the grid is lat/lon.  2) That the interpolated point is outside
 * the normal definition of the grid.  3) That the deltaX is such that 1 cell
 * from the right edge of the grid is on the left edge of the grid.
 *
 * ARGUMENTS
 *  grib_Data = The grib2 data to write. (Input)
 *        map = Holds the current map projection info to interpolate from.(In)
 * newX, newY = The location of the desired point on the input grid.
 *    missing = The value to use for missing data. (Input)
 *     Nx, Ny = Dimensions of input grid (Input)
 * missManage = How missing values are handled in grib_Data (Input)
 *    missSec = Secondary missing value if there is one. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: double
 *   Interpolated value, or "missing" if it couldn't compute it.
 *
 * HISTORY
 *   4/2004 Arthur Taylor (MDL/RSIS): Created to solve a border probe problem.
 *
 * NOTES
 *****************************************************************************
 */
static double BiLinearBorder (double *grib_Data, myMaparam * map,
                              double newX, double newY, double missing,
                              sInt4 Nx, sInt4 Ny, uChar missManage,
                              double missSec)
{
   sInt4 row;           /* The index into grib_Data for a given x,y pair
                         * using scan-mode = 0100 = GRIB2BIT_2 */
   sInt4 x1, x2, y1, y2; /* Corners of bounding box lat/lon is in. */
   double d11, d12, d21, d22; /* grib values of bounding box corners. */
   sInt4 numPts = Nx * Ny; /* Total number of points. */
   double d_temp1, d_temp2; /* Temp storage during interpolation. */
   double testLon;      /* Used to test if we should call BiLinearBorder() */

   myAssert (map->f_latlon);

   /* Check if lonN + Dx = lon1 mod 360. */
   testLon = map->lonN + map->Dx;
   while (testLon < 0) {
      testLon += 360;
   }
   while (testLon >= 360) {
      testLon -= 360;
   }
   if (testLon != map->lon1) {
      return missing;
   }

   x1 = Nx;             /* lonN (or Nx) side. */
   x2 = 1;              /* lon1 (or 1) side. */
   y1 = (sInt4) (newY);
   y2 = (y1 + 1);

   /* Get the first (1,1) corner value. */
   XY2ScanIndex (&row, x1, y1, GRIB2BIT_2, Nx, Ny);
   /* Following check is probably unnecessary, but just in case. */
   if ((row < numPts) && (row >= 0)) {
      d11 = grib_Data[row];
      if (missManage == 2) {
         if (d11 == missSec) {
            return missing;
         }
      }
   } else {
      return missing;
   }

   /* Get the second (1,2) corner value. */
   XY2ScanIndex (&row, x1, y2, GRIB2BIT_2, Nx, Ny);
   if ((row < numPts) && (row >= 0)) {
      d12 = grib_Data[row];
      if (missManage == 2) {
         if (d12 == missSec) {
            return missing;
         }
      }
   } else {
      return missing;
   }

   /* Get the third (2,1) corner value. */
   XY2ScanIndex (&row, x2, y1, GRIB2BIT_2, Nx, Ny);
   if ((row < numPts) && (row >= 0)) {
      d21 = grib_Data[row];
      if (missManage == 2) {
         if (d21 == missSec) {
            return missing;
         }
      }
   } else {
      return missing;
   }

   /* Get the fourth (2,2) corner value. */
   XY2ScanIndex (&row, x2, y2, GRIB2BIT_2, Nx, Ny);
   if ((row < numPts) && (row >= 0)) {
      d22 = grib_Data[row];
      if (missManage == 2) {
         if (d22 == missSec) {
            return missing;
         }
      }
   } else {
      return missing;
   }

   /* Do Bi-linear interpolation to get value. */
   if ((d11 != missing) && (d12 != missing) &&
       (d21 != missing) && (d22 != missing)) {
      /* Note the use of fabs() and Dx and their implications for the sign. */
      if (fabs (newX - x1) <= map->Dx) {
         d_temp1 = d11 - fabs (newX - x1) * (d11 - d12) / map->Dx;
         d_temp2 = d21 - fabs (newX - x1) * (d21 - d22) / map->Dx;
         return (float) (d_temp1 + (newY - y1) *
                         (d_temp1 - d_temp2) / (y1 - y2));
      } else {
         d_temp1 = d11 - fabs (newX - x2) * (d11 - d12) / map->Dx;
         d_temp2 = d21 - fabs (newX - x2) * (d21 - d22) / map->Dx;
         return (float) (d_temp1 + (newY - y1) *
                         (d_temp1 - d_temp2) / (y1 - y2));
      }
   } else {
      return missing;
   }
}

/*****************************************************************************
 * BiLinearCompute() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Performs a bi-linear interpolation to the given lat/lon.  If it can find
 * the four surrounding corners, uses them to interpolate, otherwise it
 * returns the missing value.
 *
 * ARGUMENTS
 *  grib_Data = The grib2 data to write. (Input)
 *        map = Holds the current map projection info to interpolate from.(In)
 *   lat, lon = The point we are interested in. (Input)
 *    missing = The value to use for missing data. (Input)
 *     Nx, Ny = Dimensions of input grid (Input)
 * missManage = How missing values are handled in grib_Data (Input)
 *    missSec = Secondary missing value if there is one. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: double
 *   Interpolated value, or "missing" if it couldn't compute it.
 *
 * HISTORY
 *  11/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *   4/2004 AAT: Added call to BiLinearBorder()
 *
 * NOTES
 *    Could speed this up a bit, since we know scan is GRIB2BIT_2
 *    (Note: map usually has Grid defined for 1..Nx and 1..Ny)
 *****************************************************************************
 */
double BiLinearCompute (double *grib_Data, myMaparam * map, double lat,
                        double lon, double missing, sInt4 Nx, sInt4 Ny,
                        uChar missManage, double missSec)
{
   double newX, newY;   /* The location of lat/lon on the input grid. */
   sInt4 row;           /* The index into grib_Data for a given x,y pair
                         * using scan-mode = 0100 = GRIB2BIT_2 */
   sInt4 x1, x2, y1, y2; /* Corners of bounding box lat/lon is in. */
   double d11, d12, d21, d22; /* grib values of bounding box corners. */
   sInt4 numPts = Nx * Ny; /* Total number of points. */
   double d_temp1, d_temp2; /* Temp storage during interpolation. */

   myCll2xy (map, lat, lon, &newX, &newY);

   if ((newX < 1) || (newX > Nx) || (newY < 1) || (newY > Ny)) {
      if (map->f_latlon) {
         /* Find out if we can do a border interpolation. */
         return BiLinearBorder (grib_Data, map, newX, newY, missing, Nx, Ny,
                                missManage, missSec);
      }
      return missing;
   }
   x1 = (sInt4) (newX);
   x2 = (x1 + 1);
   y1 = (sInt4) (newY);
   y2 = (y1 + 1);

   /* Get the first (1,1) corner value. */
   XY2ScanIndex (&row, x1, y1, GRIB2BIT_2, Nx, Ny);
   /* Following check is probably unnecessary, but just in case. */
   if ((row < numPts) && (row >= 0)) {
      d11 = grib_Data[row];
      if (missManage == 2) {
         if (d11 == missSec) {
            return missing;
         }
      }
   } else {
      return missing;
   }

   /* Get the second (1,2) corner value. */
   XY2ScanIndex (&row, x1, y2, GRIB2BIT_2, Nx, Ny);
   if ((row < numPts) && (row >= 0)) {
      d12 = grib_Data[row];
      if (missManage == 2) {
         if (d12 == missSec) {
            return missing;
         }
      }
   } else {
      return missing;
   }

   /* Get the third (2,1) corner value. */
   XY2ScanIndex (&row, x2, y1, GRIB2BIT_2, Nx, Ny);
   if ((row < numPts) && (row >= 0)) {
      d21 = grib_Data[row];
      if (missManage == 2) {
         if (d21 == missSec) {
            return missing;
         }
      }
   } else {
      return missing;
   }

   /* Get the fourth (2,2) corner value. */
   XY2ScanIndex (&row, x2, y2, GRIB2BIT_2, Nx, Ny);
   if ((row < numPts) && (row >= 0)) {
      d22 = grib_Data[row];
      if (missManage == 2) {
         if (d22 == missSec) {
            return missing;
         }
      }
   } else {
      return missing;
   }

   /* Do Bi-linear interpolation to get value. */
   if ((d11 != missing) && (d12 != missing) &&
       (d21 != missing) && (d22 != missing)) {
      d_temp1 = d11 + (newX - x1) * (d11 - d12) / (x1 - x2);
      d_temp2 = d21 + (newX - x1) * (d21 - d22) / (x1 - x2);
      return (float) (d_temp1 + (newY - y1) *
                      (d_temp1 - d_temp2) / (y1 - y2));
   } else {
      return missing;
   }
}

/*****************************************************************************
 * IndexNearest() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Finds the nearest grid index to a given lat/lon.  If the lat/lon is
 * outside the grid, then it returns the missing value.
 *
 * ARGUMENTS
 *      map = Holds the current map projection info to interpolate from.(In)
 * lat, lon = The point we are interested in. (Input)
 *   Nx, Ny = Dimensions of input grid (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: sInt4
 *   Nearest value, or "missing" if it couldn't find it.
 *
 * HISTORY
 *  10/2003 Arthur Taylor (MDL/RSIS): Created.
 *
 * NOTES
 *    Could speed this up a bit, since we know scan is GRIB2BIT_2
 *    (Note: map usually has Grid defined for 1..Nx and 1..Ny)
 *****************************************************************************
 */
static sInt4 IndexNearest (myMaparam * map, double lat, double lon, sInt4 Nx,
                           sInt4 Ny)
{
   double newX, newY;   /* The location of lat/lon on the input grid. */
   sInt4 x, y;          /* Corners of bounding box lat/lon is in. */
   sInt4 row;           /* The index into grib_Data for a given x,y pair
                         * using scan-mode = 0100 = GRIB2BIT_2 */

   myCll2xy (map, lat, lon, &newX, &newY);
   x = (sInt4) (newX + .5);
   y = (sInt4) (newY + .5);
   if ((x < 1) || (x > Nx) || (y < 1) || (y > Ny)) {
      return -1;
   }
   XY2ScanIndex (&row, x, y, GRIB2BIT_2, Nx, Ny);
   return row;
}

/*****************************************************************************
 * gribInterpFloat() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   This creates a .flt file.  The .flt file happens to match the one that
 * Esri ArcView Spatial analyst uses, and extra support is created for using
 * it in Esri, but at the same time, anyone could write a program to read
 * the meta file along with the .flt file, and display the data.
 *
 * ARGUMENTS
 *   Filename = Name of file to save to. (Output)
 *  grib_Data = The grib2 data to write. (Input)
 *       meta = The meta file structure to interpolate the data for. (Input)
 *     attrib = Grid Attributes about the parsed GRIB msg to write. (Input)
 *       scan = Either 0 or (0100)<< 4 = 64 (How to write file.) (Input)
 *              if scan is 0 create a .flt file (For input to Esri S.A.)
 *              if scan is 64 create a .tlf file (For input to NDFD Gd)
 *      f_MSB = True if we should create MSB file, false for LSB (Input)
 *    decimal = How many decimals to round to. (Input)
 *    f_GrADS = True if you want to generate GrADS .ctl files. 
 * f_SimpleWx = True if you want to simplify the weather via NDFD method,
 *              before output. (Input)
 *   f_interp = 1 : sample by nearest point, 2 : bi-linear interpolation. (in)
 *
 * FILES/DATABASES:
 *   Calls gribWriteEsriHdr to create an Esri ascii .hdr file.
 *   Calls gribWriteEsriPrj to create an Esri ascii .prj file.
 *   Creates a .flt file, which is a binary file (Big Endian) consisting of
 *   NxM floats, starting at the upper left corner of the grid, traversing
 *   to the upper right, and then starting on the next row on the left.
 *
 * RETURNS: int (could use errSprintf())
 *  0 = OK
 * -1 = illegal declaration of the grid size.
 * -2 = Problems opening the files.
 *  1 = un-supported map projection for cmapf
 *  2 = invalid parameters for gribWriteEsriHdr.
 *  3 = invalid parameters for gribWriteEsriPrj.
 *  4 = invalid parameters for gribWriteEsriAve.
 *
 * HISTORY
 *  10/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *   5/2003 AAT: Added rounding to decimal.
 *   5/2003 AAT: Enabled other spherical earths.
 *   6/2003 AAT: Switched to a Warning and then averaging if Dx != Dy.
 *   6/2003 AAT: Added GrADS .ctl file creation support.
 *   7/2003 AAT: Proper handling of Dx != Dy.
 *  10/2003 AAT: Added f_interp option.
 *  10/2003 AAT: Added f_SimpleWx option.
 *
 * NOTES
 * 1) Not sure if given a lat/lon grid cmapf would work.
 * 2) Order is .flt first so if .prj stuff doesn't work, they have something.
 *   Then .hdr, .prj (in order of importance.)
 *****************************************************************************
 */
int gribInterpFloat (const char *Filename, double *grib_Data,
                     grib_MetaData * meta, gridAttribType * attrib,
                     uChar scan, sChar f_MSB, sChar decimal, sChar f_GrADS,
                     sChar f_SimpleWx, sChar f_interp)
{
   FILE *fp;            /* The current open file pointer. */
   float *floatPtr;     /* Temporary storage of double data to float for
                         * write. */
   char *filename;      /* local copy of the filename. */
   int nameLen;         /* length of filename so we don't keep recomputing
                         * it. */
   int x, y;            /* Current grid cell location. */
   double orient;       /* Orientation longitude of projection (where N is
                         * up.) (between -180 and 180) */
   myMaparam map;       /* Holds the map projection info to project from. */
   gdsType ng;          /* Grid to intepolate to. */
   double lat, lon;     /* Holds the location of the border cells of the
                         * original grid while we compute the range so we
                         * can set up ng appropriately. */
   double newX, newY;   /* Used to help compute the cell size of the
                         * original grid at the mesh latitude, so ng can be
                         * set up appropriately. */
   double missing;      /* Missing value to use. */
   double min_lon = 1000, max_lon = -1000; /* Range of longitude. */
   double min_lat = 1000, max_lat = -1000; /* Range of latitude. */
   double shift;        /* power of 10 used in rounding. */
   double val;          /* Holds the value from Bilinear before rounding. */
   double unDef;        /* Holds the missing value, if there is one. */
   char *filename2;     /* Holds name of data file in call to CTL creation */
   sInt4 row;           /* The index into grib_Data for a given x,y pair */

   /* Perform some error checks. */
   if ((scan != 0) && (scan != GRIB2BIT_2)) {
      errSprintf ("ERROR: expecting scan to be 0 or GRIB2BIT_2 not %d",
                  scan);
      return -3;
   }

   /* Check that gds is valid before setting up map projection. */
   if (GDSValid (&meta->gds) != 0) {
      preErrSprintf ("ERROR: Sect3 was not Valid.\n");
      return -3;
   }
   /* Set up the map projection. */
   SetMapParam (&map, &(meta->gds));

   nameLen = strlen (Filename);
   if (nameLen < 4) {
      errSprintf ("ERROR: File %s is too short in length (it may need an "
                  "extension?)", Filename);
      return -2;
   }
   filename = (char *) malloc (nameLen + 1 * sizeof (char));
   strncpy (filename, Filename, nameLen - 3);
   strncpy (filename + nameLen - 3, "flt", 3);
   filename[nameLen] = '\0';
   if ((fp = fopen (filename, "wb")) == NULL) {
      errSprintf ("ERROR: Problems opening %s for write.", filename);
      free (filename);
      return -2;
   }

   /* find bounds of map projection. */
   for (x = 1; x <= meta->gds.Nx; x++) {
      myCxy2ll (&map, x, meta->gds.Ny, &lat, &lon);
      /* 
       * Can not use "else if" here because they may not have been
       * properly initialized yet.
       */
      if (min_lat > lat)
         min_lat = lat;
      if (max_lat < lat)
         max_lat = lat;
      if (min_lon > lon)
         min_lon = lon;
      if (max_lon < lon)
         max_lon = lon;
   }
   for (y = meta->gds.Ny - 1; y > 1; y--) {
      myCxy2ll (&map, 1, y, &lat, &lon);
      if (min_lat > lat)
         min_lat = lat;
      else if (max_lat < lat)
         max_lat = lat;
      if (min_lon > lon)
         min_lon = lon;
      else if (max_lon < lon)
         max_lon = lon;
      myCxy2ll (&map, meta->gds.Nx, y, &lat, &lon);
      if (min_lat > lat)
         min_lat = lat;
      else if (max_lat < lat)
         max_lat = lat;
      if (min_lon > lon)
         min_lon = lon;
      else if (max_lon < lon)
         max_lon = lon;
   }
   for (x = 1; x <= meta->gds.Nx; x++) {
      /* assert : cxy2ll (&map, x, 1 + (1 - 1) * ratio, &lat, &lon); */
      myCxy2ll (&map, x, 1, &lat, &lon);
      if (min_lat > lat)
         min_lat = lat;
      else if (max_lat < lat)
         max_lat = lat;
      if (min_lon > lon)
         min_lon = lon;
      else if (max_lon < lon)
         max_lon = lon;
   }

   /* Initialize the new grid (ng). */
   ng.projType = 0;
   ng.f_sphere = 1;
   ng.majEarth = meta->gds.majEarth;
   ng.minEarth = ng.majEarth;
   ng.resFlag = 0;
   ng.scan = 0;         /* Set to match output grid. */
   /* Set lon1 lat1 */
   ng.lon1 = min_lon;
   ng.lat1 = min_lat;
   /* figure out cellSize. */
   orient = meta->gds.orientLon;
   while (orient > 180) {
      orient -= 360;
   }
   while (orient < -180) {
      orient += 360;
   }
   myCll2xy (&map, meta->gds.meshLat, orient, &newX, &newY);
   newX += 1;
   myCxy2ll (&map, newX, newY, &lat, &lon);
   ng.Dx = sqrt (pow ((lat - meta->gds.meshLat), 2) +
                 pow ((lon - orient), 2));
   if (ng.Dx > 1) {
      ng.Dx = 1;
   }
   ng.Dy = ng.Dx;
   /* Set up dimmensions of grid. */
   ng.Nx = (int) (ceil ((max_lon - min_lon) / ng.Dx) + 1);
   ng.Ny = (int) (ceil ((max_lat - min_lat) / ng.Dx) + 1);
   ng.numPts = ng.Nx * ng.Ny;
   ng.lon2 = ng.lon1 + ng.Nx * ng.Dx;
   ng.lat2 = ng.lat1 + ng.Ny * ng.Dx;
   /* following are not used by lat/lon grid so we set to defaults. */
   ng.meshLat = 0;
   ng.orientLon = 0;
   ng.center = 0;
   ng.scaleLat1 = ng.scaleLat2 = 0;
   ng.southLat = ng.southLon = 0;

   if (decimal > 17)
      decimal = 17;
   if (decimal < 0)
      decimal = 0;
   shift = POWERS_ONE[decimal];

   /* Figure out a missing value, if there isn't one, so that when we
    * interpolate and we are out of bounds, we can return something. */
   if (attrib->missManage != 0) {
      missing = (floor (attrib->missPrim * shift + .5)) / shift;
   } else {
      missing = 9999;
      if (attrib->f_maxmin) {
         if ((missing <= attrib->max) && (missing >= attrib->min)) {
            missing = attrib->max + 1;
         }
      }
   }
   floatPtr = (float *) malloc (ng.Nx * sizeof (float));

   for (y = 0; y < ng.Ny; y++) {
      for (x = 0; x < ng.Nx; x++) {
         /* The y+1 is so that we have the lower left corner of each cell. */
         if (scan == 0) {
            lat = ng.lat1 + ((ng.Ny - (y + 1)) * ng.Dx);
         } else {
            lat = ng.lat1 + y * ng.Dx;
         }
         lon = ng.lon1 + x * ng.Dx;

         if ((f_SimpleWx && (strcmp (meta->element, "Wx") == 0)) ||
             (f_interp == 1)) {
            row = IndexNearest (&map, lat, lon, meta->gds.Nx, meta->gds.Ny);
            if (row < 0) {
               val = missing;
            } else {
               /* Look up the value at row in the grib_Data field. */
               val = grib_Data[row];
               if (attrib->missManage == 2) {
                  if (val == attrib->missSec) {
                     val = missing;
                  }
               }
               /* For Simple weather we have to look up the value (which is
                * now an index into a table) in the simple weather code
                * table. */
               if (f_interp != 1) {
                  row = (sInt4) val;
                  if ((row >= 0) && (row < meta->pds2.sect2.wx.dataLen)) {
                     val = (float) meta->pds2.sect2.wx.ugly[row].SimpleCode;
                  } else {
                     val = missing;
                  }
               }
            }
         } else {
            val = BiLinearCompute (grib_Data, &map, lat, lon, missing,
                                   meta->gds.Nx, meta->gds.Ny,
                                   attrib->missManage, attrib->missSec);
         }
         floatPtr[x] = (float) ((floor (val * shift + .5)) / shift);
      }
      if (f_MSB) {
         FWRITE_BIG (floatPtr, sizeof (float), ng.Nx, fp);
      } else {
         FWRITE_LIT (floatPtr, sizeof (float), ng.Nx, fp);
      }
   }
   free (floatPtr);
   fclose (fp);

   if (f_GrADS) {
      if (attrib->missManage != 0) {
         unDef = (floor (attrib->missPrim * shift + .5)) / shift;
      } else {
         unDef = 9999;  /* This is ignored by gribWriteGradsCTL */
      }
      filename2 = (char *) malloc ((strlen (filename) + 1) * sizeof (char));
      strcpy (filename2, filename);
      strncpy (filename + nameLen - 3, "ctl", 3);
      if (attrib->missManage == 0) {
         gribWriteGradsCTL (filename, filename2, meta, &ng, scan, f_MSB,
                            unDef, 0);
      } else {
         gribWriteGradsCTL (filename, filename2, meta, &ng, scan, f_MSB,
                            unDef, 1);
      }
      free (filename2);
   }

   /* Create the .hdr file */
   strncpy (filename + nameLen - 3, "hdr", 3);
   if ((fp = fopen (filename, "wt")) == NULL) {
      errSprintf ("ERROR: Problems opening %s for write.", filename);
      free (filename);
      return -2;
   }
   gribWriteEsriHdr (fp, &ng, attrib, ng.Dx, ng.Dy, orient, f_MSB, decimal);
   fclose (fp);

   /* Create the .prj file */
/*
   strncpy (filename + nameLen - 3, "prj", 3);
   if ((fp = fopen (filename, "wt")) == NULL) {
      errSprintf ("ERROR: Problems opening %s for write.", filename);
      free (filename);
      return -2;
   }
   gribWriteEsriPrj (fp, &ng, orient);
   fclose (fp);
*/

   free (filename);
   return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/degrib/RCS/interp.c,v $";
 static char rcs_id2[] = "$Id: interp.c,v 1.1 2004/09/16 17:14:09 dsa Exp $";}
/*  ===================================================  */

}
