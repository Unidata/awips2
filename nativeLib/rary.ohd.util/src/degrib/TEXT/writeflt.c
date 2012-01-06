/*****************************************************************************
 * writeflt.c
 *
 * DESCRIPTION
 *    This file contains all the routines used to write the grid out to
 * .flt format (which can be used via ArcGIS Spatial Analyst or by GrADS.
 * Associated with the .flt file are a .prj, .hdr, and .ave files.
 * Also calls gribWriteGradsCTL, to create a .ctl file.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL / RSIS): Created.
 *  12/2002 Rici Yu, Fangyu Chi, Mark Armstrong, & Tim Boyer
 *          (RY,FC,MA,&TB): Code Review 2.
 *   6/2003 AAT: Split write.c into 2 pieces.
 *
 * NOTES
 * 1) As far as I can tell .flt and .shp files don't allow for two missing
 *    values, so I use missPrim for both, if need be.
 *****************************************************************************
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "degrib_inc/mymapf.h"
#include "degrib_inc/tendian.h"
#include "degrib_inc/write.h"
#include "degrib_inc/myassert.h"
#include "degrib_inc/myerror.h"
#include "degrib_inc/myutil.h"
#include <errno.h>
#include "degrib_inc/type.h"
#include "degrib_inc/weather.h"
#include <math.h>

extern double POWERS_ONE[];

/*****************************************************************************
 * gribWriteEsriHdr() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To write the ascii .hdr file associated with the .flt file.
 *
 * ARGUMENTS
 *      fp = An opened pointer to a text file to write the data to. (Out)
 *     gds = Grid Definition (from the parsed GRIB msg) to write. (Input)
 *  attrib = Sect 5 from the parsed grib message to write. (Input)
 *      Dx = Grid Dx. (Input)
 *      Dy = Grid Dy. (Input)
 *  orient = Orientation longitude (between -180 and 180). (Input)
 *   f_MSB = True if we should create MSB file, false for LSB (Input)
 * decimal = How many decimals to round to. (Input)
 *
 * FILES/DATABASES:
 *   Creates an Esri ArcView Spatial analyst .hdr file.
 *   It is an ascii text file with a "varname <space> value" per line.
 *   I put in the appropriate VarNames as best as I could determine.
 *   Still want a good white paper from ESRI describing this file.
 *
 * RETURNS: int (could use errSprintf())
 *  0 = OK
 * -2 = unsupported map projection.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *   2/2003 AAT: Made 1/2 cell correction.
 *   5/2003 AAT: Added rounding to decimal.
 *   5/2003 AAT: Enabled other spherical earths.
 *   6/2003 AAT: Switched to average (Dx, Dy) for mesh size, instead of
 *               an error if Dx != Dy.
 *   7/2003 AAT: Proper handling of Dx != Dy.
 *
 * NOTES
 * 1) The reason the fp is passed in is because we may need to combine the
 * .hdr with the .prj files when creating ASCII grids.
 * 2) Assumes the following checks made by parent before calling:
 *   1) (gds->numPts != gds->Nx * gds->Ny) == Error
 *   2) (! gds->f_sphere) == Error
 *   3) (gds->majEarth != 6367.47) == Error      (no longer an error)
 *   4) (gds->Dx != gds->Dy) == Error            (no longer an error)
 *****************************************************************************
 */
int gribWriteEsriHdr (FILE * fp, gdsType * gds, gridAttribType * attrib,
                      double Dx, double Dy, double orient, sChar f_MSB,
                      sChar decimal)
{
   maparam stcprm;      /* used to find the lower left corner in ArcView
                         * units. */
   double x, y;         /* cmapf's solution to lower left corner. */
   double lat, lon;     /* Lower left corner of the grid from ArcView's
                         * perspective. */
   double meshLat;      /* The latitude where the mesh size is correct. */

   myAssert ((gds->projType == GS3_LATLON) ||
             (gds->projType == GS3_MERCATOR) ||
             (gds->projType == GS3_POLAR) || (gds->projType == GS3_LAMBERT));

   fprintf (fp, "ncols %ld\n", gds->Nx);
   fprintf (fp, "nrows %ld\n", gds->Ny);

   if (gds->projType != GS3_LATLON) {
      /* 
       * Need new lower left corner of grid... The reason is ArcView treats
       * grid cells as if the point was at the center of the grid as opposed
       * to lattice points, where the lat/lon would be the lower left corner.
       */
      lat = gds->lat1;
      lon = gds->lon1;
      /* Set up the map projection. */
#ifdef USE_DMAPF
#else
      mkGeoid (&stcprm, AB, gds->majEarth, gds->minEarth);
#endif
      if (gds->projType == GS3_POLAR) {
         stlmbr (&stcprm, gds->scaleLat1, orient);
      } else {
#ifdef USE_DMAPF
         stlmbr (&stcprm, eqvlat (gds->scaleLat1, gds->scaleLat2), orient);
#else
         stlmbr (&stcprm, eqvlat (&stcprm, gds->scaleLat1, gds->scaleLat2),
                 orient);
#endif
      }
#ifdef USE_DMAPF
      cstrad (&stcprm, gds->majEarth);
#else
#endif
      /* need km, have m. thus the / 1000. */
      stcm1p (&stcprm, 1, 1, gds->lat1, gds->lon1,
              gds->meshLat, orient, (double) (Dx / 1000.), 0);
      cxy2ll (&stcprm, .5, 1 + (.5 - 1) * (Dy / Dx), &lat, &lon);
      /* 
       * Want the ArcView lower left corner of grid in meters.
       * To do so, we assume Re = 6367.47 km (default of cmapf),
       * and assume that ArcView set (orient lon., scale_lat1.) to (0,0)
       * and use a mesh_size of 1 meter (hence the 0.001, since cmapf uses
       * km).
       *
       * Reason for orientlon, scaleLat1 = (0, 0), is because we referred to
       * that as the central parallel, and reference latitude, in other
       * procedures.
       *
       * Reason for mesh lat = eqvlat (gds->scaleLat1, gds->scaleLat2) is
       * we don't pass MeshLat to ArcView, so it must use either reference
       * latitude (scaleLat1) or eqvlat(scaleLat1, scaleLat2).
       */
      if (gds->projType == GS3_POLAR) {
         stlmbr (&stcprm, gds->scaleLat1, orient);
         meshLat = gds->scaleLat1;
      } else {
#ifdef USE_DMAPF
         stlmbr (&stcprm, eqvlat (gds->scaleLat1, gds->scaleLat2), orient);
         meshLat = eqvlat (gds->scaleLat1, gds->scaleLat2);
#else
         stlmbr (&stcprm, eqvlat (&stcprm, gds->scaleLat1, gds->scaleLat2),
                 orient);
         meshLat = eqvlat (&stcprm, gds->scaleLat1, gds->scaleLat2);
#endif
      }
#ifdef USE_DMAPF
      cstrad (&stcprm, gds->majEarth);
#else
#endif
      /* Using a Dx == Dy == 0.001 grid here. */
      stcm1p (&stcprm, 0, 0, gds->scaleLat1, orient, meshLat, orient, 0.001,
              0);
      cll2xy (&stcprm, lat, lon, &x, &y);
      fprintf (fp, "cellsize %f\n", Dx); /* need m, have m. */
      fprintf (fp, "xllcorner %f\n", x);
      fprintf (fp, "yllcorner %f\n", y);
   } else {
      fprintf (fp, "cellsize %f\n", Dx); /* need and have degrees. */
      fprintf (fp, "xllcorner %f\n", gds->lon1 - (Dx / 2.));
      fprintf (fp, "yllcorner %f\n", gds->lat1 - (Dx / 2.));
   }
   if ((attrib->missManage == 1) || (attrib->missManage == 2)) {
      fprintf (fp, "nodata_value %f\n", myRound (attrib->missPrim, decimal));
   }
   if (f_MSB) {
      fprintf (fp, "byteorder msbfirst\n");
   } else {
      fprintf (fp, "byteorder lsbfirst\n");
   }
   return 0;
}

/*****************************************************************************
 * gribWriteEsriPrj() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To write the ascii .prj file associated with the .flt file.
 *
 * ARGUMENTS
 *     fp = An opened pointer to a text file to write the data to. (Output)
 *    gds = Grid Definition (from the parsed GRIB message) to write. (Input)
 * orient = Orientation longitude (between -180 and 180). (Input)
 *
 * FILES/DATABASES:
 *   Creates an Esri ArcView Spatial analyst prj file.
 *   It is an ascii text file with a "varname <space> value" per line.
 *   Except for a "Parameters" option.
 *   I put in the appropriate VarNames as best as I could determine.
 *   Still want a good white paper from ESRI describing this file.
 *
 * RETURNS: int (could use errSprintf())
 *  0 = OK
 * -2 = unsupported map projection.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *   5/2003 AAT: Commented out: .flt doesn't need this, and it gets in the
 *          way of ArcExplorer and .shp files.  Might want to put this in
 *          the .hdr file, since otherwise one can't determine the radius of
 *          earth without looking at the .txt file.
 *
 * NOTES
 * 1) The reason the fp is passed in is because we may need to combine the
 *    .hdr with the .prj files when creating ASCII grids.
 * 2) Check that ArcView can handle : Projection Lambert_Conformal_Conic
 *    for mercator and for polar.
 * 3) Assumes the following checks made by parent before calling:
 *    1) (! gds->f_sphere) == Error
 *****************************************************************************
 */
/*
int gribWriteEsriPrj (FILE * fp, gdsType * gds, double orient)
{
*/
   /* This error check left here, because it is the only one likely to
    * change. */
/*
   if ((gds->projType != GS3_LATLON) && (gds->projType != GS3_MERCATOR)
       && (gds->projType != GS3_POLAR)
       && (gds->projType != GS3_LAMBERT)) {
      errSprintf ("ERROR: Can not handle projection yet. \n"
                  "See Grid Definition Section (sect 3) type %d\n",
                  gds->projType);
      return -2;
   }
   if (gds->projType != GS3_LATLON) {
      fprintf (fp, "Projection Lambert_Conformal_Conic\n");
      fprintf (fp, "Units METERS\n");
      fprintf (fp, "Spheroid SPHERE\n");
      fprintf (fp, "Radius %f\n", gds->majEarth * 1000);
      fprintf (fp, "Xshift 0.00000000\n");
      fprintf (fp, "Yshift 0.00000000\n");
      fprintf (fp, "Parameters\n");
      fprintf (fp, "False_Easting 0\n");
      fprintf (fp, "False_Northing 0\n");
      fprintf (fp, "Central_Meridian %f\n", orient);
      fprintf (fp, "Standard_Parallel_1 %f\n", gds->scaleLat1);
      fprintf (fp, "Standard_Parallel_2 %f\n", gds->scaleLat2);
      fprintf (fp, "Central_Parallel %f\n", gds->scaleLat1);
   } else {
      fprintf (fp, "Projection GEOGRAPHIC\n");
      fprintf (fp, "Units DD\n");
      fprintf (fp, "Spheroid SPHERE\n");
      fprintf (fp, "Radius %f\n", gds->majEarth * 1000);
      fprintf (fp, "Xshift 0.00000000\n");
      fprintf (fp, "Yshift 0.00000000\n");
      fprintf (fp, "Parameters\n");
   }
   return 0;
}
*/

/*****************************************************************************
 * gribWriteEsriAve() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   This creates a short avenue script which can set up the projection and
 * sphere correctly.
 *
 * ARGUMENTS
 * filename = The name of the file to write the .ave file to. (Input)
 *      gds = Grid Definition from the parsed GRIB msg to write. (Input)
 *   orient = Orientation longitude (between -180 and 180). (Input)
 *
 * FILES/DATABASES:
 *   Creates an Esri ArcView .ave file.
 *   It is an ascii text file with arcView script in it.
 *
 * RETURNS: int (could use errSprintf())
 *  0 = OK
 * -1 = Problems opening the file.
 * -2 = unsupported map projection.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  11/2002 AAT: Switched to passing in filename not fp.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *   5/2003 AAT: Revised the script that is generated to make it more friendly
 *
 * NOTES
 *****************************************************************************
 */
int gribWriteEsriAve (char *filename, gdsType * gds, double orient)
{
   static char *header[] = {
      "'Look up \"View (Class)\" in ArcView 3.1 help",
      "'Most of this from the SetSpheroid and Projection Class example.",
      "'Although the CoordSys example is also useful.",
      "",
      "'A rectangle for the entire world",
      "aRect = Rect.MakeXY(-180,-90,180,90)",
      "",
      "'Create the projection object"
   };
   static char *body[] = {
      "",
      "'Get the name of the View to change.",
      "aViewName = msgBox.Input(\"Name of view to change projection for\", "
         "\"Which View\", \"View1\")",
      "if (aViewName = nil) then",
      "  exit",
      "end",
      "",
      "flag = MsgBox.YesNo (\"To use the correct spheroid (of the earth) we "
         "have to create a coordinate system.  Shall I?\", \"Spheroid "
         "Correction?\", false)",
      "if (flag) then",
      "  'Get the Projection's Spheroid and modify it",
      "  aSphere = aPrj.GetSpheroid"
   };
   static char *tail[] = {
      "",
      "  'Create a Coordinate System",
      "  aPrjName = msgBox.Input(\"Enter a projection Category\", "
         "\"Projection Category?\", \"My Custom Projections\")",
      "  if (aPrjName = nil) then",
      "    exit",
      "  end",
      "  aCoordSys = CoordSys.Make",
      "  aCoordSys.SetName(aPrjName)",
      "  thePrjs = aCoordSys.GetProjections",
      "  thePrjs.Add(aPrj)",
      "  aTypeName = msgBox.Input(\"Enter a projection type?\", "
         "\"Projection type\", \"GRIB\")",
      "  if (aTypeName = nil) then",
      "    exit",
      "  end",
      "  aPrj.SetDescription(aTypeName)",
      "",
      "  'Add this sphere & projection to the default.prj file.",
      "  if (file.exists(\"$HOME/default.prj\".AsFilename).not) then",
      "    defprj = ODB.Make(\"$HOME/default.prj\".AsFilename)",
      "  else",
      "    flag = MsgBox.YesNo (\"Overwrite the custom projection file?\", "
         "\"Overwrite?\", false)",
      "    if (flag) then",
      "      defprj = ODB.Make(\"$HOME/default.prj\".AsFilename)",
      "    else",
      "      defprj = ODB.Open(\"$HOME/default.prj\".AsFilename)",
      "    end",
      "  end",
      "  defprj.Add(aCoordSys)",
      "  defprj.Commit",
      "end",
      "",
      "' Apply the projection to the selected View",
      "aView = av.GetProject.FindDoc(aViewName)",
      "aView.SetProjection(aPrj)"
   };
   FILE *fp;            /* Used to open the file. */
   int i;               /* Used to traverse the static arrays. */

   if (gds->projType == GS3_LATLON) {
      /* Don't need an avenue script for a Lat/Lon grid. */
      return 0;
   }
   if ((gds->projType != GS3_LAMBERT) && (gds->projType != GS3_POLAR) &&
       (gds->projType != GS3_MERCATOR)) {
      errSprintf ("ERROR: Can not handle projection yet. \n"
                  "See Grid Definition Section (sect 3) type %d\n",
                  gds->projType);
      return -2;
   }
   if ((fp = fopen (filename, "wt")) == NULL) {
      errSprintf ("ERROR: Problems opening %s for write.", filename);
      return -1;
   }
   for (i = 0; i < (sizeof (header) / sizeof (char *)); i++) {
      fprintf (fp, "%s\n", header[i]);
   }
   switch (gds->projType) {
      case GS3_LAMBERT:
         fprintf (fp, "aPrj = Lambert.Make(aRect)\n");
         fprintf (fp, "aPrj.SetDescription(\"GRIB-Lambert\")\n");
         fprintf (fp, "aPrj.SetCentralMeridian(%f)\n", orient);
         fprintf (fp, "aPrj.SetReferenceLatitude(%f)\n", gds->scaleLat1);
         if (gds->scaleLat1 < gds->scaleLat2) {
            fprintf (fp, "aPrj.SetLowerStandardParallel(%f)\n",
                     gds->scaleLat1);
            fprintf (fp, "aPrj.SetUpperStandardParallel(%f)\n",
                     gds->scaleLat2);
         } else {
            fprintf (fp, "aPrj.SetLowerStandardParallel(%f)\n",
                     gds->scaleLat2);
            fprintf (fp, "aPrj.SetUpperStandardParallel(%f)\n",
                     gds->scaleLat1);
         }
         fprintf (fp, "aPrj.SetFalseEasting(0)\n");
         fprintf (fp, "aPrj.SetFalseNorthing(0)\n");
         fprintf (fp, "aPrj.SetSpheroid(#SPHEROID_SPHERE)\n");
         break;
      case GS3_POLAR:
         fprintf (fp, "aPrj = Stereographic.Make(aRect)\n");
         fprintf (fp, "aPrj.SetDescription(\"GRIB-Polar\")\n");
         fprintf (fp, "aPrj.SetCentralMeridian(%f)\n", orient);
         fprintf (fp, "aPrj.SetReferenceLatitude(%f)\n", gds->scaleLat1);
         break;
      case GS3_MERCATOR:
         fprintf (fp, "aPrj = Mercator.Make(aRect)\n");
         fprintf (fp, "aPrj.SetDescription(\"GRIB-Mercator\")\n");
         fprintf (fp, "aPrj.SetCentralMeridian(%f)\n", orient);
         fprintf (fp, "aPrj.SetLatitudeOfTrueScale(%f)\n", gds->meshLat);
         break;
   }
   for (i = 0; i < (sizeof (body) / sizeof (char *)); i++) {
      fprintf (fp, "%s\n", body[i]);
   }
   fprintf (fp, "  aSphere.SetName(\"GRIB Sphere\")\n");
   fprintf (fp, "  aSphere.SetMajorAndMinorAxes(%f,%f)\n",
            gds->majEarth * 1000, gds->minEarth * 1000);
   fprintf (fp, "  aSphere.SetUnits(#UNITS_LINEAR_METERS)\n");
   for (i = 0; i < (sizeof (tail) / sizeof (char *)); i++) {
      fprintf (fp, "%s\n", tail[i]);
   }
   fclose (fp);
   return 0;
}

/*****************************************************************************
 * gribWriteFloat() -- Review 12/2002
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
 *       meta = The meta file structure to generate the .flt for. (Input)
 *     attrib = Sect 5 from the parsed grib message to write. (Input)
 *       scan = Either 0 or (0100)<< 4 = 64 (How to write file.) (Input)
 *              if scan is 0 create a .flt file (For input to Esri S.A.)
 *              if scan is 64 create a .tlf file (For input to NDFD Gd)
 *      f_MSB = True if we should create MSB file, false for LSB (Input)
 *    decimal = How many decimals to round to. (Input)
 *    f_GrADS = True if you want to create a GrADS .ctl file. (Input)
 * f_SimpleWx = True if you want to simplify the weather via NDFD method,
 *              before output. (Input)
 *
 * FILES/DATABASES:
 *   Calls gribWriteEsriHdr to create an Esri ascii .hdr file.
 *   Calls gribWriteEsriPrj to create an Esri ascii .prj file.
 *   Calls gribWriteEsriAve to create an Esri ascii .ave file.
 *   Creates a .flt file, which is a binary file (Big Endian) consisting of
 *   NxM floats, starting at the upper left corner of the grid, traversing
 *   to the upper right, and then starting on the next row on the left.
 *
 * RETURNS: int (could use errSprintf())
 *  0 = OK
 * -3 = invalid calling parameters.
 * -2 = Problems opening the files.
 * -1 = illegal declaration of the grid size.
 *  1 = un-supported map projection for cmapf
 *  2 = invalid parameters for gribWriteEsriHdr.
 *  3 = invalid parameters for gribWriteEsriPrj.
 *  4 = invalid parameters for gribWriteEsriAve.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *   5/2003 AAT: removed call to .prj write.
 *   5/2003 AAT: Added rounding to decimal.
 *   5/2003 AAT: Enabled other spherical earths.
 *   6/2003 AAT: Switched to a Warning and then averaging if Dx != Dy.
 *   6/2003 AAT: Added GrADS .ctl file creation support.
 *   7/2003 AAT: Added f_SimpleWx.
 *   7/2003 AAT: Proper handling of Dx != Dy.
 *   7/2003 AAT: switched to checking against element name for Wx instead
 *          of pds2.sect2.ptrType == GS2_WXTYPE
 *   7/2003 AAT: If index is not in range of colortable, set as undef.
 *
 * NOTES
 *   Order is .flt first so if .prj stuff doesn't work, they have something.
 *   Then .hdr, .prj, then .ave (in order of importance.)
 *****************************************************************************
 */
int gribWriteFloat (const char *Filename, double *grib_Data,
                    grib_MetaData * meta, gridAttribType * attrib,
                    uChar scan, sChar f_MSB, sChar decimal, sChar f_GrADS,
                    sChar f_SimpleWx)
{
   FILE *fp;            /* The current open file pointer. */
   float *floatPtr;     /* Temporary storage to convert double data to float
                         * for write. */
   char *filename;      /* local copy of the filename. */
   int nameLen;         /* length of filename so we don't keep recomputing
                         * it. */
   int x, y;            /* Current grid cell location. */
   double orient;       /* Orientation longitude of projection (where N is
                         * up.) (between -180 and 180) */
   double *curData;     /* Pointer to current data in grib_Data. */
   double shift;        /* power of 10 used in rounding. */
   char *filename2;     /* Holds name of data file in call to CTL creation */
   double unDef;        /* Holds the missing value, if there is one. */
   sInt4 index;         /* Index into lookup table. */

   /* Perform some error checks. */
   if ((scan != 0) && (scan != GRIB2BIT_2)) {
      errSprintf ("ERROR: expecting scan to be 0 or GRIB2BIT_2 not %d",
                  scan);
      return -3;
   }
   if (meta->gds.numPts != meta->gds.Ny * meta->gds.Nx) {
      errSprintf ("ERROR: numPts != Nx * Ny? (%ld != %ld * %ld)",
                  meta->gds.numPts, meta->gds.Nx, meta->gds.Ny);
      return -1;
   }

   nameLen = strlen (Filename);
   if (nameLen < 4) {
      errSprintf ("ERROR: File %s is too short in length (it may need an "
                  "extension?)", Filename);
      return -2;
   }
   filename = (char *) malloc (nameLen + 1 * sizeof (char));
   strncpy (filename, Filename, nameLen - 3);
   if (scan == 0) {
      strncpy (filename + nameLen - 3, "flt", 3);
   } else {
      strncpy (filename + nameLen - 3, "tlf", 3);
   }
   filename[nameLen] = '\0';

   if ((fp = fopen (filename, "wb")) == NULL) {
      errSprintf ("ERROR: Problems opening %s for write.", filename);
      free (filename);
      return -2;
   }
   /* 
    * The following assumes that data has come from ParseGrid, so
    * scan flag == 0100.  ArcView S.A. wants scan flag == 0000
    * if scan == GRIB2BIT_2 == 0100 don't do any index manipulation...
    * if scan == 0 do it ArcView's way.
    */
   floatPtr = (float *) malloc (meta->gds.Nx * sizeof (float));
   if (decimal > 17)
      decimal = 17;
   if (decimal < 0)
      decimal = 0;
   shift = POWERS_ONE[decimal];
   if (attrib->missManage != 0) {
      unDef = (floor (attrib->missPrim * shift + .5)) / shift;
   } else {
      unDef = 9999;     /* This is ignored by gribWriteGradsCTL */
      if (attrib->f_maxmin) {
         if ((unDef <= attrib->max) && (unDef >= attrib->min)) {
            unDef = attrib->max + 1;
         }
      }
   }
   for (y = 0; y < meta->gds.Ny; y++) {
      /* Index manipulation see previous note... */
      if (scan == 0) {
         curData = grib_Data + ((meta->gds.Ny - 1) - y) * meta->gds.Nx;
      } else {
         curData = grib_Data + y * meta->gds.Nx;
      }
      for (x = 0; x < meta->gds.Nx; x++) {
         /* Only allowed 1 missing value in .flt format. */
         if ((attrib->missManage == 2) && (*curData == attrib->missSec)) {
            floatPtr[x] = (float) unDef;
         } else {
            if (f_SimpleWx && (strcmp (meta->element, "Wx") == 0)) {
               index = (sInt4) * curData;
               if ((index >= 0) && (index < meta->pds2.sect2.wx.dataLen)) {
                  floatPtr[x] = (float)
                     meta->pds2.sect2.wx.ugly[index].SimpleCode;
               } else {
                  floatPtr[x] = (float) unDef;
               }
            } else {
               floatPtr[x] = (float) ((floor (*curData * shift + .5)) /
                                      shift);
            }
         }
         curData++;
      }
      if (f_MSB) {
         FWRITE_BIG (floatPtr, sizeof (float), meta->gds.Nx, fp);
      } else {
         FWRITE_LIT (floatPtr, sizeof (float), meta->gds.Nx, fp);
      }
   }
   free (floatPtr);
   fclose (fp);

   if (f_GrADS) {
      filename2 = (char *) malloc ((strlen (filename) + 1) * sizeof (char));
      strcpy (filename2, filename);
      strncpy (filename + nameLen - 3, "ctl", 3);
      if (attrib->missManage == 0) {
         gribWriteGradsCTL (filename, filename2, meta, &(meta->gds), scan,
                            f_MSB, unDef, 0);
      } else {
         gribWriteGradsCTL (filename, filename2, meta, &(meta->gds), scan,
                            f_MSB, unDef, 1);
      }
      free (filename2);
   }

   /* Perform some more error checks. */
   if (GDSValid (&(meta->gds)) != 0) {
      free (filename);
      return 1;
   }

   orient = meta->gds.orientLon;
   while (orient > 180) {
      orient -= 360;
   }
   while (orient < -180) {
      orient += 360;
   }

   /* Create the .hdr file */
   strncpy (filename + nameLen - 3, "hdr", 3);
   if ((fp = fopen (filename, "wt")) == NULL) {
      errSprintf ("ERROR: Problems opening %s for write.", filename);
      free (filename);
      return -2;
   }
   if (gribWriteEsriHdr (fp, &(meta->gds), attrib, meta->gds.Dx,
                         meta->gds.Dy, orient, f_MSB, decimal) != 0) {
      preErrSprintf ("gribWriteEsriHdr had the following error\n");
      free (filename);
      fclose (fp);
      return 2;
   }
   fclose (fp);

   /* Create the .prj file */
/*
   strncpy (filename + nameLen - 3, "prj", 3);
   if ((fp = fopen (filename, "wt")) == NULL) {
      errSprintf ("ERROR: Problems opening %s for write.", filename);
      free (filename);
      return -2;
   }
   if (gribWriteEsriPrj (fp, gds, orient) != 0) {
      preErrSprintf ("gribWriteEsriPrj had the following error\n");
      free (filename);
      fclose (fp);
      return 3;
   }
   fclose (fp);
*/

   /* Create the .ave file */
   strncpy (filename + nameLen - 3, "ave", 3);
   if (gribWriteEsriAve (filename, &(meta->gds), orient) != 0) {
      preErrSprintf ("gribWriteEsriAve had the following error\n");
      free (filename);
      return 4;
   }
   free (filename);
   return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/degrib/RCS/writeflt.c,v $";
 static char rcs_id2[] = "$Id: writeflt.c,v 1.1 2004/09/16 17:14:09 dsa Exp $";}
/*  ===================================================  */

}
