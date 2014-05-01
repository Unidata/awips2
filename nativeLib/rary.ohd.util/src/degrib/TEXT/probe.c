/*****************************************************************************
 * probe.c
 *
 * DESCRIPTION
 *    This file contains the code needed to probe a GRIB2 file for all info
 * pertaining to the given lat/lon point or file containing points.
 *
 * Stores answer in ".prb" file using "-out" option for output filename.
 *
 *    Does bi-linear interpolation from the 4 surrounding grid cells to that
 * point.  If any of the 4 surrounding cells are missing, the probed point
 * gets a value of missing.
 *
 * HISTORY
 *   12/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES

*****************************************************************************
 */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "degrib_inc/myerror.h"
#include "degrib_inc/myutil.h"
#include "degrib_inc/type.h"
#include "degrib_inc/userparse.h"
#include "degrib_inc/meta.h"
#include "degrib_inc/degrib2.h"
#include "degrib_inc/interp.h"
#include "degrib_inc/probe.h"
#include "degrib_inc/scan.h"
#include "degrib_inc/mymapf.h"

/*****************************************************************************
 * ReadPntFile() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Read in a set of points from pntFile, for use with the probe command.
 *
 * ARGUMENTS
 * pntFile = File to read the points in from. (Input)
 *    pnts = The points read in. (Input/Output)
 * NumPnts = The number of points read in (Input/Output)
 *  labels = The Labels for those points (Input/Output)
 *
 * FILES/DATABASES:
 *   A comma delimited file with (place, lat, lon) per line.
 *   A '#' at beginnig of line denotes line is commented out.
 *   Don't really need commas, just spaces.
 *
 * RETURNS: int (could use errSprintf())
 *  0 = Ok.
 * -1 = Problems opening file for read.
 *
 * HISTORY
 *  12/2002 Arthur Taylor (MDL/RSIS): Created.
 *   8/2003 AAT: Found that it didn't handle "lat,lon" correctly.
 *
 * NOTES

*****************************************************************************
 */
int ReadPntFile (char *pntFile, Point ** pnts, int *NumPnts, char ***labels)
{
   FILE *fp;            /* Ptr to point file. */
   char *buffer = NULL; /* Holds a line from the file. */
   int buffLen = 0;     /* Current length of buffer. */
   char *first;         /* The first word in buffer. */
   char *second;        /* The second word in buffer. */
   char *third;         /* The third word in buffer. */
   int numPnts;         /* Local count of number of points. */

   if ((fp = fopen (pntFile, "rt")) == NULL) {
      errSprintf ("ERROR: opening file %s for read", pntFile);
      return -1;
   }
   numPnts = *NumPnts;
   while (reallocFGets (&buffer, &buffLen, fp) > 0) {
      first = strtok (buffer, " ,\n");
      if ((first != NULL) && (*first != '#')) {
         second = strtok (NULL, " ,\n");
         if (second != NULL) {
            /* Assume: Name, lat, lon */
            third = strtok (NULL, " ,\n");
            if (third != NULL) {
               numPnts++;
               *pnts = (Point *) realloc ((void *) *pnts,
                                          numPnts * sizeof (Point));
               (*pnts)[numPnts - 1].Y = atof (second);
               (*pnts)[numPnts - 1].X = atof (third);
               (*pnts)[numPnts - 1].f_valid = 1;
               *labels = (char **) realloc ((void *) *labels,
                                            numPnts * sizeof (char *));
               mallocSprintf (&((*labels)[numPnts - 1]), "%s", first);
            } else {
               /* Assume: lat, lon */
               numPnts++;
               *pnts = (Point *) realloc ((void *) *pnts,
                                          numPnts * sizeof (Point));
               (*pnts)[numPnts - 1].Y = atof (first);
               (*pnts)[numPnts - 1].X = atof (second);
               (*pnts)[numPnts - 1].f_valid = 1;
               *labels = (char **) realloc ((void *) *labels,
                                            numPnts * sizeof (char *));
               mallocSprintf (&((*labels)[numPnts - 1]), "(%f,%f)",
                              (*pnts)[numPnts - 1].Y,
                              (*pnts)[numPnts - 1].X);
            }
         } else {
            *NumPnts = numPnts;
            errSprintf ("ERROR: problems parsing '%s'", buffer);
            free (buffer);
            fclose (fp);
            return -1;
         }
      }
   }
   free (buffer);
   fclose (fp);
   *NumPnts = numPnts;
   return 0;
}

/*****************************************************************************
 * PrintProbeWx() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Handle printing of Weather while probing.  Just prints enough to describe
 * the weather... Presumes calling routine will handle newlines, etc.
 *
 * ARGUMENTS
 *    out_fp = Where to print to. (Output)
 *       ans = index into the wx table to output. (Input)
 *        wx = The parsed wx data structure. (Input)
 *   logName = The name of a file to log messages to (or NULL) (Input)
 *    x1, y1 = The cell value we are probing (used for diagnostics) (Input)
 *  lat, lon = The location that we are probing (diagnositcs) (Input)
 * separator = User specified separator between fields in the output (Input)
 *   element = The name of this element (wx) (diagnositcs) (Input)
 *  unitName = The unit type of this element (none) (diagnositcs) (Input)
 *   comment = Long form of the name and unit (diagnositcs) (Input)
 *   refTime = The time the data was created (diagnositcs) (Input)
 * validTime = The time the forecast is valid for (diagnositcs) (Input)
 * f_WxParse = The user specified method for parsing this data (Input)
 *
 * FILES/DATABASES: None:
 *
 * RETURNS: Void
 *
 * HISTORY
 *   8/2003 Arthur Taylor (MDL/RSIS): Broke this out of Probe()
 *   8/2003 AAT: Added -WxParse option.
 *   3/2004 AAT: Rewrote to be more flexible.
 *
 * NOTES

*****************************************************************************
 */
static void PrintProbeWx (FILE * out_fp, double ans, sect2_WxType * wx,
                          char *logName, int x1, int y1, double lat,
                          double lon, char *separator, char *element,
                          char *unitName, char *comment, char *refTime,
                          char *validTime, sChar f_WxParse)
{
   long int wxIndex;    /* The index into the wx table. */
   FILE *logFp;         /* Used to log errors in Wx keys. */
   int j;               /* loop counter over the weather keys. */

   wxIndex = (long int) ans;
   if ((wxIndex >= 0) && (wxIndex < wx->dataLen)) {

      /* Log any errors that have occured in the weather string. */
      if ((wx->ugly[wxIndex].errors != NULL) && (logName != NULL)) {
         logFp = fopen (logName, "at");
         fprintf (logFp, "(%d,%d,%f,%f)%s", x1, y1, lat, lon, separator);
         if (unitName != NULL) {
            fprintf (logFp, "%s%s%s", element, unitName, separator);
         } else {
            fprintf (logFp, "%s%s%s", element, comment, separator);
         }
         fprintf (logFp, "%s%s%s%s\n", refTime, separator, validTime,
                  separator);
         fprintf (logFp, "%s\n", wx->ugly[wxIndex].errors);
         fclose (logFp);
      }

      /* Print out the weather string according to f_WxParse. */
      switch (f_WxParse) {
         case 0:
            fprintf (out_fp, "%s", wx->data[wxIndex]);
            break;
         case 1:
            for (j = 0; j < NUM_UGLY_WORD; j++) {
               if (wx->ugly[wxIndex].english[j] != NULL) {
                  if (j != 0) {
                     fprintf (out_fp, " and ");
                  }
                  fprintf (out_fp, "%s", wx->ugly[wxIndex].english[j]);
               } else {
                  if (j == 0) {
                     fprintf (out_fp, "No Weather");
                  }
                  break;
               }
            }
            break;
         case 2:
            fprintf (out_fp, "%d", wx->ugly[wxIndex].SimpleCode);
            break;
      }
   } else {
      fprintf (out_fp, "%ld", wxIndex);
   }
}

/*****************************************************************************
 * GRIB2ProbeStyle0() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Handle output'ing the original output style of probed data.  This form
 * is: "Element, unit, refTime, valTime, location1, location2, ... "
 *   Changed the algorithm for this to be concerned with just one line at a
 * time... The calling routine will loop appropriately.
 *
 * ARGUMENTS
 *      f_label = Flag if we just want to print the header out. (Input)
 *       out_fp = Where to print to. (Output)
 *    grib_Data = Extracted grid to probe from. (Input)
 * grib_DataLen = Size of grib_Data. (Input)
 *          usr = User choices. (Input)
 *      numPnts = number of points to probe. (Input)
 *         pnts = lat/lon of points to probe. (Input)
 *       labels = Station Names for each point. (Input)
 *         meta = The meta structure for a GRIB2 message (Input)
 *          map = Used to compute the lat/lon points (Input)
 *      missing = The missing value for this grid (Input)
 *    f_surface = 0 => no surface info, 1 => short form of surface name
 *                2 => long form of surface name (In)
 *
 * FILES/DATABASES: None:
 *
 * RETURNS: int (could use errSprintf())
 *  0 = Ok.
 *
 * HISTORY
 *   8/2003 Arthur Taylor (MDL/RSIS): Broke this out of Probe()
 *   8/2003 AAT: Removed dependence on fileLen
 *   3/2004 AAT: Rewrote to be more flexible.
 *   5/2004 AAT: Modified so probes that are off the grid, return missing.
 *
 * NOTES

*****************************************************************************
 */
static void GRIB2ProbeStyle0 (sChar f_label, FILE * out_fp,
                              double *grib_Data, long int grib_DataLen,
                              userType * usr, int numPnts, Point * pnts,
                              char **labels, grib_MetaData * meta,
                              myMaparam * map, double missing,
                              sChar f_surface)
{
   int i;               /* Counter for the points. */
   char format[20];     /* Format to print the data with. */
   double newX, newY;   /* The location of lat/lon on the input grid. */
   long int x1, y1;     /* The nearest grid point. */
   long int row;        /* The index into grib_Data for a given x,y pair
                         * using scan-mode = 0100 = GRIB2BIT_2 */
   double ans;          /* The interpolated value at a given point. */
   sChar f_missing;     /* flag whether the cell fell off the grid. */

   if (f_label) {
      /* Print labels */
      if (f_surface != 0) {
         fprintf (out_fp, "element%sunit%sSurface%srefTime%svalidTime%s",
                  usr->separator, usr->separator, usr->separator,
                  usr->separator, usr->separator);
      } else {
         fprintf (out_fp, "element%sunit%srefTime%svalidTime%s",
                  usr->separator, usr->separator, usr->separator,
                  usr->separator);
      }
      for (i = 0; i < numPnts; i++) {
         if (i != numPnts - 1) {
            fprintf (out_fp, "%s%s", labels[i], usr->separator);
         } else {
            fprintf (out_fp, "%s", labels[i]);
         }
      }
      fprintf (out_fp, "\n");
   } else {
      /* Print out probe data. */
      if (meta->unitName != NULL) {
         fprintf (out_fp, "%s%s%s%s", meta->element, usr->separator,
                  meta->unitName, usr->separator);
      } else {
         fprintf (out_fp, "%s%s%s%s", meta->element, usr->separator,
                  meta->comment, usr->separator);
      }
      if (f_surface == 1) {
         fprintf (out_fp, "%s%s", meta->shortFstLevel, usr->separator);
      } else if (f_surface == 2) {
         fprintf (out_fp, "%s%s", meta->longFstLevel, usr->separator);
      }
      fprintf (out_fp, "%s%s%s%s", meta->refTime, usr->separator,
               meta->validTime, usr->separator);

      sprintf (format, "%%.%df", usr->decimal);
      for (i = 0; i < numPnts; i++) {
         myCll2xy (map, pnts[i].Y, pnts[i].X, &newX, &newY);
         f_missing = 0;
         /* Find the nearest grid cell. */
         if (newX < .5) {
            x1 = 1;
            f_missing = 1;
         } else if ((newX + .5) > meta->gds.Nx) {
            x1 = meta->gds.Nx;
            f_missing = 1;
         } else {
            x1 = (long int) (newX + .5);
         }
         if (newY < .5) {
            y1 = 1;
            f_missing = 1;
         } else if ((newY + .5) > meta->gds.Ny) {
            y1 = meta->gds.Ny;
            f_missing = 1;
         } else {
            y1 = (long int) (newY + .5);
         }
         if (!(usr->f_interp)) {
            /* Get the x1, y1 value. */
            if (!f_missing) {
               XY2ScanIndex (&row, x1, y1, GRIB2BIT_2, meta->gds.Nx,
                             meta->gds.Ny);
               ans = grib_Data[row];
            } else {
               ans = missing;
            }
         } else {
            /* Figure out data value at this lat/lon */
            ans = BiLinearCompute (grib_Data, map, pnts[i].Y, pnts[i].X,
                                   missing, meta->gds.Nx, meta->gds.Ny,
                                   meta->gridAttrib.missManage,
                                   meta->gridAttrib.missSec);
         }
         if (strcmp (meta->element, "Wx") != 0) {
            fprintf (out_fp, format, myRound (ans, usr->decimal));
         } else {
            /* Handle the weather case. */
            PrintProbeWx (out_fp, ans, &(meta->pds2.sect2.wx), usr->logName,
                          x1, y1, pnts[i].Y, pnts[i].X, usr->separator,
                          meta->element, meta->unitName, meta->comment,
                          meta->refTime, meta->validTime, usr->f_WxParse);
         }
         if (i != numPnts - 1) {
            fprintf (out_fp, "%s", usr->separator);
         }
      }
      fprintf (out_fp, "\n");
   }
}

/*****************************************************************************
 * GRIB2ProbeStyle1() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Handle output'ing the first output style (after the original) of probed
 * data.  This form is: "location, Element[unit], refTime, valTime, value"
 *   Changed the algorithm for this to be concerned with just one line at a
 * time... The calling routine will loop appropriately.
 *
 * ARGUMENTS
 *      f_label = Flag if we just want to print the header out. (Input)
 *       out_fp = Where to print to. (Output)
 *    grib_Data = Extracted grid to probe from. (Input)
 * grib_DataLen = Size of grib_Data. (Input)
 *          usr = User choices. (Input)
 *      numPnts = number of points to probe. (Input)
 *         pnts = lat/lon of points to probe. (Input)
 *       labels = Station Names for each point. (Input)
 *         meta = The meta structure for a GRIB2 message (Input)
 *          map = Used to compute the lat/lon points (Input)
 *      missing = The missing value for this grid (Input)
 *    f_surface = 0 => no surface info, 1 => short form of surface name
 *                2 => long form of surface name (In)
 *      f_cells = 0 => lat/lon pnts, 1 => cells in pnts, 2 => all Cells (In)
 *
 * FILES/DATABASES: None:
 *
 * RETURNS: int (could use errSprintf())
 *  0 = Ok.
 *
 * HISTORY
 *   8/2003 Arthur Taylor (MDL/RSIS): Created
 *   8/2003 AAT: Removed dependence on fileLen
 *   3/2004 AAT: Rewrote to be more flexible, and to combine Style2() with
 *          Style1()... Style2() is now f_cells = 2, f_surface = 0, and
 *          the original Style1() is f_cells = 0, f_surface = 0
 *   5/2004 AAT: Modified so probes that are off the grid, return missing.
 *
 * NOTES

*****************************************************************************
 */
static void GRIB2ProbeStyle1 (sChar f_label, FILE * out_fp,
                              double *grib_Data, long int grib_DataLen,
                              userType * usr, int numPnts, Point * pnts,
                              char **labels, grib_MetaData * meta,
                              myMaparam * map, double missing,
                              sChar f_surface, sChar f_cells)
{
   int i;               /* Counter for the points. */
   char format[20];     /* Format to print the data with. */
   double newX, newY;   /* The location of lat/lon on the input grid. */
   long int x1, y1;     /* The nearest grid point. */
   long int row;        /* The index into grib_Data for a given x,y pair
                         * using scan-mode = 0100 = GRIB2BIT_2 */
   double ans;          /* The interpolated value at a given point. */
   double lat, lon;     /* The lat/lon at the grid cell. */
   sChar f_continue;    /* Flag to continue looping over the points or grid */
   sChar f_missing;     /* flag whether the cell fell off the grid. */

   /* Print labels */
   if (f_label) {
      if (f_surface != 0) {
         fprintf (out_fp, "Location%sElement[Unit]%sSurface%srefTime"
                  "%svalidTime%sValue\n", usr->separator, usr->separator,
                  usr->separator, usr->separator, usr->separator);
      } else {
         fprintf (out_fp, "Location%sElement[Unit]%srefTime%svalidTime"
                  "%sValue\n", usr->separator, usr->separator,
                  usr->separator, usr->separator);
      }
   } else {
      sprintf (format, "%%.%df", usr->decimal);
      f_continue = 1;
      i = 0;            /* counter over cells or lat/lon. */
      while (f_continue) {
         f_missing = 0;
         if (f_cells == 2) { /* All cells. */
            if (i == meta->gds.Nx * meta->gds.Ny) {
               f_continue = 0;
               break;
            }
            x1 = (i % meta->gds.Nx) + 1;
            y1 = (i / meta->gds.Nx) + 1;
            newX = x1;
            newY = y1;
            myCxy2ll (map, x1, y1, &lat, &lon);
            lat = myRound (lat, usr->LatLon_Decimal);
            lon = myRound (lon, usr->LatLon_Decimal);
            /* Get the x1, y1 value. */
            XY2ScanIndex (&row, x1, y1, GRIB2BIT_2, meta->gds.Nx,
                          meta->gds.Ny);
            ans = grib_Data[row];

         } else if (f_cells == 1) { /* Specified cells. */
            if (i == numPnts) {
               f_continue = 0;
               break;
            }
            x1 = pnts[i].X;
            y1 = pnts[i].Y;
            newX = x1;
            newY = y1;
            myCxy2ll (map, x1, y1, &lat, &lon);
            lat = myRound (lat, usr->LatLon_Decimal);
            lon = myRound (lon, usr->LatLon_Decimal);
            if (x1 < .5) {
               f_missing = 1;
            } else if ((x1 + .5) > meta->gds.Nx) {
               f_missing = 1;
            }
            if (y1 < .5) {
               f_missing = 1;
            } else if ((y1 + .5) > meta->gds.Ny) {
               f_missing = 1;
            }
            /* Get the x1, y1 value. */
            if (!f_missing) {
               XY2ScanIndex (&row, x1, y1, GRIB2BIT_2, meta->gds.Nx,
                             meta->gds.Ny);
               ans = grib_Data[row];
            } else {
               ans = missing;
            }

         } else {       /* lat/lon point. */
            if (i == numPnts) {
               f_continue = 0;
               break;
            }
            lat = pnts[i].Y;
            lon = pnts[i].X;
            /* Find the nearest grid cell. */
            myCll2xy (map, lat, lon, &newX, &newY);
            if (newX < .5) {
               x1 = 1;
               f_missing = 1;
            } else if ((newX + .5) > meta->gds.Nx) {
               x1 = meta->gds.Nx;
               f_missing = 1;
            } else {
               x1 = (long int) (newX + .5);
            }
            if (newY < .5) {
               y1 = 1;
               f_missing = 1;
            } else if ((newY + .5) > meta->gds.Ny) {
               y1 = meta->gds.Ny;
               f_missing = 1;
            } else {
               y1 = (long int) (newY + .5);
            }
            if (!(usr->f_interp)) {
               /* Get the x1, y1 value. */
               if (!f_missing) {
                  XY2ScanIndex (&row, x1, y1, GRIB2BIT_2, meta->gds.Nx,
                                meta->gds.Ny);
                  ans = grib_Data[row];
               } else {
                  ans = missing;
               }
            } else {
               /* Figure out data value at this lat/lon */
               ans = BiLinearCompute (grib_Data, map, pnts[i].Y, pnts[i].X,
                                      missing, meta->gds.Nx, meta->gds.Ny,
                                      meta->gridAttrib.missManage,
                                      meta->gridAttrib.missSec);
            }
         }

         /* Print the first part of the line. */
         /* Find out if user doesn't want us to use labels[], for -cells
          * all, we never use labels[]. */
         if ((usr->f_nLabel) || (f_cells == 2)) {
            fprintf (out_fp, "(%f,%f,%f,%f)%s",
                     myRound (newX, usr->LatLon_Decimal),
                     myRound (newY, usr->LatLon_Decimal), lat, lon,
                     usr->separator);
/*
            fprintf (out_fp, "(%ld,%ld,%f,%f)%s", x1, y1, lat, lon,
                     usr->separator);
*/
         } else {
            fprintf (out_fp, "%s%s", labels[i], usr->separator);
         }
         if (meta->unitName != NULL) {
            fprintf (out_fp, "%s%s%s", meta->element, meta->unitName,
                     usr->separator);
         } else {
            fprintf (out_fp, "%s%s%s", meta->element, meta->comment,
                     usr->separator);
         }
         if (f_surface == 1) {
            fprintf (out_fp, "%s%s", meta->shortFstLevel, usr->separator);
         } else if (f_surface == 2) {
            fprintf (out_fp, "%s%s", meta->longFstLevel, usr->separator);
         }
         fprintf (out_fp, "%s%s%s%s", meta->refTime, usr->separator,
                  meta->validTime, usr->separator);
         if (strcmp (meta->element, "Wx") != 0) {
            fprintf (out_fp, format, myRound (ans, usr->decimal));
         } else {
            /* Handle the weather case. */
            if (!f_missing) {
               PrintProbeWx (out_fp, ans, &(meta->pds2.sect2.wx),
                             usr->logName, x1, y1, lat, lon, usr->separator,
                             meta->element, meta->unitName, meta->comment,
                             meta->refTime, meta->validTime, usr->f_WxParse);
            } else {
               fprintf (out_fp, "%.0f", ans);
            }
         }
         fprintf (out_fp, "\n");
         i++;
      }
   }
}

/*****************************************************************************
 * GRIB2Probe() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Main control procedure for the -P option.
 *
 * ARGUMENTS
 *      usr = User choices. (Input)
 *       is = Un-parsed meta data for this GRIB2 message. As well as some
 *            memory used by the unpacker. (Reduce memory load) (In)
 *     meta = The meta structure for a GRIB2 message.
 *            (Passed in to reduce memory load) (Input)
 *
 * FILES/DATABASES:
 *    Opens a GRIB2 file for reading given its filename.
 *
 * RETURNS: int (could use errSprintf())
 *  0 = Ok.
 *
 * HISTORY
 *  12/2002 Arthur Taylor (MDL/RSIS): Created.
 *   5/2003 AAT: Modified it to look at usr->Interp to determine if we should
 *          perform bi-linear interpolation or just find nearest point.
 *   6/2003 AAT: Added refdate/time to just below label string.
 *   6/2003 AAT: Switched to a Warning and then averaging if Dx != Dy.
 *   7/2003 AAT: Proper handling of Dx != Dy.
 *   7/2003 Matthew T. Kallio (matt@wunderground.com):
 *          "If tests" had allowed : 0 <= newX < Nx + .5 (same for y).
 *          Should be : 0 <= newX < Nx
 *   7/2003 AAT: Added ability to override the radEarth.
 *   8/2003 AAT: Added -separator option.
 *   8/2003 AAT: Separated out the weather from this procedure.
 *   8/2003 AAT: Separated out the style 0 output.
 *   8/2003 AAT: Removed dependence on fileLen
 *  10/2003 AAT: Added ability to handle "-pnt all"
 *   3/2004 AAT: Rewrote to take some of the work out of Style0() and Style1()
 *
 * NOTES
 *   Passing 'is' and 'meta' in, mainly for tcldegrib memory considerations.

*****************************************************************************
 */
int GRIB2Probe (userType * usr, IS_dataType * is, grib_MetaData * meta)
{
   Point *pnts = NULL;  /* Array of points we are interested in. */
   char **labels = NULL; /* Array of labels for the points. */
   int numPnts = 0;     /* How many points in pnts */
   FILE *grib_fp;       /* The opened grib2 file for input. */
   char *outfile;       /* Temporary storage for output filename. */
   int outLen;          /* Length of outfile. */
   FILE *out_fp;        /* The opened ".prb" file for output. */
   int i;               /* Counter for the points. */
   sChar f_cellsAll;    /* 0 lat/lon pnts, 1 cells in pnts, 2 all Cells */
   sChar f_style;       /* 0 use Style0(), 1 use Style1() */
   sChar f_surface;     /* 0 no surface info, 1 short form of surface name */
   myMaparam map;       /* Used to compute the grid lat/lon points. */
   double missing = 0;  /* Missing value to use. */
   double *grib_Data;   /* Holds the grid retrieved from a GRIB2 message. */
   long int grib_DataLen; /* Current length of grib_Data. */
   int c;               /* Determine if end of the file without fileLen. */
   int subgNum = 0;     /* The subgrid in the message that we are interested
                         * in. */
   long int f_endMsg = 1; /* 1 if we read the last grid in a GRIB message */

   /* Open the grib file. */
   if (usr->inName != NULL) {
      if ((grib_fp = fopen (usr->inName, "rb")) == NULL) {
         errSprintf ("Problems opening %s for read\n", usr->inName);
         return -1;
      }
   } else {
      grib_fp = stdin;
   }

   f_cellsAll = usr->f_cellsAll;
   /* Find the points we want to probe. */
   if (usr->pnt.f_valid) {
      pnts = (Point *) malloc (sizeof (Point));
      pnts[0] = usr->pnt;
      labels = (char **) malloc (sizeof (char *));
      mallocSprintf (&(labels[0]), "(%f,%f)", pnts[0].Y, pnts[0].X);
      numPnts = 1;
   }
   if (usr->pntFile != NULL) {
      if (ReadPntFile (usr->pntFile, &pnts, &numPnts, &labels) != 0) {
         preErrSprintf ("ERROR: In call to ReadPntFile.\n");
         for (i = 0; i < numPnts; i++) {
            free (labels[i]);
         }
         free (pnts);
         free (labels);
         fclose (grib_fp);
         return -2;
      }
   } else if (!usr->pnt.f_valid) {
      if (usr->f_cellsAll != 2) {
         errSprintf ("ERROR: -pnt was not initialized.\n");
         for (i = 0; i < numPnts; i++) {
            free (labels[i]);
         }
         free (pnts);
         free (labels);
         fclose (grib_fp);
         return -2;
      } else {
         f_cellsAll = 2;
      }
   }

   /* Open the output file. */
   if (usr->outName != NULL) {
      outLen = strlen (usr->outName);
      outfile = (char *) malloc ((outLen + 1) * sizeof (char));
      strcpy (outfile, usr->outName);
      outfile[outLen] = '\0';
      strncpy (outfile + outLen - 3, "prb", 3);
      out_fp = fopen (outfile, "wt");
      free (outfile);
   } else {
      out_fp = stdout;
   }

   f_surface = usr->f_surface;
   f_style = usr->f_pntStyle;
   if (f_style == 2) {
      f_style = 1;
      if (f_surface == 0) {
         f_surface = 1;
      }
   }
   if (f_cellsAll == 2) {
      f_style = 1;
   }

   if (f_style == 0) {
/* Call GRIB2ProbeStyle0 for just header. */
      GRIB2ProbeStyle0 (1, out_fp, NULL, 0, usr, numPnts, pnts, labels, meta,
                        &map, missing, f_surface);
   } else {
/* Call GRIB2ProbeStyle1 for just header. */
      GRIB2ProbeStyle1 (1, out_fp, NULL, 0, usr, numPnts, pnts, labels, meta,
                        &map, missing, f_surface, f_cellsAll);
   }

   /* Start loop for all messages. */
   grib_DataLen = 0;
   grib_Data = NULL;

   while ((c = fgetc (grib_fp)) != EOF) {
      ungetc (c, grib_fp);
      /* Read the GRIB message. */
      if (ReadGrib2Record (grib_fp, usr->f_unit, &grib_Data, &grib_DataLen,
                           meta, is, subgNum, usr->majEarth, usr->minEarth,
                           usr->f_SimpleVer, &f_endMsg, &(usr->lwlf),
                           &(usr->uprt)) != 0) {
         preErrSprintf ("ERROR: In call to ReadGrib2Record.\n");
         for (i = 0; i < numPnts; i++) {
            free (labels[i]);
         }
         free (pnts);
         free (labels);
         fclose (grib_fp);
         free (grib_Data);
         return -3;
      }
      if (f_endMsg != 1) {
         subgNum++;
      } else {
         subgNum = 0;
      }

      /* Check that gds is valid before setting up map projection. */
      if (GDSValid (&meta->gds) != 0) {
         preErrSprintf ("ERROR: Sect3 was not Valid.\n");
         for (i = 0; i < numPnts; i++) {
            free (labels[i]);
         }
         free (pnts);
         free (labels);
         fclose (grib_fp);
         free (grib_Data);
         return -4;
      }
      /* Set up the map projection. */
      SetMapParam (&map, &(meta->gds));

      /* Figure out a missing value, if there isn't one, so that when we
       * interpolate and we are out of bounds, we can return something. */
      if (meta->gridAttrib.missManage == 0) {
         missing = 9999;
         if (meta->gridAttrib.f_maxmin) {
            if ((missing <= meta->gridAttrib.max) &&
                (missing >= meta->gridAttrib.min)) {
               missing = meta->gridAttrib.max + 1;
            }
         }
      } else {
         missing = meta->gridAttrib.missPrim;
      }
      if (f_style == 0) {
         GRIB2ProbeStyle0 (0, out_fp, grib_Data, grib_DataLen, usr, numPnts,
                           pnts, labels, meta, &map, missing, f_surface);
      } else {
         GRIB2ProbeStyle1 (0, out_fp, grib_Data, grib_DataLen, usr, numPnts,
                           pnts, labels, meta, &map, missing, f_surface,
                           f_cellsAll);
      }
      MetaFree (meta);
   }
   /* End loop for all messages. */
   free (grib_Data);

   for (i = 0; i < numPnts; i++) {
      free (labels[i]);
   }
   free (pnts);
   free (labels);
   fclose (grib_fp);
   if (usr->outName != NULL) {
      fclose (out_fp);
   }
   return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/degrib/RCS/probe.c,v $";
 static char rcs_id2[] = "$Id: probe.c,v 1.1 2004/09/16 17:14:09 dsa Exp $";}
/*  ===================================================  */

}

