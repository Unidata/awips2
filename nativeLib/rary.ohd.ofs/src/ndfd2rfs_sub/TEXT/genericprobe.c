/*****************************************************************************
 * GenericProbe() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Provide a hook to the probe routines that can be called by a fortran
 * program, in such a way as to avoid the "user" module.
 *
 * ARGUMENTS
 * filename = The GRIB file to probe. (Input)
 *  numPnts = The number of points to probe. (Input)
 *      lat = The latitudes of the points to probe. (Input)
 *      lon = The longitudes of the points to probe. (Input)
 * f_interp = true (1) if we want to perform bi-linear interp
 *            false (0) if we want nearest neighbor (Input)
 *  lenTime = The number of messages (or validTimes) in the file (Output)
 *  valTime = The array of valid times (as strings). (Output)
 *     data = The values at the various points. (Output)
 *
 * FILES/DATABASES:
 *
 * RETURNS: int
 *   0 = Ok,
 *  -3 = couldn't open the file,
 *  -4 = Grid Definition Section was not valid.
 *  -1 = can't handle weather GRIB2 files.
 *
 * HISTORY
 *   8/2004 Arthur Taylor (MDL) + Xiaobiao Fan (OHD/RSIS): Created.
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
#include <messenger_inc/msgwrappers.h>


int genericprobe (char *filename, int *lenFile,  
                  int *numPnts, double *lat, double *lon,
                  int *f_interp, int *lenTime, char valTime[][12],
                  double data[][60000])
{
   FILE *grib_fp;       /* The opened grib2 file for input. */
   long int grib_DataLen; /* Current length of grib_Data. */
   double *grib_Data;   /* Holds the grid retrieved from a GRIB2 message. */
   int c;               /* Determine if end of the file without fileLen. */
   int f_unit = 1;      /* Convert to "english" (use F instead of K for
                         * temperature) */
   IS_dataType is;      /* Un-parsed meta data for this GRIB2 message. As
                         * well as some memory used by the unpacker. */
   grib_MetaData meta;  /* The meta structure for this GRIB2 message. */
   int subgNum = 0;     /* The subgrid in the message that we are interested
                         * in. */
   double majEarth = 0; /* If > 6000 use this to over-ride the radEarth. */
   double minEarth = 0; /* If > 6000 use this to over-ride the radEarth. */
   int f_SimpleVer = 2; /* We don't use Weather, but if we did use most
                         * recent version of the weather tables. */
   long int f_endMsg = 1; /* 1 if we read the last grid in a GRIB message */
   LatLon lwlf;         /* ReadGrib2Record allows subgrids.  We want entire
                         * grid, so set the lat to -100. */
   LatLon uprt;         /* ReadGrib2Record allows subgrids.  We want entire
                         * grid, so set the lat to -100. */
   char *msg;           /* Used to print the error stack. */
   myMaparam map;       /* Used to compute the grid lat/lon points. */
   double missing = 0;  /* Missing value to use. */
   int i;               /* Counter for the points. */
   double newX, newY;   /* The location of lat/lon on the input grid. */
   sChar f_missing;     /* flag whether the cell fell off the grid. */
   long int x1, y1;     /* The nearest grid point. */
   long int row;        /* The index into grib_Data for a given x,y pair
                         * using scan-mode = 0100 = GRIB2BIT_2 */
   double ans;          /* The interpolated value at a given point. */
   int retVal;          /* The return value on error. */

   filename[*lenFile]='\0';  /*cfan*/

   /* Open the grib file. */
   if (filename != NULL) {
      if ((grib_fp = fopen (filename, "rb")) == NULL) {
       /*  fprintf (stderr, " Problems opening %s for read\n", filename); */
         writemsg(49152+384+0," Problems opening %s\n",filename);
         return -1;
      }
      else {
       /*  fprintf (stderr, " Opened %s successfully!\n", filename); */
         writemsg(49152+128+0," Opened file succesfully - %s\n",filename);
      }
   } else {
      grib_fp = stdin;
   }
   IS_Init (&is);
   MetaInit (&meta);

   /* Start loop for all messages. */
   grib_DataLen = 0;
   grib_Data = NULL;
   *lenTime = 0;
/*   *valTime = NULL; 
   *data = NULL;  */
   lwlf.lt = -100;
   uprt.lt = -100;

   while ((c = fgetc (grib_fp)) != EOF) {
      ungetc (c, grib_fp);
      /* Read the GRIB message. */
      if (ReadGrib2Record (grib_fp, f_unit, &grib_Data, &grib_DataLen, &meta,
                           &is, subgNum, majEarth, minEarth, f_SimpleVer,
                           &f_endMsg, &lwlf, &uprt) != 0) {
         msg = errSprintf (NULL);
         fprintf (stderr, "ERROR: In call to GenericProbe().\n%s", msg);
         writemsg(32768+384+0," In call to GenericProbe().\n%s\n", msg);
         free (msg);
         retVal = -3;
         goto error;
      }
      if (f_endMsg != 1) {
         subgNum++;
      } else {
         subgNum = 0;
      }

      /* Check that gds is valid before setting up map projection. */
      if (GDSValid (&(meta.gds)) != 0) {
         fprintf (stderr, "ERROR: Sect3 was not Valid.\n");
         retVal = -4;
         goto error;
      }
      /* Set up the map projection. */
      SetMapParam (&map, &(meta.gds));

      /* Figure out a missing value, if there isn't one, so that when we
       * interpolate and we are out of bounds, we can return something. */
      if (meta.gridAttrib.missManage == 0) {
         missing = 9999;
         if (meta.gridAttrib.f_maxmin) {
            if ((missing <= meta.gridAttrib.max) &&
                (missing >= meta.gridAttrib.min)) {
               missing = meta.gridAttrib.max + 1;
            }
         }
      } else {
         missing = meta.gridAttrib.missPrim;
      }

      /* Allocate space for the returned data. */
      *lenTime = *lenTime + 1;
/*      *valTime = (char **) realloc ((void *) *valTime,
                                    *lenTime * sizeof (char *));
      (*valTime)[*lenTime - 1] = (char *) malloc ((strlen (meta.validTime)
                                                   + 1) * sizeof (char));
      strcpy ((*valTime)[*lenTime - 1], meta.validTime);  */
      strcpy ((valTime)[*lenTime - 1], meta.validTime);  
/*      *data = (double **) realloc ((void *) *data,
                                   *lenTime * sizeof (double *));
      (*data)[*lenTime - 1] = (double *) malloc (*numPnts * sizeof (double)); */

      for (i = 0; i < *numPnts; i++) {
         myCll2xy (&map, lat[i], lon[i], &newX, &newY);
         f_missing = 0;
         /* Find the nearest grid cell. */
         if (newX < .5) {
            x1 = 1;
            f_missing = 1;
         } else if ((newX + .5) > meta.gds.Nx) {
            x1 = meta.gds.Nx;
            f_missing = 1;
         } else {
            x1 = (long int) (newX + .5);
         }
         if (newY < .5) {
            y1 = 1;
            f_missing = 1;
         } else if ((newY + .5) > meta.gds.Ny) {
            y1 = meta.gds.Ny;
            f_missing = 1;
         } else {
            y1 = (long int) (newY + .5);
         }
         if (!*f_interp) {
            /* Get the x1, y1 value. */
            if (!f_missing) {
               XY2ScanIndex (&row, x1, y1, GRIB2BIT_2, meta.gds.Nx,
                             meta.gds.Ny);
               ans = grib_Data[row];
            } else {
               ans = missing;
            }
         } else {
            /* Figure out data value at this lat/lon */
            ans = BiLinearCompute (grib_Data, &map, lat[i], lon[i],
                                   missing, meta.gds.Nx, meta.gds.Ny,
                                   meta.gridAttrib.missManage,
                                   meta.gridAttrib.missSec);
         }
         if (strcmp (meta.element, "Wx") != 0) {
/*            (*data)[*lenTime - 1][i] = ans;  */
            (data)[*lenTime - 1][i] = ans;  

         } else {
            /* Handle the weather case. */
            fprintf (stderr, "ERROR: Currently doesn't handle weather "
                     "strings.\n");
            retVal = -1;
            goto error;
         }
      }

      MetaFree (&meta);
   }
   /* End loop for all messages. */
   free (grib_Data);
   fclose (grib_fp);
   MetaFree (&meta);
   IS_Free (&is);
   return 0;
error:
   free (grib_Data);
   fclose (grib_fp);
   MetaFree (&meta);
   IS_Free (&is);
   return retVal;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/ndfd2rfs_sub/RCS/genericprobe.c,v $";
 static char rcs_id2[] = "$Id: genericprobe.c,v 1.3 2004/11/02 20:16:52 xfan Exp $";}
/*  ===================================================  */

}
