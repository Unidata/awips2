/*****************************************************************************
 * writegra.c
 *
 * DESCRIPTION
 *    This file contains all the routines used to write the grid out to a
 * binary format that GrADS can use.  Currently this means adding a .ctl
 * file which allows GrADS software to use the .flt file.
 *
 *    The .flt file can be considered a single layer / level.  GrADS prefers
 * to work with the cube (all the GRIB messages) (similar to what NetCDF
 * needs).  So in the future I hope to have an option to dump the data cube.
 * Note: GrADS does have a NetCDF conversion routine..
 *
 * HISTORY
 *   6/2003 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 * 1) Currently GrADS has hardwired the radius of earth to 6371.2 instead
 * of what GRIB messages usually are which is 6367.47.
 *****************************************************************************
 */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include "degrib_inc/meta.h"
#include "degrib_inc/mymapf.h"
#include "degrib_inc/tendian.h"
#include "degrib_inc/write.h"
#include "degrib_inc/myassert.h"
#include "degrib_inc/myerror.h"

extern double POWERS_ONE[];

/*****************************************************************************
 * gribWriteGradsCTL() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To write the control file for GrADS (.clt file), so that they can use
 * the associated .flt file.
 *
 * ARGUMENTS
 *  CLTFile = Name of control file to write to. (Output)
 * DataFile = Name of Data file to refer to. (Input)
 *     meta = The meta file structure to generate the CTL file for. (Input)
 *      gds = The grid used in the DataFile. (typically = meta->gds, but
 *            if we used the interp option, then it isn't). (Input)
 *     scan = Either 0 or (0100)<< 4 = 64 (Input)
 *            if scan is 0 use "OPTIONS yref"
 *    f_MSB = True if we should create MSB file, false for LSB (Input)
 *    unDef = The undefined value. (Input)
 *  f_unDef = True if unDef is valid. (Input)
 *
 * FILES/DATABASES:
 *
 * RETURNS: int
 *  0 = OK
 * -2 = unsupported map projection.
 *
 * HISTORY
 *   6/2003 Arthur Taylor (MDL/RSIS): Created.
 *   7/2003 AAT: Proper handling of Dx != Dy.
 *
 * NOTES
 * 1) Since the Windows version of GrADS can't handle the
 *    <carriage return><line feed> that the pc creates with "wt", we use
 *    a "wb" to open the file.
 *****************************************************************************
 */
void gribWriteGradsCTL (char *CLTFile, char *DataFile, grib_MetaData * meta,
                        gdsType * gds, uChar scan, sChar f_MSB, double unDef,
                        uChar f_unDef)
{
   FILE *op;            /* The open file pointer to CLTFile. */
   char buffer[100];    /* Temporary memory when formating the time. */
   double orient;       /* Orientation longitude of projection (where N is
                         * up.) (between -180 and 180) */
   myMaparam map;       /* Used to help compute north polar stereographic
                         * projection, so we can find poleI, poleJ */
   double poleI, poleJ; /* I,J values of the pole in the n.p.s. projection */
   char *ptr;           /* Helps get rid of path dependence in DataFile. */
   char *msg;           /* Used to print the error stack. */

   if (gds->projType == GS3_MERCATOR) {
      fprintf (stderr, "Warning! GrADS does not provide explicit support"
               " for a mercator grid...\n Attempting to use lambert "
               " conformal conic, since mercator could be considered "
               " to be one.\n");
   }
   if ((gds->projType == GS3_LAMBERT) || (gds->projType == GS3_MERCATOR) ||
       (gds->projType == GS3_POLAR)) {
      if (gds->majEarth != 6371.2) {
         fprintf (stderr, "Warning! GrADS used radEarth = 6371.2, your data"
                  " used radEarth = %f\n", gds->majEarth);
      }
      if (!gds->f_sphere) {
         fprintf (stderr, "Warning! GrADS used a spherical earth, your data"
                  " did not.\n");
      }
   } else if (gds->projType != GS3_LATLON) {
      fprintf (stderr, "Warning! GrADS does not support a pre-projected "
               " grid of this type.  \nYou can still work with it as an"
               " unprojected grid.\n");
   }
   if (GDSValid (gds) != 0) {
      msg = errSprintf (NULL);
      fprintf (stderr, "\nError was: %s", msg);
      free (msg);
      return;
   }

   /* Has to be "wb" or else \n becomes 2 char in MS-Windows which breaks
    * GrADS. */
   op = fopen (CLTFile, "wb");
   /* Try to get rid of path dependence in DataFile name. */
   if ((ptr = strrchr (DataFile, '/')) == NULL) {
      if ((ptr = strrchr (DataFile, '\\')) == NULL) {
         ptr = DataFile;
      } else {
         ptr++;
      }
   } else {
      ptr++;
   }
   fprintf (op, "DSET ^%s\n", ptr);

   /* Attempt to provide the projection information. */
   orient = gds->orientLon;
   while (orient > 180) {
      orient -= 360;
   }
   while (orient < -180) {
      orient += 360;
   }
   if ((gds->projType == GS3_LAMBERT) || (gds->projType == GS3_MERCATOR)) {
      fprintf (op, "* MAJEARTH %f (GrADS used 6371.2) \n", gds->majEarth);
      fprintf (op, "* MINEARTH %f (GrADS used 6371.2) \n", gds->minEarth);
      fprintf (op, "pdef %ld %ld lcc %f %f 1 1 %f %f %f %f %f\n",
               gds->Nx, gds->Ny, gds->lat1, gds->lon1,
               gds->scaleLat1, gds->scaleLat2, orient, gds->Dx, gds->Dy);
   } else if (gds->projType == GS3_POLAR) {
      /* Set up a map projection so I can compute the poleI, poleJ. */
      SetMapParam (&map, gds);

      /* Find the poleI, poleJ */
      myCll2xy (&map, 90, orient, &poleI, &poleJ);

      /* Round it off. */
      poleI = floor (poleI + .5);
      poleJ = floor (poleJ + .5);
      fprintf (op, "* MAJEARTH %f (GrADS used 6371.2)\n", gds->majEarth);
      fprintf (op, "* MINEARTH %f (GrADS used 6371.2)\n", gds->minEarth);
      /* Grid increment is in km instead of m. */
      fprintf (op, "pdef %ld %ld nps %.0f %.0f %f %f\n", gds->Nx,
               gds->Ny, poleI, poleJ, orient, gds->Dx / 1000.);
   }

   if (f_MSB) {
      fprintf (op, "OPTIONS big_endian\n");
   } else {
      fprintf (op, "OPTIONS little_endian\n");
   }
   if ((scan & 64) == 0) {
      fprintf (op, "OPTIONS yrev\n");
   }
   if (f_unDef) {
      fprintf (op, "UNDEF %f\n", unDef);
   } else {
      /* There is no undefined data in the grid but GrADS requires this
       * element. */
      unDef = 9.999e20;
      if ((unDef < meta->gridAttrib.max) && (unDef > meta->gridAttrib.min)) {
         unDef = meta->gridAttrib.max + 1;
      }
      fprintf (op, "UNDEF %f\n", unDef);
   }
   fprintf (op, "TITLE GrADS control file based on GRIB2 file.\n");
   fprintf (op, "XDEF %ld LINEAR %f %f\n", gds->Nx, gds->lon1, gds->Dx);
   fprintf (op, "YDEF %ld LINEAR %f %f\n", gds->Ny, gds->lat1, gds->Dy);
   fprintf (op, "ZDEF 1 LINEAR 0 1\n");
   if (meta->GribVersion == 1) {
      strftime (buffer, 100, "%H:%MZ%d%b%Y", gmtime (&(meta->pds1.P1)));
   } else if (meta->GribVersion == 2) {
      strftime (buffer, 100, "%H:%MZ%d%b%Y", gmtime (&(meta->pds2.refTime)));
   }
   fprintf (op, "TDEF 1 LINEAR %s 1MN\n", buffer);
   fprintf (op, "VARS 1\n");
   /* GrADS only wants 40 char comment strings. */
   strncpy (buffer, meta->comment, 40);
   buffer[40] = '\0';
   fprintf (op, "%s 1 99 %s\n", meta->element, buffer);
   fprintf (op, "ENDVARS\n");
   fclose (op);
}

/*****************************************************************************
 * WriteGradsCube() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To add a GRIB message to the data cube.  If the file does not exist we
 * create a new one, and then write to that, otherwise we append to the end of
 * the file.
 *
 * ARGUMENTS
 *  filename = The file to add the message to. (Input)
 * grib_Data = The data to write to the file. (Input)
 *      meta = The meta data from the GRIB message to write. (Input)
 *    attrib = Sect 5 from the parsed GRIB message to write. (Input)
 *      scan = Either 0 or (0100)<< 4 = 64 (How to write file.) (Input)
 *             if scan is 0 create a .flt file (For input to Esri S.A.)
 *             if scan is 64 create a .tlf file (For input to NDFD Gd)
 *     f_MSB = True if we should create MSB file, false for LSB (Input)
 *   decimal = How many decimals to round to. (Input)
 *    offset = < 0 => append to file. >= 0 => add to file at this point.
 *             After the procedure, is where we started writing. (In/Out)
 *  f_delete = True if we should overwrite any existing .dat file. (Input)
 *
 * FILES/DATABASES:
 *
 * RETURNS: int (could use errSprintf())
 *  0 = OK
 * -1 = Problems opening the file.
 *
 * HISTORY
 *   8/2003 Arthur Taylor (MDL/RSIS): Created.
 *
 * NOTES
 * 1) f_GrADS is currently ignored but it should be possible to use it to
 *    call some form of gribWriteGradsCTL()
 *****************************************************************************
 */
int WriteGradsCube (char *filename, double *grib_Data, grib_MetaData * meta,
                    gridAttribType * attrib, uChar scan, sChar f_MSB,
                    sChar decimal, sInt4 * offset, sChar f_delete)
{
   FILE *fp;            /* The current open file pointer. */
   float *floatPtr;     /* Temporary storage to convert double data to float
                         * for write. */
   int nameLen;         /* length of 'filename' */
   int x, y;            /* Current grid cell location. */
   double *curData;     /* Pointer to current data in grib_Data. */
   double shift;        /* power of 10 used in rounding. */
   /* char *filename2; *//* Holds name of data file in call to CTL creation */
   double unDef;        /* Holds the missing value, if there is one. */

   myAssert ((scan == 0) || (scan == GRIB2BIT_2));
   myAssert (meta->gds.numPts == meta->gds.Ny * meta->gds.Nx);
   myAssert (filename != NULL);
   myAssert (sizeof (float) == 4);

   nameLen = strlen (filename);
   myAssert (nameLen >= 4);
   strncpy (filename + nameLen - 3, "dat", 3);

   /* Open the file for update and get to the "right" place in the file. */
   if (f_delete) {
      if ((fp = fopen (filename, "wb")) == NULL) {
         errSprintf ("ERROR: Problems opening %s.", filename);
         return -1;
      } else {
         *offset = 0;
      }
   } else {
      if ((fp = fopen (filename, "r+b")) == NULL) {
         /* File doesn't exist? Try create. */
         if ((fp = fopen (filename, "wb")) == NULL) {
            errSprintf ("ERROR: Problems opening %s.", filename);
            return -1;
         } else {
            *offset = 0;
         }
      } else {
         if (*offset < 0) {
            fseek (fp, 0L, SEEK_END);
            *offset = ftell (fp);
         } else if (*offset > 0) {
            fseek (fp, *offset, SEEK_SET);
         }
      }
   }

   /* 
    * The following assumes that data has come from ParseGrid, so scan = 0100.
    * if scan = GRIB2BIT_2 = 0100 don't do any index manipulation...
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
            floatPtr[x] = (float) ((floor (*curData * shift + .5)) / shift);
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
   return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/degrib/RCS/writegra.c,v $";
 static char rcs_id2[] = "$Id: writegra.c,v 1.1 2004/09/16 17:14:09 dsa Exp $";}
/*  ===================================================  */

}
