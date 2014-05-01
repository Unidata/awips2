/*****************************************************************************
 * mymapf.c
 *
 * DESCRIPTION
 *    This file contains a bunch of wrapper routines so that when dealing
 * with lambert conformal projections, I can use the cmapf routines, but
 * when dealing with lat/lon projections, I can call the same functions, but
 * not do anything.
 *
 * HISTORY
 *   11/2003 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#include <string.h>
#include "degrib_inc/mymapf.h"
#include "degrib_inc/myassert.h"
#include "degrib_inc/myerror.h"
#ifdef DEBUG
#include <stdio.h>
#endif

/*****************************************************************************
 * myCxy2ll() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Wrapper for cxy2ll which converts from an x/y point to a lat/lon point.
 * In other words, goes from projected space back to unprojected space.
 *
 * ARGUMENTS
 *      map = Holds the current map projection info to compute from. (Input)
 *     x, y = The point to convert from. (Input)
 * lat, lon = The resulting point (Output)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void
 *
 * HISTORY
 *  11/2003 Arthur Taylor (MDL/RSIS): Created.
 *   3/2004 AAT: Added lat bound checks.
 *
 * NOTES
 *****************************************************************************
 */
void myCxy2ll (myMaparam * map, double x, double y, double *lat, double *lon)
{
   if (map->f_latlon == 1) {
      *lat = map->lat1 + (map->latN - map->lat1) * (y - 1) / (map->Ny - 1.);
      *lon = map->lon1 + (map->lonN - map->lon1) * (x - 1) / (map->Nx - 1.);
      while (*lon > 180)
         *lon = *lon - 360;
      while (*lon < -180)
         *lon = *lon + 360;
      if (*lat > 90)
         *lat = 90;
      if (*lat < -90)
         *lat = -90;
   } else {
      /* Reason for 1 + (y - 1) * ratio is that the 1, 1 location is fixed
       * and correct.  The 1, 2 location is off by 1 (not 2) ratio amounts. */
      y = 1 + (y - 1) * map->ratio;
      cxy2ll (&(map->stcprm), x, y, lat, lon);
   }
}

/*****************************************************************************
 * myCll2xy() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Wrapper for cll2xy which converts from a lat/lon point to an x/y point.
 * In other words, goes from unprojected space back to a projected space.
 *
 * ARGUMENTS
 *      map = Holds the current map projection info to compute from. (Input)
 * lat, lon = The resulting point (Input)
 *     x, y = The point to convert from. (Output)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void
 *
 * HISTORY
 *  11/2003 Arthur Taylor (MDL/RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
void myCll2xy (myMaparam * map, double lat, double lon, double *x, double *y)
{
   if (map->f_latlon == 1) {
      myAssert (map->latN != map->lat1);
      myAssert (map->lonN != map->lon1);
      /* Get lon in correct range.. */
      if (map->lonN < map->lon1) {
         while (lon > map->lon1)
            lon -= 360;
         while (lon < map->lonN)
            lon += 360;
      } else {
         while (lon < map->lon1)
            lon += 360;
         while (lon > map->lonN)
            lon -= 360;
      }
      *y = 1 + (map->Ny - 1) * ((lat - map->lat1) / (map->latN - map->lat1));
      *x = 1 + (map->Nx - 1) * ((lon - map->lon1) / (map->lonN - map->lon1));
   } else {
      cll2xy (&(map->stcprm), lat, lon, x, y);
      /* Reason for 1 + (y - 1) / ratio is that the 1, 1 location is fixed
       * and correct.  The 1, 2 location is off by 1 (not 2) ratio amounts. */
      *y = 1 + (*y - 1) / map->ratio;
   }
}

/*****************************************************************************
 * GDSValid() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Validates GDS for use with the current mapping library routines.
 *
 * ARGUMENTS
 * gds = The filled in section 3 data to check. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: int (could use errSprintf())
 *  0 = Ok.
 * -1 = Something was wrong with section 3.
 *
 * HISTORY
 *  12/2002 Arthur Taylor (MDL/RSIS): Created.
 *   5/2003 AAT: Enabled other spherical earths.
 *   6/2003 AAT: Enabled Dx != Dy (by taking average).
 *   1/2004 AAT: Emapf now supports elliptical earth.
 *
 * NOTES
 *****************************************************************************
 */
int GDSValid (gdsType * gds)
{
   if (gds->numPts != gds->Ny * gds->Nx) {
      errSprintf ("ERROR: numPts != Nx * Ny? (%ld != %ld * %ld)",
                  gds->numPts, gds->Nx, gds->Ny);
      return -1;
   }

   /* Add GS3_LATLON ? */

   if ((gds->projType != GS3_MERCATOR) && (gds->projType != GS3_POLAR) &&
       (gds->projType != GS3_LAMBERT) && (gds->projType != GS3_LATLON)) {
      errSprintf ("Projection code can not handle this projection yet. \n"
                  "See section 3 template type %d\n", gds->projType);
      return -1;
   }
/* Emapf now supports elipsoidal earth
   if (!gds->f_sphere) {
      errSprintf ("Projection code can not handle a non-sphere yet.");
      return -1;
   }
*/
   if ((gds->Dx <= 0) || (gds->Dy <= 0)) {
      errSprintf ("Projection code requires Dx (%f) > 0 and Dy (%f) > 0",
                  gds->Dx, gds->Dy);
      return -1;
   }
   return 0;
}

/*****************************************************************************
 * SetMapParam() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   Sets up a map structure given a gds data structure.
 *
 * ARGUMENTS
 *   map = The map structure to fill. (Output)
 *   gds = The filled in section 3 data to use for initialization. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: int (could use errSprintf())
 *  0 = Ok.
 * -1 = Something was wrong with section 3.
 *
 * HISTORY
 *  12/2002 Arthur Taylor (MDL/RSIS): Created.
 *   5/2003 AAT: Enabled other spherical earths.
 *   6/2003 AAT: Enabled Dx != Dy (by taking average).
 *   3/2004 AAT: Made sure that for latlon, lon is in [0..360)
 *
 * NOTES
 *****************************************************************************
 */
void SetMapParam (myMaparam * map, gdsType * gds)
{
   double orient;       /* Orientation longitude of projection (where N is
                         * up.) (between -180 and 180) */

   if (gds->projType == GS3_LATLON) {
      map->f_latlon = 1;
      switch (gds->scan) {
         case 0:
            map->lat1 = gds->lat2;
            map->lon1 = gds->lon1;
            map->latN = gds->lat1;
            map->lonN = gds->lon2;
            break;
         case 64:
            map->lat1 = gds->lat1;
            map->lon1 = gds->lon1;
            map->latN = gds->lat2;
            map->lonN = gds->lon2;
            break;
         case 128:
            map->lat1 = gds->lat2;
            map->lon1 = gds->lon2;
            map->latN = gds->lat1;
            map->lonN = gds->lon1;
            break;
         case 192:
            map->lat1 = gds->lat1;
            map->lon1 = gds->lon2;
            map->latN = gds->lat2;
            map->lonN = gds->lon1;
            break;
         default:
            /* Should not get here */
#ifdef DEBUG
            printf ("gds Scan mode should not be %d?\n", gds->scan);
#endif
            myAssert (1 == 0);
            map->lat1 = gds->lat1;
            map->lon1 = gds->lon1;
            map->latN = gds->lat2;
            map->lonN = gds->lon2;
      }

      while (map->lon1 < 0) {
         map->lon1 += 360;
      }
      while (map->lon1 >= 360) {
         map->lon1 -= 360;
      }
      while (map->lonN < 0) {
         map->lonN += 360;
      }
      while (map->lonN >= 360) {
         map->lonN -= 360;
      }
      myAssert (map->lat1 < map->latN);
      myAssert (map->lon1 < map->lonN);
      map->Nx = gds->Nx;
      map->Ny = gds->Ny;
      map->Dx = gds->Dx;
      map->Dy = gds->Dy;
      map->ratio = 1;
   } else {
#ifdef USE_DMAPF
#else
      mkGeoid (&(map->stcprm), AB, gds->majEarth, gds->minEarth);
#endif

      map->f_latlon = 0;
      orient = gds->orientLon;
      while (orient > 180) {
         orient -= 360;
      }
      while (orient < -180) {
         orient += 360;
      }
      if (gds->projType == GS3_POLAR) {
         stlmbr (&(map->stcprm), gds->scaleLat1, orient);
      } else {
#ifdef USE_DMAPF
         stlmbr (&(map->stcprm), eqvlat (gds->scaleLat1, gds->scaleLat2),
                 orient);
#else
         stlmbr (&(map->stcprm), eqvlat (&(map->stcprm), gds->scaleLat1,
                                         gds->scaleLat2), orient);
#endif
      }
#ifdef USE_DMAPF
      cstrad (&(map->stcprm), gds->majEarth);
#endif
      /* need km, have m. thus the / 1000. */
      stcm1p (&(map->stcprm), 1, 1, gds->lat1, gds->lon1, gds->meshLat,
              orient, (double) (gds->Dx / 1000.), 0);
      map->ratio = gds->Dy / gds->Dx;
   }
}

/* 1) find lwlf coresponding x1,y1, and uprt corresponding x2,y2
 * 2) create a new gds based on old one, but shifted with lwlf in bottom
 *    left, and uprt in upper right.
 * 3) Shift data around on grid? create new grid and copy data?
 */
int computeSubGrid (LatLon * lwlf, int *x1, int *y1, LatLon * uprt, int *x2,
                    int *y2, gdsType * gds, gdsType * newGds)
{
   myMaparam map;       /* Used to compute the grid lat/lon points. */
   double x, y;

   /* Check that gds is valid before setting up map projection. */
   if (GDSValid (gds) != 0) {
      preErrSprintf ("ERROR: gds was not Valid.\n");
      return -1;
   }

   /* Validate that lwlf < uprt */
   if (lwlf->lt > uprt->lt) {
      x = lwlf->lt;
      lwlf->lt = uprt->lt;
      uprt->lt = x;
   }
   if (lwlf->lg > uprt->lg) {
      x = lwlf->lg;
      lwlf->lg = uprt->lg;
      uprt->lg = x;
   }

   /* Set up the map projection. */
   SetMapParam (&map, gds);
   myCll2xy (&map, lwlf->lt, lwlf->lg, &x, &y);
   *x1 = (int) floor (x);
   *y1 = (int) floor (y);
   myCll2xy (&map, uprt->lt, uprt->lg, &x, &y);
   *x2 = (int) ceil (x);
   *y2 = (int) ceil (y);
   if ((*x1 >= *x2) || (*y1 >= *y2)) {
      errSprintf ("ERROR: subgrid is not at least one cell wide/high\n");
      return -1;
   }

   myAssert ((gds->projType == GS3_LATLON) || (gds->projType == GS3_POLAR) ||
             (gds->projType == GS3_LAMBERT) ||
             (gds->projType == GS3_MERCATOR));
   memcpy (newGds, gds, sizeof (gdsType));
   newGds->Nx = *x2 - *x1 + 1;
   newGds->Ny = *y2 - *y1 + 1;
   newGds->numPts = (newGds->Nx * newGds->Ny);
   myCxy2ll (&map, (double) (*x1), (double) (*y1), &(newGds->lat1),
             &(newGds->lon1));
   if (gds->projType == GS3_LATLON) {
      myCxy2ll (&map, (double) (*x2), (double) (*y2), &(newGds->lat2),
                &(newGds->lon2));
   }
   return 0;
}

/* Find the latitude that the line segment crosses the dateline. */
int DateLineLat (double lon1, double lat1, double lon2, double lat2,
                 double *ans)
{
#ifdef DEBUG
   printf ("%f\n", lat1);
#endif
   myAssert ((lat1 >= -90) && (lat1 <= 90));
   myAssert ((lat2 >= -90) && (lat2 <= 90));

   /* Step 1... get lon1, lon2 in range of 0..360 */
   while (lon1 < 0) {
      lon1 += 360;
   }
   while (lon1 >= 360) {
      lon1 -= 360;
   }
   /* Check weird case 1. */
   if (lon1 == 180) {
      *ans = lat1;
      return 0;
   }
   while (lon2 < 0) {
      lon2 += 360;
   }
   while (lon2 >= 360) {
      lon2 -= 360;
   }
   /* Check weird case 2 and 3. */
   if (lon2 == 180) {
      *ans = lat2;
      return 0;
   }
   myAssert (lon1 != lon2);
   if (lon1 == lon2) {
      *ans = lat1;
      return 1;
   }
   myAssert (((lon1 <= 180) && (180 <= lon2)) ||
             ((lon2 <= 180) && (180 <= lon1)));
   *ans = (180 - lon1) * (lat2 - lat1) / (lon2 - lon1) + lat1;
   myAssert ((*ans >= -90) && (*ans <= 90));
   return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/degrib/RCS/mymapf.c,v $";
 static char rcs_id2[] = "$Id: mymapf.c,v 1.1 2004/09/16 17:14:09 dsa Exp $";}
/*  ===================================================  */

}
