#ifndef MYMAPF_H
#define MYMAPF_H

#include "ndfd2rfs_inc/cmapf.h"
#include "meta.h"

typedef struct {
  maparam stcprm;
  char f_latlon;
  double lat1, lon1;
  double latN, lonN;
  double Nx, Ny;
  double Dx, Dy;
  double ratio;         /*  Ratio of Dy / Dx. */
} myMaparam;

void myCxy2ll (myMaparam * map, double x, double y, double *lat, double *lon);

void myCll2xy (myMaparam * map, double lat, double lon, double *x, double *y);

int GDSValid (gdsType * gds);

void SetMapParam (myMaparam * map, gdsType *gds);

int computeSubGrid (LatLon *lwlf, int *x1, int *y1, LatLon *uprt, int *x2,
                    int *y2, gdsType *gds, gdsType *newGds);

int DateLineLat (double lon1, double lat1, double lon2, double lat2,
                 double *ans);

#endif
