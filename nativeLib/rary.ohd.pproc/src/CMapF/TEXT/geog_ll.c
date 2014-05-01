#include <math.h>
#include "cmapf.h"

void geog_ll(vector_3d * geog,double * lat,double * longit){
double fact;
  if ((fact = geog->v[0]*geog->v[0] + geog->v[1]*geog->v[1]) <= 0.) {
    * lat = geog->v[2]>0. ? 90. : -90.;
    * longit = 90. + *lat;
/* Change made 02/12/02 to acommodate WMO reporting conventions.  North
   pole is longitude 180., so "North" points to the Greenwich Meridian,
   South Pole is longitude 0. so "North" again points to the Greenwich
   Meridian. */
  } else {
    fact = sqrt(fact);
    * lat = DEGPRAD * atan2(geog->v[2], fact);
    * longit = DEGPRAD * atan2(geog->v[1],geog->v[0]);
  }
}
