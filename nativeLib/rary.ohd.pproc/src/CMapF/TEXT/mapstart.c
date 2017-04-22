#include <math.h>
#include "cmapf.h"

void mapf_start(maparam * stcprm, double cone_ang,
		double p_lat, double p_longit ,
		double r_lat, double r_longit) {
/*
 *  General Purpose routine to Set Map Parameters.  Called by the
 *  special purpose routines for oblique stereographic, oblique and
 *  transverse Mercator, oblique Lambert Conformal, etc. Projections
 *  Inputs: p_lat,p_longit - Latitude and Longitude of the Pole
 *            Point for the Projection
 *          r_lat,r_longit - Latitude and Longitude of a
 *            reference point - 180degrees around the Pole Point
 *            from the cut
 *          cone_ang - angle between the Projection Pole axis and
 *            the generators (sides) of the cone on which the Earth
 *            is projected. + or - 90 degrees indicates a plane,
 *            hence Stereographic; 0 degrees indicates a cylinder
 *            and hence Mercator.
 *  Outputs: stcprm - map parameters
 */
vector_3d temp;
int k;
double norm;
   temp = ll_geog(p_lat,p_longit);
   for (k=0;k<3;k++) stcprm->rotate[2][k] = temp.v[k];
   temp = ll_geog(r_lat,r_longit);
   for (k=0;k<3;k++) stcprm->rotate[0][k] = temp.v[k];
   norm =
   x_product (stcprm->rotate[2],stcprm->rotate[0],stcprm->rotate[1]);

   for (k=0;k<3;k++) stcprm->rotate[1][k] /= norm;
   x_product (stcprm->rotate[1],stcprm->rotate[2],stcprm->rotate[0]);
   stcprm->x0 = stcprm->y0 = stcprm->srotate = 0;
   stcprm->crotate = 1.;
   stcprm->gridszeq = stcprm->EarthRad = REARTH;
   stcprm->gamma = sin(RADPDEG * cone_ang);
/*geographic triple : i = equator @ std merid,
		      j = equator @ 90E,
		      k = North Pole */
/*map base triple : i' = M-prime meridian @ M-equator,
		    j' = M-East at location i',
		    k' = M-Pole */
}

void cstrad (maparam * stcprm, double radius) {
double factor = radius / stcprm->EarthRad;
   stcprm->EarthRad = radius;
   stcprm->gridszeq *= factor;
}

double cgtrad(maparam * stcprm) {
   return stcprm->EarthRad;
}

