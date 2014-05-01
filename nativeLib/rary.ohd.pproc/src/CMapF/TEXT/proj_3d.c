#include <math.h>
#include "cmapf.h"

void proj_3d(maparam * stcprm,vector_3d * point,vector_3d * vect,
	     double * enx,double * eny,double * enz){
/*
 *  At a given point, resolves the components of a vector vect in the
 *  local coordinate system of the map projection.  It is assumed that
 *  vect and point is given in the 3-dimensional geocentric _map_
 *  coordinate system, rather than the North centered _geo_ system.
 *  returns these components as enx, eny, and enz.  Replaces vect with
 *  its projection on the tangent plane at point.
 */
double dot_prod,fact;
double xi,eta,theta,cos_gtheta,sin_gtheta;
int k;
  for (k=0,dot_prod=0.;k<3;k++) {
    dot_prod += point->v[k] * vect->v[k];
  }
/*
 *  dot_prod is the local vertical component of vect.  Note, point is
 *  assumed to be a unit vector.
 */
  *enz = dot_prod;
  for (k=0;k<3;k++) vect->v[k] -= dot_prod * point->v[k];
/*  vect has now been projected to a plane tangent to point.
 */
  fact = 1. + point->v[2] ;
  xi = vect->v[0] - vect->v[2] * point->v[0] / fact;
  eta = vect->v[1] - vect->v[2] * point->v[1] / fact;
/*
 *  xi, eta represent components of vector on the projection plane,
 *  i.e. the 2-dimensional map system.
 */
  if ( ( fact = stcprm->gamma - 1.0) < 0. )
/*
 *  For Lambert Conformal and Mercator projections (gamma < 1.0) ,
 *  a rotation of the vector components is needed.
 */
  if ( fabs(point->v[2]) < 1.) {
    theta = fact * atan2(point->v[1],point->v[0]);
    cos_gtheta = cos( theta );
    sin_gtheta = sin( theta );
    fact = xi * cos_gtheta - eta * sin_gtheta;
    eta = eta * cos_gtheta + xi * sin_gtheta;
    xi = fact;
  }
/*
 *  Now rotate xi, eta to the final map projection direction.
 */
  *enx = eta * stcprm->crotate - xi * stcprm->srotate;
  *eny = - xi * stcprm->crotate - eta * stcprm->srotate;
}
