#include "cmapf.h"

void xy_xe(maparam * stcprm, double x, double y,
			double * xi,double * eta) {
  x -= stcprm->x0;
  y -= stcprm->y0;
  * xi = stcprm->gridszeq / stcprm->EarthRad *
	 (stcprm->crotate * x - stcprm->srotate * y);
  * eta = stcprm->gridszeq / stcprm->EarthRad *
	 (stcprm->crotate * y + stcprm->srotate * x);
}

vector_3d xy_map(maparam * stcprm, double x, double y) {
vector_3d map;
double vsq,fact;
double xi0,eta0,xi,eta;
double ymerc,theta,cosphi;
  xi0 = (x - stcprm->x0) * stcprm->gridszeq / stcprm->EarthRad;
  eta0 = (y - stcprm->y0) * stcprm->gridszeq / stcprm->EarthRad;
  xi = xi0 * stcprm->crotate - eta0 * stcprm->srotate;
  eta = xi0 * stcprm->srotate + eta0 * stcprm->crotate;
  vsq = stcprm->gamma * (xi*xi + eta*eta) - 2. * eta ;
  if (stcprm->gamma * vsq + 1. <= 3.e-8) {
/* Case of point at Projection Pole */
	 map.v[0] = map.v[1] = 0.;
	 map.v[2] = stcprm->gamma > 0 ? 1. : -1. ;
  } else {
	 if ( stcprm->gamma >= 1.) {
/* Stereographic Case, std */
		fact = 2. / (2. + vsq);
		map.v[1] = xi * fact;
		map.v[0] = (1. - eta) * fact;
		map.v[2] = fact - 1.;
	 } else if (stcprm->gamma <= -1.) {
/* Stereographic Case, centered at antipodes */
		fact = -2. / (2. - vsq);
      map.v[1] = xi * fact;
      map.v[0] = (- 1. - eta) * fact;
      map.v[2] = fact + 1.;
    } else {
/* Mercator or Lambert Case */
      ymerc = -.5 * lnabova(stcprm->gamma, vsq);
      if ( ymerc >0.) {
	fact = exp(-ymerc);
	cosphi = fact / (1. + fact * fact);
	cosphi += cosphi;
	map.v[2] = 1. - fact * cosphi;
      } else {
	fact = exp(ymerc);
	cosphi = fact / (1. + fact * fact);
	cosphi += cosphi;
	map.v[2] =  fact * cosphi - 1.;
      }
/*      map.v[2] = 1. - fact * cosphi; */
      if (fabs(map.v[2]) < 1.) {
	theta = atnabova(stcprm->gamma, xi, eta);
	map.v[0] = cosphi * cos(theta);
	map.v[1] = cosphi * sin(theta);
      } else map.v[0] = map.v[1] = 0.;
    }
  }
  return map;
}

