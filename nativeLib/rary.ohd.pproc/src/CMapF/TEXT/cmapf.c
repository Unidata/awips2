#include <math.h>
#include <float.h> /*Switch from deprecated <values.h> to <float.h>*/
#include <stdio.h>
#include "cmapf.h"

void cll2xy(maparam * stcprm,double lat,double longit,
				  double * x,double *y ) {
vector_3d geog;
  geog = ll_geog(lat, longit);
  map_xy(stcprm, basegtom(stcprm,geog) , x, y);
}

void cxy2ll(maparam * stcprm ,double x,double y,
				double * lat,double * longit ) {
vector_3d map,geog;
  map = xy_map(stcprm,x,y);
  geog = basemtog(stcprm,map);
  geog_ll( &geog, lat,longit);
}

int n_quad(vector_3d map) {
/* Return "Quadrant" of map point, based on x=map.v[0]
 * and y=map.v[1].  quadrant is zero when theta = atan2(y,x)
 * satisfies -pi/4 < theta <= pi/4, one when pi/4 < theta <= 3pi/4,
 * two when 3pi/4 < theta <= 2pi, minus one when -3pi/4 < theta < -pi/4,
 * and minus two when -2pi < theta <= 3pi/4.  The origin, x,y = 0,0, is
 * in quadrant zero.  There are 5 standard "quadrants", three of them
 * being a quarter circle, and two an eighth of a circle each.
 */
 /*
                \  1  /
               2 \   /
                  \|/
            -------+------ 0
                  /|\
              -2 /   \
                / -1  \
  */
int q=0;
  if (map.v[1] >= 0.) {
    if (map.v[0] < map.v[1]) q++;
    if (map.v[0] < - map.v[1]) q++;
  } else {
    if (map.v[0] <= -map.v[1]) q--;
    if (map.v[0] <= map.v[1]) q--;
  }
  return q;
}

double cgszxy(maparam * stcprm,double x,double y){
vector_3d map = xy_map(stcprm,x,y);
double ymerc;
  
  if (map.v[2] >= 1.)  return stcprm->gridszeq *
			      (stcprm->gamma >=  1. ? 2. : 0.);
  if (map.v[2] <= -1.) return stcprm->gridszeq *
			      (stcprm->gamma <= -1. ? 2. : 0.);
  if (fabs(stcprm->gamma) >= 1.)
         return stcprm->gridszeq *
	    (1. + (stcprm->gamma > 0.?map.v[2]:-map.v[2]) );
  ymerc = -.5 * log( (1. - map.v[2])/(1. + map.v[2]) );
  return stcprm->gridszeq *
	  exp( - (1.-stcprm->gamma) * ymerc) * (1. + map.v[2]);
}

double cgszll(maparam * stcprm,double lat,double longit){
vector_3d map = basegtom(stcprm, ll_geog(lat,longit) );

double ymerc;
  
  if (map.v[2] >=  1.) return stcprm->gridszeq *
			      (stcprm->gamma  >=  1. ? 2.:0.);
  if (map.v[2] <= -1.) return stcprm->gridszeq *
			      (stcprm->gamma <= -1. ? 2.:0.);
  if (fabs(stcprm->gamma) >= 1.)
         return stcprm->gridszeq *
	    (1. + (stcprm->gamma > 0.?map.v[2]:-map.v[2]) );
  ymerc = -.5 * log( (1. - map.v[2])/(1. + map.v[2]) );
  return stcprm->gridszeq *
	  exp( - (1.-stcprm->gamma) * ymerc) * (1. + map.v[2]);
}

void ccrvxy(maparam * stcprm, double x, double y,
			      double * gx, double * gy){
vector_3d map = xy_map(stcprm,x,y);
double xi,eta,xi0,eta0;
double fact;
  if ( fabs(map.v[2]) >= 1.) {
    if (stcprm->gamma == map.v[2]) {
      * gx = * gy = 0.;
		return;
    } else {
      xi = 0.;
		eta = ((map.v[2] > 0.) ?
		  -.5 * FLT_MAX : .5 * FLT_MAX) / stcprm->EarthRad;
    }
  } else {
    xi0 = (x - stcprm->x0) * stcprm->gridszeq / stcprm->EarthRad;
	 eta0 = (y - stcprm->y0) * stcprm->gridszeq / stcprm->EarthRad;
    xi = xi0 * stcprm->crotate - eta0 * stcprm->srotate;
    eta = xi0 * stcprm->srotate + eta0 * stcprm->crotate;
	 eta = 1. - stcprm->gamma * eta;
	 xi *= - stcprm->gamma;
    fact = sqrt((xi*xi + eta*eta) /
	  (1. - map.v[2]) / (1. + map.v[2]) );
    xi /= fact; eta /= fact;
/*    fact = (stcprm->gamma >= 1.) ? 1./(1. + map.v[2]) :
		(stcprm->gamma <= -1.) ? - 1./(1. - map.v[2]): */
    fact = (stcprm->gamma - map.v[2]) /
	      (1. - map.v[2]) / (1. + map.v[2]) / stcprm->EarthRad;
	 xi *= fact; eta *= fact;
  }
  *gx = xi * stcprm->crotate + eta * stcprm->srotate;
  *gy = eta * stcprm->crotate - xi * stcprm->srotate;
}

void ccrvll(maparam * stcprm, double lat, double longit,
			      double * gx, double * gy){
vector_3d map = basegtom(stcprm,ll_geog(lat,longit));
double xi,eta;
double fact;
double glambda,slambda,clambda;
  if ( fabs(map.v[2]) >= 1.) {
    if (stcprm->gamma == map.v[2]) {
      * gx = * gy = 0.;
		return;
	 } else {
      xi = 0.;
		eta = ((map.v[2] > 0.) ?
	     -.5 * FLT_MAX : .5 * FLT_MAX) / stcprm->EarthRad;
	 }
  } else {
/*    fact = (stcprm->gamma >= 1.) ? 1./(1. + map.v[2]) :
	   (stcprm->gamma <= -1.) ? - 1./(1. - map.v[2]): */
	 fact = (stcprm->gamma - map.v[2]) /
	      (1. - map.v[2]) / (1. + map.v[2]) / stcprm->EarthRad;
    xi = - map.v[1] * fact;
	 eta = map.v[0] * fact;
	 if (fabs(stcprm->gamma) < 1.) {
      glambda = (stcprm->gamma - 1.) * atan2(map.v[1],map.v[0]);
		clambda = cos(glambda); slambda = sin(glambda);
      fact = xi * clambda - eta * slambda;
		eta = xi * slambda + eta * clambda;
		xi = fact;
    }
  }
  *gx = xi * stcprm->crotate + eta * stcprm->srotate;
  *gy = eta * stcprm->crotate - xi * stcprm->srotate;
}

void cc2gxy(maparam * stcprm,double x,double y,
		 double ue,double vn, double * ug,double * vg) {
/* Takes North- and East Wind components, and returns gridded
 * wind components ug, vg in the grid x- and y- direction.
 */
double enx,eny,enz;
double norm;
  cpolxy(stcprm,x,y,&enx,&eny,&enz);
  if ((norm=sqrt(enx*enx + eny*eny)) <= .1e-3) {
    cgrnxy(stcprm,x,y,&enx,&eny,&enz);
	 norm = sqrt(enx*enx + eny*eny);
  }
  enx /= norm; eny /= norm;
  *ug = enx * vn + eny * ue;
  *vg = eny * vn - enx * ue;
}

void cw2gxy(maparam * stcprm,double x,double y,
		 double ue,double vn, double * ug,double * vg) {
/* Takes North- and East Wind components, and returns gridded
 * wind components ug, vg in the grid x- and y- direction.
 */
#define SIN1DEG .017452406437284
double enx,eny,enz;
double norm;
  cpolxy(stcprm,x,y,&enx,&eny,&enz);
  if ((norm=sqrt(enx*enx + eny*eny)) <= SIN1DEG) {
    cgrnxy(stcprm,x,y,&enx,&eny,&enz);
	 norm = sqrt(enx*enx + eny*eny);
  }
  enx /= norm; eny /= norm;
  *ug = enx * vn + eny * ue;
  *vg = eny * vn - enx * ue;
}

void cg2cxy(maparam * stcprm,double x,double y,
	    double ug,double vg, double * ue,double * vn) {
/* Takes gridded wind components ug, vg in the grid x- and y- direction
 * and returns North- and East Wind components vn and ue.  North is defined as
 * toward the North Pole, except at the poles, where it is defined as toward
 * the Greenwich Meridian at the equator.
 */
double enx,eny,enz;
double norm;
  cpolxy(stcprm,x,y,&enx,&eny,&enz);
  if ((norm=sqrt(enx*enx + eny*eny)) <= .1e-3) {
	 cgrnxy(stcprm,x,y,&enx,&eny,&enz);
	 norm = sqrt(enx*enx + eny*eny);
  }
  enx /= norm; eny /= norm;
  *ue = - enx * vg + eny * ug;
  *vn = eny * vg + enx * ug;
}

void cg2wxy(maparam * stcprm,double x,double y,
	    double ug,double vg, double * ue,double * vn) {
/* Takes gridded wind components ug, vg in the grid x- and y- direction
 * and returns North- and East Wind components vn and ue according to the
 * WMO conventions.
 */
double enx,eny,enz;
double norm;
  cpolxy(stcprm,x,y,&enx,&eny,&enz);
  if ((norm=sqrt(enx*enx + eny*eny)) <= SIN1DEG) {
	 cgrnxy(stcprm,x,y,&enx,&eny,&enz);
	 norm = sqrt(enx*enx + eny*eny);
  }
  enx /= norm; eny /= norm;
  *ue = - enx * vg + eny * ug;
  *vn = eny * vg + enx * ug;
}

void cc2gll(maparam * stcprm,double lat,double longit,
		 double ue,double vn, double * ug,double * vg) {
/* Takes North- and East Wind components, and returns gridded
 * wind components ug, vg in the grid x- and y- direction.
 * At the poles"North" is defined in terms of the longit meridian.*/
double enx,eny,enz;
double norm;
  cpolll(stcprm, lat,longit, &enx,&eny,&enz);
  if ((norm=sqrt(enx*enx + eny*eny)) <= .1e-3) {
    double csx,sny,gnx,gny,gnz;
    csx = cos(RADPDEG * longit);
    sny = sin(RADPDEG * longit);
	 cgrnll(stcprm,lat,longit,&gnx,&gny,&gnz);
    if (enz > 0.) {
    /* (gnx,gny) is the direction of Greenwich Meridian, (-gny,gnx) is
     *  the direction of the 90E meridian.  "North" is the direction
     *  toward the North pole along the along meridian.*/
      enx = - csx * gnx + sny * gny;
      eny = - csx * gny - sny * gnx;
    } else {
    /* (gnx,gny) is the direction of Greenwich Meridian, (gny,-gnx) is
     * the direction of the 90E meridian.  "North" is the direction
     * away from the South pole along the along meridian.*/
      enx = csx * gnx + sny * gny;
      eny = csx * gny - sny * gnx;
    }
  } else {
    enx /= norm; eny /= norm;
  }
  *ug = enx * vn + eny * ue;
  *vg = eny * vn - enx * ue;
}

void cw2gll(maparam * stcprm,double lat,double longit,
		 double ue,double vn, double * ug,double * vg) {
/* Takes North- and East Wind components, and returns gridded
 * wind components ug, vg in the grid x- and y- direction.
 * Within one degree of the poles, "North is defined as toward the intersection
 * of the Greenwich Meridian at the Equator, as in the WMO convention.*/
double enx,eny,enz;
double norm;
  cpolll(stcprm, lat,longit, &enx,&eny,&enz);
  if ((norm=sqrt(enx*enx + eny*eny)) <= SIN1DEG) {
	 cgrnll(stcprm,lat,longit,&enx,&eny,&enz);
	 norm = sqrt(enx*enx + eny*eny);
  }
  enx /= norm; eny /= norm;
  *ug = enx * vn + eny * ue;
  *vg = eny * vn - enx * ue;
}

void cg2cll(maparam * stcprm,double lat,double longit,
		 double ug,double vg, double * ue,double * vn) {
/* Takes gridded wind components ug, vg in the grid x- and y- direction
 * and returns North- and East Wind components vn and ue.
 */
double enx,eny,enz;
double norm;
  cpolll(stcprm, lat,longit ,&enx,&eny,&enz);
  if ((norm=sqrt(enx*enx + eny*eny)) <= .1e-3) {
    double csx,sny,gnx,gny,gnz;
    csx = cos(RADPDEG * longit);
    sny = sin(RADPDEG * longit);
	 cgrnll(stcprm,lat,longit,&gnx,&gny,&gnz);
    if (enz > 0.) {
    /* (gnx,gny) is the direction of Greenwich Meridian, (-gny,gnx) is
     *  the direction of the 90E meridian.  "North" is the direction
     *  toward the North pole along the along meridian.*/
      enx = - csx * gnx + sny * gny;
      eny = - csx * gny - sny * gnx;
    } else {
    /* (gnx,gny) is the direction of Greenwich Meridian, (gny,-gnx) is
     * the direction of the 90E meridian.  "North" is the direction
     * away from the South pole along the along meridian.*/
      enx = csx * gnx + sny * gny;
      eny = csx * gny - sny * gnx;
    }
  } else {
    enx /= norm; eny /= norm;
  }
  *ue = - enx * vg + eny * ug;
  *vn = eny * vg + enx * ug;
}

void cg2wll(maparam * stcprm,double lat,double longit,
		 double ug,double vg, double * ue,double * vn) {
/* Takes gridded wind components ug, vg in the grid x- and y- direction
 * and returns North- and East Wind components vn and ue.
 */
double enx,eny,enz;
double norm;
  cpolll(stcprm, lat,longit ,&enx,&eny,&enz);
  if ((norm=sqrt(enx*enx + eny*eny)) <= SIN1DEG) {
	 cgrnll(stcprm, lat,longit, &enx,&eny,&enz);
	 norm = sqrt(enx*enx + eny*eny);
  }
  enx /= norm; eny /= norm;
  *ue = - enx * vg + eny * ug;
  *vn = eny * vg + enx * ug;
}

#undef SIN1DEG

void cpolxy(maparam * stcprm,double x, double y,
		double * enx,double * eny, double * enz) {
/* Returns the components, in the map's x-, y-, and z- directions, of a
 * vector in the direction of the Earth' Axis, from South Pole to North
 * Pole, i.e. from 90S,180W to 90N,180W.  This is useful in establishing
 * wind directions away from the North or South Poles.  The enx,eny pair
 * form a vector of magnitude cos(lat) in the direction of North, while the
 * enz component, equal to sin(lat), is proportional to the coriolis
 * parameter.
 */
vector_3d map;vector_3d pole;
int k;
  map = xy_map(stcprm,x,y);
  for (k=0;k<3;k++) pole.v[k] = stcprm->rotate[k][2];
  proj_3d(stcprm, &map,&pole, enx,eny,enz);
}

void cgrnxy(maparam * stcprm,double x, double y,
		double * enx,double * eny, double * enz) {
/* Returns the components, in the map's x-, y-, and z- directions, of a
 * vector in the direction of the Greenwich Meridian, i.e. from 0N,180W
 * to 0N,0E.  This is useful in establishing wind directions near the
 * North or South Poles, which are based on a compass rose with "North"
 * pointed along the Greenwich Meridian.
 */
vector_3d map;vector_3d pole;
int k;
  map = xy_map(stcprm,x,y);
  for (k=0;k<3;k++) pole.v[k] = stcprm->rotate[k][0];
  proj_3d(stcprm, &map,&pole, enx,eny,enz);
}

void cpolll(maparam * stcprm,double lat, double longit,
		double * enx,double * eny, double * enz) {
/* Returns the components, in the map's x-, y-, and z- directions, of a
 * vector in the direction of the Earth' Axis, from South Pole to North
 * Pole, i.e. from 90S,180W to 90N,180W.  This is useful in establishing
 * wind directions away from the North or South Poles.  The enx,eny pair
 * form a vector of magnitude cos(lat) in the direction of North, while the
 * enz component, equal to sin(lat), is proportional to the coriolis
 * parameter.
 */
vector_3d map;vector_3d pole;
int k;
  map = basegtom(stcprm, ll_geog(lat,longit) );
  for (k=0;k<3;k++) pole.v[k] = stcprm->rotate[k][2];
  proj_3d(stcprm, &map,&pole, enx,eny,enz);
}

void cgrnll(maparam * stcprm,double lat, double longit,
		double * enx,double * eny, double * enz) {
/* Returns the components, in the map's x-, y-, and z- directions, of a
 * vector in the direction of the Greenwich Meridian, i.e. from 0N,180W
 * to 0N,0E.  This is useful in establishing wind directions near the
 * North or South Poles, which are based on a compass rose with "North"
 * pointed along the Greenwich Meridian.
 */
vector_3d map;vector_3d pole;
int k;
  map = basegtom(stcprm, ll_geog(lat,longit) );
  for (k=0;k<3;k++) pole.v[k] = stcprm->rotate[k][0];
  proj_3d(stcprm, &map,&pole, enx,eny,enz);
}


