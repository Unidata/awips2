#include "cmapf.h"

void map_xy(maparam * stcprm, vector_3d map,double * x,double * y) {
x_y xi_eta[1];
  map_xe(stcprm,map,xi_eta,0);
  xe_xy(stcprm, xi_eta[0].x,xi_eta[0].y, x,y);
}

void map_xe(maparam * stcprm, vector_3d map,x_y xi_eta[],int mode){
double ymerc,rhog,theta,thetalo,thetahi;
   if (fabs(map.v[2]) >= 1.) {
/* Projection Pole or its antipodes ("Northing" or Southing" Pole) */
      if (stcprm->gamma*map.v[2] > 0.) {
/* This pole is in a finite part of the map, and all returned poles are
 * the same
 */
         xi_eta[0].x = 0.;
         xi_eta[0].y = 1./stcprm->gamma;
         if (mode > 0) {
            xi_eta[1].x = xi_eta[2].x = xi_eta[0].x;
            xi_eta[1].y = xi_eta[2].y = xi_eta[0].y;
         }
         return;
      } else {
/* These poles are infinitely far away.  Substitute ymerc=+-20. for "infinity"*/
         ymerc =  (map.v[2] > 0. ? 20. : -20.);
         theta = thetahi = M_PI;
         thetalo = - M_PI;
      }
   } else {
/*Away from projection poles*/
      ymerc = .5 * log( ( 1. + map.v[2] ) / (1. - map.v[2]) );
      theta = atan2(map.v[1],map.v[0]);
      if (theta <= -M_PI) theta += (M_PI + M_PI);
      if (theta > 0 ) {
         thetahi = theta;
         thetalo = theta - (M_PI+M_PI);
      } else {
         thetalo = theta;
         thetahi = theta + (M_PI+M_PI);
      }
   }
   rhog = xpabova( stcprm->gamma, - ymerc);
/*  rhog = ( exp( - gamma * ymerc ) - 1. ) / gamma */
   xi_eta[0].x = (1. + stcprm->gamma * rhog) *
         snabova(stcprm->gamma, theta);
/* snabova = sin(gamma*theta)/gamma */
   xi_eta[0].y =  stcprm->gamma * (1. + stcprm->gamma * rhog) *
         csabova(stcprm->gamma, theta) - rhog;
/*csabova = (1.-cos(gamma*theta))/(gamma^2) */
   if (mode > 0) {
      xi_eta[1].x = (1. + stcprm->gamma * rhog) *
            snabova(stcprm->gamma, thetalo);
      xi_eta[1].y =  stcprm->gamma * (1. + stcprm->gamma * rhog) *
            csabova(stcprm->gamma, thetalo) - rhog;
      xi_eta[2].x = (1. + stcprm->gamma * rhog) *
            snabova(stcprm->gamma, thetahi);
      xi_eta[2].y =  stcprm->gamma * (1. + stcprm->gamma * rhog) *
            csabova(stcprm->gamma, thetahi) - rhog;
   }
}


void xe_xy(maparam * stcprm, double xi, double eta,
			double * x,double * y) {
  *x = stcprm->x0 + stcprm->EarthRad / stcprm->gridszeq  *
     (stcprm->crotate * xi + stcprm->srotate * eta);
  *y = stcprm->y0 + stcprm->EarthRad / stcprm->gridszeq  *
     (stcprm->crotate * eta - stcprm->srotate * xi);
}

