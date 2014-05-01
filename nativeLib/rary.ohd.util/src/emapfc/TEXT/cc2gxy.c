#include <stdlib.h>
#include "emapfc_inc/cmapf.h"
#define REARTH stcprm->arad

/*
 * cc2gxy.c  - source file for conformal mapping function utility.
 * Written 12/21/94 by
 * Dr. Albion Taylor
 * NOAA / OAR / ARL                  Phone: (301) 713-0295 ext 132
 * Rm. 3151, 1315 East-West Highway  Fax:   (301) 713-0119
 * Silver Spring, MD 20910           E-mail: ADTaylor@arlrisc.ssmc.noaa.gov
 */

static void cnxyll(maparam * stcprm,double xi,double eta,
		double * lat,double * longit) ;

void cxy2ll(maparam * stcprm,double x, double y,
		double * lat, double * longit) {
double xi0,eta0,xi,eta;
  xi0 = (x - stcprm->x0) * stcprm->gridszeq / REARTH;
  eta0 = (y - stcprm->y0) * stcprm->gridszeq / REARTH;
  xi = xi0 * stcprm->crotate - eta0 * stcprm->srotate;
  eta = xi0 * stcprm->srotate + eta0 * stcprm->crotate;
  cnxyll(stcprm, xi,eta, lat,longit);
  *longit = cperiodic(*longit, -180., 180.);
}

void cc2gxy(maparam * stcprm,double x,double y,
	    double ue,double vn, double * ug,double * vg) {
double xi0,eta0;
double xpolg,ypolg,temp;
  xi0 = (x - stcprm->x0) * stcprm->gridszeq / REARTH;
  eta0 = (y - stcprm->y0) * stcprm->gridszeq / REARTH;
/*  Normal Case; meteorological coordinate related to true North*/
  xpolg = stcprm->srotate - stcprm->gamma * xi0;
  ypolg = stcprm->crotate - stcprm->gamma * eta0;
  temp = sqrt(xpolg*xpolg + ypolg*ypolg);
  if (temp < 1.e-6) {
    double xi,eta,xlat,xlong;
      xi = xi0 * stcprm->crotate - eta0 * stcprm->srotate;
      eta = xi0 * stcprm->srotate + eta0 * stcprm->crotate;
      cnxyll(stcprm, xi,eta, &xlat,&xlong);
      cc2gll(stcprm, xlat,xlong, ue,vn, ug,vg);
  } else {
    xpolg /= temp;
    ypolg /= temp;
    *ug = ypolg * ue + xpolg * vn;
    *vg = ypolg * vn - xpolg * ue;
  }
}

void cw2gxy(maparam * stcprm,double x,double y,
	    double ue,double vn, double * ug,double * vg) {
double xi0,eta0,xi,eta;
double radial;
  xi0 = (x - stcprm->x0) * stcprm->gridszeq / REARTH;
  eta0 = (y - stcprm->y0) * stcprm->gridszeq / REARTH;
  xi = xi0 * stcprm->crotate - eta0 * stcprm->srotate;
  eta = xi0 * stcprm->srotate + eta0 * stcprm->crotate;
  radial = 2. * eta - stcprm->gamma * (xi*xi + eta*eta);
  if(radial > stcprm->npwarn) {
/* North Pole Case; "North" along Prime meridian */
  double xlat,xlong;
    cnxyll(stcprm, xi,eta, &xlat,&xlong);
    cw2gll(stcprm, xlat,xlong, ue,vn, ug,vg);
  } else {
    if (radial < stcprm->spwarn) {
/* South Pole Case; "North" along Prime meridian */
    double xlat,xlong;
      cnxyll(stcprm, xi,eta, &xlat,&xlong);
      cw2gll(stcprm, xlat,xlong, ue,vn, ug,vg);
    } else {
/*  Normal Case; meteorological coordinate related to true North*/
    double temp,xpolg,ypolg;
      xpolg = stcprm->srotate - stcprm->gamma * xi0;
      ypolg = stcprm->crotate - stcprm->gamma * eta0;
      temp = sqrt(xpolg*xpolg + ypolg*ypolg);
      xpolg /= temp;
      ypolg /= temp;
      *ug = ypolg * ue + xpolg * vn;
      *vg = ypolg * vn - xpolg * ue;
    }
  }
}

void cg2cxy(maparam * stcprm,double x,double y,
	    double ug, double vg, double * ue, double * vn) {
double xi0,eta0;
double xpolg,ypolg,temp;
  xi0 = (x - stcprm->x0) * stcprm->gridszeq / REARTH;
  eta0 = (y - stcprm->y0) * stcprm->gridszeq / REARTH;
/*  Normal Case; meteorological coordinate related to true North*/
  xpolg = stcprm->srotate - stcprm->gamma * xi0;
  ypolg = stcprm->crotate - stcprm->gamma * eta0;
  temp = sqrt(xpolg*xpolg + ypolg*ypolg);
  if (temp < 1.e-6) {
    double xi,eta,xlat,xlong;
      xi = xi0 * stcprm->crotate - eta0 * stcprm->srotate;
      eta = xi0 * stcprm->srotate + eta0 * stcprm->crotate;
      cnxyll(stcprm, xi,eta, &xlat,&xlong);
      cg2cll(stcprm, xlat,xlong, ug,vg, ue,vn);
  } else {
    xpolg /= temp;
    ypolg /= temp;
    *ue = ypolg * ug - xpolg * vg;
    *vn = ypolg * vg + xpolg * ug;
  }
}

void cg2wxy(maparam * stcprm,double x,double y,
	    double ug, double vg, double * ue, double * vn) {
double xi0,eta0,xi,eta;
double radial,xpolg,ypolg;
  xi0 = (x - stcprm->x0) * stcprm->gridszeq / REARTH;
  eta0 = (y - stcprm->y0) * stcprm->gridszeq / REARTH;
  xi = xi0 * stcprm->crotate - eta0 * stcprm->srotate;
  eta = xi0 * stcprm->srotate + eta0 * stcprm->crotate;
  radial = 2. * eta - stcprm->gamma * (xi*xi + eta*eta);
  if(radial > stcprm->npwarn) {
/* North Pole Case; "North" along Prime meridian */
  double xlat,xlong;
    cnxyll(stcprm, xi,eta, &xlat,&xlong);
    cg2wll(stcprm, xlat,xlong, ug,vg, ue,vn);
  } else {
    if (radial < stcprm->spwarn) {
/* South Pole Case; "North" along Prime meridian */
    double xlat,xlong;
      cnxyll(stcprm, xi,eta, &xlat,&xlong);
      cg2wll(stcprm, xlat,xlong, ug,vg, ue,vn);
    } else {
/*  Normal Case; meteorological coordinate related to true North*/
    double temp;
      xpolg = stcprm->srotate - stcprm->gamma * xi0;
      ypolg = stcprm->crotate - stcprm->gamma * eta0;
      temp = sqrt(xpolg*xpolg + ypolg*ypolg);
      xpolg /= temp;
      ypolg /= temp;
      *ue = ypolg * ug - xpolg * vg;
      *vn = ypolg * vg + xpolg * ug;
    }
  }
}

static void cnxyll(maparam * stcprm,double xi,double eta,
		double * lat,double * longit) {
#define FSM .01
#define NEARONE .9999999999999
double ymerc,temp;
double arg;
double radial = 2.*eta - stcprm->gamma * (xi*xi + eta*eta);
  if ( stcprm->gamma * radial >= NEARONE) {
    *lat = stcprm->gamma>0? 90. : -90.;
/*    *longit = stcprm->reflon; */
	 *longit = 90. + *lat;
/* Change made 02/12/02 to acommodate WMO reporting conventions.  North
   pole is longitude 180., so "North" points to the Greenwich Meridian,
   South Pole is longitude 0. so "North" again points to the Greenwich
   Meridian.  */
    return;
  }
  ymerc = .5 * log1pabovera( - stcprm->gamma, radial);
  * lat = cymr2l(stcprm, ymerc) ;
  temp = stcprm->gamma * xi;
  arg = 1. - stcprm->gamma * eta;
  if ( (temp<0 ? - temp:temp) < FSM*arg) {
    temp = temp/arg; temp *= temp;
    temp = xi / arg * (1.    - temp *
		      (1./3. - temp *
		      (1./5. - temp *
		      (1./7.))));
  } else {
    temp = atan2(temp,arg) / stcprm->gamma;
  }
  * longit = stcprm->reflon + DEGPRAD * temp;
#undef FSM
#undef NEARONE
#undef REARTH

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc/RCS/cc2gxy.c,v $";
 static char rcs_id2[] = "$Id: cc2gxy.c,v 1.1 2004/09/16 15:00:43 dsa Exp $";}
/*  ===================================================  */

}
