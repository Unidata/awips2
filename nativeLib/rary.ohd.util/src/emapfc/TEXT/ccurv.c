#include <stdlib.h>
#include <math.h>
#include "emapfc_inc/cmapf.h"
#define REARTH stcprm->arad

/*
 * ccurv.c  - source file for conformal mapping function utility.
 * Written 12/21/94 by
 * Dr. Albion Taylor
 * NOAA / OAR / ARL                  Phone: (301) 713-0295 ext 132
 * Rm. 3151, 1315 East-West Highway  Fax:   (301) 713-0119
 * Silver Spring, MD 20910           E-mail: ADTaylor@arlrisc.ssmc.noaa.gov
 */

static double cspanf(double value,double begin,double end);
void ccrvll(maparam * stcprm,double lat, double longit,
			     double * gx, double * gy) {
double along = cspanf(longit - stcprm->reflon,-180.,180.)
		   * RADPDEG * stcprm->gamma;
double slong,clong,xpolg,ypolg,curv,slat,temp;
  slong = sin(along);
  clong = cos(along);
  xpolg = -slong * stcprm->crotate + clong * stcprm->srotate;
  ypolg = clong * stcprm->crotate + slong * stcprm->srotate;
  slat = sin(RADPDEG * lat);
  temp = stcprm->eccen*slat;temp = sqrt((1.-temp)*(1.+temp));
  curv = (stcprm->gamma - slat) / cos(RADPDEG * lat) *temp /
					REARTH;
  *gx = curv * xpolg;
  *gy = curv * ypolg;
}

#define REARTH stcprm->arad
void ccrvxy(maparam * stcprm,double x,double y,
			     double * gx, double * gy) {
double temp = stcprm->gamma * stcprm->gridszeq / REARTH;
double xpolg,ypolg;
  xpolg = stcprm->srotate + temp * (stcprm->x0 - x);
  ypolg = stcprm->crotate + temp * (stcprm->y0 - y);
  temp = sqrt(xpolg*xpolg + ypolg * ypolg);
  if(temp > 0.) {
    double ymerc,curv,sinlat,coslat,temp2;
    ymerc = -log(temp)/stcprm->gamma;
    cmr2sc(stcprm,ymerc,&sinlat,&coslat);
    temp2 = stcprm->eccen*sinlat;temp2 = sqrt((1.-temp2)*(1.+temp2));
    curv = (stcprm->gamma - sinlat) / coslat * temp2 /
					REARTH;
    *gx = xpolg * curv / temp;
    *gy = ypolg * curv / temp;
  } else {
    if (stcprm -> gamma == 1. || stcprm->gamma == -1.) {
      *gx = *gy = 0.;  /*At a pole, in Polar Stereographic, curv=0.*/
    } else {
      *gx = *gy = 1.e20 / REARTH;  /*In any other projection, it is infin*/
    }
  }
}
#undef REARTH

static double cspanf(double value,double begin,double end){
double first,last;
first = begin<end?begin:end;
last = begin<end?end:begin;
value = fmod(value-first,last-first);
return value<0 ? value + last : value + first;
#undef REARTH

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc/RCS/ccurv.c,v $";
 static char rcs_id2[] = "$Id: ccurv.c,v 1.1 2004/09/16 15:00:43 dsa Exp $";}
/*  ===================================================  */

}
