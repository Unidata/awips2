#include <stdlib.h>
#include <math.h>
#include "emapfc_inc/cmapf.h"
#define REARTH stcprm->arad

/*
 * cgszxy.c  - source file for conformal mapping function utility.
 * Written 12/21/94 by
 * Dr. Albion Taylor
 * NOAA / OAR / ARL                  Phone: (301) 713-0295 ext 132
 * Rm. 3151, 1315 East-West Highway  Fax:   (301) 713-0119
 * Silver Spring, MD 20910           E-mail: ADTaylor@arlrisc.ssmc.noaa.gov
 */
static double gszlim(double eccen,double slat,double gamma) ;

double cgszxy(maparam * stcprm,double x,double y) {
double xi,eta,xi0,eta0,ymerc,slat,clat;
#define NEARONE .9999999999999
double radial;
  xi0 = (x - stcprm->x0) * stcprm->gridszeq / REARTH;
  eta0 = (y - stcprm->y0) * stcprm->gridszeq / REARTH;
  xi = xi0 * stcprm->crotate - eta0 * stcprm->srotate;
  eta = xi0 * stcprm->srotate + eta0 * stcprm->crotate;
  radial = 2. * eta - stcprm->gamma * (xi*xi + eta*eta);
  if ((NEARONE - stcprm->gamma * radial) <= 0.) {
    if ( stcprm->gamma < -NEARONE ) return
      stcprm->gridszeq * gszlim(stcprm->eccen,-1.,-stcprm->gamma);
    else {
      if (stcprm->gamma > NEARONE) return
        stcprm->gridszeq * gszlim(stcprm->eccen,1.,stcprm->gamma);
      else return 0;
    }
  }
  ymerc = .5 * log1pabovera(- stcprm->gamma,radial);
/*If y mercator > 15, we are eithin 1.0e-5 degrees of pole.*/
  if (ymerc >  15.)
     return stcprm->gamma > NEARONE ?
             stcprm->gridszeq*gszlim(stcprm->eccen,1.,stcprm->gamma) : 0. ;
  if (ymerc <  -15.)
     return stcprm->gamma < -NEARONE ?
             stcprm->gridszeq*gszlim(stcprm->eccen,-1,-stcprm->gamma) : 0. ;
  cmr2sc(stcprm,ymerc,&slat,&clat);
  return stcprm->gridszeq * exp(stcprm->gamma * ymerc) * clat /
       sqrt(1. - stcprm->eccen*stcprm->eccen * slat*slat);
#undef NEARONE
#undef REARTH
}


static double gszlim(double eccen,double slat,double gamma) {
/*approximate gridsize near pole in nearly stereographic projections.*/
double ecslat=eccen*slat;
  return exp(.5 * (
          eccen *gamma* log((1.-ecslat)/(1.+ecslat)) +
         (1.+gamma)*log(1. + slat)
                  ) ) /
         sqrt(1. - ecslat*ecslat);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc/RCS/cgszxy.c,v $";
 static char rcs_id2[] = "$Id: cgszxy.c,v 1.1 2004/09/16 15:00:43 dsa Exp $";}
/*  ===================================================  */

}

