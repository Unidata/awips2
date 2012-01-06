#include <stdlib.h>
#include <math.h>
#include "emapfc_inc/cmapf.h"

/*
 * cgszll.c  - source file for conformal mapping function utility.
 * Written 12/21/94 by
 * Dr. Albion Taylor
 * NOAA / OAR / ARL                  Phone: (301) 713-0295 ext 132
 * Rm. 3151, 1315 East-West Highway  Fax:   (301) 713-0119
 * Silver Spring, MD 20910           E-mail: ADTaylor@arlrisc.ssmc.noaa.gov
 */

static double gszlim(double eccen,double slat,double gamma) ;

double cgszll(maparam * stcprm,double lat,double longit) {
#define NEARONE .9999999999999
double slat = sin(RADPDEG * lat);
double clat,ymerc;
  if (slat <= -NEARONE)
    return
     (stcprm->gamma < -NEARONE ?
      stcprm->gridszeq * gszlim(stcprm->eccen,-slat,-stcprm->gamma) : 0.);

  if (slat >= NEARONE)
    return
     (stcprm->gamma > NEARONE ?
      stcprm->gridszeq * gszlim(stcprm->eccen,slat,stcprm->gamma) : 0.);

  clat = cos(RADPDEG * lat);clat = clat<0. ? -clat : clat;
  ymerc = cl2ymr(stcprm,lat);
  return stcprm->gridszeq * exp(stcprm->gamma * ymerc) * clat /
  sqrt(1. - stcprm->eccen*stcprm->eccen * slat*slat);
  #undef NEARONE
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
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc/RCS/cgszll.c,v $";
 static char rcs_id2[] = "$Id: cgszll.c,v 1.1 2004/09/16 15:00:43 dsa Exp $";}
/*  ===================================================  */

}

