#include <stdlib.h>
#include <math.h>
#include "emapfc_inc/cmapf.h"

/*
 * cc2gll.c  - source file for conformal mapping function utility.
 * Written 12/21/94 by
 * Dr. Albion Taylor
 * NOAA / OAR / ARL                  Phone: (301) 713-0295 ext 132
 * Rm. 3151, 1315 East-West Highway  Fax:   (301) 713-0119
 * Silver Spring, MD 20910           E-mail: ADTaylor@arlrisc.ssmc.noaa.gov
 */

void cc2gll(maparam * stcprm,double lat,double longit,
	    double ue,double vn, double * ug,double * vg) {
double along = cperiodic(longit - stcprm->reflon, -180., 180.);
double rot,slong,clong,xpolg,ypolg;
  rot = RADPDEG * (-stcprm->gamma * along);
  slong = sin(rot); clong = cos(rot);
  xpolg = slong * stcprm->crotate + clong * stcprm->srotate;
  ypolg = clong * stcprm->crotate - slong * stcprm->srotate;
  *ug = ypolg * ue + xpolg * vn;
  *vg = ypolg * vn - xpolg * ue;
}

void cw2gll(maparam * stcprm,double lat,double longit,
	    double ue,double vn, double * ug,double * vg) {
double along = cperiodic(longit - stcprm->reflon, -180., 180.);
double rot,slong,clong,xpolg,ypolg;
  if ( lat > 89. ) {
    rot = RADPDEG * ( -stcprm->gamma * along + longit - 180.);
  } else {
    if (lat < -89.) {
      rot = RADPDEG * (-stcprm->gamma * along - longit);
    } else {
      rot = RADPDEG * (-stcprm->gamma * along);
    }
  }
  slong = sin(rot); clong = cos(rot);
  xpolg = slong * stcprm->crotate + clong * stcprm->srotate;
  ypolg = clong * stcprm->crotate - slong * stcprm->srotate;
  *ug = ypolg * ue + xpolg * vn;
  *vg = ypolg * vn - xpolg * ue;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc/RCS/cc2gll.c,v $";
 static char rcs_id2[] = "$Id: cc2gll.c,v 1.1 2004/09/16 15:00:43 dsa Exp $";}
/*  ===================================================  */

}

