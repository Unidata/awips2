#include <stdlib.h>
#include "emapfc_inc/cmapf.h"

/*
 * stcm1p.c  - source file for conformal mapping function utility.
 * Written 12/21/94 by
 * Dr. Albion Taylor
 * NOAA / OAR / ARL                  Phone: (301) 713-0295 ext 132
 * Rm. 3151, 1315 East-West Highway  Fax:   (301) 713-0119
 * Silver Spring, MD 20910           E-mail: ADTaylor@arlrisc.ssmc.noaa.gov
 */

static double cspanf(double value,double begin,double end);

void stcm1p(maparam * stcprm,
	    double x1, double y1, double xlat1, double xlong1,
	    double xlatg, double xlong, double gridsz, double orient) {
double x1a,y1a;
double turn=RADPDEG * (orient - stcprm->gamma *
    cspanf(xlong - stcprm->reflon,-180.,180.) );
  stcprm->x0 = stcprm->y0 = 0.;
  stcprm->gridszeq = 1.;
  stcprm->crotate = cos(turn);
  stcprm->srotate = - sin(turn);
  stcprm->gridszeq *=
	   (gridsz / cgszll(stcprm,xlatg,stcprm->reflon)) ;
  cll2xy(stcprm, xlat1,xlong1, &x1a,&y1a);
  stcprm->x0 += x1 - x1a;
  stcprm->y0 += y1 - y1a;
}

static double cspanf(double value,double begin,double end){
double first,last;
first = begin<end?begin:end;
last = begin<end?end:begin;
value = fmod(value-first,last-first);
return value<0 ? value + last : value + first;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc/RCS/stcm1p.c,v $";
 static char rcs_id2[] = "$Id: stcm1p.c,v 1.1 2004/09/16 15:00:43 dsa Exp $";}
/*  ===================================================  */

}

