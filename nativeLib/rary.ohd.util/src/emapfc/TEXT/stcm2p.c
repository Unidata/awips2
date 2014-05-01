#include <stdlib.h>
#include "emapfc_inc/cmapf.h"

/*
 * stcm2p.c  - source file for conformal mapping function utility.
 * Written 12/21/94 by
 * Dr. Albion Taylor
 * NOAA / OAR / ARL                  Phone: (301) 713-0295 ext 132
 * Rm. 3151, 1315 East-West Highway  Fax:   (301) 713-0119
 * Silver Spring, MD 20910           E-mail: ADTaylor@arlrisc.ssmc.noaa.gov
 */



void stcm2p(maparam * stcprm,
	    double x1, double y1, double xlat1, double xlong1,
	    double x2, double y2, double xlat2, double xlong2) {
double x1a,y1a, x2a,y2a, den,dena;
  stcprm->x0 = stcprm->y0 = stcprm->srotate = 0;
  stcprm->crotate = stcprm->gridszeq = 1.;
  cll2xy(stcprm, xlat1,xlong1, &x1a, &y1a);
  cll2xy(stcprm, xlat2,xlong2, &x2a, &y2a);
  den = sqrt((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2));
  dena = sqrt((x1a - x2a)*(x1a - x2a) + (y1a - y2a)*(y1a - y2a));
  stcprm->crotate = ((x1a - x2a)*(x1 - x2) + (y1a - y2a)*(y1 - y2) )
		    /den /dena;
  stcprm->srotate = ((y1a - y2a)*(x1 - x2) - (x1a - x2a)*(y1 - y2) )
		    /den /dena;
  stcprm->gridszeq *= dena /den;
  cll2xy(stcprm, xlat1,xlong1, &x1a,&y1a);
  stcprm->x0 += x1 - x1a;
  stcprm->y0 += y1 - y1a;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc/RCS/stcm2p.c,v $";
 static char rcs_id2[] = "$Id: stcm2p.c,v 1.1 2004/09/16 15:00:43 dsa Exp $";}
/*  ===================================================  */

}
