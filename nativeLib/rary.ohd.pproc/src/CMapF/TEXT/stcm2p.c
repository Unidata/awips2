#include "cmapf.h"

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
}

