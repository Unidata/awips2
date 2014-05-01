#include "cmapf.h"

void stcm1p(maparam * stcprm,
		 double x1, double y1, double xlat1, double xlong1,
		 double xlatrf, double xlonrf,
		 double gridsz, double orient) {
double enx,eny,enz,norm;
double x1a,y1a;
double c_or = cos(RADPDEG * orient),s_or = - sin(RADPDEG * orient);
  stcprm->x0 = stcprm->y0 = stcprm->srotate = 0;
  stcprm->crotate = stcprm -> gridszeq = 1.0;
  cpolll(stcprm, xlatrf,xlonrf, &enx, &eny, &enz);
  if ((norm = sqrt(enx*enx + eny*eny)) == 0.) {
	 cgrnll(stcprm, xlatrf,xlonrf, &enx,&eny,&enz);
	 norm = sqrt(enx*enx + eny*eny);
  }
  enx /= norm; eny /= norm;
  stcprm->gridszeq *= gridsz / cgszll(stcprm, xlatrf,xlonrf);
  stcprm -> crotate = eny * c_or - enx * s_or;
  stcprm -> srotate = -eny * s_or - enx * c_or;
  cll2xy(stcprm, xlat1,xlong1, &x1a,&y1a);
  stcprm->x0 += x1 - x1a;
  stcprm->y0 += y1 - y1a;
}

