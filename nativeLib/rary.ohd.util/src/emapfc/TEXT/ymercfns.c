#include <stdio.h>
#include "emapfc_inc/cmapf.h"

double cl2ymr(maparam * stcprm,double lat) {
double result,sinlat;
#define ECC stcprm->eccen
  sinlat = sin(RADPDEG*lat);
  result = .5*ECC*log((1. - ECC*sinlat) / (1. + ECC*sinlat)) +
           .5*log((1. + sinlat) / (1. - sinlat));
  return result;
}

double cymr2l(maparam * stcprm,double ymerc) {
double sinlat,coslat;
  cmr2sc(stcprm,ymerc,& sinlat,& coslat) ;
  return DEGPRAD*atan2(sinlat,coslat);
}

void cmr2sc(maparam * stcprm,double ymerc,double * sinlat,double * coslat) {
double slat,clat,expy,expmy,adjust=1.;
int k;
  expy=exp(ymerc);
  expmy=exp(-ymerc);
  for(k=0;k<5;k++) {
    clat = 2. / (expy*adjust + expmy/adjust) ;
    slat = .5 * (expy*adjust - expmy/adjust) * clat ;
    adjust=exp( .5*ECC*log((1. + ECC*slat) / (1. - ECC*slat)) );
  }
  *sinlat = slat;*coslat=clat;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc/RCS/ymercfns.c,v $";
 static char rcs_id2[] = "$Id: ymercfns.c,v 1.1 2004/09/16 15:00:43 dsa Exp $";}
/*  ===================================================  */

}

