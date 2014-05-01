#include <stdlib.h>
#include <stdio.h>
#include "emapfc_inc/cmapf.h"

emapfc_geoidtst_main (int argc,char ** argv) {
  maparam stcprm;
  int result;
  result = useGeoid(&stcprm,"dummyGeoid");
  printf("%d\n",result);
  lsGeoid();
  result = useGeoid(&stcprm,"wgs84");
  printf("%d\n",result);
  result = stlmbr(&stcprm,45.,70.);
  printf("%d\n",result);
  printf("%f %f %f \n",stcprm.arad,stcprm.brad,stcprm.eccen);
  stcm2p(&stcprm, 1.,1., 30.,90., 75,1,30.,50.);
  fgetc(stdin);
  stcm2p(&stcprm, 1.,1., 30.,90., 75,1,30.,50.);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc_main/RCS/geoidtst.c,v $";
 static char rcs_id2[] = "$Id: geoidtst.c,v 1.1 2004/09/16 15:05:41 dsa Exp $";}
/*  ===================================================  */

}
