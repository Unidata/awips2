#include <stdlib.h>
#include <stdio.h>
#include "emapfc_inc/cmapf.h"

emapfc_testemapf_main(){
maparam stcprm,stcprm2;
double cnflat,cnflat2;
double reflat=60.,refgrid=381.;

double x,y,xlat,ylong,gsza,gszb;
int rc;
rc = useGeoid(&stcprm,"wgs84");
if ( rc != 0) {
  fprintf(stderr,"useGeoid failed\n");
  return 1;
}

cnflat = eqvlat(&stcprm,40.,60.);
printf("%f ",cnflat);
rc = stlmbr(&stcprm,cnflat,-90.);
if ( rc != 0) {
  fprintf(stderr,"stlmbr failed\n");
  return 1;
}
printf("%lf \n",cgszll(&stcprm,cnflat,-90.));
printf("%lf %lf \n", cgszll(&stcprm,40.,-90.),cgszll(&stcprm,60.,-90.));

rc = mkGeoid(&stcprm2,AE,6367.470,0.); /*Sphere according to GRIB*/
cnflat2 = eqvlat(&stcprm2,40.,60.);
printf("%f ",cnflat2);
rc = stlmbr(&stcprm2,cnflat2,-90.);
printf("%lf \n",cgszll(&stcprm2,cnflat2,-90.));
printf("%lf %lf \n", cgszll(&stcprm2,40.,-90.),cgszll(&stcprm2,60.,-90.));

printf ( "Polar Stereographic, Gridsize %f at %f N\n",refgrid,reflat);

stlmbr(&stcprm,90.,-90.);stlmbr(&stcprm2,90.,-90.);
stcm1p(&stcprm ,33.,33.,90.,0.,  reflat,-90.,refgrid,0.);
stcm1p(&stcprm2,33.,33.,90.,0.,  reflat,-90.,refgrid,0.);

for (x=1.;x<=33.;x+=1.) {
  y=x;
  cxy2ll(&stcprm, x,y, &xlat,&ylong);
  gsza = cgszxy(&stcprm, x,y);
  gszb = cgszll(&stcprm, xlat,ylong);
  printf(" %2.0f,%2.0f %8.3f %8.3f %8.3f ",x,y,xlat,gsza,gszb);

  cxy2ll(&stcprm2, x,y , &xlat,&ylong);
  gsza = cgszxy(&stcprm2, x,y);
  gszb = cgszll(&stcprm2, xlat,ylong);
  printf(" %8.3f %8.3f %8.3f\n",xlat,gsza,gszb);
}
getc(stdin);
return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc_main/RCS/testemapf.c,v $";
 static char rcs_id2[] = "$Id: testemapf.c,v 1.1 2004/09/16 15:05:41 dsa Exp $";}
/*  ===================================================  */

}
