#include "cmapf.h"


vector_3d basegtom(maparam * stcprm,vector_3d xin) {
vector_3d this={{0.,0.,0.}};
/* converts vectors from the "geographic" coordinate system
 * (based on the North pole, equator and Greenwich meridian) to
 * vectors in the "map" coordinate system, from which the x-y
 * coordinates of the map are obtained.  Uses stcprm->rotate to
 * perform the translation.
 */
int k,l;
for(k=0;k<3;k++) for(l=0;l<3;l++)
   this.v[l]+= xin.v[k] * stcprm->rotate[l][k];
return this;
}

vector_3d basemtog(maparam * stcprm,vector_3d xin) {
vector_3d this={{0.,0.,0.}};
/* Reverses the transformation of basemtog
 */
int k,l;
for(k=0;k<3;k++) for(l=0;l<3;l++)
   this.v[l]+= xin.v[k] * stcprm->rotate[k][l];
return this;
}


