#include "cmapf.h"

void stvmrc(maparam * stcprm,double r_lat,double r_longit){
/*
 *  Set Map Parameters for a TransVerse MeRCator Projection
 *  Inputs: r_lat,r_longit - latitude and longitude of the reference
 *            point on the meridian tangent to the cylinder,
 *            and 180 degrees from the cut.
 *  Outputs: stcprm - map parameters
 */
vector_3d refpoint,anchorpt,projpole;
double p_lat,p_longit;
  refpoint = ll_geog(r_lat,r_longit);
  anchorpt = ll_geog(r_lat - 90.,r_longit);
  x_product(anchorpt.v, refpoint.v, projpole.v);
  geog_ll(&projpole,&p_lat,&p_longit);
  mapf_start(stcprm, 0., p_lat,p_longit, r_lat,r_longit);
}
