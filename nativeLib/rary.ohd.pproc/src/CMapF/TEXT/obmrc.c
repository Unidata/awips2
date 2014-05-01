#include "cmapf.h"

void sobmrc(maparam * stcprm, double r_lat, double r_longit,
					double anch_lat, double anch_longit) {
/*
 *  Set Map Parameters for an Oblique MeRCator Projection
 *  Inputs: r_lat,r_longit - latitude and longitude of reference
 *            point on the great circle tangent to the cylinder,
 *            and 180 degrees from the cut.
 *	    anch_lat,anch_longit - latitude and longitude of
 *            an anchor point elsewhere on the same great circle.
 *  Outputs: stcprm - map parameters
 */
vector_3d refpoint,anchorpt,projpole;
double p_lat,p_longit,norm;
  refpoint = ll_geog(r_lat,r_longit);
  anchorpt = ll_geog(anch_lat,anch_longit);
  norm = x_product(anchorpt.v, refpoint.v, projpole.v);
  if (norm != 0.) {
	 geog_ll(&projpole,&p_lat,&p_longit);
	 mapf_start(stcprm, 0., p_lat,p_longit, r_lat,r_longit);
  } else {
	 stvmrc(stcprm, r_lat, r_longit);
  }
}
