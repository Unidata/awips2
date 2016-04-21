#include "cmapf.h"

void soblmbr(maparam * stcprm, double ref_lat,double ref_long,
					 double an_lat1,double an_long1,
					 double an_lat2,double an_long2) {
/*
 *  Set Map Parameters for an Oblique LaMBeRt Conic Conformal
 *          Projection.
 *  Inputs:   ref_lat,ref_long,
 *            an_lat1,an_long1,
 *            an_lat2,an_long2  - latitudes and longitudes of three
 *            points on the circle (great or small) which is
 *            tangent to the projection cone.  ref_lat,ref_long is the
 *            reference point; 180degrees away from the cut.
 *  Outputs: stcprm - map parameters
 */
vector_3d an_point1 = ll_geog(an_lat1,an_long1),
	  ref_point = ll_geog(ref_lat,ref_long),
	  an_point2 = ll_geog(an_lat2,an_long2),
	  pole_point;
double b_a[3],c_b[3],norm,sin_cone,cos_cone,temp;
double lat_pole,longit_pole;
int k;
  for(k=0;k<3;k++) {
	 b_a[k] = ref_point.v[k] - an_point1.v[k];
	 c_b[k] = an_point2.v[k] - ref_point.v[k];
  }
  if ((norm = x_product(b_a, c_b, pole_point.v)) != 0. ) {
	 geog_ll(&pole_point, &lat_pole, &longit_pole);
	 for (k=0,sin_cone=0.;k<3;k++)
		sin_cone += ref_point.v[k] * (pole_point.v[k] /= norm);
	 for (k=0,cos_cone=0.;k<3;k++) {
		temp = sin_cone*pole_point.v[k] - ref_point.v[k];
		cos_cone += temp*temp;
	 }
	 mapf_start(stcprm,DEGPRAD*atan2(sin_cone,cos_cone),
		lat_pole,longit_pole, ref_lat,ref_long);
  } else {
	 for(k=0,temp=0.;k<3;k++) temp += b_a[k]*b_a[k];
	 if (temp != 0.) sobmrc(stcprm, ref_lat,ref_long, an_lat1,an_long1);
	 else {
	 for(k=0,temp=0.;k<3;k++) temp += c_b[k]*c_b[k];
	 if (temp != 0.) sobmrc(stcprm, ref_lat,ref_long, an_lat2,an_long2);
	 else {
	 sobstr(stcprm, ref_lat,ref_long);
  }}}
}
