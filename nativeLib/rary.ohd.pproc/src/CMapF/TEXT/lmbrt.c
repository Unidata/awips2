#include "cmapf.h"
                                                           
void stlmbr(maparam * stcprm,double reflat,double reflon) {
/*
 *  Set Map Parameters for a North Polar LaMBeRt Conic Conformal
 *    Projection
 *  Inputs: reflat - tangent latitude of the tangent latitude of
 *                   cone.
 *          reflon - midrange longitude (180 degrees from cut)
 *  Outputs: stcprm - map parameters
 */
  mapf_start(stcprm,reflat,90.,reflon,0.,reflon);
}

void stcmap(maparam * stcprm,double tnglat,double reflon) {
/*
 *  Set Map Parameters for a North Polar LaMBeRt Conic Conformal
 *    Projection
 *  Included for compatibility with previous version
 *  Inputs: tnglat - tangent latitude of the tangent latitude of
 *                   cone.
 *          reflon - midrange longitude (180 degrees from cut)
 *  Outputs: stcprm - map parameters
 */
  mapf_start(stcprm,tnglat,90.,reflon,0.,reflon);
}
