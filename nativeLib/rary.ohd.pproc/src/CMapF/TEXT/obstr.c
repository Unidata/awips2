#include "cmapf.h"

void sobstr(maparam * stcprm,double p_lat,double p_longit){
/*
 *  Set Map Parameters for an OBlique STeReographic Projection
 *  Inputs: p_lat,P_longit - latitude and longitude of the
 *          projection pole (tangent point)
 *  Outputs: stcprm - map parameters 
 */
  mapf_start(stcprm, 90., p_lat,p_longit, p_lat-90.,p_longit);
}

