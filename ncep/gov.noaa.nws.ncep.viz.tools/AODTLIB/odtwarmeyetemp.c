/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"

/* AODT library function */
extern int aodtv64_gettemps(int);

int aodtv64_getwarmeyetemplocation(float *lat, float *lon)
/* Routine to search for warmest pixel temperature within search radius
   and return location to API.
   Inputs : none
   Outputs: warmest pixel location from automated storm positioning routine
   Return : 91 : good warmest pixel latitude/longitude
*/
{

  aodtv64_gettemps(keyerA_v64);
  *lat=odtcurrent_v64->IR.warmlatitude;
  *lon=odtcurrent_v64->IR.warmlongitude;

  return 91;
}
