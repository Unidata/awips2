#include <stdlib.h>
#include <math.h>
#include "cmapf.h"

/*
 * cmapf.c  - source file for conformal mapping function utility.
 * Written 12/21/94 by
 * Dr. Albion Taylor
 * NOAA / OAR / ARL                  Phone: (301) 713-0295 ext 132
 * Rm. 3151, 1315 East-West Highway  Fax:   (301) 713-0119
 * Silver Spring, MD 20910           E-mail: ADTaylor@arlrisc.ssmc.noaa.gov
 */


double eqvlat(double lat1,double lat2) {

/*  Written 12/21/94 by Dr. Albion Taylor
 *
 *    This function is provided to assist in finding the tangent latitude
 *    equivalent to the 2-reference latitude specification in the legend
 *    of most lambert conformal maps.  If the map specifies "scale
 *    1:xxxxx true at 40N and 60N", then eqvlat(40.,60.) will return the
 *    equivalent tangent latitude.
 *  INPUTS:
 *    lat1, lat2:  The two latitudes specified in the map legend
 *  RETURNS:
 *    the equivalent tangent latitude
 *  EXAMPLE:  stcmap(& strcmp, eqvlat(40.,60.), 90.)
 */
/*   Changes Made May 9, 2003 to accomodate the following special
 *   situations:
 *   1. if lat1 == lat2, returned value will be lat1 (reduced to between -90.
 *      and 90.).
 *   2. If either lat1 or lat2 is 90. (or -90.) then 90. (or -90.) will be
 *      returned.  This reflects the fact that, for y fixed (-90. < y < 90.),
 *      as x -> 90. (or -90.), eqvlat(x,y) ->90. (or -90.)  This limiting
 *      case of tangent latitude 90. is a polar stereographic projection,
 *      for which the scale at 90. is a maximum, and therefore greater than the
 *      other latitude y.  Thus, eqvlat(90.,60.) returns 90., although the
 *      scale at 90. will be greater than at 60. For eqvlat(90.,-90.), the
 *      limit process is ambiguous; for the sake of symmetry, such a case
 *      will return 0.
 */

double slat1,slat2,al1,al2;
  slat1 = sin(RADPDEG * lat1);
  slat2 = sin(RADPDEG * lat2);
/* reorder, slat1 larger */
  if (slat1 < slat2) {
     double temp = slat1;
     slat1 = slat2;
     slat2 = temp;
  }
/*  Take care of special cases first */
  if (slat1 == slat2) return asin(slat1) * DEGPRAD;
  if (slat1 == -slat2 ) return 0.;
  if (slat1 >= 1.) return 90.;
  if (slat2 <= -1.) return -90.;
/********************************************************/
#define FSM 1.e-3
/* Compute al1 = log((1. - slat1)/(1. - slat2))/(slat1 - slat2) */
  { double tau = (slat1 - slat2)/(2. - slat1 - slat2);
    if ( tau > FSM ) {
       al1 = log((1. - slat1)/(1. - slat2))/(slat1 - slat2);
    } else {  
       tau *= tau;
       al1 = -2./(2. - slat1 - slat2) *
                  (1.    + tau *
                  (1./3. + tau *
                  (1./5. + tau *
                  (1./7. + tau))));
    }
  }
/* Compute al2 = log((1. + slat1)/(1. + slat2))/(slat1 - slat2) */
  { double tau = (slat1 - slat2)/(2. + slat1 + slat2);
    if ( tau > FSM ) {
       al2 = log((1. + slat1)/(1. + slat2))/(slat1 - slat2);
    } else {
       tau *= tau;
       al2 =  2./(2. + slat1 + slat2) *
                  (1.    + tau *
                  (1./3. + tau *
                  (1./5. + tau *
                  (1./7. + tau))));
    }
  }
  return asin ((al1 + al2) / (al1 - al2)) * DEGPRAD;
#undef FSM
}
