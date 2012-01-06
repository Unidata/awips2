#include <stdlib.h>
#include "emapfc_inc/cmapf.h"
#define REARTH stcprm->arad
#define EPSIL stcprm->eccen
#define BRAD stcprm->brad

/*
 * eqvlat.c  - source file for conformal mapping function utility.
 * Written 12/21/94 by
 * Dr. Albion Taylor
 * NOAA / OAR / ARL                  Phone: (301) 713-0295 ext 132
 * Rm. 3151, 1315 East-West Highway  Fax:   (301) 713-0119
 * Silver Spring, MD 20910           E-mail: ADTaylor@arlrisc.ssmc.noaa.gov
 */
double eqvlat(maparam * stcprm,double lat1,double lat2) {

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
 *  EXAMPLE:  stlmbr(& stcprm, eqvlat(40.,60.), 90.)
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

double result,midslat,slat1,slat2,dlat,ymerc1,ymerc2,s1,s2;


/*First, test whether stcprm is a valid geoid
 */
  if (mkGeoid(stcprm,TST,0.,0.) != 0) return 999.;
  
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
#define FSM 1.e-4
  midslat = .5*(slat1+slat2);
  dlat = (slat1-slat2);
  if ( fabs(dlat) > FSM*(1-midslat)*(1.+midslat) ) {
    ymerc1 = cl2ymr(stcprm, lat1);
    ymerc2 = cl2ymr(stcprm, lat2);
    s1 = stcprm->brad * tan(RADPDEG * lat1) / REARTH;
    s2 = stcprm->brad * tan(RADPDEG * lat2) / REARTH;
    result = .5 * (log(1.+s2*s2)-log(1.+s1*s1)) /
           (ymerc2 - ymerc1);
  } else {
    /*If two latitudes are extremely close, power series representation of above*/
    result = midslat * (1. + .5*dlat*dlat*(1./(1.-midslat)/(1.+midslat)+
          EPSIL*EPSIL/(1. - EPSIL*midslat) / (1.+EPSIL*midslat) )/3.);
  }
  /*At this point, result is a sine of latitude.*/
  return DEGPRAD*atan2(result,sqrt((1.-result)*(1.+result)));
#undef FSM
#undef REARTH
#undef EPSIL
#undef BRAD

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc/RCS/eqvlat.c,v $";
 static char rcs_id2[] = "$Id: eqvlat.c,v 1.1 2004/09/16 15:00:43 dsa Exp $";}
/*  ===================================================  */

}
