#include <math.h>
#include "cmapf.h"

vector_3d ll_geog(double lat, double longit){
vector_3d geog;
/* Converts a latitude-longitude pair to the 3-vector of "geographic"
 * coordinates.  In the 3-vector, component 0 (x) is in the direction
 * of the equator at the Greenwich meridian, component 1 (y) is in the
 * direction of the equator toward 90 East, and component 2 (z) is in the
 * of the North Pole.
 */
double clat;
  geog.v[2] = sin(RADPDEG * lat);
  geog.v[0] = cos(RADPDEG * longit) * (clat = cos(RADPDEG * lat));
  geog.v[1] = sin(RADPDEG * longit) * clat;
return geog;
}
