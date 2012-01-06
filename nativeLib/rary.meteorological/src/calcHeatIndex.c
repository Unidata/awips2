#include "meteoLib.h"
#include <math.h>
#include <stdio.h>
/* ------------------------------------------------------------------
** calcHeatIndex()
**
** calculate the heatIndex by temperature and dew point
** temperatures in Celsius
** -----------------------------------------------------------------*/
float calcHeatIndex(float temp, float dewPoint)
    {

    float RH;
    float heatIdx;
    float t_sq, rh_sq;
    int mni = 1, ni = 1, nj = 1;

    /* Bail out if T is < 80F or Td missing. Lans' formula really doesn't
    ** work well below 80F, and there's not much point in calculating it,
    ** anyway. */
    if (temp < 26.5 || dewPoint > temp) return 1e37;

    /* get relative humidity in integer percent and temp in Fahrenheit */
    calcrh(&temp, &dewPoint, &mni, &ni, &nj, &RH);
    temp = temp*1.8+32;

    t_sq = temp * temp;
    rh_sq = RH * RH;

    /* the Lans Rothfusz formula */
    heatIdx = -42.379 + 2.04901523 * temp + 10.14333127 * RH
              - 0.22475541 * temp * RH - 0.00683783 * t_sq - 0.05481717 * rh_sq
              + 0.00122874 * t_sq * RH + 0.00085282 * temp * rh_sq
              - 0.00000199 * t_sq * rh_sq;

    return (heatIdx-32)/1.8;
    }
