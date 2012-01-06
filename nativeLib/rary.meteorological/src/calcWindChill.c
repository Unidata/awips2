# include "meteoLib.h"
# include <math.h>

/* -----------------------------------------------------------------
** calcWindChill()
**
** calculate the windChill from temperature and windSpeed
** this is the 7/01 "official" NWS formula
** input temp in Celsius and windSpeed in km/h
**
**-----------------------------------------------------------------*/
float calcWindChill(float temp, float windSpd)
    {

    float spd;

    /* arbitrarily do the calculation only for temps at or below 60F */
    if ( temp > 16. ) return 1e37;

    /* no chilling if speed < 4 mph = 6.44km/h */
    if ( windSpd < 6.4 ) return temp;
    /* peg speed at 80 mph (= 128.75 km/h) */
    if ( windSpd > 128.75 )
        spd = 128.75;
    else
        spd = windSpd;

    spd = pow (spd, 0.16);
    float windChillTemp = 13.12 + 0.6215*temp - 11.37*spd + 0.3965*temp*spd;
    return windChillTemp; /* in Celsius */
    }
