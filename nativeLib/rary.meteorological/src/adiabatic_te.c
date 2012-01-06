#include "meteoLib.h"
#include <math.h>

/* This routine calculates the equivalent tempurature of a temperature 
   and pressure using the adiabatic definition, assuming saturation 
   put a fudge factor into L/cp to get agreement of moist adiabats
   with a published thermodynamic diagram */

float adiabatic_te (const float * temp, const float * press)
    {
    float e = exp(26.660820-0.0091379024*(*temp)-6106.396/(*temp));
    e = 0.622*e/(*press-e);
    return *temp*exp(2740.0*e/(*temp));
    }
