#include "meteoLib.h"
#include <math.h>
/***************************************************************************
* Calculate the temperature at a given pressure and mixing ratio  
* Borrowed the derived equation from tdofesat.f routine written by
* J. Ramer.     
* input : pressure and mixing ratio
* output : tempmr
* Author:  Dale Perry
***************************************************************************/
void FTN_MANGLE (temp_mixratio) ( float * press,
                                  float * mixratio, float * tempmr )
    {
    float e = (*press * *mixratio)/(0.622 + *mixratio);
    float b = 26.66082 - log(e);
    *tempmr = (b - sqrt(b*b - 223.1986))/0.0182758048;
    return;
    }
