#include "meteoLib.h"
/***************************************************************************
* interp the data       
* input : p1, p2, temp1, temp2, td1, td2, levelP
* output : interT, interTd
* Harry Chen
***************************************************************************/
void FTN_MANGLE (interp) ( float p1, float p2, float temp1, 
                           float temp2, float td1, float td2, 
                           float levelP, float * interT, float * interTd )
    {
    *interT = temp1 + ( ( temp2 - temp1 ) * ( levelP - p1 ) / ( p2 - p1 ) );
    *interTd = td1 + ( ( td2 - td1 ) * ( levelP - p1 ) / ( p2 - p1 ) );
    return;
    }
