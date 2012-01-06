#include "meteoLib.h"
/***************************************************************************
* Calculate the total index
* input : press, temp, td, numOfLevel
* output : total
* Harry Chen
***************************************************************************/
void FTN_MANGLE (calctotidx) ( float * press, float * temp,
                               float * td, int numOfLevel, float * total )
    {
    float temp850 = 0;
    float temp500 = 0;
    float td850 = 0;
    float td500 = 0;
    int i;
    
    *total = 999;
    if ( press[0] < 820 || press[numOfLevel-1] > 500 )
        return;
/*
** Determine 850 mb, 500mb temperature and dew point from sounding.
*/
    for ( i=0; i<numOfLevel; i++ )
        {
        if ( press[i] == 850 ) 
            {
            temp850 = temp[i];
            td850 = td[i];
            }
        else if ( press[i] < 850 && temp850 == 0 )
            {
            if ( i == 0 )
                {
                interp ( press[0], press[1], temp[0], temp[1], td[0],
                         td[1], 850, &temp850, &td850 );
               }
            else
                {
                interp ( press[i-1], press[i], temp[i-1], temp[i],
                         td[i-1], td[i], 850, &temp850, &td850 );
                }
            }
        else if ( press[i] == 500 )
            {
            temp500 = temp[i];
            } 
        else if ( press[i] < 500 && temp500 == 0 )
            {
            interp ( press[i-1], press[i], temp[i-1], temp[i], td[i-1], td[i],
                     500, &temp500, &td500 );
            }
        }
    *total = ( td850 - temp500 ) + ( temp850 - temp500 );
    return;
    }
            
