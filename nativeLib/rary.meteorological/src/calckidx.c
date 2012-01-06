#include <stdio.h>
# include "meteoLib.h"
/***************************************************************************
* Calculate the K index
* input : press, temp, td, numOfLevel
* output : K
* Harry Chen
***************************************************************************/
void FTN_MANGLE (calckidx) ( float * press, float * temp,
                             float * td, int numOfLevel, float * K )
    {
    float temp850 = 0;
    float temp700 = 0;
    float temp500 = 0;
    float td850 = 0;
    float td700 = 0;
    float td500 = 0;
    int i;
    
    *K = 999;
    if ( press[0] < 820 || press[numOfLevel-1] > 500 )
        return;
/*
** Determine 850 mb, 700mb, 500mb temperature and dew point from sounding.
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
        else if ( press[i] == 700 )
            {
            temp700 = temp[i];
            td700 = td[i];
            } 
        else if ( press[i] < 700 && temp700 == 0 )
            {
            interp ( press[i-1], press[i], temp[i-1], temp[i], td[i-1], td[i],
                     700, &temp700, &td700 );
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
    *K = temp850 + td850 - temp500 - ( temp700 - td700 );
    *K = *K - 273.15;
    return;
    }
            
