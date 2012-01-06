/**********************************************************************
 * FILENAME:    timesmooth.c
 *
 * Purpose:
 * This function averages the velocity vector component from the current and previous
 *    mosaics. If there is a valid vector in only one of the two mosaics, then use it as 
 *    is only if it is in the current mosaic.
 *
 * calling function: speed
 * functions called: none
 *
 * input variables
 *   rowSize
 *   colSize
 *   previousVelocity
 *   currentVelocity
 *
 * output variables
 *   smoothedVelocity
 *
 * MODIFICATION HISTORY:
 *   DATE          PROGRAMMER        DESCRIPTION/REASON
 *   Sep. 5,  2006 Shucai Guan       finish conversion to C Language
 *   Jan.,    2008 Guoxian Zhou      finish first operational version
 *
 ***********************************************************************/
#include "empe_fieldgen.h"
void timesmooth(const int rowSize, const int colSize,
        double ** previousVelocity, double ** currentVelocity,
        double ** smoothedVelocity)
{
    int i, j;

    fill(smoothedVelocity, rowSize, colSize, VELOCITY_MISSING);

    for (i = 4; i < rowSize; i += 5)
    {
        for (j = 4; j < colSize; j += 5)
        {
            if ((previousVelocity[i][j] < VELOCITY_MISSING)
                    && (currentVelocity[i][j] < VELOCITY_MISSING))
            {
                smoothedVelocity[i][j] = (previousVelocity[i][j]
                        + currentVelocity[i][j]) / 2.0;
            } else
            {
                smoothedVelocity[i][j] = currentVelocity[i][j];
            }
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/timesmooth.c,v $";
 static char rcs_id2[] = "$Id: timesmooth.c,v 1.2 2008/05/14 19:01:10 gzhou Exp $";}
/*  ===================================================  */

}
