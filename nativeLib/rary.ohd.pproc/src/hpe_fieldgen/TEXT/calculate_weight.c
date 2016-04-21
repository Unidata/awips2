/**********************************************************************
 * FILENAME:            calculate_weight.c
 *
 * Purpose:
 * This function calculates the weights for interpolation.
 * Weights are normalized to 1.
 *
 * calling function: interpolateVelocityGrid
 * functions called: none
 *
 * input variables
 *
 * none
 * 
 * output variables
 *
 * weight   --   9 * 9 normalized weight array
 * 
 * MODIFICATION HISTORY:
 *   DATE         PROGRAMMER        DESCRIPTION/REASON
 *   Jul,    1987 Paul Tilles       Original FORTRAN code  
 *   Aug. 4, 2006 Shucai Guan       finish conversion to C Language 
 *   Jan.,   2008 Guoxian Zhou      finish first operational version
 *
 ***********************************************************************/

#include <stdlib.h>
#include "empe_fieldgen.h"

void calculateWeight(double weight[9][9])
{
    double work[5][5];
    double d1, d2, d3, d4;
    int i, j;

    /*
     * calculate weights on 5 x 5 grid
     */

    for (i = 2; i < 6; i++)
    {
        for (j = 2; j < 6; j++)
        {
            d1 = (i - 1) * (i - 1) + (j - 1) * (j - 1);
            d2 = (i - 1) * (i - 1) + (j - 6) * (j - 6);
            d3 = (i - 6) * (i - 6) + (j - 1) * (j - 1);
            d4 = (i - 6) * (i - 6) + (j - 6) * (j - 6);
            work[i-1][j-1] = (1/d1)/(1/d1 + 1/d2 + 1/d3 + 1/d4);
        }
    }

    work[0][0] = 0.0;

    for (i = 2; i < 6; i++)
    {
        d1 = (i - 1) * (i - 1);
        d2 = (i - 6) * (i - 6);
        work[0][i-1] = (1/d1) / (1/d1 + 1/d2);
        work[i-1][0] = work[0][i-1];
    }

    /*
     * extrapolate above to 9 x 9 grid
     */

    for (i = 0; i < 9; i++)
    {
        for (j = 0; j < 9; j++)
        {
            weight[i][j] = work[abs(i-4)][abs(j-4)];
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/calculate_weight.c,v $";
 static char rcs_id2[] = "$Id: calculate_weight.c,v 1.2 2008/05/14 19:00:02 gzhou Exp $";}
/*  ===================================================  */

}
