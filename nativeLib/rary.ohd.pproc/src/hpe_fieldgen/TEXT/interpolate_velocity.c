/**********************************************************************
 * FILENAME:   interpolate_Velocity.c
 *
 * Purpose:
 c   this subroutine interpolates within the 5 x 5 grids of
 c     velocity to fill in velocity array.  the weights have
 c     been previously calculated on a 9 x 9 grid.  the
 c     interpolation involves a 1/d**2 scheme.
 c     all missing-valued (999.99) gridpoints are reassigned with a velocity
 c     equal to the mean value so that all gridpoints have a valid non-zero
 c     velocity.
 *
 * calling function: mainprj
 * functions called: none
 *
 * input variables
 c     array()
 c     mean
 c     weight() - via common block
 *
 * output variables
 c     array()
 * MODIFICATION HISTORY:
 *   DATE          PROGRAMMER        DESCRIPTION/REASON
 *   Jul,     1987 Paul Tilles       Initial version
 *   Sep.,    2006 S. Guan           Finish conversion to C Language
 *   Jan.,    2008 Guoxian Zhou      finish first operational version
 *   Mar.,    2008 Guoxian Zhou      finish code review
 *
 ***********************************************************************/
#include "empe_fieldgen.h"

void interpolateVelocityGrid(double ** array, const int rowSize,
        const int colSize, const double mean)
{
    int i, j, iof, jof, ipos, jpos;

    double ** array2 = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);

    double weight[9][9];

    /*
     * get the normalized weights for interpolation.
     */

    calculateWeight(weight);

    /*
     * set all non-5th boxes to zero so that the interpolation loop
     * will work right
     */

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            array2[i][j] = array[i][j];

            if (array[i][j] > VELOCITY_MISSING)
            {
                array[i][j] = 0.0;
            }
        }
    }

    /*
     * set every missing 5th box to the mean velocity vector
     */

    for (i = 4; i < rowSize - 5; i += 5)
    {
        for (j = 4; j < colSize - 5; j += 5)
        {
            if (array2[i][j] > VELOCITY_MISSING)
            {
                array[i][j] = mean;
            }

            for (iof = 0; iof < 9; iof ++)
            {
                ipos = i + iof - 4;
                for (jof = 0; jof < 9; jof ++)
                {
                    jpos = j + jof - 4;
                    array[ipos][jpos] += array[i][j] * weight[iof][jof];
                }
            }
        }
    }

    free2DDoubleArray(array2, rowSize);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/interpolate_velocity.c,v $";
 static char rcs_id2[] = "$Id: interpolate_velocity.c,v 1.2 2008/05/14 19:01:18 gzhou Exp $";}
/*  ===================================================  */

}
