/***********************************************************************
 * Filename: move.c
 *
 * Original Author: 
 *
 * File Creation Date: 
 *
 * Development Group: HSEB/OHD
 *
 * Description:
 * This function moves residuals in the old array to xnew array
 *     with offsets specified by velocityX * hourDiff (x direction) and
 *     velocityY * hourDiff (y direction)
 * 
 * Modules:
 * move
 *
 ***********************************************************************/

/* Include files, definitions, globals go here. */

#include <math.h>
#include "empe_fieldgen.h"

#define  DEFAULT_MEAN   -99.0

/***********************************************************************
 * Module Name: move
 *
 * Original Author: 
 *
 * Module Creation Date: 
 * 
 * Description:
 * This function moves residuals in the old array to xnew array
 *     with offsets specified by velocityX * hourDiff (x direction) and
 *     velocityY * hourDiff (y direction)
 *
 * calling function: runProjection
 * functions called: 
 *
 * Calling Arguments:
 * Name         Input/Output Type             Description
 *
 * pGeoData     Input        geo_data_struct* global HRAP lowerleft-corner
 *                                            bin and dimension and dimension
 *                                            of the RFC estimation domain
 * hourDiff     Input        const double     the time difference from
 *                                            the previous record
 * pPrevResidual Input       double **        previous rainrate residual
 * velocityX    Input        double **        velocity array every box
 * velocityY    Input        double **        velocity array every box
 * ibins        Input        int **           bin array for radar coverage
 * pPoints      Output       int **           the number of points used to
 *                                            estimate each grid; npts( ) = 0
 *                                            implies a hole in new array
 * pMovedResidual Output     double **        moved rainrate residual
 * 
 * Required
 * None
 *
 * Required Files/Databases:
 * None
 *
 * Non System Routines Called:
 * 
 *
 * Return Value:
 * Type          Description
 * None
 *
 * Error Codes/Exceptions:
 * 
 *
 * OS Specific Assumptions:
 * None
 *
 * Local Variables:
 * Name     Type       Description
 *
 *
 * MODIFICATION HISTORY:
 *   DATE          PROGRAMMER        DESCRIPTION/REASON
 *   Jul,     1987 Paul Tilles       Original FORTRAN code
 *   Apr. 15  1988 Paul Tilles       Added test on xnew
 *   Aug. 31, 2006 Shucai Guan       finish conversion to C Language
 *   Jan. 28  2008 Guoxian Zhou      Finish first operational version
 *
 ***********************************************************************/

void move(const geo_data_struct * pGeoData, const double hourDiff,
        double ** pPrevResidual, double ** velocityX, double ** velocityY,
        int ** ibins, double ** pMovedResidual, int ** pPoints)
{
    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;

    int i, j, ii, jj, n, ik, jk;

    //	fill npts with zeroes
    //	fill(pPoints,0);

    //	fill xnew with missing value
    //	fill(pMovedResidual,999.99);

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            if (ibins[i][j] == 0)
            {
                continue;
            }

            ii = i + floor(velocityX[i][j] * hourDiff + 0.5);
            jj = j + floor(velocityY[i][j] * hourDiff + 0.5);

            /*
             * tests for itarg and jtarg in range of rowSize x colSize
             */

            if (ii < 0 || ii >= rowSize || jj < 0 || jj >= colSize)
            {
                continue;
            }

            n = pPoints[ii][jj];
            pPoints[ii][jj] = n + 1;

            if (n == 0)
            {
                /*
                 * only one value is moved to target box
                 */

                pMovedResidual[ii][jj] = pPrevResidual[i][j];
            } else
            {
                /*
                 * more than one value is moved to target box; average them
                 */

                if (pMovedResidual[ii][jj] > 999.9)
                {
                    pMovedResidual[ii][jj] = 0.0;
                }

                pMovedResidual[ii][jj] = (n * pMovedResidual[ii][jj]
                        + pPrevResidual[i][j]) / (n + 1.0);
            }
        }
    }

    /*
     * initialize boxes outside of the 230 km radius in case the
     * target box is beyond that range
     */

    for (ik = 0; ik < rowSize; ik++)
    {
        for (jk = 0; jk < colSize; jk++)
        {
            if (ibins[ik][jk] == 0)
            {
                pMovedResidual[ik][jk] = pPrevResidual[0][0];
            }
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/move.c,v $";
 static char rcs_id2[] = "$Id: move.c,v 1.2 2008/05/14 18:58:46 gzhou Exp $";}
/*  ===================================================  */

}
