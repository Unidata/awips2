/***********************************************************************
 * Filename: move2.c
 *
 * Original Author: 
 *
 * File Creation Date: 
 *
 * Development Group: HSEB/OHD
 *
 * Description:
 * This function moves values in the old array to new array
 *  with offsets specified by velocityX * projection-time (x direction) and
 *  velocityY * projection-time (y direction)
 *  projected rates and observed error variance of projected rates
 *      are moved by this subroutine
 * 
 * Modules:
 * move2
 *
 ***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

/***********************************************************************
 * Module Name: move2
 *
 * Original Author: 
 *
 * Module Creation Date: 
 * 
 * Description:
 * This function moves values in the old array to new array
 *  with offsets specified by velocityX * projection-time (x direction) and
 *  velocityY * projection-time (y direction)
 *  projected rates and observed error variance of projected rates
 *      are moved by this subroutine
 *
 * calling function: runProjection
 * functions called: 
 *
 * Calling Arguments:
 * Name         Input/Output Type             Description
 *
 * rowSize      Input        const int        geo row size 
 * colSize      Input        const int        geo column size 
 * pProjectedRate
 *              Input        double **        projected rainrate array
 * pObservedVar Input        double **        current observed error variance
 *                                            of rainrates array
 * pPoints      Input        int **           the number of points used to
 *                                            estimate each grid; npts( ) = 0
 *                                            implies a hole in new array
 * ibins        Input        int **           bin array for radar coverage
 * pVelocityX   Input        double **        velocity array every box
 * pVelocityY   Input        double **        velocity array every box
 * projectionTime
 *              Input        double           projection time period
 *                                            (e.g., 10,20,30,40,50,60 mins)
 * pMovedProjectedRate
 *              Output       double **        moved projected rainrate array.
 * pMovedObservedVar
 *              Output       double **        moved observed error variance
 *                                            of rainrates array
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
 * MODIFICATION HISTORY:
 *    DATE       PROGRAMMER         DESCRIPTION/REASON
 *    Jul,  1987 Paul Tilles        Original FORTRAN code
 *    Oct.  2006 Shucai Guan        finish conversion to C Language
 *  Jan. 29 2008 Guoxian Zhou       Finish first operational version
 * 
 ***********************************************************************/

void move2(const int rowSize, const int colSize, double ** pProjectedRate,
        double ** pObservedVar, int ** pCount, int ** ibins,
        double ** pVelocityX, double ** pVelocityY, double projectionTime,
        double ** pMovedProjectedRate, double ** pMovedObservedVar)
{
    double sum, cnt;

    double ** tmp = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);

    int i, j, itarg, jtarg, num, ii, jj;

    /*
     * fill pCount, pMovedProjectedRate, pMovedObservedVar with zeroes
     */

    fillInt(pCount, rowSize, colSize, 0);
    fill(pMovedProjectedRate, rowSize, colSize, 0.0);
    fill(pMovedObservedVar, rowSize, colSize, 0.0);

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            if (ibins[i][j] == 0)
            {
                continue;
            }

            itarg = i + floor(pVelocityX[i][j] * projectionTime + 0.5);
            jtarg = j + floor(pVelocityY[i][j] * projectionTime + 0.5);

            /*
             * tests for itarg and jtarg in range of 1 to rowSize, ColSize
             */

            if (itarg < 0 || itarg >= rowSize || jtarg < 0 || jtarg >= colSize)
            {
                continue;
            }

            num = pCount[itarg][jtarg];
            pCount[itarg][jtarg] = num + 1;

            /*
             * only one value is moved to target box
             */

            if (num == 0)
            {
                pMovedProjectedRate[itarg][jtarg] = pProjectedRate[i][j];
                pMovedObservedVar[itarg][jtarg] = pObservedVar[i][j];
            } else
            {

                /*
                 * more than one value is moved to target box.  
                 * use max value for rainrates
                 * and average value for rainrate error variances.
                 */

                if (pMovedProjectedRate[itarg][jtarg] > pProjectedRate[i][j])
                {
                    pMovedProjectedRate[itarg][jtarg]
                            = pMovedProjectedRate[itarg][jtarg];
                } else
                {
                    pMovedProjectedRate[itarg][jtarg] = pProjectedRate[i][j];
                }

                pMovedObservedVar[itarg][jtarg] = (num
                        * pMovedObservedVar[itarg][jtarg] + pObservedVar[i][j])
                        / (num + 1);
            }
        }
    }

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            if (pCount[i][j]==0)
            {
                pMovedProjectedRate[i][j]=0;

                pProjectedRate[i][j]=0;
            }
        }
    }

    /*
     * fill holes in the projected/moved rain rate array
     */

    fill_hole(rowSize, colSize, ibins, pMovedProjectedRate);

    /*
     *    initialize boxes which are missing data
     *    fill the empty boxe by its old rain rate if it is not filled by 
     *    the above fill_hole function  
     */

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            tmp[i][j] = pMovedProjectedRate[i][j];
        }
    }

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            if (ibins[i][j] == 0)
            {
                pMovedProjectedRate[i][j] = 0;
                pMovedObservedVar[i][j] = 0;
            }

            if ((ibins[i][j] == 1) && (pMovedProjectedRate[i][j] == 0)
                    && (pCount[i][j] == 0))
            {

                /*
                 * codes from fill_hole.f
                 */

                cnt = 0;
                sum = 0.0;

                for (ii = i - 1; ii <= i + 1; ii++)
                {
                    for (jj= j - 1; jj <= j + 1; jj++)
                    {
                        if ((ii < 0) || (ii >= rowSize))
                        {
                            continue;
                        }

                        if ((jj < 0) || (jj >= colSize))
                        {
                            continue;
                        }

                        if (tmp[ii][jj] > 0)
                        {
                            cnt ++;
                            sum += tmp[ii][jj];
                        }
                    }
                }

                pMovedProjectedRate[i][j] = (pProjectedRate[i][j] + sum) / (cnt
                        + 1);
            }
        }
    }
    free2DDoubleArray(tmp, rowSize);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/move2.c,v $";
 static char rcs_id2[] = "$Id: move2.c,v 1.3 2008/05/14 18:58:51 gzhou Exp $";}
/*  ===================================================  */

}
