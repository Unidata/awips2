/**********************************************************************
 * FILENAME:        pparm.c
 *
 * Purpose:
 * This function setes the projection parameter by comparing the moved
 *   previous rate residual scans with the current rate residual scan.
 *   there is one value of the projection parameter (pp) and residual 
 *   innovations variance (varres) for the entire array.
 * 
 * calling function: mainprj
 * functions called: none
 *
 * input variables
 *  pPrevResidual is the array of moved rainrate residuals from the previous scan
 *  pPoint is the array of counts of the number of rainrate residual gridbins
 *        ending up in each of the hrap gridboxes after moving
 *  pCurrResidual is the array of current rainrate residuals
 *  pCurrMeanRate is the array of current mean rainrates
 *
 * parameter list with const identifier
 *
 * output variables
 *     pProjectionParam is the projection parameter
 *     pResidualVariance is the variance of the residuals innovation
 *
 * MODIFICATION HISTORY:
 *   DATE          PROGRAMMER        DESCRIPTION/REASON
 *   Jul,     1987 Paul Tilles         Original FORTRAN code
 *   Apr. 15  1988 Paul Tilles         added test on rold
 *   Aug. 31, 2006 Shucai Guan       finish conversion to C Language
 *   Jan. 28  2008 Guoxian Zhou      Finish first operational version
 *
 ***********************************************************************/
#include "empe_fieldgen.h"

void setProjectionParam(const int rowSize, const int colSize,
        double ** pPrevResidual, int ** pPoint, double ** pCurrResidual,
        double ** pCurrMeanRate, double hourDiff, double delt, double minsam,
        double * pProjectionParam, double * pResidualVariance)
{
    double sold, snew, snew2, scross, xnew, pp;

    /*
     * xl0a is the lag 0 autocovariance of the current scan's rainrate
     * residuals (same as the variance of the current rainrate residuals)
     * 
     * xl1a is the lag 1 autocovariance of the current rainrate residual array
     * with the previous scan's rainrate residuals that have been moved
     * by the storm motion to the current scan's time
     * 
     */

    double xl0a, xl1a;
    int nsam, i, j;

    sold = 0.0;
    snew = 0.0;
    snew2 = 0.0;
    scross = 0.0;
    nsam = 0;

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            if (pPoint[i][j] > 0 && pCurrMeanRate[i][j] > 0.0
                    && pPrevResidual[i][j] < 999.9)
            {
                nsam ++;
                sold += pPrevResidual[i][j];
                xnew = pCurrResidual[i][j];
                snew += xnew;
                snew2 += xnew * xnew;
                scross += xnew * pPrevResidual[i][j];
            }
        }
    }

    if (nsam > minsam)
    {
        xl0a = (snew2 / (nsam-1)) - (snew * snew/((nsam-1) * nsam));
        xl1a = (scross / (nsam-1)) - (snew / nsam) * (sold / nsam);

        /*
         * calculates the projection parameter
         */

        if (xl1a < 0.0)
        {
            xl1a = 0.0;
        }

        pp = pow((xl1a / xl0a), (delt / hourDiff));

        if (pp > 1.0)
        {
            pp = 0.99;
        }

        if (pp <= 0.0)
        {
            pp = 0.0001;
        }

        *pResidualVariance = ((1 - pp * pp) * xl0a) * (delt / hourDiff);
    } else
    {
        pp = 1.0;
        *pResidualVariance = -1.0;
    }
    *pProjectionParam = pp;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/pparm.c,v $";
 static char rcs_id2[] = "$Id: pparm.c,v 1.1 2008/05/07 16:40:28 gzhou Exp $";}
/*  ===================================================  */

}
