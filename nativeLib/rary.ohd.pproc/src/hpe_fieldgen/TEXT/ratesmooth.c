/**********************************************************************
 * FILENAME:    ratesmooth.c
 *
 * Purpose:
 *  spatially smooths the current raw rainrate array according to the power law
 *  l=kappa*t^lamda  where l=length of one side of a square box (km), t is the forecast
 *  lead time (minutes), and kappa and lambda are parameters.  this is the bellon and
 *  zawadski (1994) method.  increasing smoothing is performed as the flt increases.
 *  account for growth/decay if necessary.
 *
 * calling function: project
 * functions called: none
 *
 * input variables
 c     rmean
 c     resid
 c     prtim (seconds)
 c     growthflg
 c     growth2 array (mm/hr/hr)
 c     mxpra
 *
 * output variables
 *     pProjectedRate
 *
 * MODIFICATION HISTORY:
 *    DATE             PROGRAMMER          DESCRIPTION/REASON
 *    Apr,      2004 R. Fulton            Initial version
 *    Sep.,     2006 S. Guan              Finish conversion to C Language
 *   Jan., 31   2008   Guoxian Zhou       Finish first operational version
 *   Apr., 11   2008   Guoxian Zhou       move lamda and kappa to constant file
 *
 ***********************************************************************/
#include "empe_fieldgen.h"

void ratesmooth(const int rowSize, const int colSize, const int growthflg,
        const double projectTimePeriod, const double maxProjectedRate,
        const double lamda, const double kappa, int ** ibins,
        double ** pMeanRate, double ** pResidual, double ** growth2,
        double ** pProjectedRate)
{
    double len, sum, growthamt;
    int i, j, ii, jj, idel, jdel, count, lenhrap, off1, off2;

    /*
     * compute l for this forecast lead time (assume hrap box is nominally
     * 4 km on a side) and find the closest whole integer number of hrap
     * boxes that fit into that length (odd or even)
     */

    sprintf(message, "STATUS: lamda = %lf, kappa = %lf", lamda, kappa);
    printMessage(message);

    len = kappa * pow(projectTimePeriod * 60.0, lamda);
    lenhrap = (int) floor(len/4.0 + 0.5);

    if (lenhrap % 2 == 0) //even value
    {
        /*
         * for even averaging, alternate the offsets for consecutive
         * timesteps to prevent a consistent bias in the location
         * of the echoes
         */

        if (len / 4.0 < lenhrap)
        {
            off1 = -1 * (lenhrap / 2 - 1);
            off2 = lenhrap / 2;
        } else
        {
            off1 = -1 * lenhrap / 2;
            off2 = lenhrap / 2 - 1;
        }
    } else //odd value
    {
        off1 = -1 * (lenhrap - 1) / 2;
        off2 = (lenhrap - 1) / 2;
    }

    /*
     *  loop over all pixels within radar range and
     *  compute spatial moving averages
     *  of raw rainrate field
     */

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            if (ibins[i][j] == 0)
            {
                continue;
            }

            count = 0;
            sum = 0.0;
            for (idel = off1; idel <= off2; idel++)
            {
                ii = i + idel;
                for (jdel = off1; jdel <= off2; jdel++)
                {
                    jj = j + jdel;
                    if ((ii < 0) || (ii >= rowSize))
                    {
                        continue;
                    }

                    if ((jj < 0) || (jj >= colSize))
                    {
                        continue;
                    }

                    if (ibins[ii][jj] == 1)
                    {
                        sum += pMeanRate[ii][jj] + pResidual[ii][jj];
                        count ++;
                    }
                }
            }

            if (growthflg != 0)
            {
                growthamt = growth2[i][j] * projectTimePeriod;
            } else
            {
                growthamt = 0.0;
            }

            pProjectedRate[i][j] = sum / count + growthamt;

            /*
             * constrain projected rainrates to fall within (0,mxpra)
             * in case growth or decay accounting causes unrealistic rates
             */

            if (pProjectedRate[i][j] < 0.0)
            {
                pProjectedRate[i][j] = 0.0;
            }

            if (pProjectedRate[i][j] > maxProjectedRate)
            {
                pProjectedRate[i][j] = maxProjectedRate;
            }

            if (sum <= 0.0)
            {
                pProjectedRate[i][j] = 0.0;
            }
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/ratesmooth.c,v $";
 static char rcs_id2[] = "$Id: ratesmooth.c,v 1.2 2008/05/14 18:59:58 gzhou Exp $";}
/*  ===================================================  */

}
