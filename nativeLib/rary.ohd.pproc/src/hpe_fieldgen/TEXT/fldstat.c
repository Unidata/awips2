/***********************************************************************
 * Filename: fldstat.c
 *
 * Original Author: 
 *
 * File Creation Date: 
 *
 * Development Group: HSEB/OHD
 *
 * Description:
 * This function calculates the mean precip rate, residual,
 *     error variance and running total of error variance
 * 
 * Modules:
 * fldstat
 *
 ***********************************************************************/

/* Include files, definitions, globals go here. */

#include <math.h>
#include "empe_fieldgen.h"

#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))

/***********************************************************************
 * Module Name: fldstat
 *
 * Original Author: 
 *
 * Module Creation Date: 
 * 
 * Description:
 * This function calculates the mean precip rate, residual,
 *     error variance and running total of error variance
 *
 * calling function: runNowcast
 * functions called: fldstat, speed, move, setProjectionParam, project
 *
 * Calling Arguments:
 * Name         Input/Output Type             Description
 *
 * pGeoData     Input        geo_data_struct* global HRAP lowerleft-corner
 *                                            bin and dimension and dimension
 *                                            of the RFC estimation domain
 * scanInterval Input        const double     nominal scan interval (hr)
 * hourDiff     Input        const double     the time difference from
 *                                            the previous record
 * pMosaic      Input        double **        DHR/BDHR mosaic product
 * pResidual    Input        double **        residual of mosaic product
 * pErrorVariance
 *              Input        double **        error variance of the raw rate
 *                                            array computed over 5x5 windows
 *                                            (units of (mm/hr)^2)
 * avg55RainRate
 *              Input        double **        5 * 5 mean rain rate array
 * avg33RainRate
 *              Input        double **        3 * 3 mean rain rate array
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
 *   DATE          PROGRAMMER        DESCRIPTION/REASON
 *   Jul,     1987 Paul Tilles         Original FORTRAN code
 *   Aug. 30, 2006 Shucai Guan       finish conversion to C Language
 *   Jan., 24 2008 Guoxian Zhou      Finish first operational version
 *   Apr., 23 2008 Guoxian Zhou      final operational version
 *
 ***********************************************************************/

void fldstat(const geo_data_struct * pGeoData, const double scanInterval,
        double ** pMosaic, double errprp, const double hourDiff,
        double ** pResidual, double ** pErrorVariance, double ** avg55RainRate,
        double ** avg33RainRate)
{
    double minrate, maxrate, minresid, maxresid, minerrvar, maxerrvar;
    int i, j, ii, jj, ist, iend, jst, jend, n, ist3, iend3, jst3, jend3, nn;
    double x, xx, x2;

    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;

    minrate = FLT_MAX;
    maxrate = FLT_MIN;
    minresid = FLT_MAX;
    maxresid = FLT_MIN;
    minerrvar = FLT_MAX;
    maxerrvar = FLT_MIN;

    /*
     * do for all rowSize x colSize boxes
     */

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            ist = max(0, i - 2);
            iend = min(rowSize, i + 3);
            jst = max(0, j - 2);
            jend = min(colSize, j + 3);
            n = (iend - ist) * (jend - jst);

            ist3 = max(0, i - 1);
            iend3 = min(rowSize, i + 2);
            jst3 = max(0, j - 1);
            jend3 = min(colSize, j + 2);
            nn = (iend3 - ist3) * (jend3 - jst3);

            x = 0.0;
            xx = 0.0;
            x2 = 0.0;

            /*
             * for this box, compute 5x5 sum of rates and rates^2
             */

            for (ii = ist; ii < iend; ii++)
            {
                for (jj = jst; jj < jend; jj++)
                {
                    if (pMosaic[ii][jj] < 0)
                    {
                        continue;
                    }
                    x += pMosaic[ii][jj];
                    x2 += pMosaic[ii][jj] * pMosaic[ii][jj];
                }
            }

            if (n > 0)
            {
                avg55RainRate[i][j] = x / n;
                pResidual[i][j] = pMosaic[i][j] - avg55RainRate[i][j];
            }

            /*
             * for this box, compute 3x3 sum of rates
             */

            for (ii = ist3; ii < iend3; ii++)
            {
                for (jj = jst3; jj < jend3; jj++)
                {
                    if (pMosaic[ii][jj] < 0)
                    {
                        continue;
                    }
                    xx += pMosaic[ii][jj];
                }
            }

            if (nn > 0)
            {
                avg33RainRate[i][j] = xx/nn;
            }

            if (n > 1)
            {
                pErrorVariance[i][j] = (x2 / (n-1) - x * x/(n * (n - 1)))
                        * errprp;
            }

            if (pMosaic[i][j] < minrate)
            {
                minrate = pMosaic[i][j];
            }

            if (pMosaic[i][j] > maxrate)
            {
                maxrate = pMosaic[i][j];
            }

            if (pResidual[i][j] < minresid)
            {
                minresid = pResidual[i][j];
            }

            if (pResidual[i][j] > maxresid)
            {
                maxresid = pResidual[i][j];
            }

            if (pErrorVariance[i][j] < minerrvar)
            {
                minerrvar = pErrorVariance[i][j];
            }

            if (pErrorVariance[i][j] > maxerrvar)
            {
                maxerrvar = pErrorVariance[i][j];
            }
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/fldstat.c,v $";
 static char rcs_id2[] = "$Id: fldstat.c,v 1.1 2008/05/07 16:39:58 gzhou Exp $";}
/*  ===================================================  */

}
