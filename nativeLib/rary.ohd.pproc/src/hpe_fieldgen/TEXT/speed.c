/***********************************************************************
 * Filename: speed.c
 *
 * Original Author: 
 *
 * File Creation Date: 
 *
 * Development Group: HSEB/OHD
 *
 * Description:
 * This function calculates the velocities of the rates on a box
 * by box basis.  the velocities are calculated at every fifth
 * box, quality controlled, spatially smoothed using nearest neighbor
 * velocities, and then temporally smoothed with previous scan's vectors.
 * the other non-fifth velocities are calculated by interpolation.
 * the velocity units are in [hrap boxes/hour]. 
 * 
 * Modules:
 * speed
 *
 ***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"
#define  DEFAULT_MEAN   -99.0

/***********************************************************************
 * Module Name: speed
 *
 * Original Author: 
 *
 * Module Creation Date: 
 * 
 * Description:
 * This function calculates the velocities of the rates on a box
 * by box basis.  the velocities are calculated at every fifth
 * box, quality controlled, spatially smoothed using nearest neighbor
 * velocities, and then temporally smoothed with previous scan's vectors.
 * the other non-fifth velocities are calculated by interpolation.
 * the velocity units are in [hrap boxes/hour]. 
 *
 * calling function: runProjection
 * functions called: 
 *
 * Calling Arguments:
 * Name         Input/Output Type             Description
 *
 * rowSize      Input        const int        geo row size 
 * colSize      Input        const int        geo column size 
 * mosaicID     Input        const char *     the mosaic type 
 * currTimeT    Input        const time_t     current run time 
 * hourDiff     Input        const double     the time difference from
 *                                            the previous record
 * currMosaic   Input        double **        current DHR/BDHR mosaic product
 * currMeanRate Input        double **        current mean rain rate
 * prevMeanRate Input        double **        previous mean rain rate
 * smoothedVelocityX
 *              Output       double **        smoothed velocity array every box
 * smoothedVelocityY
 *              Output       double **        smoothed velocity array every box
 * growth       Output       double **        growth/decay rates every fifth box (mm/hr)
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
 * DATE             PROGRAMMER          DESCRIPTION/REASON
 * Jul,      1987 Paul Tilles           Initial version
 * Apr. 15  1988 Paul Tilles            tempx,tempy filled with 999.99
 * Nov.      2000 R. Fulton             changes to min. thresholds for rnew,
 *                                      changes to use of normalized x instead
 *                                      of x in computation of vectors
 * Feb.      2001 R. Fulton             updated computation of growth array to
 *                                      include only raining bins
 * Sep.      2003 R. Fulton             added temporal smoothing of vectors and
 *                                      computation of storm-relative vectors 
 * Sep. 5,  2006 S. Guan                Finish conversion to C Language
 * Feb.25,  2008 Guoxian Zhou           Finish first operational version
 * Apr.17,  2008 Guoxian Zhou           Finish code review.
 * 
 ***********************************************************************/

void speed(const int rowSize, const int colSize, const char * mosaicID,
        const time_t currTimeT, const int flgvector, const double hourDiff,
        int ** ibins, double ** prevMeanRate, double ** currMeanRate,
        double thrdifmax1, double ** difmax, double * pmaxdifmax,
        double ** smoothedVelocityX, double ** smoothedVelocityY,
        double ** growth)
{

    int mxoff = 5;
    double minrr = 0.01;
    double minvx, maxvx, minvy, maxvy;
    int minnbox = (int)(0.20 * (mxoff * 2 + 1) * (mxoff * 2 + 1));

    double spdrawsum, spdsmosum, dirrawsum, dirsmosum, delta, delt1,
            mnrdiffmin, meancondrr, growthsum, growthavg, umean, vmean;
    int i, j, i5, j5, nplus, iof, jof, ii1, ii2, jj1, jj2;

    int growthcnt;

    double ** currSmoothedVelocityX = init2DDoubleArray(ZERO_CONSTANT, rowSize,
            colSize);
    double ** currSmoothedVelocityY = init2DDoubleArray(ZERO_CONSTANT, rowSize,
            colSize);
    double ** guessVelocityX = init2DDoubleArray(ZERO_CONSTANT, rowSize,
            colSize);
    double ** guessVelocityY = init2DDoubleArray(ZERO_CONSTANT, rowSize,
            colSize);
    double x, x3, x4;
    int count, count3, count4;
    int iipof, jjpof;
    double xmin, xmin0, xmax;
    int cntspd, cntdirraw, cntdirsmo;
    int ii, jj, n, m;
    int iminvx, jminvx, iminvy, jminvy, imaxvx, jmaxvx, imaxvy, jmaxvy;
    double maxdifmax;

    maxdifmax = *pmaxdifmax;
    int imin = 0;
    int jmin = 0;

    double ** mnrabsdiff =
            init2DDoubleArray(VELOCITY_MISSING, rowSize, colSize);
    double ** mnrabsdiff2 = init2DDoubleArray(VELOCITY_MISSING, rowSize,
            colSize);

    int ** goodvel = init2DIntArray(VELOCITY_MISSING, rowSize, colSize);

    *pmaxdifmax = -99.0;
    growthsum = 0.0;
    growthcnt = 0;

    /*
     * for loop is executed nx5 x ny5 times, 
     * once for each 5th hrap box
     */

    i5 = -1;
    for (i = 4; i < rowSize - 5; i += 5)
    {
        i5++;
        j5 = -1;
        for (j = 4; j < colSize - 5; j += 5)
        {
            j5++;
            difmax[i5][j5] = -1.0;

            if (ibins[i][j] != 1)
            {
                continue;
            }

            /*
             * check to make sure you don't try to compute velocities
             * too close to the max. radar range
             */

            nplus = 0;
            for (iof = i - mxoff; iof <= i + mxoff; iof ++)
            {
                for (jof = j - mxoff; jof <= j + mxoff; jof ++)
                {
                    if ((iof < 0) || (iof >= rowSize))
                    {
                        continue;
                    }

                    if ((jof < 0) || (jof >= colSize))
                    {
                        continue;
                    }

                    if (currMeanRate[iof][jof] > minrr)
                    {
                        nplus ++;
                    }
                }
            }

            /*
             * need more than 20% of (mxoff*2+1)^2 (=121 for mxoff=5) boxes
             * to be raining in current scan to compute velocity at a grid
             * point (heavy number crunching only done in areas
             * where it's raining).
             */

            if (nplus <= minnbox)
            {
                continue;
            }

            ii1 = i - mxoff;
            ii2 = i + mxoff;
            jj1 = j - mxoff;
            jj2 = j + mxoff;

            xmin = DBL_MAX;
            xmin0 = DBL_MAX;
            mnrdiffmin = DBL_MAX;
            meancondrr = 0.0;
            xmax = DBL_MIN;

            /*
             * the followingloop is executed (mxoff*2+1)^2 times,
             * once for each hrap box within +-mxoff boxes of the
             * current gridpoint (e.g., 81 times for mxoff=4)
             */

            for (iof = -1 * mxoff; iof <= mxoff; iof++)
            {
                for (jof = -1 * mxoff; jof <= mxoff; jof++)
                {
                    x = 0.0;
                    x3 = 0.0;
                    x4 = 0.0;
                    count = 0;
                    count3 = 0;
                    count4 = 0;

                    /*
                     * loop is executed (mxoff*2+1)^2 times for each of
                     * the (mxoff*2+1)^2 offsets.
                     * sum up the absolute difference over a
                     * (mxoff*2+1)x(mxoff*2+1) grid between
                     * the current rate array at zero offset
                     * and the previous rate array at the current offset
                     */

                    for (ii = ii1; ii <= ii2; ii++)
                    {
                        iipof = ii + iof;

                        /*
                         * don't step outside of the arrays
                         */
                        if (iipof < 0 || iipof >= rowSize)
                        {
                            continue;
                        }
                        if (ii < 0 || ii >= rowSize)
                        {
                            continue;
                        }
                        for (jj = jj1; jj <= jj2; jj++)
                        {
                            jjpof = jj + jof;

                            if (jjpof < 0 || jjpof >= colSize)
                            {
                                continue;
                            }

                            if (jj < 0 || jj >= colSize)
                            {
                                continue;
                            }

                            if (ibins[iipof][jjpof] != 1)
                            {
                                continue;
                            }

                            delt1 = currMeanRate[ii][jj]
                                    - prevMeanRate[iipof][jjpof];

                            /*
                             * sum up conditional rainrates and differences
                             * over the current box of size (2*mxoff+1)
                             * on a side.  only include the gridcell
                             * if at least one of the pixels in the previous
                             * and current image is raining.
                             */

                            if (!((prevMeanRate[iipof][jjpof] < minrr)
                                    && (currMeanRate[ii][jj] < minrr)))
                            {
                                x3 += delt1;
                                count3 ++;
                                x4 += currMeanRate[ii][jj];
                                count4 ++;
                            }

                            delta = fabs(delt1);
                            x += delta;
                            count ++;

                        }
                    }

                    /*
                     * only save into the array if within 2 gridpts of center
                     * to prevent overlap with adjacent 5th-hrap gridpoints
                     */

                    if (abs(iof) <= 2 && abs(jof) <= 2)
                    {
                        if (count != 0)
                        {
                            mnrabsdiff[i+iof][j+jof] = x / count;
                            mnrabsdiff2[i+iof][j+jof] = x / count;
                        } else
                        {
                            mnrabsdiff[i+iof][j+jof] = 0.0;
                            mnrabsdiff2[i+iof][j+jof] = 0.0;
                        }
                    }

                    if (count != 0)
                    {
                        if (iof == 0 && jof == 0)
                        {
                            xmin0 = x / count;
                        }

                        if (x/count < xmin)
                        {
                            xmin = x / count;

                            /*
                             * compute the areal mean of conditional rainrate
                             * and growth for the current offset
                             */

                            if (count3 != 0)
                            {
                                mnrdiffmin = x3 / count3;
                            } else
                            {
                                mnrdiffmin = 0.0;
                            }

                            if (count4 != 0)
                            {
                                meancondrr = x4/count4;
                            } else
                            {
                                meancondrr = 0.0;
                            }

                            imin = -iof;
                            jmin = -jof;
                        }
                        if (x / count > xmax)
                        {
                            xmax = x / count;
                        }
                    }
                }
            }

            if (xmin < xmin0)
            {
                guessVelocityX[i][j] = (double)(imin / hourDiff);
                guessVelocityY[i][j] = (double)(jmin / hourDiff);
            } else
            {
                guessVelocityX[i][j] = 0.0;
                guessVelocityY[i][j] = 0.0;
            }

            difmax[i5][j5] = xmax - xmin;
            if (difmax[i5][j5] > *pmaxdifmax)
            {
                *pmaxdifmax = difmax[i5][j5];
            }
        }
    }

    /*
     * quality control the raw u and v vector components (every 5th box)
     * using the difmax array.  guessVelocityX and guessVelocityY
     * are used as input and output.
     */

    qcvect(rowSize, colSize, mosaicID, currTimeT, flgvector, guessVelocityX,
            guessVelocityY, difmax, *pmaxdifmax, thrdifmax1, &umean, &vmean,
            goodvel);

    /*
     * spatially smooth the current u and v vector components (every 5th box)
     */

    vsmooth(rowSize, colSize, guessVelocityX, goodvel, difmax, *pmaxdifmax,
            currSmoothedVelocityX);

    vsmooth(rowSize, colSize, guessVelocityY, goodvel, difmax, *pmaxdifmax,
            currSmoothedVelocityY);

    /*
     * temporally smooth the u and v vector components (every 5th box)
     * for this scan and the previous scan assuming there are vectors
     * available from the previous scan.  do not permit the averaging
     * of vectors from current scan and scans other than the immediately
     * previous scan.  in this case just use vectors from the current scan.
     * note that vectors are smoothed even though the current and/or
     * previous scan's vectors may not have been used to produce projections
     * (for example due to maxdifmax less than the minimum threshold).
     * 
     * no temporally smooth -- gzhou 03/2008
     */

    for (i = 4; i < rowSize - rowSize % 5; i+=5)
    {
        for (j = 4; j < colSize - colSize % 5; j+=5)
        {
            smoothedVelocityX[i][j] = currSmoothedVelocityX[i][j];
            smoothedVelocityY[i][j] = currSmoothedVelocityY[i][j];
        }
    }

    /*
     * interpolate the smoothed velocities every 5th hrap box to every box.
     * all missing-valued (999.99) velocity gridpoints are reassigned to
     * the mean value computed previously.
     */

    interpolateVelocityGrid(smoothedVelocityX, rowSize, colSize, umean);
    interpolateVelocityGrid(smoothedVelocityY, rowSize, colSize, vmean);

    /*
     * compute growth
     */

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            ii = floor(smoothedVelocityX[i][j] * hourDiff + 0.5);
            jj = floor(smoothedVelocityY[i][j] * hourDiff + 0.5);

            if (((i-ii) < 0) || ((i-ii) >= rowSize))
            {
                continue;
            }

            if (((j-jj) < 0) || ((j-jj) >= colSize))
            {
                continue;
            }

            if (ibins[i][j] != 1)
            {
                continue;
            }

            if (ibins[i-ii][j-jj] != 1)
            {
                continue;
            }

            /*
             * divide by delta scan time (hrs)
             * since growth is in units of mm/hr
             */

            growth[i][j] = (currMeanRate[i][j] - prevMeanRate[i-ii][j-jj])
                    / hourDiff;
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/speed.c,v $";
 static char rcs_id2[] = "$Id: speed.c,v 1.1 2008/05/07 16:39:53 gzhou Exp $";}
/*  ===================================================  */

}
