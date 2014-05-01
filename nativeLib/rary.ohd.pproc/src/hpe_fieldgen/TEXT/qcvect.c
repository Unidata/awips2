/**********************************************************************
 * FILENAME:  qcvect.c
 *
 * Purpose:
 *
 * quality control the storm motion vectors using the reliability information
 * (difmax array) computed previously.  compute the space-averaged vector for
 * the current scan for all vectors in which the difmax value is above a threshold
 * value computed as a percentage of the maximum array value. this mean vector for the
 * current scan is computed using high-difmax local vectors weighted by value of difmax
 * of each vector (high difmax vectors get more weight in computing the mean storm
 * motion vector).  then compute a simple mean over the last hour using all mean
 * vectors from past scans in which the number of local vectors for that
 * scan were above some minimum threshold.  then reassign the u and v components for
 * the low-difmax storm vectors in the current scan to be equal to the mean values
 * for the good vectors over the past hour.
 *
 * calling function: speed
 * functions called: none
 *
 * input variables
 *
 *   u (input and output)
 *   v (input and output)
 *   difmax
 *   maxdifmax
 *   vector (common)
 *   time
 *   date
 *   flgvector (1 to use local vectors, 2 to use uniform hrly-avg vectors)
 *   thrdifmax1
 *
 * output variables
 *
 *   u (input and output)
 *   v (input and output)
 *   vector (common)
 *   umean3
 *   vmean3
 *
 * MODIFICATION HISTORY:
 *   DATE          PROGRAMMER        DESCRIPTION/REASON
 *   Feb.     2001 R. Fulton         initial version
 *   May      2003 R. Fulton         added logic to compute time-averaged vectors
 *   May      2003 R. Fulton         added logic to use time-averaged vectors uniformly 
 *                                   for all storms if flag is set
 *   Sep.,    2006 S. Guan           Finish conversion to C Language
 *
 ***********************************************************************/
#include "empe_fieldgen.h"

/*
 * need at least this many good vectors in the current scan to use this scan's
 * mean vector in computing the hourly-averaged vector in the low-difmax areas
 */
#define MIN_GOOD_VECTORS 5

/*
 * need at least this many scans in the last hour with enough storm motion
 * vectors to use the hourly-averaged vector in the low-difmax areas
 */
#define MIN_SCAN_NUMBER 2

void qcvect(const int rowSize, const int colSize, const char * mosaicID,
        const time_t currTimeT, const int flgvector, double ** velocityX,
        double ** velicotyY, double ** difmax, double maxdifmax,
        double thrdifmax1, double *umean3, double *vmean3, int ** goodvel)
{
    int i, j, ii, jj, count;
    double thrdifmax2, difmaxthr, usum, vsum, ratio, denom, umean, vmean,
            uvmean, uvdir, timedif;

    double ** uraw = init2DDoubleArray(VELOCITY_MISSING, rowSize, colSize);
    double ** vraw = init2DDoubleArray(VELOCITY_MISSING, rowSize, colSize);

    Velocity * pVelocity= NULL;
    Velocity * pMeanVelocity= NULL;
    const int searchTime = 3600; // search for previous one hour velocity record

    dtime_t dtCurrent;
    int status;

    pVelocity = (Velocity *) malloc(sizeof(Velocity));

    if (pVelocity == NULL)
    {
        sprintf(message, "ERROR: memory allocation failure"
            " in qcvect function."
            "\n\tProgram exit.") ;
        shutdown(message);
    }

    pMeanVelocity = (Velocity *) malloc(sizeof(Velocity));

    if (pMeanVelocity == NULL)
    {
        sprintf(message, "ERROR: memory allocation failure"
            " in qcvect function."
            "\n\tProgram exit.") ;
        shutdown(message);
    }

    /*
     * more vectors are discarded as thrdifmax2 increases
     */

    thrdifmax2 = 0.4;

    difmaxthr = maxdifmax * thrdifmax2;

    usum = 0.0;
    vsum = 0.0;
    denom = 0.0;
    count = 0;

    /*
     * copy the input velocity arrays to temporary working names.
     * initialize goodvel.
     */

    for (i = 0; i < rowSize; i ++)
    {
        for (j = 0; j < colSize; j ++)
        {
            uraw[i][j] = velocityX[i][j];
            vraw[i][j] = velicotyY[i][j];
            goodvel[i][j] = 0;
        }
    }

    ii = -1;
    for (i = 4; i < rowSize - 5; i += 5)
    {
        ii++;
        jj = -1;
        for (j = 4; j < colSize - 5; j += 5)
        {
            jj = jj + 1;
            if (uraw[i][j] < VELOCITY_MISSING) // if uraw=999.99, so is vraw
            {
                if (difmax[ii][jj] <= difmaxthr)
                {
                    velocityX[i][j] = 888.88;
                    velicotyY[i][j] = 888.88;
                } else
                {
                    ratio = difmax[ii][jj] / maxdifmax;
                    usum += velocityX[i][j] * ratio;
                    vsum += velicotyY[i][j] * ratio;
                    denom += ratio;
                    count ++;
                    goodvel[i][j] = 1;
                }
            }
        }
    }

    /*
     * compute the mean u and v weighted by the normalized difmax value for this
     * current volume scan
     */

    if (denom != 0)
    {
        umean = usum / denom;
        vmean = vsum / denom;
    } else
    {
        umean = 0.0;
        vmean = 0.0;
    }

    /*
     * compute speed in [mph] assuming 1 hrap box is 4.0 km
     */

    uvmean = sqrt(umean * umean + vmean * vmean) * 2.49;
    if (count != 0)
    {
        uvdir = 270.0 - (atan2(vmean, umean) * 180.0 / 3.1415927);
    } else
    {
        uvdir = 0.0;
    }

    if (uvdir >= 360.0)
    {
        uvdir -= 360.0;
    }

    /*
     * the array "vector" has 5 columns: current u, current v, time, date, count.
     * rows get older in time as row index gets larger (row 1 is latest).
     * store the current vector to be used later in computing the mean hourly storm
     * vector as long as maxdifmax is large enough and as long as the count is
     * large enough
     * 
     * need save the record to DB.
     * items:
     * mosaicID, datetime, umean, vmean, count
     * -- guoxian 03-03-2008
     *  
     */

    if (maxdifmax > thrdifmax1)
    {
        if (count > MIN_GOOD_VECTORS)
        {
            /*
             * save the velocity to the database.
             * gzhou 02-06-2008
             */

            timet_to_yearsec_dt(currTimeT, &dtCurrent);

            strcpy(pVelocity->mosaicid, mosaicID);
            pVelocity->createtime = dtCurrent;
            pVelocity->count = count;
            pVelocity->umean = umean;
            pVelocity->vmean = vmean;

            InsertOrUpdateVelocity(pVelocity);
        }
    }

    /*
     * compute time-averaged vector using all good vectors within
     * the last 1 hour weighted by the number of vectors in each hour
     */

    count = 0;

    getMeanVelocity(mosaicID, currTimeT, searchTime, pMeanVelocity, &status,
            &count);

    /*
     * either replace only the low-difmax vectors with the time- and
     * space-averaged values or replace all vectors
     * with them depending on value of input flag.  as a last resort, 
     * use the current scan's mean vector if there were not enough vectors in
     * the past hour.
     */

    for (i = 4; i < rowSize - 5; i += 5)
    {
        for (j = 4; j < colSize - 5; j += 5)
        {
            if (flgvector == 2)
            {
                /*
                 * replace all local vectors with the hourly-averaged vector
                 * (or current scan's mean vector if not enough past scans)
                 */

                if (velocityX[i][j] < VELOCITY_MISSING)
                {
                    if (count >= MIN_SCAN_NUMBER)
                    {
                        velocityX[i][j] = pMeanVelocity->umean;
                        velicotyY[i][j] = pMeanVelocity->vmean;
                    } else
                    {
                        velocityX[i][j] = umean;
                        velicotyY[i][j] = vmean;
                    }
                }
            } else
            {
                /*
                 * replace only low-difmax local vectors with
                 * the hourly-averaged vector (or current scan's
                 * mean vector if not enough past scans)
                 */

                if ((velocityX[i][j] > 888.8) && (velocityX[i][j] < 888.9))
                {
                    if (count >= MIN_SCAN_NUMBER)
                    {
                        velocityX[i][j] = pMeanVelocity->umean;
                        velicotyY[i][j] = pMeanVelocity->vmean;
                    } else
                    {
                        velocityX[i][j] = umean;
                        velicotyY[i][j] = vmean;
                    }
                }
            }
        }
    }

    if (count >= MIN_SCAN_NUMBER)
    {
        *umean3 = pMeanVelocity->umean;
        *vmean3 = pMeanVelocity->vmean;
    } else
    {
        *umean3 = umean;
        *vmean3 = vmean;
    }

    if (pVelocity != NULL)
    {
        free(pVelocity);
        pVelocity = NULL;
    }

    if (pMeanVelocity != NULL)
    {
        free(pMeanVelocity);
        pMeanVelocity = NULL;
    }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/qcvect.c,v $";
 static char rcs_id2[] = "$Id: qcvect.c,v 1.3 2008/12/04 21:41:51 gzhou Exp $";}
/*  ===================================================  */

}

