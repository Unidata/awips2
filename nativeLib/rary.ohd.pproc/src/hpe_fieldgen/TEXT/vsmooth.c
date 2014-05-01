/**********************************************************************
 * FILENAME:  vsmooth.c
 *
 * Purpose:
 c   this subroutine smooths either the u or v velocities using the mean of nearest
 c   3x3 neighbor velocities.  do not do smoothing for gridpoints where the
 c   velocities are below the difmax threshold defined previously in qcvect.f
 *
 * calling function: speed
 * functions called: none
 *  
 * input variables
 *
 * output variables
 *
 * MODIFICATION HISTORY:
 *   DATE          PROGRAMMER        DESCRIPTION/REASON
 *   Mar., 29 1988 Paul Tilles       Initial version
 *   Apr., 15 1988 Paul Tilles       Added tests on temp array
 *   Jan.,    2004 R. Fulton         Changed averaging method to being
 *                                   weighted by difmax using 3x3 neighbors 
 *   Sep.,    2006 S. Guan           Finish conversion to C Language
 *   Jan., 23 2008 Guoxian Zhou      Finish first operational version
 *   Mar., 12 2008 Guoxian Zhou      finish code review
 *
 ***********************************************************************/
#include "empe_fieldgen.h"

void vsmooth(const int rowSize, const int colSize, double ** temp,
        int ** goodvel, double ** difmax, double maxdifmax, double ** velocity)
{
    double sum, ratio;
    int i, i2, ii, ii2, iof, j, j2, jj, jj2, jof, denom;

    /*
     * initialize output velocity array to missing
     */

    fill(velocity, rowSize, colSize, VELOCITY_MISSING);

    ii = -1;

    for (i = 4; i < rowSize - 5; i += 5)
    {
        ii++;
        jj = -1;
        for (j = 4; j < colSize - 5; j += 5)
        {
            jj++;

            /*
             * only compute velocity average if the center gridpoint is
             * within range of the radar and it has a non-missing velocity
             * or a velocity above the difmax threshold
             * (goodvel(i,j)=t only in these cases)
             * 
             * Do not need to check the radar range for HPE Mosaic products.
             * -- gzhou
             * 
             */

            //if(ibins[i][j] == 1 && goodvel[i][j] != 0) 
            if (goodvel[i][j] != 0)
            {
                sum = 0.0;
                denom = 0;

                for (iof = -5; iof <= 5; iof += 5)
                {
                    i2 = i + iof;
                    ii2 = ii + iof/5;

                    if (i2 < 0 || i2 >= rowSize - 5)
                    {
                        continue;
                    }

                    for (jof = -5; jof <= 5; jof += 5)
                    {
                        j2 = j + jof;
                        jj2 = jj + jof / 5;

                        if (j2 < 0 || j2 >= colSize - 5)
                        {
                            continue;
                        }

                        /*
                         * don't include gridpoints in the average that
                         * are outside the radar umbrella or those that
                         * are either missing or below the difmax threshold
                         * 
                         * Do not need to check the radar range
                         * for HPE Mosaic products.
                         * -- gzhou
                         */

                        //if(ibins[i2][j2] != 1) continue;

                        if (goodvel[i2][j2] != 0)
                        {
                            ratio = difmax[ii2][jj2] / maxdifmax;
                            sum += temp[i2][j2] * ratio;
                            denom ++;
                        }
                    }
                }

                /*
                 * compute weighted mean for this grid point
                 */
                if (denom > 0)
                {
                    velocity[i][j] = sum / denom;
                }
            } else
            {
                velocity[i][j] = temp[i][j];
            }
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/vsmooth.c,v $";
 static char rcs_id2[] = "$Id: vsmooth.c,v 1.2 2008/05/14 19:03:16 gzhou Exp $";}
/*  ===================================================  */

}
