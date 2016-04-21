/**********************************************************************
 * FILENAME: 	fill_hole.c
 *
 * Purpose:
 * This function fills in holes in the input array.
 *
 * calling function: mainprj
 * functions called: none
 *
 * input variables
 *
 * parameter list with const identifier
 *
 * output variables
 *
 * MODIFICATION HISTORY:
 *	DATE 			PROGRAMMER  		DESCRIPTION/REASON
 *	Jul,  	2005 Shucai Guan			Original FORTRAN code
 *	Aug. 29, 2006 Shucai Guan 		finish conversion to C Language
 *
 ***********************************************************************/
#include "empe_fieldgen.h"

void fill_hole(const int rowSize, const int colSize, int ** ibins,
        double ** anew)
{
    /*
     *  fill in holes (defined as zero-valued pixels) in the input array using
     *  the 8 neighbors.  if 4 or more of the 8 neighbors have valid (non-zero)
     *  values, then replace the center point with the average of the non-zero values.
     *
     *  input variables
     * 	anew
     *
     *  output variables
     * 	anew
     */

    int i, j, ii, jj, count;
    double sum;

    double ** old = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            old[i][j] = anew[i][j];
        }
    }

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            if (ibins[i][j] == 1 && old[i][j] == 0)
            {
                count = 0;
                sum = 0.0;
                for (ii = i-1; ii <= i+1; ii++)
                {
                    for (jj = j-1; jj <= j+1; jj++)
                    {
                        if ((ii < 0) || (ii >= rowSize))
                        {
                            continue;
                        }

                        if ((jj < 0) || (jj >= colSize))
                        {
                            continue;
                        }

                        if (ibins[ii][jj] == 1 && old[ii][jj] != 0.0)
                        {
                            count ++;
                            sum += old[ii][jj];
                        }
                    }
                }

                if (count >= 4)
                {
                    anew[i][j] = sum / count;
                }
            }
        }
    }

    free2DDoubleArray(old, rowSize);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/fill_hole.c,v $";
 static char rcs_id2[] = "$Id: fill_hole.c,v 1.2 2008/05/14 19:00:30 gzhou Exp $";}
/*  ===================================================  */

}
