/**********************************************************************
 * FILENAME:        fillHole.c
 *
 * Purpose:
 * This function fills the holes in the projected accum
 *      and the projected variance arrays with the local average
 *      of the quantities in the 8 surrounding bins.  this only occurs
 *      if five or more of the surrounding 8 bins has data in it, otherwise
 *      the hole value is assigned a value of 0 (edges are not extended).
 *
 * calling function: main
 * functions called: none
 *
 * input variables
 *      i, j = the coordinates of the gridpoint that needs filling
 *      xar = the array that is being filled
 *      npts = the array of the number of data values in each hrap box
 *
 * parameter list with const identifier
 *
 * output variables
 *      x = the interpolated grid value 
 *      filflg = binary flag indicating if the hole was filled or not 
 *
 * MODIFICATION HISTORY:
 *    DATE             PROGRAMMER          DESCRIPTION/REASON
 *    Jul,      1987 Paul Tilles            Original FORTRAN code
 *    Nov.      2000 R. Fulton              added filflg
 *    Aug. 29, 2006 Shucai Guan         finish conversion to C Language
 *   Jan. 28  2008 Guoxian Zhou      Finish first operational version
 *
 ***********************************************************************/
#include "empe_fieldgen.h"

void fillHole(const int rowSize, const int colSize, const int index_i,
        const int index_j, double ** pData, int ** pCount, double * x,
        int *isFilled)
{
    double n;
    int i, j;

    *x = 0.0;
    n = 0;

    /*
     * sum the eight gridpoints surrounding the hole if there is data there
     */

    for (i = index_i - 1; i <= index_i + 1; i++)
        for (j = index_j - 1; j <= index_j + 1; j++)
        {
            if ((i < 0) || (i >= rowSize))
            {
                continue;
            }

            if ((j < 0) || (j >= colSize))
            {
                continue;
            }

            if (pCount[i][j] > 0)
            {
                *x += pData[i][j];
                n ++;
            }
        }

    if (n < 5)
    {
        *isFilled = 0;
    } else
    {
        *isFilled = 1;
        *x /= n;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/fillHole.c,v $";
 static char rcs_id2[] = "$Id: fillHole.c,v 1.2 2008/05/14 19:00:24 gzhou Exp $";}
/*  ===================================================  */

}
