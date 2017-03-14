/**********************************************************************
 * FILENAME:            fill.c
 *
 * Purpose:
 * This function sets all elements of an array to the same value
 *
 * calling function: main
 * functions called: none
 *
 * input variables
 *
 * parameter list with const identifier
 *
 * output variables
 *
 * MODIFICATION HISTORY:
 *   DATE          PROGRAMMER        DESCRIPTION/REASON
 *   Jul,     1987 Paul Tilles       Original FORTRAN code
 *   Aug. 29, 2006 Shucai Guan       finish conversion to C Language
 *   Jan.,    2008 Guoxian Zhou      finish first operational version
 *
 ***********************************************************************/
#include "hpn.h"

void fill(double ** array, const int rowSize, const int colSize,
        const double value)
{
    int i, j;
    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            array[i][j] = value;
        }
    }
}

void fillInt(int ** array, const int rowSize, const int colSize, const int value)
{
    int i, j;
    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            array[i][j] = value;
        }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/fill.c,v $";
 static char rcs_id2[] = "$Id: fill.c,v 1.2 2008/05/14 19:00:27 gzhou Exp $";}
/*  ===================================================  */

}
