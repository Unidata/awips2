/***********************************************************************
 * Filename: predetfun.c
 *
 * Original Author: Guoxian Zhou
 *
 * File Creation Date: 01/13/2008
 *
 * Development Group: HSEB/OHD
 *
 * Description:
 * nowcast based on the DHR mosaic data
 * 
 * Modules:
 * predetfun
 *
 ***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

/***********************************************************************
 * Module Name: predetfun
 *
 * Original Author: Guoxian Zhou
 *
 * Module Creation Date: 02/19/2008
 * 
 * Description:
 *  compute the area of echo whose intensity is greater than an input
 *  threshold and compare it with an input echo area minimum threshold.
 *  set the flag-zero-hybrid-scan if the area exceeds the threshold.
 *
 * calling function: run_nowcast
 * functions called: none
 *
 * Calling Arguments:
 * Name         Input/Output Type             Description
 *
 * rowSize     Input         const int        row size of geo data
 * colSize     Input         const int        column size of geo data
 * minPDFRainrateThreshold
 *             Input         const double     minimum PDF rainrate threshold
 * minPDFAreaThreshold
 *             Input         const double     minimum PDF areal threshold
 * pMosaic     Input         double **        mosaic array
 *
 * flgzhs      Output        int *            flag of the zero hybrid scan 
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
 * Modification History:
 * Date        Developer     Action
 * Sep.,2006   S. Guan       Finish conversion to C Language
 * 2/19/2008   Guoxian Zhou  Build operational version 
 *
 ***********************************************************************/

#include "empe_fieldgen.h"

void predetfun(const int rowSize, const int colSize,
        const double minPDFRainrateThreshold, const double minPDFAreaThreshold,
        double ** pMosaic, int * flgzhs)
{
    double area;

    int i, j;

    area = 0.0;

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            if (pMosaic[i][j] > minPDFRainrateThreshold)
            {
                area += 16; // for resolution = 4 km
            }
        }
    }

    if (area > minPDFAreaThreshold)
    {
        *flgzhs = 0;
    } else
    {
        *flgzhs = 1;
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/predetfun.c,v $";
 static char rcs_id2[] = "$Id: predetfun.c,v 1.2 2008/05/14 19:00:17 gzhou Exp $";}
/*  ===================================================  */

}
