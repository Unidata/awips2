/***********************************************************************
 * Filename: run_projection.c
 *
 * Original Author: 
 *
 * File Creation Date: 
 *
 * Development Group: HSEB/OHD
 *
 * Description:
 * flash flood projection algorithm main subprogram.
 * 
 * Modules:
 * runProjection
 *
 ***********************************************************************/

/* Include files, definitions, globals go here. */

#include "empe_fieldgen.h"

/***********************************************************************
 * Module Name: runProjection
 *
 * Original Author: 
 *
 * Module Creation Date: 
 * 
 * Description:
 *   This function runs the main flash flood projection algorithm.
 *
 * calling function: runNowcast
 * functions called: fldstat, speed, move, setProjectionParam, project
 *
 * Calling Arguments:
 * Name         Input/Output Type             Description
 *
 * tRunTime     Input        time_t           run time 
 * pGeoData     Input        geo_data_struct* global HRAP lowerleft-corner
 *                                            bin and dimension and dimension
 *                                            of the RFC estimation domain
 * mosaicID     Input        char *           the mosaic type 
 * pEMPEParams  Input        empe_params_struct*
 *                                            static parameters
 * pProjectionParams
 *              Input        projection_params_struct *
 *                                            static projection parameters
 * pInputDir    Input        char *           the directory of mosaic xmrg files
 * timeDiff     Input        int              the time difference from
 *                                            the previous record
 * currMosaic   Input        double **        current DHR/BDHR mosaic product
 * prevMosaic   Input        double **        previous DHR/BDHR mosaic product
 *
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
 *   Jul. 1987     Paul Tilles       initial version    
 *   Aug. 16, 2006 Shucai Guan       finish conversion to C Language
 *   Mar., 2008    Guoxian Zhou      finish first operational version
 *
 ***********************************************************************/

#include "empe_fieldgen.h"

void runProjection(const geo_data_struct * pGeoData, const time_t tRunTime,
        const char * mosaicID, const empe_params_struct * pEMPEParams,
        const projection_params_struct * pProjectionParams, const int timeDiff,
        double ** currMosaic, double ** prevMosaic, const int radar_data_source)
{
    const int rowSize = pGeoData->num_rows;
    const int colSize = pGeoData->num_cols;
    const int rowSize5 = rowSize / 5 - 1;
    const int colSize5 = colSize / 5 - 1;

    int ** ibins = init2DIntArray(ZERO_CONSTANT, rowSize, colSize);

    double ** pMosaic = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);

    double ** curr33Mean = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);
    double ** prev33Mean = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);

    double ** curr55Mean = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);
    double ** prev55Mean = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);

    double ** currResidual = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);
    double ** prevResidual = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);
    double ** movedPrevResidual = init2DDoubleArray(999.99, rowSize, colSize);

    double ** currErrVar = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);
    double ** prevErrVar = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);

    int ** pPoints = init2DIntArray(ZERO_CONSTANT, rowSize, colSize);
    double ** growth = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);

    double ** velocityX = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);
    double ** velocityY = init2DDoubleArray(ZERO_CONSTANT, rowSize, colSize);

    double ** difmax = init2DDoubleArray(ZERO_CONSTANT, rowSize5, colSize5);

    double projectionParam;
    double residualVariance;

    double maxdifmax;

    double thrdifmax1 = 0.7;
    
 /* 
  * limit maximum precipitation rate to 16 inches per hour so projected rates 
  * don't get too large, especially if user turns on growth flag
  */ 
    
    double mxpra = 406.4; 
    
    int i, j;

    double maxspeed = 45.0;
    double delt = pProjectionParams->normalGridSize * 0.621 / maxspeed;

    /*
     * check the current mosaic data and fill in the bin array. 
     */

    for (i = 0; i < rowSize; i++)
    {
        for (j = 0; j < colSize; j++)
        {
            if (currMosaic[i][j] >= 0.0)
            {
                ibins[i][j] = 1;
            } else
            {
                currMosaic[i][j] = 0.0;
            }
        }
    }

    /*
     * calculate field statistics of rainrate array
     * 
     * the totacc array output from fldstat is really an error variance array.
     *
     */

    double hourDiff = 1.0 * timeDiff / 3600;

    fldstat(pGeoData, pProjectionParams->nominalScanInterval, currMosaic,
            pProjectionParams->errorProportionFactor, hourDiff, currResidual,
            currErrVar, curr55Mean, curr33Mean);

    fldstat(pGeoData, pProjectionParams->nominalScanInterval, prevMosaic,
            pProjectionParams->errorProportionFactor, hourDiff, prevResidual,
            prevErrVar, prev55Mean, prev33Mean);

    /*
     *   calculate storm velocity fields every 5th box using mean rainrates
     *   from the current scan and previous scan closest to scanint min.
     *   ago.  note: for the second volume scan processed, storm velocity is
     *   calculated from the first to the second scan regardless of scanint time
     *   difference.
     */

    speed(rowSize, colSize, mosaicID, tRunTime,
            pProjectionParams->stormMotionVector, hourDiff, ibins, prev33Mean,
            curr33Mean, thrdifmax1, difmax, &maxdifmax, velocityX, velocityY,
            growth);

    /*
     * move the old rate residuals using the computed speeds (temp1 is the
     * output array holding them)
     */

    move(pGeoData, hourDiff, prevResidual, velocityX, velocityY, ibins,
            movedPrevResidual, pPoints);

    /*
     * calculate projection parameter and residual variance
     * 
     * note that the variable delt...delta time used in computing forecast
     * accumulations...depends on
     *  1) maximum expected storm speed and
     *  2) grid spacing (e.g., 4 km hrap grid).
     * for max speed=45 mph and grid size of 4 km, delt=3.3
     * minutes=0.0556 hrs.  for 2 km grid, delt=1.7 mins.
     */

    setProjectionParam(rowSize, colSize, movedPrevResidual, pPoints,
            currResidual, curr55Mean, hourDiff, delt,
            pProjectionParams->minSampleNumber, &projectionParam,
            &residualVariance);

    /*
     * if varres < 0.0 cannot project (too few samples)
     */

    if (residualVariance <= 0.0)
    {
        sprintf(message, "STATUS: residual variance < 0.0...cannot project.");
        printLogMessage(message);
        return;
    }

    /*
     * calculate projection times and projection time counter.
     * project rates and rate residual error variances over the future
     * forecast period.  integrate in time over the future period to get total
     * forecasted rainfall and total forecasted rate residual variance.
     * (note: totacc below is really an error variance array).
     */

    int projStep = (int)(pProjectionParams->projectionInterval/delt + 0.5);

    project(pGeoData, mosaicID, pEMPEParams, tRunTime, pProjectionParams,
            projStep, delt, mxpra, ibins, velocityX, velocityY, growth,
            pPoints, curr55Mean, currResidual, currErrVar, residualVariance,
            projectionParam, radar_data_source);

    free2DIntArray(ibins, rowSize);
    free2DDoubleArray(pMosaic, rowSize);

    free2DDoubleArray(curr33Mean, rowSize);
    free2DDoubleArray(prev33Mean, rowSize);

    free2DDoubleArray(curr55Mean, rowSize);
    free2DDoubleArray(prev55Mean, rowSize);

    free2DDoubleArray(currResidual, rowSize);
    free2DDoubleArray(prevResidual, rowSize);
    free2DDoubleArray(movedPrevResidual, rowSize);

    free2DDoubleArray(currErrVar, rowSize);
    free2DDoubleArray(prevErrVar, rowSize);

    free2DIntArray(pPoints, rowSize);
    free2DDoubleArray(growth, rowSize);

    free2DDoubleArray(velocityX, rowSize);
    free2DDoubleArray(velocityY, rowSize);

    free2DDoubleArray(difmax, rowSize5);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob92/ohd/pproc/src/hpe_fieldgen/RCS/run_projection.c,v $";
 static char rcs_id2[] = "$Id: run_projection.c,v 1.3 2008/05/16 13:16:58 millerd Exp $";}
/*  ===================================================  */

}

