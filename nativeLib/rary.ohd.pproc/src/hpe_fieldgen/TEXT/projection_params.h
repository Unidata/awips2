/*******************************************************************************
 * FILENAME:    projection_params.h
 *
 * DESCRIPTION: This file contains static parameters
 *              for the HPN projection algorithm
 *
 * ORIGINAL AUTHOR:  Guoxian Zhou
 * CREATION DATE:    January  2008
 * ORGANIZATION:     HSEB / OHD
 * MACHINE:          HP-UX / Dell-Redhat Linux
 * MODIFICATION HISTORY:
 *   DATE         PROGRAMMER        DESCRIPTION/REASON
 *
 ********************************************************************************
 */

#ifndef PROJECTION_PARAMS_H_
#define PROJECTION_PARAMS_H_

typedef struct _projection_params_struct
{
    double resetTime; // hours

    double projectionInterval; // hours

    double normalGridSize; // km

    double errorProportionFactor;

    // NOMINAL SCAN INTERVAL (HRS)
    double nominalScanInterval;

    // MINIMUM THRESHOLD PRECIP RATE (MM)
    double minThresholdPrecipRate;

    // MINIMUM NUMBER OF SAMPLES
    int minSampleNumber;

    // Max. allowable missing period of radar data (hr) for reset
    double maxMissingperiodOfRadar;

    // PDF min. area threshold (km^2)
    double minPDFAreaThreshold;

    // PDF min. rainrate threshold (dBR)(modified to rainrate)
    double minPDFRainrateThreshold;

    // Account for growth/decay?
    int isGrowth;

    // Rainrate temporal smoothing method (0=none, 1=FFP, 2=BZ94)
    //int smoothingMethod;

    // Storm motion vectors used (1=local, 2=uniform hrly-avg)
    int stormMotionVector;

    // lamda for rate smooth
    double lamda;

    // kappa for rate smooth
    double kappa;

} projection_params_struct;

#endif /*PROJECTION_PARAMS_H_*/

