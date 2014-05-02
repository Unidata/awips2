#ifndef HPN_H_
#define HPN_H_
#include <float.h>
#include <time.h>

#include "empe_data_structs.h"
#include "empe_params.h"
#include "projection_params.h"
#include "Velocity.h"

#define VELOCITY_MISSING    999.99

extern int useFakeMean;
extern double fakeUMean;
extern double fakeVMean;

void fldstat(const geo_data_struct * pGeoData, const double scnint,
        double ** pMosaic, double errprp, const double hourDiff,
        double ** pResidual, double ** pErrorVariance, double ** avg55RainRate,
        double ** avg33RainRate);

void readProjectionParams(projection_params_struct * pProjectionParams);

void getPreviousFilename(const char * targetDir, const time_t targetTime,
        int * timeDiff, char * outputFilename);

void freeProjectionParams();

void fill(double ** array, const int rowSize, const int colSize,
        const double value);

void
        fillInt(int ** array, const int rowSize, const int colSize,
                const int value);

void predetfun(const int rowSize, const int colSize,
        const double minPDFRainrateThreshold, const double minPDFAreaThreshold,
        double ** pMosaic, int *flgzhs);

void calculateWeight(double weight[9][9]);

void vsmooth(const int rowSize, const int colSize, double ** temp,
        int ** goodvel, double ** difmax, double maxdifmax, double ** velocity);

void timesmooth(const int rowSize, const int colSize,
        double ** previousVelocity, double ** currentVelocity,
        double ** smoothedVelocity);

void interpolateVelocityGrid(double ** array, const int rowSize,
        const int colSize, const double mean);

void fillHole(const int rowSize, const int colSize, const int index_i,
        const int index_j, double ** pData, int ** pCount, double * x,
        int *isFilled);

void fill_hole(const int rowSize, const int colSize, int ** ibins,
        double ** anew);

void move(const geo_data_struct * pGeoData, const double hourDiff,
        double ** pPrevResidual, double ** velocityX, double ** velocityY,
        int ** ibins, double ** pMovedResidual, int ** pPoints);

void move2(const int rowSize, const int colSize, double ** pProjectedRate,
        double ** pObservedVar, int ** pCount, int ** ibins,
        double ** pVelocityX, double ** pVelocityY, double projectionTime,
        double ** pMovedProjectedRate, double ** pMovedObservedVar);

void getMeanVelocity(const char * mosaicid, const time_t currTimeT,
        const int search_window, Velocity * pMeanVelocity, int * status,
        int * count);

void qcvect(const int rowSize, const int colSize, const char * mosaicID,
        const time_t currTime, const int flgvector, double ** u, double ** v,
        double ** difmax, double maxdifmax, double thrdifmax1, double *umean3,
        double *vmean3, int ** goodvel);

void ratesmooth(const int rowSize, const int colSize, const int growthflg,
        const double projectTimePeriod, const double maxProjectedRate,
        const double lamda, const double kappa, int ** ibins,
        double ** pMeanRate, double ** pResidual, double ** growth2,
        double ** pProjectedRate);

void setProjectionParam(const int rowSize, const int colSize,
        double ** pPrevResidual, int ** pPoint, double ** pCurrResidual,
        double ** pCurrMeanRate, double hourDiff, double delt, double minsam,
        double * pProjectionParam, double *pResidualVariance);

void speed(const int rowSize, const int colSize, const char * mosaicID,
        const time_t currTimeT, const int flgvector, const double hourDiff,
        int ** ibins, double ** prevMeanRate, double ** currMeanRate,
        double thrdifmax1, double ** difmax, double * pmaxdifmax,
        double ** smoothedVelocityX, double ** smoothedVelocityY,
        double ** growth);

void project(const geo_data_struct * pGeoData, const char * mosaicID,
        const empe_params_struct * pEMPEParams, const time_t tRunTime,
        const projection_params_struct * pProjectionParams,
        const int projectionStep, const double delt,
        const double maxProjectedRate, int ** ibins, double ** velocityX,
        double ** velocityY, double ** growth, int ** pCount, double ** rmean,
        double ** resid, double ** pObservedVar, const double varres,
        double projParam, const int radar_data_source);

void runProjection(const geo_data_struct * pGeoData, const time_t tRunTime,
        const char * mosaicID, const empe_params_struct * pEMPEParams,
        const projection_params_struct * pProjectionParams, const int timeDiff,
        double ** currMosaic, double ** prevMosaic, const int radar_data_source);

void runNowcast(const geo_data_struct * pGeoData, const time_t tRunTime,
        const char * mosaicID, const empe_params_struct * pEMPEParams,
        const char * pInputDir, double ** currMosaic, const int radar_data_source);

#endif /*HPN_H_*/
