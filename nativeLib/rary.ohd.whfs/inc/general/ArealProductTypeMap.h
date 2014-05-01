#ifndef AREAL_PRODUCT_TYPE_MAP
#define  AREAL_PRODUCT_TYPE_MAP

#include "ArealProductType.h"



typedef enum _ArealDisplayMode
{
   PRECIP_MODE,
   FFG_MODE,
   COMPARISON_MODE,
   NUM_AREAL_DISPLAY_MODES
   
} ArealDisplayMode;


typedef enum _PrecipType
{
    STAGE1_PRECIP,
    STAGE2_GAGE_ONLY_PRECIP,
    STAGE2_GAGE_RADAR_PRECIP,
    PRECIP_GRID_SEPARATOR,
    POINT_GAGE_PRECIP,
    PRECIP_FCST_SEPARATOR,
    QPF_PRECIP,
    PRECIP_NA,
    NUM_PRECIP_TYPES

} PrecipType;


typedef enum _ResolutionLevel
{
   GRID_RES,
   COUNTY_RES,
   ZONE_RES,
   BASIN_RES,
   NUM_RESOLUTION_LEVELS
   
} ResolutionLevel;


typedef struct ArealProductTypeMap
{
   
  ArealProductType productType;
  
  ArealDisplayMode displayMode;
  PrecipType precipType;
  ResolutionLevel resLevel;
   
} ArealProductTypeMap;

/*****************************************************************************/

const ArealProductTypeMap * getArealProductTypeMap();

ArealProductType determineProductType(ArealDisplayMode displayMode,
				      PrecipType precipType,
				      ResolutionLevel resLevel);

void determineSettingsFromProductType(ArealProductType productType,
				      ArealDisplayMode *displayMode,
				      PrecipType *precipType,
				      ResolutionLevel *resLevel);

#endif
