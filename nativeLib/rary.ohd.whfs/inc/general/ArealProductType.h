#ifndef AREAL_PRODUCT_TYPE_H
#define AREAL_PRODUCT_TYPE_H

typedef enum _ArealProductType
{
   /*
   		GO = GAGE ONLY
		MS = MULTI-SENSOR
   */
   /*
        NO_PRODUCT,
   
        NONE_MAP_SEP,
   */
   
        BASIN_MAP_GO,
        BASIN_MAP_MS,
	COUNTY_MAP_GO,
        COUNTY_MAP_MS,
	ZONE_MAP_GO,
        ZONE_MAP_MS,

	/*
	MAP_FFG_SEP,
	*/
	
	
	BASIN_FFG,
	COUNTY_FFG,
	ZONE_FFG,
	
	/*
	FFG_STAGE_SEP,
	*/
	
	STAGE_I_GRID,
   	STAGE_II_GRID_GO,
	STAGE_II_GRID_MS,
   
        NUM_AREAL_PRODUCT_TYPES,
   
        NON_PRODUCT
} ArealProductType;


int isBoundedArealProduct(ArealProductType productType);
int isGriddedArealProduct(ArealProductType productType);

int isFfgProduct(ArealProductType productType);
int isMapProduct(ArealProductType productType);

int isMultiSensorMapProduct(ArealProductType productType);
int isGageOnlyMapProduct(ArealProductType productType);

int isBasinProduct(ArealProductType productType);
int isCountyProduct(ArealProductType productType);
int isZoneProduct(ArealProductType productType);



#endif
