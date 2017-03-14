
#include "ArealProductTypeMap.h"


/***************************************************************************/

const ArealProductTypeMap * getArealProductTypeMap()
{
     static ArealProductTypeMap map[NUM_AREAL_PRODUCT_TYPES] =   
     {
	  {BASIN_MAP_GO,  PRECIP_MODE, STAGE2_GAGE_ONLY_PRECIP, BASIN_RES}, 
	  {BASIN_MAP_MS,  PRECIP_MODE, STAGE2_GAGE_RADAR_PRECIP, BASIN_RES},
	  
	  {COUNTY_MAP_GO, PRECIP_MODE, STAGE2_GAGE_ONLY_PRECIP, COUNTY_RES}, 
	  {COUNTY_MAP_MS, PRECIP_MODE, STAGE2_GAGE_RADAR_PRECIP, COUNTY_RES},
	  
	  {ZONE_MAP_GO,   PRECIP_MODE, STAGE2_GAGE_ONLY_PRECIP, ZONE_RES}, 
	  {ZONE_MAP_MS,   PRECIP_MODE, STAGE2_GAGE_RADAR_PRECIP, ZONE_RES},
	  
	  {STAGE_I_GRID,     PRECIP_MODE, STAGE1_PRECIP, GRID_RES},
	  {STAGE_II_GRID_GO, PRECIP_MODE, STAGE2_GAGE_ONLY_PRECIP, GRID_RES},
	  {STAGE_II_GRID_MS, PRECIP_MODE, STAGE2_GAGE_RADAR_PRECIP, GRID_RES},
	
	  {BASIN_FFG,     FFG_MODE, PRECIP_NA, BASIN_RES},
          {COUNTY_FFG,    FFG_MODE, PRECIP_NA, COUNTY_RES},
          {ZONE_FFG,      FFG_MODE, PRECIP_NA, ZONE_RES}
     };
   
     return map;
}

/***************************************************************************/

ArealProductType determineProductType(ArealDisplayMode displayMode,
				      PrecipType precipType,
				      ResolutionLevel resLevel
				      )
{
     
     ArealProductType productType = NON_PRODUCT;   
     const ArealProductTypeMap * map = getArealProductTypeMap ( ) ;
   
     int i;
  
     /*
          find the matching ArealProductType
     
     */
     if (displayMode == FFG_MODE)
     {
          precipType = PRECIP_NA;	  
     }
     
     for (i = 0; i < NUM_AREAL_PRODUCT_TYPES; i++)
     {
	   if (  (displayMode == map[i].displayMode) &&
	         (precipType == map[i].precipType) &&
		 (resLevel == map[i].resLevel)
	      )
	   {
		productType = map[i].productType;
	        break;	
	   }
	  
     }
     
   
     return productType;
}

/***********************************************************************/
void determineSettingsFromProductType(ArealProductType productType,
				      ArealDisplayMode *displayMode,
				      PrecipType *precipType,
				      ResolutionLevel *resLevel)
{
   
     const ArealProductTypeMap * map = getArealProductTypeMap ( ) ;
     int i;
   
     for (i = 0; i < NUM_AREAL_PRODUCT_TYPES; i++)
     {
	   if (productType == map[i].productType) 
	   {
		*displayMode = map[i].displayMode;
		*precipType = map[i].precipType;
		*resLevel = map[i].resLevel;
		
	        break;	
	   }
	  
     }
   
   
     return;   
}

/***********************************************************************/
