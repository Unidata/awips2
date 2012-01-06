#ifndef AREAL_DATA_ATTR_H
#define AREAL_DATA_ATTR_H

/* this .h file has no single parent .c file. 
   it accomodates the need for these definitions in both
   the accessdb/man code (read_radargrids) and much
   of the ffmutil type stuff.  its presences prevents
   problems from doing clean rebuilds without a need
   for cyclical references to the include files.
   (note there are not cyclical dependecies between
   the functions in the two libraries) */

/* enumerated variables ------------------------ */

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
   QPF_PRECIP,
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

typedef enum _ArealDataStatus
   {
   MISSING_STATUS,
   ALL_ZERO,
   NON_ZERO,
   NOT_APPLICABLE
   } ArealDataStatus;



#endif
