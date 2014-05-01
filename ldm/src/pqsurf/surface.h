/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: surface.h,v 1.6 1995/10/19 23:16:08 davis Exp $	*/
#ifndef _LDMSURFACE_H_
#define _LDMSURFACE_H_

#include "dtime.h"
#if 0
#include "awsmsc.h"
#else
#define CALL_SIGN_LEN 7
#endif

/*
 * Qsubc, Code table 3333
 */
typedef enum {
	QUADRANT_UNAVAIL = -1 ,
	QUADRANT_NE = 1 ,
	QUADRANT_SE = 3 ,
	QUADRANT_SW = 5 ,
	QUADRANT_NW = 7 
} quadrant_t  ;

/*
 * isubw, Code table 1855
 */
typedef enum {
	WIND_UNAVAIL = -1 ,
	MPS_EST = 0 ,	/* metres per second, estimated */
	MPS = 1 ,	/* metres per second */
	KNOTS_EST = 3 ,	/* knots, estimted */
	KNOTS = 4	/* knots	*/
} wind_units_t  ;


/*
 * isubr, Code table 1819
 */
typedef enum {
	SEC1	= 1 ,
	SEC3	= 2 ,
	PRECIP_NONE	= 3 , 	/* no precip */
	PRECIP_UNAVAIL = 4 
} precip_ind_t  ;


/*
 * Code table 1860
 */
typedef enum {
	STATIONT_UNAVAIL = -1 ,
	MANNED , 	/*  0 < isubx < 4 */
	AUTOMATIC	/* 3 < isubx < 7 */
} 	stationt_t ;

typedef enum {	/* isubx or (isub - 3) */
	G7_YES = 1 ,	
	G7_NONE = 2 ,
	G7_UNAVAIL = 3 
} g7_avail_t ;

/*
 * h, Code table 1600
 */
typedef enum {
	CLOUDBASE_UNAVAIL = -1 , /* '/' */
	M0_50 ,
	M50_100 ,
	M100_200 ,
	M200_300 ,
	M300_600 ,
	M600_1000 ,
	M1000_1500 ,
	M1500_2000 ,
	M2000_2500 ,
	M2500_
} cloudbase_range_t ;


/*
 * N, Code table 2700
 */
typedef enum {
	CLOUDCOVER_UNAVAIL = -1 , /* '/' */
	CLEAR ,
	CLOUDCOVER_ONE_OCTA ,
	CLOUDCOVER_TWO_OCTA ,
	CLOUDCOVER_THREE_OCTA ,
	CLOUDCOVER_FOUR_OCTA ,
	CLOUDCOVER_FIVE_OCTA ,
	CLOUDCOVER_SIX_OCTA ,
	CLOUDCOVER_SEVEN_OCTA ,
	OVERCAST ,
	FOG
} cloudcover_t ;


/*
 * a, Code table 0200
 */
typedef enum {
	PRESSURE_TENDENCY_UNAVAIL = -1 , /* '/' */
	PRESSURE_TENDENCY_UP_DOWN_HIGHER ,
	PRESSURE_TENDENCY_UP_SLOW ,
	PRESSURE_TENDENCY_UP ,
	PRESSURE_TENDENCY_UP_FAST,
	PRESSURE_TENDENCY_STEADY ,
	PRESSURE_TENDENCY_DOWN_UP_LOW ,
	PRESSURE_TENDENCY_DOWN_SLOW ,
	PRESSURE_TENDENCY_DOWN ,
	PRESSURE_TENDENCY_DOWN_FAST 
} pressure_tendency_t ;


/*
 * CsubL, Code table 0513
 */
typedef enum {
	LOW_CLOUD_UNAVAIL = -1 , /* '/' */
	LOW_CLOUD_NONE ,
	CUMULUS_HUMILIS_OR_FRACTUS ,
	CUMULUS_MEDIOCRIS_OR_CONGESTUS ,
	CUMULONIMBUS_CALVUS ,
	STRATOCUMULUS_CUMULOGENITUS ,
	STRATOCUMULUS ,
	STRATUS_NEBULOSUS_OR_FRACTUS ,
	LOW_CLOUD_PRECIP ,
	CUMULUS_OR_STRATOCUMULUS ,
	CUMULONIMBUS_CAPILLATUS
} low_cloud_t ;


/*
 * CsubM, Code table 0515
 */
typedef enum {
	MED_CLOUD_UNAVAIL = -1 , /* '/' */
	MED_CLOUD_NONE ,
	ALTOSTRATUS_TRANSLUCIDUS ,
	ALTOSTRATUS_OPACUS_OR_NIMBOSTRATUS ,
	ALTOCUMULUS_TRANSLUCIDUS ,
	LENTICULAR_ALTOCUMULUS_TRANSLUCIDUS ,
	BANDED_ALTOCUMULUS_TRANSLUCIDUS ,
	ALTOCUMULUS_CUMULOGENITUS ,
	LAYERED_ALTOCUMULUS_TRANSLUCIDUS_OR_OPACUS ,
	ALTOCUMULUS_CASTELLANUS_OR_FLOCCUS ,
	CHAOTIC_ALTOCUMULUS
} med_cloud_t ;


/*
 * CsubH, Code table 0509
 */
typedef enum {
	HIGH_CLOUD_UNAVAIL = -1 , /* '/' */
	HIGH_CLOUD_NONE ,
	CIRRUS_FIBRATUS ,
	CIRRUS_SPISSATUS ,
	CIRRUS_SPISSATUS_CUMULONIMBOGENITUS ,
	INVADING_CIRRUS_SPISSATUS_OR_FIBRATUS ,
	INVADING_CIRRUS_OR_CIRROSTRATUS_MINUS , 
	INVADING_CIRRUS_OR_CIRROSTRATUS_PLUS , 
	COVER_CIRRUS_OR_CIRROSTRATUS , 
	PARTIAL_CIRRUS_OR_CIRROSTRATUS , 
	CIRROCUMULUS
} high_cloud_t ;


/*
 * E, Code table 0901
 * E', Code table 0975
 */
typedef enum {
	GROUND_STATE_UNAVAIL = -1 , /* '/' */
	/* E */
	GROUND_DRY,
	GROUND_MOIST,
	GROUND_WET,
	GROUND_FLOODED,
	GROUND_FROZEN,
	GROUND_GLAZED,
	GROUND_DUSTY_OR_SANDY,
	GROUND_DUST_OR_SAND_COVERED,
	GROUND_THICK_DUST_OR_SAND_COVERED,
	GROUND_EXTREMELY_DRY,	/* 9 */
	/* E' + 10 */
	GROUND_ICE_COVERED,	/* 10 */
	GROUND_PARTIAL_WET_SNOW_COVERED,
	GROUND_MOSTLY_WET_SNOW_COVERED,
	GROUND_WET_SNOW_COVERED_EVENLY,
	GROUND_WET_SNOW_COVERED_UNEVENLY,
	GROUND_PARTIAL_DRY_SNOW_COVERED,
	GROUND_MOSTLY_DRY_SNOW_COVERED,
	GROUND_DRY_SNOW_COVERED_EVENLY,
	GROUND_DRY_SNOW_COVERED_UNEVENLY,
	GROUND_SNOW_DRIFTS_COVERED /* 19 */
} ground_state_t ;


/* 
 * FM 12-VIII Ext. aka AAXX
 */
typedef struct {
	/* section 0 */
	dtime time ;	/* time of observation, UTC */
	wind_units_t wind_units ;	/* i sub w */
	long station_id ;	/* "IIiii", the WMO block and station number */
	char call_sign[CALL_SIGN_LEN+1] ;
	float latitude ;
	quadrant_t quadrant ;
	float longitude ;
	float elevation ;
	/* section 1 */
	precip_ind_t precip_ind ;	/* i sub R */
	stationt_t stationt ;
	g7_avail_t g7_avail ;
	cloudbase_range_t cloudbase_range ;	/* h */
#define VISIBILITY_UNAVAIL	9999 /* > Earth Radius */
#define VISIBILITY_LOW		.048 /* 90 => < .05 */
#define VISIBILITY_HIGH_50	99. /* 99 => >= 50 km */
#define VISIBILITY_HIGH		89. /* 89 => > 70 km */
	float visibility ;	/* visibility in km */
	cloudcover_t cloudcover ;
#define WIND_DIRECTION_UNKNOWN 990
#define WIND_CALM 0
#define WIND_NORTH 360
	int wind_direction ;	/* What about the pole? */
#define WIND_SPEED_UNKNOWN -1
	int wind_speed ;	
#define TEMPERATURE_UNKNOWN 9999
	float temperature  ;	
#define DEW_POINT_UNKNOWN 9999
	float dew_point  ;	
#define REL_HUMIDITY_UNKNOWN -1
	int rel_humidity ;
#define STATION_PRESSURE_UNKNOWN 9999
	float station_pressure ;	/* hectopascals */
#define SEALEVEL_PRESSURE_UNKNOWN 9999
	float sealevel_pressure ;
	pressure_tendency_t pressure_tendency ;
#define PRESSURE_CHANGE_UNKNOWN 9999
	float pressure_change ;
#define PRECIP_UNREPORTED 999
#define PRECIP_TRACE 990	/* code table 3590 */
#define PRECIP_HIGH 989
	float precip ;	/* millimetres */
#define PRECIP_REF_PERIOD_UNKNOWN -1
#define PRECIP_REF_PERIOD_INC 0
	int precip_ref_period ;	/* hours */
#define WEATHER_UNKNOWN -1
	int weather ; /* interpretation conditional on stationt MANNED vs AUTO */
	int past_weather_1 ; 
	int past_weather_2 ; 
	cloudcover_t cloudcover_2 ;
	low_cloud_t low_cloud ;
	med_cloud_t med_cloud ;
	high_cloud_t high_cloud ;
#define CLOUDBASE_UNREPORTED 9999
#define CLOUDBASE_LOW 0.	/* code table 1677 */
#define CLOUDBASE_HIGH 89.
	float cloudbase ;	/* height in metres of lowest cloud */
	/* section 2, not yet used */
	/* section 3 */
	float maximum_temperature ;
	float minimum_temperature ;
	ground_state_t ground_state ;
		/* skip jjj of sec3 */
#define SNOW_DEPTH_UNREPORTED 999	/* code table 3889 */
#define SNOW_DEPTH_SCATTERED 998
#define SNOW_DEPTH_TRACE 997
	int snow_depth ;  /* centimeters */
		/* skip 5jjjj (jjjjj) for now */
		/* 6RRRtsubR here supercedes sec1 */
	float precip_24 ; /* region IV 24 hour precip, millimetres */
	/* section 4, not yet used */
	/* section 5, not yet used */
} surface_ob ;


#if 0
extern void free_surface_ob(/* surface_ob * */) ;
extern surface_ob *new_synop_ob(
	/* dtime *time, wind_units_t wind_ind, long station_id */
	) ;
extern surface_ob *new_ship_ob(
	/* char *call_sign, dtime *time, wind_units_t wind_ind, int latitude, quadrant_t quadrant, int longitude */
	) ;

float tbl4377(/* int vv */) ;
float tbl3590(/* int ppp */) ;
float tbl1677(/* int hh */) ;
#endif

#endif /* _LDMSURFACE_H_ */
