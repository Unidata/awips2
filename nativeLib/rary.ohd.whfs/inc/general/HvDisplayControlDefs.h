#ifndef HV_CONTROL_DEFS_H
#define HV_CONTROL_DEFS_H

#include "map_menubar_cb.h"
#include "Report.h"


/* whfs external utilities */
#include "Xtools.h"
#include "time_convert.h"

/* database tables definitions */
#include "DPARadar.h"

/* assorted includes */
#include "hv_util.h"
#include "ColoredPoint.h"
#include "TimePeriod.h"
#include "GridDef.h"
#include "ArealProductSettings.h"
#include "pointcontrol_loc_shift.h" //from pdc engine

/*****************************************************************************/


#define MAX_LISTED_PRODUCTS 3000

#define FEATURE_NAME_LEN 40

#define DISP_CLASS_FCSTPT    "F"
#define DISP_CLASS_RIVER     "R"
#define DISP_CLASS_RESERVOIR "D"
#define DISP_CLASS_PRECIP    "P"
#define DISP_CLASS_SNOW      "S"
#define DISP_CLASS_TEMP      "T"
#define DISP_CLASS_OTHER     "O"


/*****************************************************************************/

/* part of ArealDisplaySettings */

typedef enum FfmComparisonType
   {    
   COMPARE_DIFFERENCE,
   COMPARE_RATIO   
   } FfmComparisonType;


/*****************************************************************************/


typedef struct Station
{
   char lid[LOC_ID_LEN + 1];
   char name[LOC_NAME_LEN + 1];
   
   char streamName[STREAM_NAME_LEN + 1];
   
   
   /* map points */
   
   M_MapPoint   loc;
   M_XYPoint    pos;
   M_XYPoint    pos_shift;
   char paramCode[PARAM_CODE_LEN + 1];
   
   
   /* whether or not station is displayed or not */
   
   int isDisplayed;
   
   
   /* stage or discharge threshold data */
   
   double floodLevel;
   double actionLevel;
   
   
   /* station types */
   char isFcstPt;
   char isRiver;
   char	isReservoir;
   char isNonRiver;
   
   
   /* holds the Maximum of the Observed and FOrecast data for
      the station for use in coloring the river icon, if applicable */
   Report mofoReport;
   
   
   /* holds actual data */
   Report curReport;

   /* Contains the second value to be displayed (if any).  This value
      is in addition to the one being supplied in the current report. */
   double value2 ;
  
   /* Contains the threat index as retrieved from the ReportList structure.
      This can have the following values:
      "R" - MOFO stage/flow is at or above flood
            stage/flow. Red.
      "Y" - MOFO stage/flow is at or above action
            stage/flow. Yellow.
      "G" - MOFO stage/flow is below action stage/flow.
            Green.
      "Z" - Threat is no available. Missing. */
   char threat_index ;
   
} Station;


/*****************************************************************************/
/*  Map structures */
/*****************************************************************************/


typedef struct _MapPoint 
{
   char		id[LOC_ID_LEN + 1];
   char		name[FEATURE_NAME_LEN + 1];
   int		npts;
   M_MapPoint	loc;
   M_XYPoint	pos;	
} MapPoint;


typedef struct _MapLine 
{
   char		id[LOC_ID_LEN + 1];
   char		name[FEATURE_NAME_LEN + 1];
   int		npts;
   M_MapPoint	*actual;
   XPoint	*poly;	
} MapLine;


typedef struct _MapArea 
{
   char		id[LOC_ID_LEN + 1];
   char		name[FEATURE_NAME_LEN + 1];
   
   double	value1;
   double	value2;
   
   
   M_MapPoint	interiorPoint;
   int		npts;
   M_MapPoint	*actual;
   XPoint	*poly;	
} MapArea; 


/****************************************************************************/


typedef struct BackgroundDisplaySettings
{
   Boolean showMajorStreamsLakes;
   Boolean showAllStreamsLakes;
   
   Boolean showRivers;
   Boolean showStreams;
   Boolean showReservoirs;
   
   Boolean showBasins;  
   Boolean showCounties;
   Boolean showZones;
     
   Boolean showMajorCities;
   Boolean showAllCities;
   Boolean showCities;
   Boolean showTowns;
   
   Boolean showMajorRoads;
   Boolean showAllRoads;
   Boolean showHiways;
   Boolean showRoads;
   
   Boolean showRadars;
   
} BackgroundDisplaySettings;


/*************************************************************************/

typedef struct PointDataSettings
{
 
   
   /* general options */	
   
   Boolean showId;  
   Boolean showName;      
   Boolean showTime;
   Boolean showValue1;
   Boolean showValue2;
   
   Boolean showElevation;
   Boolean showParamCode;
   
   Boolean stagebasis;
   Boolean valueordepart;
   Boolean stageflow ; 
   
   Boolean suppressIcons;
   
} PointDataSettings;


/*****************************************************************************/

typedef struct ArealDisplaySettings
{
   
   /*   display options */
   
   Boolean fillArea;
   Boolean showValue1;
   Boolean showValue2;
   Boolean showId;
   Boolean showName;
   
   
   TimePeriod timePeriod;
   
   
   /* specifiers to declare what product times are preferred */
   
   ArealProductControl 	*DesiredControls;
   long 		numDesiredControls;
   
   
   /* the product type descriptor and list of specifiers */
   
   ArealProductTypeDescriptor 	descriptor;
   ArealProductSpecifier 	specifiers[MAX_LISTED_PRODUCTS];
   int 				numSpecifiers;
   
   
   ArealProductSpecifier selectedSpecifier;
   
   FfmComparisonType comparisonType;
   
   
   int selectedItemPos;
   
   ColorThresholdArray ctArray;
   
   
} ArealDisplaySettings;


/****************************************************************************/


typedef struct HvDisplaySettings
{
   BackgroundDisplaySettings	background;
   
   PointDataSettings 		point;
   
   ArealDisplaySettings		areal;
   
} HvDisplaySettings;


/****************************************************************************/


typedef struct HvDisplayControl
{
   
   Station * curStation;
   
   
   /* display objects */
   
   Station 	*stations;
   long 	numStations;
   
   MapArea	*areas;
   long 	numAreas;
   
   
   /* grid data */
   
   Grid 	grid;
   
   
   /* control objects */
   
   HvDisplaySettings displaySettings;
   
} HvDisplayControl;

/****************************************************************************/

#endif


