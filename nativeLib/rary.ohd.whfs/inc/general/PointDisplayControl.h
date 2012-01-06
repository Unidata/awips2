#ifndef POINT_DISPLAY_CONTROL
#define POINT_DISPLAY_CONTROL

#include <Xm/Xm.h>

#include "HvDisplayControl.h"

/* table definitions */

#include "HvStation.h"
#include "Temperature.h"
#include "Snow.h"
#include "TelmType.h"
#include "RiverStatus.h"
#include "pointcontrol_show.h"


/* utility functions */

#include "QualityCode.h"
#include "LoadUnique.h"


/* prototypes */
Station * get_current_station ( ) ;
void initStations(HvDisplayControl *hdc);
void initStation(Station 	*station,
		 HvStation 	*hvStation);

void load_PointData(HvDisplayControl *hdc);
void load_StationPointData(Station 	*station,
			   ReportList	*rPtr);

void load_latest_river_reports(HvDisplayControl 	*hdc,
			       ReportList		*riverHead);
void load_river_report(Station 		*station,
		       ReportList	*rPtr);

Station * locateStation ( int x , int y , int * new_station ) ;
Station * locateStationWithShifting ( int x , int y , int * new_station );

void initStationFromReport(Station *stationPtr, ReportList * pReportListNode);
int isSameStation(Station *station, Station *current_station);

void    refreshStationData ( Widget top_widget ) ;


Station * findDisplayedStation ( char * station_id ) ; 

long findStationIndex(const char * lid);
Station * findStationPtr(const char * lid);
Boolean isStationDisplayed ( char * station_id ) ;


/* HvDisplayControl structure functions */

void loadSourceTypeList(HvDisplayControl *hdc);
void mapHighlightStation ( ) ;


/* printing routines */

void printStations(Station *stations, long numStations);
void printStation(Station *station);


#endif
