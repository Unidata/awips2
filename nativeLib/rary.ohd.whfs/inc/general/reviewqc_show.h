/*
	Name:		reviewqc_show.h	
	Description:	Header file for Questionable and Bad data window	
*/

#ifndef REVIEWQC_SHOW_H
#define REVIEWQC_SHOW_H

#include "DbmsDefs.h"


/* defines */

#define T_AGRICULTURAL  0
#define T_DISCHARGE     1
#define T_EVAPORATION   2
#define T_FISHCOUNT     3 
#define T_GATEDAM       4
#define T_GROUND        5
#define T_HEIGHT        6
#define T_ICE           7
#define T_LAKE          8
#define T_MOISTURE      9
#define T_POWER        10
#define T_RAWPC        11
#define T_RAWPP        12
#define T_RAWPOTHER    13
#define T_PRESSURE     14
#define T_RADIATION    15
#define T_SNOW         16
#define T_TEMPERATURE  17
#define T_WATERQUALITY 18
#define T_WEATHER      19
#define T_WIND         20
#define T_YUNIQUE      21


/* prototypes */

void reviewqc_show          (Widget w, char* lid);
void filter_toggleCB        (Widget w, XtPointer client_data, XtPointer call_data);
void qclistCB               (Widget w, XtPointer client_data, XtPointer call_data);
void update_listCB          (Widget w, XtPointer client_data, XtPointer call_data);
void load_QB_scrolledlist();
void free_obs_linkedList();
void close_reviewqcCB();

void invoke_timeseries      (Widget w, XtPointer client_data, XtPointer call_data);

void setmissing_reviewqcCB  (Widget w, XtPointer ptr, XtPointer cbs);
void delete_reviewqcCB      (Widget w, XtPointer ptr, XtPointer cbs);
void add_reviewqc_cbs();

#endif 
