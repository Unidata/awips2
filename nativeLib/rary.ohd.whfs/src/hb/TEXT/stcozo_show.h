/*
	File:		stcozo_show.h
	Date:		3/24/97
	Author:		Mark Glaudemans
	
	Purpose:	Provide support for the "State/County/Zones Setup" DS.
	
*/


#ifndef STCOZO_SHOW_H
#define STCOZO_SHOW_H

/*
	Includes.
*/
#include "State.h"
#include "Counties.h"
#include "Eligzon.h"
#include "DbmsDefs.h"


/*
	Prototypes.
*/
void ShowStCoZoDS	(Widget w);
void add_stcozo_cbs	(void);


void load_statelist	(void);
void load_statetextCB	(Widget w, XtPointer ptr, XtPointer cbs);

void load_countylist	(void);
void load_countytextCB	(Widget w, XtPointer ptr, XtPointer cbs);

void load_zonelist	(void);
void load_zonetextCB	(Widget w, XtPointer ptr, XtPointer cbs);


void state_addCB	(Widget w, XtPointer ptr, XtPointer cbs);
void state_updateCB	(Widget w, XtPointer ptr, XtPointer cbs);
void state_deleteCB	(Widget w, XtPointer ptr, XtPointer cbs);

void county_addCB	(Widget w, XtPointer ptr, XtPointer cbs);
void county_updateCB	(Widget w, XtPointer ptr, XtPointer cbs);
void county_deleteCB	(Widget w, XtPointer ptr, XtPointer cbs);

void zone_addCB		(Widget w, XtPointer ptr, XtPointer cbs);
void zone_updateCB	(Widget w, XtPointer ptr, XtPointer cbs);
void zone_deleteCB	(Widget w, XtPointer ptr, XtPointer cbs);


void read_state_info	(State *stateinfo);
void read_county_info	(Counties *countiesinfo);
void read_zone_info	(Eligzon *eligzoninfo);

void stcozo_findstate	(State *statePtr);
void stcozo_findcounty	(Counties *countiesPtr);
void stcozo_findzone	(Eligzon *eligzonPtr);


void free_stcozo(void);

void ok_stcozoCB	(Widget w, XtPointer ptr, XtPointer cbs);


#endif 


