/*
	File:		cities_show.h
	Date:		4/2/97
	Author:		Paul Taylor
	
	Purpose:	Provide support for the Cities DS.
*/


#ifndef CITIES_SHOW_H
#define CITIES_SHOW_H

/*
	Includes.
*/
#include "City.h"


/*
	Prototypes.
*/
void ShowCitiesDs	(Widget w);
void add_cities_cbs	(void);
void add_cities_textfilter_cbs		(void);
void remove_cities_textfilter_cbs	(void);


void load_citylist	(void);
void load_citytextCB	(Widget w, XtPointer ptr, XtPointer cbs);


void cities_addCB	(Widget w, XtPointer ptr, XtPointer cbs);
void cities_updateCB	(Widget w, XtPointer ptr, XtPointer cbs);
void cities_deleteCB	(Widget w, XtPointer ptr, XtPointer cbs);


int  read_city_info	(City *cityinfo);

void cities_findcity	(City *cityPtr);


void free_cities(void);

void cities_closeCB	(Widget w, XtPointer ptr, XtPointer cbs);


#endif 


