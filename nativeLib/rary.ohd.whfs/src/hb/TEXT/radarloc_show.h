/*
	File:		radarloc_show.h
	Date:		June 1997
	Author:		Paul Taylor
	
	Purpose:	Provides support for the RadarLocations DS.
*/


#ifndef radarloc_show_h
#define radarloc_show_h

#include "DbmsDefs.h"

#include "RadarLoc.h"


/* prototypes */

void	radarloc_show(Widget w);
void	add_radarloc_cbs();
void	add_radarloc_textfilter_cbs();
void	remove_radarloc_textfilter_cbs();

void	load_radarloc_list();
void	load_radarloc_textCB();

void	radarloc_addCB();
int	radarloc_add_table_entries(RadarLoc *rlocPtr);

void	radarloc_updateCB();
void	radarloc_deleteCB();
void	radarloc_delete(Widget w, XtPointer ptr, XtPointer cbs);

RadarLoc*	radarloc_current_getRadarPtr(RadarLoc *rlocPtr);

int	read_radarloc_info(RadarLoc *rlocInfo);
void	radarloc_selectItem(RadarLoc *rlocPtr);

void	free_radarloc();
void	ok_radarlocCB();


#endif 



