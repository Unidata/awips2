/*
	File:		ugc_show.h
	Date:		12/20/96
	Author:		Paul Taylor
	
	Purpose:	Provides support for UGC DS.
	
*/


#ifndef ugc_show_h
#define ugc_show_h


#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "Counties.h"
#include "CountyInfo.h"
#include "Eligzon.h"
#include "ZoneInfo.h"


void    ShowUgcDs(Widget w, char *lid);

void	ugc_add(Widget w, XtPointer ptr, XtPointer cbs);
void	ugc_delete(Widget w, XtPointer ptr, XtPointer cbs);
void	ugc_clear(Widget w, XtPointer ptr, XtPointer cbs);


void    storeCountyInfo  (CountyInfo *dest, Counties *source);
int	ugc_countyfindpos(Counties *nthPtr, CountyInfo *firstPtr);
void	ugc_countyadd    (int nth, int insert_pos);

void	storeZoneInfo  (ZoneInfo *dest, Eligzon *source);
int	ugc_zonefindpos(Eligzon *nthPtr, ZoneInfo *firstPtr);
void	ugc_zoneadd    (int nth, int insert_pos);


void	ugc_ok(Widget w, XtPointer ptr, XtPointer cbs);
void	ugc_apply(Widget w, XtPointer ptr, XtPointer cbs);
void    ugc_close(Widget w, XtPointer ptr, XtPointer cbs);
void    ugc_help(Widget w, XtPointer ptr, XtPointer cbs);

void    ugc_checkPB(Widget w, XtPointer ptr, XtPointer cbs);


void	ugc_load(const char *tablename, const char *lid);
Counties*	ugc_getCountyAvail(void);
CountyInfo*	ugc_getCountySel(const char *lid);
Eligzon*	ugc_getZoneAvail(void);
ZoneInfo*	ugc_getZoneSel(const char *lid);


void	ugc_defact(Widget w, XtPointer ptr, XtPointer cbs);
void	ugc_extsel(Widget w, XtPointer ptr, XtPointer cbs);


#endif

