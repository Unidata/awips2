

#ifndef select_show_h
#define select_show_h

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <X11/Xatom.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include "Filter.h"
#include "Xtools.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"
#include "pointcontrol_report.h"
#include "select.h"
#include "hv_util.h"
#include "HvDisplayControl.h"
#include "hv_mainCallbacks.h"
#include "tsgen_info.h"

/*
	prototypes
*/

int     findStationXmListPos ( Station * station ) ;

void	loadStationList ( ReportList * pReportListHead ) ;

void    selectStation ( Station * station , Boolean selectListPos ) ;
void    selectStationCallback(Widget w, XtPointer ptr, XtPointer cbs);
void    selectTimeSeriesCallback(Widget w, XtPointer ptr, XtPointer cbs);

void	close_select(Widget w, XtPointer ptr, XtPointer cbs);

void	select_list_position(char *lid);

int	get_list_position_binary(char *lid);
int	get_list_position_linear(char *lid);

void	lookup_lid(Widget w, XtPointer ptr, XtPointer cbs);
void	clear_searchTxt(Widget w, XtPointer ptr, XtPointer cbs);

int	is_select_shown();


/* to get rid off */
void GetStationLid (XmString xmStr, char * lid);

#endif
