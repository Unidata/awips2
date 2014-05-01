#ifndef HV_MAIN_CALLBACKS_H
#define HV_MAIN_CALLBACKS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <Xm/Xm.h>

#include "DbmsDefs.h"
#include "HvDisplayControlDefs.h"
#include "HvDisplayControlProto.h"
#include "HvColorList.h"
#include "GenericLegendDialog.h"
#include "mapBackgrounds.h"
#include "drawStations.h"

#include "display_control_show.h"
#include "polygon_RFCW.h"
#include "pointcontrol_show.h" 
#include "select.h"
#include "select_show.h"
#include "grid.h"

/*
	callbacks for main map
*/
void	buttonpress_cb ( clicks * mouse_clicks , int launch_timeseries ) ;
void	clearClickCounter(XtPointer counter, XtIntervalId *id);
int     free_dynamic_memory ( ) ;

void	redrawBase ( int * map_index );

/*
	Tool form PB callbacks
*/
void    pointdisplayCallback(Widget w, XtPointer ptr, XtPointer cbs) ;
void    arealdisplayCallback(Widget w, XtPointer ptr, XtPointer cbs) ;
void    precipAccumCallback ( Widget w , XtPointer ptr , XtPointer cbs ) ;
void	refreshDataCallback(Widget w, XtPointer ptr, XtPointer cbs) ;
void    stationLegendCallback(Widget w, XtPointer ptr, XtPointer cbs) ;

/* 
        callbacks for zooming and panning
*/
void hv_pan_callback ( Widget w , XtPointer client_data ,
                       XtPointer call_data ) ;
void hv_zoom_callback ( Widget w , XtPointer client_data ,
                        XtPointer call_data ) ;
void hv_recenter_callback ( Widget w , XtPointer client_data ,
                            XtPointer call_data ) ;

/*
	callbacks for menu options
*/

void drawFfg ( Widget w , XtPointer clientdata , XtPointer calldata ) ;

void clear_highlight_flag ( ) ;
int get_highlight_flag ( ) ;
void set_highlight_flag ( ) ;
void set_mpe_gage_color ( Widget w , XtPointer clientdata ,
                          XtPointer calldata ) ;
void set_mpe_gage_missing ( Widget w, XtPointer clientdata ,
		            XtPointer calldata ) ;
void display_image_callback ( Widget w, XtPointer client_data, XtPointer call_data );

#endif
