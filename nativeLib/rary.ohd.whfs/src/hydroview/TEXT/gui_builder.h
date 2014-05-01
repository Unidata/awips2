/*******************************************************************************
* FILENAME:            gui_builder.h
* NUMBER OF MODULES:   1
* GENERAL INFORMATION:  
*         DESCRIPTION: This file contains prototypes of routines defined
*                      within the gui_builder.c file.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       October 30, 2001
* ORGANIZATION:        HSEB
* MACHINE:             HP-UX 9000 / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        10/30/2001   Bryon Lawrence    Original Coding
********************************************************************************
*/

#ifndef GUI_BUILDER_H
#define GUI_BUILDER_H

#include <Xm/Xm.h>

#define MPE_LEGEND_HEIGHT 60
#define MPE_LEGEND_WIDTH 600
#define HV_LEGEND_HEIGHT 600
#define HV_LEGEND_WIDTH 600
#define HV_DEFAULT_WIDTH_IN_PIXELS 900
#define HV_DEFAULT_HEIGHT_IN_PIXELS 900

/* Define the bounding Latitudes and Longitudes. */
#define HV_REGION_TOP_LAT 75.0
#define HV_REGION_BOTTOM_LAT 15.0
#define HV_REGION_LEFT_LON -170.0
#define HV_REGION_RIGHT_LON -60.0

#define HV_GUAM_REGION_TOP_LAT 20.0 
#define HV_GUAM_REGION_BOTTOM_LAT 10.0 
#define HV_GUAM_REGION_LEFT_LON 140.0    /* Note that Guam is east of the
                                            international date line and has
                                            positive longitude values. */
#define HV_GUAM_REGION_RIGHT_LON 160.0 

#define GUI_BUILDER_INIT_PAUSE_DURATION 50 


void draw_launch_gui ( XtPointer clientdata , XtIntervalId * id ) ; 
void set_parent_widget ( Widget w ) ;


void init_point_data ( XtPointer clientdata , XtIntervalId * id ) ;
void init_mpe_data ( XtPointer clientdata , XtIntervalId * id ) ;
void popdown_dialog_window ( XtPointer clientdata , XtIntervalId * id ) ;

void redrawMap ( ) ;

#endif /* #ifndef GUI_BUILDER_H */
