/*=========================================================================*/
/*                        FILE NAME:   post_functions.h                    */
/*                                                                         */
/*                        HEADER FILE FOR POST ANALYSIS PROGRAM            */
/*                                 CONTAINING:                             */
/*                             FUNCTION PROTOTYPES                         */
/*=========================================================================*/

#ifndef POST_FUNCTIONS_H
#define POST_FUNCTIONS_H

#include <Xm/Xm.h>
#include "drawa.h"
#include "HydroStatus.h"
#include "stage3.h"
#include "stage3_interface.h"

 /* An enumeration defining the 4 panes on the single site radar window. */
 enum draw_struct_ptr { MsData , GgData , St1iData , St1iiData } ;

 /* An enumeration defining the corners of a HRAP bin
    starting with the bottom right corner and proceding clockwise
    around the bin's perimeter. */
 enum HrapCorners { BOTTOM_RIGHT_CORNER , BOTTOM_LEFT_CORNER ,
                    TOP_LEFT_CORNER , TOP_RIGHT_CORNER ,
                    NUM_CORNERS_PER_HRAP_BIN } ;
 enum MpeImageDisplay { DISPLAY_AS_CONTOUR, DISPLAY_AS_IMAGE,
                        NUM_DISPLAY_OPTIONS };

 /* Definitions for the maximum lengths of the basin and county names.
    These do not take into account the need for a terminating '\0'
    character at the end of a C-style string. */
 #define MAX_BASIN_NAME_LEN 9
 #define MAX_COUNTY_NAME_LEN 21

void   fill_pixmap_radar_site ( Widget w , draw_struct * data ,
		                XmDrawingAreaCallbackStruct * call_data ) ;
 void   post_analysis();
 void   display_post();
 void   initialize_draw_data();
 void   create_legend();

 void   merge_data();
 void   quit();
 void   quit_and_save();
 void   quit_post_from_WM();
 void   zoom();
 void   time_lapse();
 void   zoom_sensitive();
 void   switch_sensitive();
 void   show_gage_table();
 void   remove_ap();
 void   close_no_gage();
 void   close_shell();
 void   redo_gageonly();
 void   show_gages();
 void   show_values();

 void   Sum24();
 void   GetGageData();
 void   GetMisbin();
 void   GageOnly();
 float  WeightedValue(int, int);
 void   ReadParam();
 void   MergeData();
 void   timedist();

 int    isThereMpeData ( ) ;
 void   turnOffMpeData ( ) ;
 void   turnOnMpeData ( ) ;
 draw_struct * getDrawStruct ( ) ;
 void   setDrawStruct ( draw_struct * draw_data ) ;
 Pixmap getMpeDataPixmap ( Pixmap user_pixmap ) ;
 GC     getMpeDataGC ( ) ;
 HydroStatus createLatLonGrid ( int hrap_origin_x , int hrap_origin_y ,
                                int max_hrap_x , int max_hrap_y ) ;
 HydroStatus createPixelGrid ( ) ;
 const HRAP ** getLatLonGrid ( ) ;
 const point ** getPixelGrid ( ) ;
 void   freeLatLonGrid ( ) ;
 void   freePixelGrid ( ) ;
 void   setPixmapToDraw ( Pixmap * pixmap , GC * pGraphicsContext ) ;
 void   unsetPixmapToDraw ( ) ;
 void   MPEUtil_copy_area();
 void   read_colors();
 void   draw_bound();

 void   display_merge();
 void   initialize_merged_data();

 Widget create_working_dialog();
 void   display_date_window();
 void   un_map_shell();
 void   pop_down();
 void   select_callback();
 void   ok_callback();
 void   popup_workingDialog();
 Widget create_working_dialog();
 void   set_duration();

 GC     xs_create_xor_gc();
 void   mpe_start_rubber_band ( Widget w , XtPointer clientdata ,
                                XEvent * event ,
                                Boolean * continue_to_dispatch_return ) ;
 void   mpe_track_rubber_band ( Widget w , XtPointer clientdata ,
                                XEvent * event ,
                                Boolean * continue_to_dispatch_return ) ;
 void   mpe_end_rubber_band ( Widget w , XtPointer clientdata ,
                              XEvent * event ,
                              Boolean * continue_to_dispatch_return ) ;

 HydroStatus get_geo_data ( ) ;
 void   read_geo_data_free_memory();
 void   add_overlays();
 void   show_rfc_boundaries( Widget w , XtPointer client_data , 
 			      XtPointer call_data ) ;
 void   MPEUtil_show_states( Widget w , XtPointer client_data , XtPointer call_data ) ;
 void   MPEUtil_show_rivers( Widget w , XtPointer client_data , XtPointer call_data ) ;
 void   MPEUtil_show_basin_boundaries( Widget w , XtPointer client_data , 
                               XtPointer call_data ) ;
 void   MPEUtil_show_cities_and_towns( Widget w , XtPointer client_data , 
                               XtPointer call_data ) ;
 void   MPEUtil_show_county( Widget w , XtPointer client_data , 
                     XtPointer call_data ) ;

 void   create_gage_table();
 void   MPEUtil_display_no_gage();
 void   MPEFieldGen_sort();
 int    MPEUtil_sort_by_gageval();
 int    sort_by_diff();
 int    sort_by_ratio();
 int    MPEUtil_sort_by_gageid();
 int    sort_by_radar();
 void   change_rc();
 void   edit_gage_value();
 char  *table_data();

 void   show_single_gage();
 void   display_single_gage();
 void   display_gage();
 void   close_gage();

 void   locate_main();
 void   locate_merge();
 void   MPEUtil_close_locate();

 void label_radclim ( Widget w , XtPointer clientdata , XtPointer calldata ) ;
 void label_radcov ( Widget w , XtPointer clientdata , XtPointer calldata ) ;
 void label_rawrad ( Widget w , XtPointer clientdata , XtPointer calldata ) ;
 void label_unbrad ( Widget w , XtPointer clientdata , XtPointer calldata ) ;

void show_ss_gages_RFCW ( Widget w , XtPointer client_data , 
                          XtPointer call_data);
void setToPlotContour ( Boolean state );
void setToPlotImage ( Boolean state );

#endif /* #ifndef POST_FUNCTIONS_H */
