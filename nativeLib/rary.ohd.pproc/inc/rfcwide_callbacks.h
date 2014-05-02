/*******************************************************************************
* FILENAME:            rfcwide_callbacks.h
* GENERAL INFORMATION:
* DESCRIPTION:         Contains the prototypes for the routines defined in
*                      the rfcwide_callbacks.c file.  Also contains any
*                      user-defined types required by these routines.
*
* ORIGINAL AUTHOR:     Hmap_mpe Team
* CREATION DATE:       February 6, 2002
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE               PROGRAMMER         DESCRIPTION/REASON
* February 6, 2002   Bryon Lawrence     Original Coding
********************************************************************************
*/

#ifndef RFCWIDE_CALLBACKS_H
#define RFCWIDE_CALLBACKS_H

#include <Xm/Xm.h>

#include "drawa.h"
#include "newhour_RFCW.h"
#include "rfcwide_interface.h"

#define MAX_GAGE_STRING_SIZE 12
#define GAGE_VAL_STR_SIZE 9
#define X_VALUES_OFFSET  10
#define Y_VALUES_OFFSET  10

enum MPEgageColorOptions { MPEgageColorSolid , MPEgageColorContrast ,
                           MPEgageColorQC , MPEgageColorByValue ,
                           MPEnumGageColorOptions } ; 
enum MPEgageMissingOptions { MPEgageMissingAll, MPEgageMissingReported,
	                     MPEgageMissingNone, MPEnumGageMissingOptions } ;

Widget create_save_data_dialog_RFCW ( ControlMenuItemInfo value ) ;

void init_single_gage_RFCW ( Widget w , XtPointer clientdata , 
                               XtPointer calldata ) ;

void display_single_gage_RFCW ( Widget w , XtPointer clientdata , 
                                XEvent * event , 
                                Boolean * continue_to_dispatch_return ) ;
				
void do_time_lapse_6_RFCW ( Widget w , XtPointer clientdata , 
                            XtPointer calldata ) ;

void do_time_lapse_12_RFCW ( Widget w , XtPointer clientdata , 
                             XtPointer calldata ) ;

void do_time_lapse_24_RFCW ( Widget w , XtPointer clientdata , 
                             XtPointer calldata ) ;

void do_time_lapse_RFCW ( Widget w , XtPointer clientdata , 
                             XtPointer calldata ) ;

void ignore_radar_RFCW ( Widget w , XtPointer clientdata ,
                         XtPointer calldata ) ;
void ignore_dp_radar_RFCW ( Widget w , XtPointer clientdata ,
                         XtPointer calldata ) ;

void next_callback_RFCW ( Widget w , XtPointer clientdata ,  
                             XtPointer calldata ) ;

void show_gage_ids ( Widget w , XtPointer clientdata ,  
                             XtPointer calldata ) ;

void show_gage_table_RFCW ( Widget w , XtPointer data , 
                             XtPointer call_data ) ;
			     
void show_gage_values ( Widget w , draw_struct * data , caddr_t * call_data ) ;

/* add Mpe gage identificators to Hydromap GUI */
void add_mpe_gage_ids ( int map_index ) ;

/* returns mpe_gage_ids_flag to check 
   if MpeControl->Gage->GageIdentifiers was choosen */
int isThereMpeGageIds ( ) ;

/* turns off mpe_gage_ids_flag */
void turnOffMpeGageIds ( ) ;

/* turns on mpe_gage_ids_flag */
void turnOnMpeGageIds ( ) ;

void drawMpeGageIds ( int map_index ) ;

void set_mpe_gage_ids ( Widget w , XtPointer clientdata, XtPointer calldata ) ;

void set_mpe_gage_triangles ( Widget w, XtPointer clientdata,
                              XtPointer calldata );

/* add Mpe gage values to Hydromap GUI */
void add_mpe_gage_values ( ) ;

/* returns mpe_gage_values_flag to check 
   if MpeControl->Gage->GageValues was choosen */
int isThereMpeGageValues ( ) ;

/* turns off mpe_gage_values_flag */
void turnOffMpeGageValues ( ) ;

/* turns on mpe_gage_values_flag */
void turnOnMpeGageValues ( ) ;

void drawMpeGageValues ( int map_index ) ;

void drawPrecipPrism ( int map_index ) ;

void set_mpe_gage_values ( Widget w , XtPointer clientdata, 
                           XtPointer calldata ) ;

void store_mpe_gage_label_color ( enum MPEgageColorOptions gage_color_option ) ;
void store_mpe_gage_missing ( enum MPEgageMissingOptions gage_missing_option ) ;

/* October 8, 2003  - Bryon L.  This function has been made inline because
   it will be called many times when displaying mpe gage data. */ 
inline void set_mpe_gage_label_color ( const gage_struct * pGage,
		                       int map_index ) ;
void unset_mpe_gage_label_color ( ) ;

/* July 26, 2005 - Bryon L.  These functions enable the toggling between
   the split and full screen views. */
void split_screen_callback ( Widget w, XtPointer clientdata,
                             XtPointer calldata );
void full_screen_callback ( Widget w, XtPointer clientdata,
                            XtPointer calldata );
int is_screen_split ( );

int isThereMpeGageTriangles ( );

void turnOffMpeGageTriangles ( );

void set_values_for_change_and_cancel();

void _draw_topography           ( int index , void * pData , Pixmap map ,
                                  void * pUserData ) ;
void _contour_topo ( int index, void * pData, Pixmap map, void * pUserData );
void set_topo_change_flag ( );

Pixmap get_topo_pixmap ( );
void transmit_rfc_qpe ( Widget w, XtPointer client_data, XtPointer call_data );
void transmit_rfc_bias ( Widget w, XtPointer client_data, XtPointer call_data );

#endif /* #ifndef RFCWIDE_CALLBACKS_H */
