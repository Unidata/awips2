/*******************************************************************************
* FILENAME:             create_ss_interface_rfcwide.h
* GENERAL INFORMATION:  
* DESCRIPTION:          This file contains the prototypes for the 
*                       following routines 
*				 create_ss_interface_rfcwide,
*				 initialize_draw_data_rfcwide,
*				 missing_data_rfcwide,
*				 locate_ss_RFCW,
*				 edit_bias_value_RFCW,
*				 show_ss_gages_RFCW,
*				 init_draw_data_free_memory
*				 popdown_window_single_site
*				 cancel_window_single_site
*			in the create_ss_interface_rfcwide source file.
*                       
* ORIGINAL AUTHOR:      Hmap_mpe Team
* CREATION DATE:        February 11, 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell Linux 
* MODIFICATION HISTORY:
* DATE                    PROGRAMMER         DESCRIPTION/REASON
* February 11, 2002     Moria Shebsovich      Revision 
********************************************************************************
*/


#ifndef CREATE_SS_INTERFACE_RFCWIDE_H
#define CREATE_SS_INTERFACE_RFCWIDE_H

#include "drawa.h"
#include "List.h"

/* Create a wrapper around the draw_struct structure. 
   This will allow the linked list library to be used with this structure.
   Since this is needed only for the Single Site Radar Window, the definition
   of this structure is being placed in the create_ss_interface_rfcwide.h
   header file. */

void create_ss_interface_rfcwide ( ) ;
void initialize_draw_data_rfcwide (draw_struct *data , int type , Widget w ) ;
void init_draw_data_free_memory ( ) ;
void missing_data_rfcwide ( draw_struct *data ) ;
void edit_radcov ( Widget w , XtPointer clientdata , XtPointer calldata ) ;
void locate_ss_RFCW ( Widget w, XtPointer clientdata,  XEvent *event , 
		Boolean *continue_to_dispatch_return ) ;
void edit_bias_value_RFCW ( Widget w , XtPointer clientdata , XtPointer calldata) ;
void init_draw_data_free_memory ( ) ;
void popdown_window_single_site ( Widget w, XtPointer clientdata, 
                                            XtPointer calldata);
void popdown_all_windows_single_site ( Widget w, XtPointer clientdata, 
                                                 XtPointer calldata);
void cancel_window_single_site ( Widget w, XtPointer clientdata, 
                                 XtPointer calldata);
void toggle_overlays ( Widget w, XtPointer clientdata, XtPointer calldata);

#endif /* #ifndef CREATE_SS_INTERFACE_RFCWIDE_H */



