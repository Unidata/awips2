/*******************************************************************************
* FILENAME:            time_lapse_RFCW.h
* GENERAL INFORMATION:
* DESCRIPTION:         Contains the prototype for the time_lapse_RFCW.h 
*                      routine.
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       February 7, 2002
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX / Dell Linux
* MODIFICATION HISTORY:
* DATE              PROGRAMMER        DESCRIPTION/REASON
* February 7, 2002  Bryon Lawrence    Original Coding
********************************************************************************
*/
#ifndef TIME_LAPSE_RFCW_H
#define TIME_LAPSE_RFCW_H

#define LOOP_BACKWARD 0
#define LOOP_FORWARD 1

#include <Xm/Xm.h>

void time_lapse_RFCW ( Widget w , XtPointer clientdata , XtPointer calldata ) ;
void free_time_lapse_memory ( ) ;
void set_time_lapse_flag_on ( ) ;
void set_time_lapse_flag_off ( ) ;
int get_time_lapse_flag ( ) ;
void end_time_lapse_RFCW ( Widget w , XtPointer clientdata ,
                           XtPointer calldata ) ;
void end_time_lapse ( Widget w , XtPointer clientdata , XEvent * event ,
                      Boolean * continue_to_dispatch_return ) ;
void manual_time_lapse ( Widget w , XtPointer clientdata,
                         XEvent * event, 
                         Boolean * continue_to_dispatch_event );
void end_manual_loop_callback ( Widget w, XtPointer clientdata, XtPointer calldata );
void manual_loop_callback ( Widget w, 
                            XtPointer clientdata, 
                            XtPointer calldata );

void hydroview_manual_loopCB ( Widget w,
                               XtPointer clientdata,
                               XtPointer calldata );

void loop_step_callback ( Widget w, 
                          XtPointer clientdata, 
                          XtPointer calldata );
int get_date_array_index ( );
void set_date_array_index ( int date_index );

#endif /* #ifndef TIME_LAPSE_RFCW_H */
