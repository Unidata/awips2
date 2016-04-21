/*******************************************************************************
* ORIGINAL AUTHOR:       Hmap_mpe team.
* CREATION DATE:         February 11, 2002
* ORGANIZATION:          OHD / HSEB
* MACHINE:               HP-UX / Dell Linux
* MODIFICATION HISTORY:
*  DATE                  PROGRAMMER          DESCRIPTION/REASON
*  February 11, 2002     Moria Shebsovich    Original Coding
********************************************************************************
*/

#ifndef CHOOSE_RFCWIDE_DATE_H
#define CHOOSE_RFCWIDE_DATE_H

#include <Xm/Xm.h>
/* Function Prototype. */
void show_dates_RFCW () ;
void popup_rfcwide_workingDialog ( Widget w, XtPointer clientdata , 
							XtPointer calldata );
Widget create_working_dialog_RFCW();
#endif /* #ifndef CHOOSE_RFCWIDE_DATE_H */
void choose_hour_manage(Widget ww);
void set_choose_hour_window_values(int call);
void set_values_for_change_and_cancel();
void restore_working_date(Widget w, XtPointer clientdata, XtPointer calldata);
void display_rfcwide_date_window(Widget ww, XtPointer clientdata, XtPointer calldata);
void send_date_to_gageqc_engine_struct (Widget w, XtPointer clientdata,XtPointer calldata);
