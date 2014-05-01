#ifndef ARROW_BUTTON_CALLBACKS_H
#define ARROW_BUTTON_CALLBACKS_H

#include <Xm/Xm.h>
#include "gageqc_defs.h"

void set_choose_hour_window_values(int call);
void hour_increment(int inc_call_val);
void hour_decrement(int dec_call_val);
void set_call(int call_val);
void hour_decrement_callback(Widget w, XtPointer client_data, XtPointer call_data);
void hour_increment_callback(Widget w, XtPointer client_data, XtPointer call_data);
void days_decrement_callback(Widget w, XtPointer client_data, XtPointer call_data);
void days_increment_callback(Widget w, XtPointer client_data, XtPointer call_data);
void date_increment_callback(Widget w, XtPointer client_data, XtPointer call_data);
void date_decrement_callback(Widget w, XtPointer client_data, XtPointer call_data);
/*
void set_prev_hour(Widget w, XtPointer client_data, XtPointer call_data);
void set_next_hour(Widget w, XtPointer client_data, XtPointer call_data);
void unset_next_hour(Widget w, XtPointer client_data, XtPointer call_data);
void unset_prev_hour(Widget w, XtPointer client_data, XtPointer call_data);
*/
void set_choose_hour_window_to_previous ( const struct _dqc_run_date * pDate );

#endif /* #ifndef ARROW_BUTTON_CALLBACKS_H */
