/*
	Name:		latestrep_show.h	
	Description:	Header file for latestrep callbacks.	
	
*/
#ifndef LATESTREP_SHOW_H
#define LATESTREP_SHOW_H

#include "DbmsDefs.h"


/* defines */

#define LOCATION     0
#define TIME         1
#define TIME_REVERSE 2

#define LIST_ALL      0
#define LIST_DURATION 1
#define LIST_NEVER    2

#define DURATION_DEFAULT "6"
#define DURATION_MIN 0
#define DURATION_MAX 9999


/* prototypes */

void latestrep_show(Widget w, char* lid); /* Show dialog */
void add_latestrep_cbs();		/* Add callbacks to dialog */
void free_dbgen();			/* Free memory for database routines */
void ok_latestrepCB();
void fill_top_listbox();		/* Fills top listbox */
void fill_bottom_listbox(char* parent_lid);
void latestlistCB(Widget w, XtPointer client_data, XtPointer call_data);
void currenttimeTimer(XtPointer client_data, XtIntervalId *id);

#endif 
