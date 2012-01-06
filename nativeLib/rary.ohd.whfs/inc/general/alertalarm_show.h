#ifndef ALERTALARM_SHOW_H
#define ALERTALARM_SHOW_H

#include "DbmsDefs.h"
#include "get_limits.h"

#include "AlertAlarmVal.h"
#include "time_convert.h"
#include "alert_util.h"

#include "QualityCode.h"
#include "DbmsUtils.h"
#include "Xtools.h"

#include "tsgen_info.h"          /* for time series displays */
#include "TSControl_show.h"


/* prototypes */

void alertalarm_show(Widget w);

void aalistCB(Widget w, XtPointer client_data, XtPointer call_data);
void update_aalistCB(Widget w, XtPointer client_data, XtPointer call_data);
void fill_aa_list();
void invoke_AAtimeseries(Widget w, XtPointer client_data,
                         XtPointer call_data);
void delete_alertalarm(Widget w, XtPointer client_data,
                         XtPointer call_data);
void aa_delete(Widget w, XtPointer client_data,
                         XtPointer call_data);			 			 

void close_alertalarmCB();
void add_alertalarm_cbs();

char *format_aatime(time_t timeval);

#endif 
