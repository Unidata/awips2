#ifndef HV_REFRESH_TIMER_H
#define  HV_REFRESH_TIMER_H



#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <Xm/Xm.h>
#include "HvDisplayControl.h"

#include "time_defs.h"

#define REFRESH_MINUTES 15

void addRefreshTimeOut ( Widget top_widget ) ;
void refreshDataCycle(XtPointer clientdata, XtIntervalId *id);

#endif
