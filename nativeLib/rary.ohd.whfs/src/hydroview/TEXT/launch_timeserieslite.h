#ifndef LAUNCH_TIMESERIES_LITE_H
#define LAUNCH_TIMESERIES_LITE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <Xm/Xm.h>

#include "DbmsDefs.h"
#include "PointDisplayControl.h"
#include "HvDisplayControlDefs.h"
#include "HvDisplayControlProto.h"
#include "HvColorList.h"
#include "mapBackgrounds.h"
#include "pointcontrol_mgr.h"

#include "pointcontrol_options.h"
#include "display_control_show.h"
#include "pointcontrol_show.h" 
#include "select.h"
#include "select_show.h"
#include "grid.h"
#include "jni_calls.h"

void launch_timeserieslite ( Widget map_widget , clicks  * mouse_clicks );
void displayTimeSeriesLite(const char *lid, const char *paramCode, const char *fcstParamCode);
void getParamCode(const char * lid, char * paramCode);
void getFcstParamCode(const char * lid, const char * paramCode, char * fcstParamCode);

#endif
