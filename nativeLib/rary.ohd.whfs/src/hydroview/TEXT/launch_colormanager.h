#ifndef LAUNCH_COLORMANAGER_H
#define LAUNCH_COLORMANAGER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <Xm/Xm.h>

#include "jni_calls.h"

void launch_color_manager_callback ( Widget widget , XtPointer client_data, XtPointer call_data );
void launch_mpe_color_manager_callback ( Widget widget , XtPointer client_data, XtPointer call_data );
void processHvLegendMouseClick ( Widget w, XtPointer client_data, XEvent * event, Boolean * flag );

#endif
