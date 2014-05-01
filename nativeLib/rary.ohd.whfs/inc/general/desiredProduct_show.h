

#ifndef DESIRED_PRODUCT_SHOW_H
#define DESIRED_PRODUCT_SHOW_H


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <X11/cursorfont.h>
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "desiredProduct.h"
#include "DesiredProductRecord.h"
#include "desiredProductRel.h"
#include "desiredProductAbs.h"
#include "ArealProductSettings.h"
#include "ArealProductControl.h"
#include "time_convert.h"


void initDesiredProductRecord(DesiredProductRecord *dpr);
DesiredProductRecord * getDesiredProductRecord(void);


void desiredProductShow(Widget w);
void addDesiredProductCallbacks(DesiredProductRecord *dpr);
void updateDesiredList(DesiredProductRecord *dpr);

/*
     Overall section
*/
void desiredProductCloseCallback(Widget w, XtPointer ptr, XtPointer cbs);
void dpResultSelectCallback(Widget w, XtPointer ptr, XtPointer cbs);

#endif
