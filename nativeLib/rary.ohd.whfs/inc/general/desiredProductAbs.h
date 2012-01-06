#ifndef DESIRED_PRODUCT_ABS_H
#define DESIRED_PRODUCT_ABS_H

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
#include "ArealProductControl.h"
#include "Xtools.h"
#include "DbmsUtils.h"
#include "DbmsDefs.h"
#include "desiredProduct.h"
#include "DesiredProductRecord.h"
/*#include "HvAbsDesiredProd.h" */

#define MAX_DPRODUCTS 5000


/*
     Absolute callbacks section
*/
void	dpAbsApplyCallback(Widget w, XtPointer ptr, XtPointer cbs);
void	dpAbsDeleteCallback(Widget w, XtPointer ptr, XtPointer cbs);
void	dpAbsSelectItemCallback(Widget w, XtPointer ptr, XtPointer cbs);


/*
     Absolute utility functions
*/
void    addAbsProductCallbacks ( DesiredProductRecord * dpr ) ;
int     getHvAbsDesiredProdPos(HvAbsDesiredProd *relHead,
			       HvAbsDesiredProd *prod);

void    dpAbsLoadList(DesiredProductRecord *dpr);
void    dpAbsGetWhereClauseFromKey(HvAbsDesiredProd *hPtr, char *where);

void    dpAbsLoadWidgets(HvAbsDesiredProd *hPtr);
void    dpAbsUnloadWidgets(HvAbsDesiredProd *hPtr);

void    dpAbsSave(HvAbsDesiredProd *hPtr);
void    dpAbsDelete(HvAbsDesiredProd *hPtr);


#endif
