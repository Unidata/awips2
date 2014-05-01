#ifndef DESIRED_PRODUCT_REL_H
#define DESIRED_PRODUCT_REL_H

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
/*#include "HvRelDesiredProd.h"*/

#define MAX_DPRODUCTS 5000


/*
     Relative callbacks section
*/
void	dpRelApplyCallback(Widget w, XtPointer ptr, XtPointer cbs);
void	dpRelDeleteCallback(Widget w, XtPointer ptr, XtPointer cbs);
void	dpRelSelectItemCallback(Widget w, XtPointer ptr, XtPointer cbs);


/*
     Relative utility functions
*/
void    addRelProductCallbacks ( DesiredProductRecord * dpr ) ;
int     getHvRelDesiredProdPos( HvRelDesiredProd *relHead , 
                                HvRelDesiredProd *prod ) ;

void    dpRelLoadList(DesiredProductRecord *dpr);
void    dpRelGetWhereClauseFromKey(HvRelDesiredProd *hPtr, char *where);

void    dpRelLoadWidgets(HvRelDesiredProd *hPtr);
void    dpRelUnloadWidgets(HvRelDesiredProd *hPtr);

void    dpRelSave(HvRelDesiredProd *hPtr);
void    dpRelDelete(HvRelDesiredProd *hPtr);


#endif
