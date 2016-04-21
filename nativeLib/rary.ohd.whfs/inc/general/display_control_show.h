#ifndef DISPLAY_CONTROL_SHOW_H
#define DISPLAY_CONTROL_SHOW_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <time.h>

#include "MotifWidgets.h"
#include <Xm/AtomMgr.h>

#include "display_control.h"

#include "Xtools.h"
#include "DbmsDefs.h"
#include "DbmsUtils.h"

#include "TelmType.h"

#include "HvDisplayControl.h"
#include "ArealData.h"
#include "hv_util.h"
#include "grid.h"
#include "ColorBar.h"
#include "FfmUtils.h"
#include "ArealProductSettings.h"
#include "color_threshold_show.h"
#include "ffm_summary_show.h"


/* function prototypes */

void	displayControlShow(Widget w);

void dc_AddCallbacks(void);
void dc_ArealAddCallbacks(void);

void dc_CloseCallback(Widget w, XtPointer ptr, XtPointer cbs);
void dc_ClearCallback(Widget w, XtPointer ptr, XtPointer cbs);



void  setDataFromGuiCallback(Widget w, XtPointer ptr, XtPointer cbs);
void iconFilterStationCallback(Widget w, XtPointer ptr, XtPointer cbs);
void dataFilterStationCallback(Widget w, XtPointer ptr, XtPointer cbs);

void	showDesiredProductsDialogCallback(Widget w, XtPointer ptr,
					  XtPointer cbs);
void	showThresholdDialogCallback(Widget w, XtPointer ptr, XtPointer cbs);
void	showFfmSummaryDialogCallback(Widget w, XtPointer ptr, XtPointer cbs);


/*  setting data structure from the Gui */

void dc_setDataFromGui(HvDisplayControl *hdc);

ArealProductType determineProductTypeFromGUI(void);
void sensitizeArealProductSettingsGui();

void dc_setGuiFromData(HvDisplayControl *hdc);

void setArealOptionGuiFromData(HvDisplayControl *hdc);
void setArealFilterGuiFromData(HvDisplayControl *hdc);
void setGuiFromArealProductType(ArealProductType productType);


void productTypeChangedCallback(Widget w, XtPointer ptr, XtPointer cbs);
void arealOptionChangedCallback(Widget w, XtPointer ptr, XtPointer cbs);
void arealProductSelectedCallback(Widget w, XtPointer ptr, XtPointer cbs);
void comparisonTypeChangedCallback(Widget w, XtPointer ptr, XtPointer cbs);

Boolean isSelectedPos(Widget xmList, int pos);

void dc_drawArealDataCallback(Widget w, XtPointer ptr, XtPointer cbs);

void getDurationText(long duration, char *durationText);

ColorBar *dc_GetColorBar(void);

int dc_window_status(int new_state);

void disableMgrClose( Widget w );


#endif
