/* ***************************************** */
/*      File:           TSCallbacks.h              */
/*      Date:           Jan 2002             */
/*      Author:         Ai Vo                */
/*      Purpose:                             */
/*                                           */
/* ***************************************** */
 
#ifndef _TSCallbacks_H
#define _TSCallbacks_H

#include "MotifHeader.h"
#include "TSWidget.h"
#include "TSPlots.h"

void TSTraceSelectCB ( Widget w,
        XtPointer data,
        XtPointer cbs);

void TSCloseCB ( Widget w,
        XtPointer data,
        XtPointer cbs);

void TSPressCB( Widget w, 
	XtPointer data,  
	XEvent *event);

void TSReleaseCB( Widget w, 
	XtPointer data,  
	XEvent *event);

void TSDragCB( Widget w, 
	XtPointer data,  
	XEvent *event);

void pushCB( Widget w, 
	XtPointer data,  
	XtPointer cbs);

void initialExposeCB ( Widget w, 
	XtPointer data, 
	XtPointer cbs);

void scrolled ( Widget scrollbar,
        XtPointer data,
        XtPointer cbs);
 
void exposeCB ( Widget w,
        XtPointer data,
        XtPointer cbs);

void resizeCB ( Widget w,
        XtPointer data,
        XtPointer cbs);

void TSLineCB( Widget w, 
	XtPointer data,  
	XtPointer cbs);

void TSPointCB( Widget w, 
	XtPointer data,  
	XtPointer cbs);

void TSLPointCB( Widget w, 
	XtPointer data,  
	XtPointer cbs);

void TSGridOnCB( Widget w, 
	XtPointer data,  
	XtPointer cbs);

void TSGridOffCB( Widget w, 
	XtPointer data,  
	XtPointer cbs);

void TSEditCB( Widget w, 
	XtPointer data,  
	XtPointer cbs);
 
void TSEditSaveCB( Widget w, 
	XtPointer data,  
	XtPointer cbs);
 
void TSEditCancelCB( Widget w, 
	XtPointer data,  
	XtPointer cbs);
        
void TSResetGraph(GRAPH *graph);


void TSaddCallbacks();

void TSFreeResources();


#endif
