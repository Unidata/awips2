#ifndef _TSedit_h
#define _TSedit_h


#include <stdio.h>
#include "TSPlots.h"
#include "TSWidget.h"
#include "TSCallbacks.h"
#include "TSUtils.h"
#include "TSDefs.h"

void TSEditPressCB (Widget w, XtPointer ptr, XEvent *event);
void TSEditReleaseCB (Widget w, XtPointer ptr, XEvent *event);
void TSEditDragCB (Widget w, XtPointer ptr, XEvent *event);
int  TSEditMoveIndex ( int x, int y );    
void TSEditMoveUpdate(  GRAPH *g);
int  TSEditMove ( int x, int y );    
void TSEditSave();
void TSEditUndo();
void TSInterpolate( TRACE *tptr) ;
void draw_move( GRAPH *g);
 


#endif

