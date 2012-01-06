/**
 *
 * $Id: DragCP.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

#ifndef _XM_DRAGCP_H
#define _XM_DRAGCP_H

#include <Xm/XmP.h>
#include <Xm/DragC.h>
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <Xm/DragIcon.h>
#include <Xm/DragOverS.h>
#include <Xm/DropSMgrP.h>

#ifdef __cplusplus
extern "C" {
#endif
   
typedef void (*XmDragStartProc)(XmDragContext, Widget, XEvent *);
typedef void (*XmDragCancelProc)(XmDragContext) ;
   
#define _XmDragStart(dc, srcW, event) \
  (*((XmDragContextClass)XtClass(dc))->drag_class.start)\
  (dc, srcW, event)

#define _XmDragCancel(dc) \
  (*((XmDragContextClass)XtClass(dc))->drag_class.cancel)\
  (dc)

typedef struct {
   XmDragStartProc start;
   XmDragCancelProc cancel;
   XtPointer extension;
} XmDragContextClassPart;

typedef struct _XmDragContextClassRec {
   CoreClassPart core_class;
   XmDragContextClassPart drag_class;
} XmDragContextClassRec;

XMLIBEXPORT extern XmDragContextClassRec xmDragContextClassRec;

#define XtDragByPoll 	0
#define XtDragByEvent	1
   
typedef struct {
   Window frame;
   Window window;
   Widget shell;
   unsigned char flags;
   unsigned char dragProtocolStyle;
   int xOrigin, yOrigin;
   unsigned int width, height;
   unsigned int depth;
   XtPointer iccInfo;
} XmDragReceiverInfoStruct, *XmDragReceiverInfo;


typedef union _XmConvertSelectionRec {
   XtConvertSelectionIncrProc sel_incr;
   XtConvertSelectionProc sel;
} XmConvertSelectionRec;

typedef struct _XmDragContextPart {
   Atom	 *exportTargets;
   Cardinal numExportTargets;
   XmConvertSelectionRec convertProc;
   XtPointer clientData;
   XmDragIconObject sourceCursorIcon;
   XmDragIconObject stateCursorIcon;
   XmDragIconObject operationCursorIcon;
   XmDragIconObject sourcePixmapIcon;
   Pixel cursorBackground;
   Pixel cursorForeground;
   Pixel validCursorForeground;
   Pixel invalidCursorForeground;
   Pixel noneCursorForeground;
   XtCallbackList dragMotionCallback;
   XtCallbackList operationChangedCallback;
   XtCallbackList siteEnterCallback;
   XtCallbackList siteLeaveCallback;
   XtCallbackList topLevelEnterCallback;
   XtCallbackList topLevelLeaveCallback;
   XtCallbackList dropStartCallback;
   XtCallbackList dropFinishCallback;
   XtCallbackList dragDropFinishCallback;
   unsigned char dragOperations;
   Boolean incremental;
   unsigned char blendModel;
   
   Window srcWindow;
   Time	dragStartTime;
   Atom	iccHandle;
   Widget sourceWidget;
   Boolean sourceIsExternal;

   Boolean topWindowsFetched;
   unsigned char commType;
   unsigned char animationType;

   unsigned char operation;
   unsigned char operations;
   unsigned int	lastEventState;
   unsigned char dragCompletionStatus;
   unsigned char dragDropCompletionStatus;
   Boolean forceIPC;
   Boolean serverGrabbed;
   Boolean useLocal;
   Boolean inDropSite;
   XtIntervalId dragTimerId;
   
   Time	roundOffTime;
   Time	lastChangeTime;
   Time	crossingTime;
   
   Time	dragFinishTime;
   Time	dropFinishTime;
   
   Atom	dropSelection;
   Widget srcShell;
   Position startX, startY;

   XmID	siteID;

   Screen *currScreen;
   Window currWmRoot;
   XmDragOverShellWidget curDragOver;
   XmDragOverShellWidget origDragOver;
   
   XmDragReceiverInfoStruct *currReceiverInfo;
   XmDragReceiverInfoStruct *rootReceiverInfo;
   XmDragReceiverInfoStruct *receiverInfos;
   Cardinal numReceiverInfos;
   Cardinal maxReceiverInfos;
   
   unsigned char trackingMode;
   unsigned char activeProtocolStyle;
   unsigned char activeBlendModel;
   Boolean dragDropCancelEffect;
} XmDragContextPart;

typedef struct _XmDragContextRec {
   CorePart core;
   XmDragContextPart drag;
} XmDragContextRec;

#define _XmDCtoDD(dc) ((XmDisplay)XtParent(dc))
     
XMLIBEXPORT extern XmDragReceiverInfo _XmAllocReceiverInfo(XmDragContext dc);
XMLIBEXPORT extern unsigned char _XmGetActiveProtocolStyle(Widget w);
   
#ifdef __cplusplus
}
#endif

#endif /* _XM_DRAGC_H */
