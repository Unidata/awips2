/**
 *
 * $Id: DragC.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
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

#ifndef _XM_DRAGC_H
#define _XM_DRAGC_H

#include <Xm/Xm.h>
#include <Xm/Display.h>

#ifdef __cplusplus
extern "C" {
#endif

#define XmHELP			2

typedef unsigned int	XmID;

#ifndef XmIsDragContext
#define XmIsDragContext(w) XtIsSubclass(w, xmDragContextClass)
#endif 

#ifndef _XA_MOTIF_DROP
#define _XA_MOTIF_DROP "_MOTIF_DROP"
#define _XA_DRAG_FAILURE "_MOTIF_DRAG_FAILURE"
#define _XA_DRAG_SUCCESS "_MOTIF_DRAG_SUCCESS"
#endif 

/* values used in the message type for the ClientMessage we send */
enum {
    XmTOP_LEVEL_ENTER,
    XmTOP_LEVEL_LEAVE,
    XmDRAG_MOTION,
    XmDROP_SITE_ENTER,
    XmDROP_SITE_LEAVE,
    XmDROP_START,
    XmDROP_FINISH,
    XmDRAG_DROP_FINISH,
    XmOPERATION_CHANGED
};
   
/* values used for the completionStatus */
enum {
    XmDROP,
    XmDROP_HELP,
    XmDROP_CANCEL,
    XmDROP_INTERRUPT
};
      
/* values for operations */

#define XmDROP_NOOP  0L
#define XmDROP_MOVE (1L << 0)
#define XmDROP_COPY (1L << 1)
#define XmDROP_LINK (1L << 2)

/* enum for operation result */

enum {
    XmDROP_FAILURE,
    XmDROP_SUCCESS
};

/* values for blending */

enum {
    XmBLEND_ALL,
    XmBLEND_STATE_SOURCE,
    XmBLEND_JUST_SOURCE,
    XmBLEND_NONE
};

/* values sent for the public callback reasons */
   
enum {
    XmCR_TOP_LEVEL_ENTER,
    XmCR_TOP_LEVEL_LEAVE,
    XmCR_DRAG_MOTION,
    XmCR_DROP_SITE_ENTER,
    XmCR_DROP_SITE_LEAVE,
    XmCR_DROP_START,
    XmCR_DROP_FINISH,
    XmCR_DRAG_DROP_FINISH,
    XmCR_OPERATION_CHANGED,
    _XmNUMBER_DND_CB_REASONS
};

typedef struct _XmDragContextClassRec	*XmDragContextClass;
typedef struct _XmDragContextRec	*XmDragContext;
XMLIBEXPORT extern WidgetClass xmDragContextClass;
   
typedef struct _XmAnyICCCallbackStruct {
   int reason;
   XEvent *event;
   Time timeStamp;
} XmAnyICCCallbackStruct, *XmAnyICCCallback;

typedef struct _XmTopLevelEnterCallbackStruct {
   int reason;
   XEvent *event;
   Time timeStamp;
   Screen *screen;
   /* see DragC.h in Motif 1.2.3 for comment. line 106 */
   Window window;
   Position x,y;
   unsigned char dragProtocolStyle;
   Atom iccHandle;
} XmTopLevelEnterCallbackStruct, *XmTopLevelEnterCallback;
   
typedef struct _XmTopLevelLeaveCallbackStruct {
   int reason;
   XEvent *event;
   Time timeStamp;
   Screen *screen;
   Window window;
} XmTopLevelLeaveCallbackStruct, *XmTopLevelLeaveCallback;

   /* this message is sent from the receive to the initiator to 
    * indicate that the motion message with the associated timestamp has
    * caused a drop-site to be entered
    */
typedef struct _XmDropSiteEnterCallbackStruct {
   int reason;
   XEvent *event;
   Time timeStamp;
   unsigned char operation;
   unsigned char operations;
   unsigned char dropSiteStatus;
   Position x, y;
} XmDropSiteEnterCallbackStruct, *XmDropSiteEnterCallback;

typedef struct _XmDropSiteLeaveCallbackStruct {
   int reason;
   XEvent *event;
   Time timeStamp;
} XmDropSiteLeaveCallbackStruct, *XmDropSiteLeaveCallback;

typedef struct _XmDragMotionCallbackStruct {
   int reason;
   XEvent *event;
   Time timeStamp;
   unsigned char operation;
   unsigned char operations;
   unsigned char dropSiteStatus;
   Position x, y;
} XmDragMotionCallbackStruct, *XmDragMotionCallback;
   
typedef struct _XmOperationChangedCallbackStruct {
   int reason;
   XEvent *event;
   Time timeStamp;
   unsigned char operation;
   unsigned char operations;
   unsigned char dropSiteStatus;
} XmOperationChangedCallbackStruct, *XmOperationChangedCallback;
   
typedef struct _XmDropStartCallbackStruct {
   int reason;
   XEvent *event;
   Time timeStamp;
   unsigned char operation;
   unsigned char operations;
   unsigned char dropSiteStatus;
   unsigned char dropAction;
   Position x, y;
   Window window;
   Atom iccHandle;
} XmDropStartCallbackStruct, *XmDropStartCallback;
   
typedef struct _XmDropFinishCallbackStruct {
   int reason;
   XEvent *event;
   Time timeStamp;
   unsigned char operation;
   unsigned char operations;
   unsigned char dropSiteStatus;
   unsigned char dropAction;
   unsigned char completionStatus;
} XmDropFinishCallbackStruct, *XmDropFinishCallback;

typedef struct _XmDragDropFinishCallbackStruct {
   int reason;
   XEvent *event;
   Time timeStamp;
} XmDragDropFinishCallbackStruct, *XmDragDropFinishCallback;

/* public functions */
   
XMLIBEXPORT extern Widget XmDragStart(Widget w,
			  XEvent *event,
			  ArgList args,
			  Cardinal numArgs);
			  
XMLIBEXPORT extern void XmDragCancel(Widget dragContext);
   
XMLIBEXPORT extern Boolean XmTargetsAreCompatible(Display *dpy,
				      Atom *exportTargets,
				      Cardinal numExportTargets,
				      Atom *importTargets,
				      Cardinal numImportTargets);

#ifdef __cplusplus
}
#endif

#endif /* _XM_DRAGC_H */
