/**
 *
 * $Id: DropSMgr.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_DROPSMGR_H
#define _XM_DROPSMGR_H

#include <Xm/Xm.h>       /* to get the version number */

#ifdef __cplusplus
extern "C" {
#endif

enum {
    XmCR_DROP_SITE_LEAVE_MESSAGE=1,
    XmCR_DROP_SITE_ENTER_MESSAGE,
    XmCR_DROP_SITE_MOTION_MESSAGE,
    XmCR_DROP_MESSAGE
};

enum {
    XmNO_DROP_SITE=1,
    XmINVALID_DROP_SITE,
    XmVALID_DROP_SITE
};

/* 
 * as per the Motif 1.2.3 headers, the above enumerated constants are 
 * incorrect.  The correct constants, listed below, are defined in terms
 * of the incorrect ones above. 
 */

#define XmDROP_SITE_INVALID XmINVALID_DROP_SITE
#define XmDROP_SITE_VALID XmVALID_DROP_SITE

enum { 
    XmDRAG_UNDER_NONE,
    XmDRAG_UNDER_PIXMAP,
    XmDRAG_UNDER_SHADOW_IN,
    XmDRAG_UNDER_SHADOW_OUT,
    XmDRAG_UNDER_HIGHLIGHT 
};

enum { 
    XmDROP_SITE_SIMPLE,
    XmDROP_SITE_COMPOSITE,
    XmDROP_SITE_SIMPLE_CLIP_ONLY = 128,
    XmDROP_SITE_COMPOSITE_CLIP_ONLY
};

enum { 
    XmABOVE,
    XmBELOW
};


#if XmVERSION > 1
enum { 
    XmDROP_SITE_ACTIVE, 
    XmDROP_SITE_INACTIVE,
    XmDROP_SITE_IGNORE
};
#else
enum { 
    XmDROP_SITE_ACTIVE, 
    XmDROP_SITE_INACTIVE 
};
#endif

typedef struct _XmDragProcCallbackStruct {
    int reason;
    XEvent *event;
    Time timeStamp;
    Widget dragContext;
    Position x, y;
    unsigned char dropSiteStatus;
    unsigned char operation;
    unsigned char operations;
    Boolean animate;
} XmDragProcCallbackStruct, *XmDragProcCallback;

typedef struct _XmDropProcCallbackStruct {
    int reason;
    XEvent *event;
    Time timeStamp;
    Widget dragContext;
    Position x, y;
    unsigned char dropSiteStatus;
    unsigned char operation;
    unsigned char operations;
    unsigned char dropAction;
} XmDropProcCallbackStruct, *XmDropProcCallback;

typedef struct _XmDropSiteVisualsRec {
    Pixel background;
    Pixel foreground;
    Pixel topShadowColor;
    Pixmap topShadowPixmap;
    Pixel bottomShadowColor;
    Pixmap bottomShadowPixmap;
    Dimension shadowThickness;
    Pixel highlightColor;
    Pixmap highlightPixmap;
    Dimension highlightThickness;
    Dimension borderWidth;
} XmDropSiteVisualsRec, *XmDropSiteVisuals;


XMLIBEXPORT extern WidgetClass xmDropSiteManagerObjectClass;

typedef struct _XmDropSiteManagerClassRec *XmDropSiteManagerObjectClass;
typedef struct _XmDropSiteManagerRec *XmDropSiteManagerObject;

#ifndef XmIsDropSiteManager
#define XmIsDropSiteManager(w)  XtIsSubclass((w), xmDropSiteManagerObjectClass)
#endif 

XMLIBEXPORT extern void XmDropSiteRegister(Widget widget,
			       ArgList args,
			       Cardinal argCount);
XMLIBEXPORT extern void XmDropSiteUnregister(Widget widget);
XMLIBEXPORT extern void XmDropSiteStartUpdate(Widget refWidget);
XMLIBEXPORT extern void XmDropSiteUpdate(Widget enclosingWidget,
			     ArgList args,
			     Cardinal argCount);
XMLIBEXPORT extern void XmDropSiteEndUpdate(Widget refWidget);
XMLIBEXPORT extern void XmDropSiteRetrieve(Widget enclosingWidget,
			       ArgList args,
			       Cardinal argCount);
XMLIBEXPORT extern int XmDropSiteQueryStackingOrder(Widget widget,
					Widget *parent_rtn,
					Widget **children_rtn,
					Cardinal *num_children_rtn);
XMLIBEXPORT extern void XmDropSiteConfigureStackingOrder(Widget widget,
					     Widget sibling,
					     Cardinal stack_mode);
XMLIBEXPORT extern XmDropSiteVisuals XmDropSiteGetActiveVisuals(Widget widget);

#if XmVERSION > 1
XMLIBEXPORT extern Boolean XmDropSiteRegistered(Widget w);
#endif

#ifdef __cplusplus
}
#endif

#endif /* _XM_DROPSMGR_H */
