/**
 *
 * $Id: ScreenP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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

#ifndef _XM_SCREENP_H
#define _XM_SCREENP_H

#include <Xm/DesktopP.h>
#include <Xm/Screen.h>
#include <Xm/DragIcon.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _XmScreenClassPart {
    XtPointer extension;
} XmScreenClassPart, *XmScreenClassPartPtr;

typedef struct _XmScreenClassRec {
    CoreClassPart core_class;
    XmDesktopClassPart desktop_class;
    XmScreenClassPart screen_class;
} XmScreenClassRec;

typedef struct _XmDragCursorRec {
    struct _XmDragCursorRec *next;
    Cursor cursor;
    XmDragIconObject stateIcon;
    XmDragIconObject opIcon;
    XmDragIconObject sourceIcon;
    Boolean dirty;
} XmDragCursorRec, *XmDragCursorCache;

typedef struct _XmScratchPixmapRec *XmScratchPixmap;

typedef struct _XmScratchPixmapRec {
    XmScratchPixmap next;
    Pixmap pixmap;
    Cardinal depth;
    Dimension width;
    Dimension height;
    Boolean inUse;
} XmScratchPixmapRec;

typedef struct {
    Boolean mwmPresent;
    unsigned short numReparented;
    int darkThreshold;
    int foregroundThreshold;
    int lightThreshold;
    XmDragIconObject defaultNoneCursorIcon;
    XmDragIconObject defaultValidCursorIcon;
    XmDragIconObject defaultInvalidCursorIcon;
    XmDragIconObject defaultMoveCursorIcon;
    XmDragIconObject defaultCopyCursorIcon;
    XmDragIconObject defaultLinkCursorIcon;
    XmDragIconObject defaultSourceCursorIcon;

    Cursor nullCursor;
    XmDragCursorRec *cursorCache;
    Cardinal maxCursorWidth;
    Cardinal maxCursorHeight;

    Cursor menuCursor;
    unsigned char unpostBehavior;
    XFontStruct *font_struct;
    int h_unit;
    int v_unit;
    XmScratchPixmap scratchPixmaps;
    unsigned char moveOpaque;

    XmDragIconObject xmStateCursorIcon;
    XmDragIconObject xmMoveCursorIcon;
    XmDragIconObject xmCopyCursorIcon;
    XmDragIconObject xmLinkCursorIcon;
    XmDragIconObject xmSourceCursorIcon;

    GC imageGC;
    int imageGCDepth;
    Pixel imageForeground;
    Pixel imageBackground;

    XtPointer screenInfo;
} XmScreenPart, *XmScreenPartPtr;

typedef struct _XmScreenInfo {
    XtPointer menu_state;
    Boolean destroyCallbackAdded;
} XmScreenInfo;

XMLIBEXPORT extern XmScreenClassRec xmScreenClassRec;

typedef struct _XmScreenRec {
    CorePart core;
    XmDesktopPart desktop;
    XmScreenPart screen;
} XmScreenRec;

extern XrmQuark _XmInvalidCursorIconQuark;
extern XrmQuark _XmValidCursorIconQuark;
extern XrmQuark _XmNoneCursorIconQuark;
extern XrmQuark _XmDefaultDragIconQuark;
extern XrmQuark _XmMoveCursorIconQuark;
extern XrmQuark _XmCopyCursorIconQuark;
extern XrmQuark _XmLinkCursorIconQuark;

/*
 * protos
 */
extern XmDragIconObject _XmScreenGetOperationIcon(Widget w,
						  unsigned char operation);
extern XmDragIconObject _XmScreenGetStateIcon(Widget w, unsigned char state);
extern XmDragIconObject _XmScreenGetSourceIcon(Widget w);
extern Pixmap _XmAllocScratchPixmap(XmScreen xmScreen,
				    Cardinal depth,
				    Dimension width,
				    Dimension height);
extern void _XmFreeScratchPixmap(XmScreen xmScreen, Pixmap pixmap);
extern XmDragCursorCache * _XmGetDragCursorCachePtr(XmScreen xmScreen);
extern void _XmGetMaxCursorSize(Widget w,
				Dimension *width,
				Dimension *height);
extern Cursor _XmGetNullCursor(Widget w);
extern Cursor _XmGetMenuCursorByScreen(Screen *screen);
extern Boolean _XmGetMoveOpaqueByScreen(Screen *screen);
extern unsigned char _XmGetUnpostBehavior(Widget wid);
extern int _XmGetFontUnit(Screen *screen,
			  int dimension);
extern void _XmScreenRemoveFromCursorCache(XmDragIconObject icon);


#ifdef __cplusplus
}
#endif

#endif /* _XM_SCREENP_H */
