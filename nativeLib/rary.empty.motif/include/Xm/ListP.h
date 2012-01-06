/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/ListP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2000, 2002 LessTif Development Team
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

#ifndef _XM_LISTP_H
#define _XM_LISTP_H

#include <Xm/List.h>
#include <Xm/PrimitiveP.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>

#ifdef __cplusplus
extern "C" {
#endif

/* internal structure used to lay out the
   list elements. */

typedef struct {
	_XmString name;
	Dimension height;
	Dimension width;
	Dimension CumHeight; /* the cumulative height, including this widget */
	Boolean selected;
	Boolean last_selected;
	Boolean LastTimeDrawn;
	unsigned short NumLines;
	int length;
	Boolean saved_select; /* This will be removed */
} Element, *ElementPtr;

/* Define the list instance part */

typedef struct {
	Dimension spacing;
	short ItemSpacing;   
	Dimension margin_width;
	Dimension margin_height;
	XmFontList font;		/* Both XmNrenderTable and XmNfontList */
	XmString *items;
	int itemCount;
	XmString *selectedItems;
	int *selectedPositions; /* was: selectedIndices */
	int selectedItemCount;
	int visibleItemCount;
	int LastSetVizCount; /* new */
	unsigned char SelectionPolicy;
	unsigned char ScrollBarDisplayPolicy;
	unsigned char SizePolicy;
	XmStringDirection StrDir;

	Boolean AutoSelect;
	Boolean DidSelection; /* new */
	Boolean FromSetSB; /* new */
	Boolean FromSetNewSize; /* new */
	Boolean AddMode;
	unsigned char LeaveDir; /* new */
	unsigned char HighlightThickness;
	int                ClickInterval;
	XtIntervalId DragID; /* new */
	XtCallbackList     SingleCallback;
	XtCallbackList     MultipleCallback;
	XtCallbackList     ExtendCallback;
	XtCallbackList     BrowseCallback;
	XtCallbackList     DefaultCallback;

	GC NormalGC;
	GC InverseGC;
	GC HighlightGC;
	Pixmap DashTile;          /* new */
	ElementPtr *InternalList; /* new */
	int LastItem;             /* new */
	int FontHeight;           /* new */
	int                top_position;
	char Event;               /* new */
	int LastHLItem;
	int StartItem;            /* new */
	int OldStartItem;         /* new */
	int EndItem;              /* new */
	int OldEndItem;           /* new */
	Position BaseX;           /* new */
	Position BaseY;           /* new */
	Boolean MouseMoved;       /* new */
	Boolean AppendInProgress; /* new */
	Boolean Traversing;       /* new */
	Boolean KbdSelection;     /* new */

	short DownCount;          /* new */
	Time DownTime;            /* new */
	int CurrentKbdItem;       /* new */
	unsigned char SelectionType; /* new */
	GC InsensitiveGC;         /* new */

	int vmin; /* new */
	int vmax; /* new */
	int vOrigin; /* new */
	int vExtent; /* new */

	int hmin; /* new */
	int hmax; /* new */
	int hOrigin; /* new */
	int hExtent; /* new */

	Dimension MaxWidth; /* new */
	Dimension CharWidth; /* new */
	Position XOrigin; /* new */

	XmScrollBarWidget             hScrollBar;
	XmScrollBarWidget             vScrollBar;
	XmScrolledWindowWidget Mom; /* new */
	Dimension MaxItemHeight;

	Boolean	check_set_render_table;		/* for setting font */
	unsigned char AutoSelectionType; /* some status variable */
} XmListPart;

/* Define the full instance record */
typedef struct _XmListRec {
	CorePart core;
	XmPrimitivePart primitive;
	XmListPart list;
} XmListRec;

/* Define class part structure */
typedef struct {
	XtPointer extension;
} XmListClassPart;

/* Define the full class record */
typedef struct _XmListClassRec {
	CoreClassPart core_class;
	XmPrimitiveClassPart primitive_class;
	XmListClassPart list_class;
} XmListClassRec;

/* External definition for class record */
XMLIBEXPORT extern XmListClassRec xmListClassRec;

#ifdef __cplusplus
}
#endif

#endif /* _XM_LISTP_H */
