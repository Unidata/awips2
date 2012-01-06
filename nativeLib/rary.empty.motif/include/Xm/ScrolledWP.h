/**
 *
 * $Id: ScrolledWP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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

#ifndef _XM_SCROLLEDWP_H
#define _XM_SCROLLEDWP_H

#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/DrawingA.h>
#include <Xm/ManagerP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Define the Scrolled window instance part */
typedef struct {
    int vmin;				/* vsb min coord pos (in work area?) */
    int vmax;
    int vOrigin;			/* slider edge */
    int vExtent;			/* slider size */

    int hmin;
    int hmax;
    int hOrigin;
    int hExtent;

    Position hsbX, hsbY;
    Dimension hsbWidth, hsbHeight;

    Position vsbX, vsbY;
    Dimension vsbWidth, vsbHeight;

    Dimension GivenHeight, GivenWidth;	/* work window size ? */

    Dimension AreaWidth, AreaHeight;	/* clip window size ? */
    Dimension WidthPad, HeightPad;	/* XmNscrolledWindowMarginWidth,
					 * XmNscrolledWindowMarginHeight */
    Position XOffset, YOffset;		/* position of clip window ? */

    Dimension pad;			/* XmNspacing */

    Boolean hasHSB;
    Boolean hasVSB;
    Boolean InInit;
    Boolean FromResize;

    unsigned char VisualPolicy;		/* XmNvisualPolicy */
    unsigned char ScrollPolicy;		/* XmNscrollingPolicy */
    unsigned char ScrollBarPolicy;	/* XmNscrollBarDisplayPolicy */
    unsigned char Placement;		/* XmNscrollBarPlacement */

    XmScrollBarWidget hScrollBar;	/* XmNhorizontalScrollBar */
    XmScrollBarWidget vScrollBar;	/* XmNverticalScrollBar */
    XmDrawingAreaWidget ClipWindow;	/* XmNclipWindow */
    Widget WorkWindow;			/* XmNworkWindow */

    XtCallbackList traverseObscuredCallback;
} XmScrolledWindowPart;

/* Define the full instance record */
typedef struct _XmScrolledWindowRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmScrolledWindowPart swindow;
} XmScrolledWindowRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmScrolledWindowClassPart;

/* Defint the full class record */
typedef struct _XmScrolledWindowClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmScrolledWindowClassPart scrolled_window_class;
} XmScrolledWindowClassRec;

XMLIBEXPORT extern XmScrolledWindowClassRec xmScrolledWindowClassRec;

/*
 * This is new for 2.0
 */

typedef struct _XmScrolledWindowConstraintPart {
    unsigned char child_type;
    Position orig_x;
    Position orig_y;
} XmScrolledWindowConstraintPart, *XmScrolledWindowConstraint;

/*
 * This is new for 2.0
 */

typedef struct _XmScrolledWindowConstraintRec {
    XmManagerConstraintPart manager;
    XmScrolledWindowConstraintPart swindow;
} XmScrolledWindowConstraintRec;

#ifdef __cplusplus
}
#endif

#endif /* _XM_SCROLLEDWP_H */
