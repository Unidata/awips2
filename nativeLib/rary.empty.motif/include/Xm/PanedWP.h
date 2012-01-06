/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/PanedWP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 * 
 * Copyright (C) 1995-1998 Free Software Foundation, Inc.
 * Copyright (C) 1998-2000 LessTif Development Team
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

#ifndef _XM_PANEDWP_H
#define _XM_PANEDWP_H

#include <Xm/PanedW.h>
#include <Xm/ManagerP.h>

#ifdef __cplusplus
extern "C" {
#endif


/* Define the Constraint Resources */
typedef struct _XmPanedWindowConstraintPart {
    int position;		/* position location in PanedWindow */
    int dwidth;			/* desired width */	/* NEW, needed ? */
    int dheight;		/* desired height */
    Position dx;		/* desired location */	/* NEW, needed ? */
    Position dy;		/* desired location */
    Position olddx;		/* last value of above */
    Position olddy;		/* last value of above */
    Dimension min;		/* min height */
    Dimension max;		/* max height */
    Boolean isPane;		/* true if constraint of a pane, else false */

    Boolean allow_resize;	/* true if child resize requests ok */
    Boolean skip_adjust;	/* true if child height can't change without
				 * user input */
    /* private part */
    Widget sash;               /* Sash widget attached to this child */
    Widget separator;          /* Separator widget attached to this child */
    short position_index;
} XmPanedWindowConstraintPart;

typedef struct _XmPanedWindowConstraintRec {
    XmManagerConstraintPart manager;
    XmPanedWindowConstraintPart panedw;
} XmPanedWindowConstraintRec, *XmPanedWindowConstraintPtr;

/* Define the paned window instance part */
typedef struct {
    Boolean refiguremode;		/* refigure now or later */
    Boolean separator_on;		/* separators visible */

    Dimension margin_height;
    Dimension margin_width;

    Dimension spacing;

    Dimension sash_width;
    Dimension sash_height;
    Dimension sash_shadow_thickness;
    Position sash_indent;

    /* private part */
    int startx, starty;			/* mouse start y */
    short increment_count;		/* sash increment count */
    short pane_count;			/* number of managed panes */
    short num_slots;			/* number of avail slots for children */
    short num_managed_children;		/* number of managed children */

    Boolean recursively_called;		/* for change_managed and creation
					 * of sash and sep children */
    Boolean resize_at_realize;		/* Obscure M*tif comment: for realize
					 * if GeometryNo condition??? */

    XmPanedWindowConstraintPtr top_pane;
    XmPanedWindowConstraintPtr bottom_pane;

    GC flipgc;				/* GC for animating borders */
    WidgetList managed_children;	/* guess */

    unsigned char orientation;		/* new for 2.0 */
} XmPanedWindowPart;

/* Define the full instance record */
typedef struct _XmPanedWindowRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmPanedWindowPart paned_window;
} XmPanedWindowRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmPanedWindowClassPart;

/* Define the full class record */
typedef struct _XmPanedWindowClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmPanedWindowClassPart paned_window_class;
} XmPanedWindowClassRec;

XMLIBEXPORT extern XmPanedWindowClassRec xmPanedWindowClassRec;

/* Access macros */
#define PW_Orientation(w) \
	(((XmPanedWindowWidget)w)->paned_window.orientation)
#define PW_StartX(w) \
    (((XmPanedWindowWidget)w)->paned_window.startx)

#define PWC_DX(w) \
	(((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.dx)
#define PWC_DWidth(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.dwidth)
#define PWC_OldDX(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.olddx)

#ifdef __cplusplus
}
#endif


#endif /* _XM_PANEDWP_H */
