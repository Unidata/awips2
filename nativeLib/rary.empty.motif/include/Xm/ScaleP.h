/**
 *
 * $Id: ScaleP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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
 
#ifndef _XM_SCALEP_H
#define _XM_SCALEP_H

#include <Xm/Scale.h>
#include <Xm/ManagerP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Define the scale instance part */
typedef struct {
    int value;
    int maximum;
    int minimum;
    unsigned char orientation;
    unsigned char processing_direction;
    XmString title;
    XmFontList font_list;
    XFontStruct *font_struct;
    Boolean show_value;
    short decimal_points;
    Dimension scale_width;
    Dimension scale_height;
    Dimension highlight_thickness;
    Boolean highlight_on_enter;
    XtCallbackList drag_callback;
    XtCallbackList value_changed_callback;

    int last_value;
    int slider_size;
    GC foreground_GC;
    int show_value_x;
    int show_value_y;
    int show_value_width;
    int show_value_height;
    int scale_multiple;

    /* New for 2.0 */
    Boolean editable;
} XmScalePart;

/* Define the full instance record */
typedef struct _XmScaleRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmScalePart scale;
} XmScaleRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmScaleClassPart;

/* Define the full class record */
typedef struct _XmScaleClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmScaleClassPart scale_class;
} XmScaleClassRec;

/* Define the Constraint Resources */
typedef struct _XmScaleConstraintPart {
    int foo;
} XmScaleConstraintPart;

typedef struct _XmScaleConstraintRec {
    XmScaleConstraintPart scale;
} XmScaleConstraintRec, *XmScaleConstraints;

XMLIBEXPORT extern XmScaleClassRec xmScaleClassRec;

#ifdef __cplusplus
}
#endif

#endif /* _XM_SCALEP_H */
