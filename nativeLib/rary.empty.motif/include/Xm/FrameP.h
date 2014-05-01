/** 
 *
 * $Id: FrameP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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


#ifndef _XM_FRAMEP_H
#define _XM_FRAMEP_H

#include <Xm/Frame.h>
#include <Xm/ManagerP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Define the frame instance part */
typedef struct {
    Dimension margin_width;
    Dimension margin_height;
    unsigned char shadow_type;

    /* private instance variables , don't look here */
    Dimension old_width;
    Dimension old_height;
    Dimension old_shadow_thickness;
    Position old_shadow_x;
    Position old_shadow_y;

    Widget work_area;
    Widget title_area;
    Boolean processing_constraints;
} XmFramePart;

/* define the full instance record */
typedef struct _XmFrameRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmFramePart frame;
} XmFrameRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmFrameClassPart;

/* Define the full class record */
typedef struct _XmFrameClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmFrameClassPart frame_class;
} XmFrameClassRec;

/* Define the Constraint Resources */
typedef struct _XmFrameConstraintPart {
    int unused;
    unsigned char child_type;
    unsigned char child_h_alignment;
    Dimension child_h_spacing;
    unsigned char child_v_alignment;
} XmFrameConstraintPart, *XmFrameContraint;

typedef struct _XmFrameConstraintRec {
    XmManagerConstraintPart manager;
    XmFrameConstraintPart frame;
} XmFrameConstraintRec, *XmFrameConstraintPtr;

XMLIBEXPORT extern XmFrameClassRec xmFrameClassRec;

#ifdef __cplusplus
}
#endif

#endif /* _XM_FRAMEP_H */
