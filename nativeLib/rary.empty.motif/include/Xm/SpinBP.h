/**
 * 
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/SpinBP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright © 1997, 2000, 2001 LessTif Development Team
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

#ifndef _XM_SPINBP_H
#define _XM_SPINBP_H

#include <Xm/LabelP.h>
#include <Xm/ManagerP.h>
#include <Xm/SpinB.h>

#ifdef __cplusplus
extern "C" {
#endif

/* SpinBox constraints */
typedef struct _XmSpinBoxConstraint
{
    XmStringTable values;
    int position;
    int num_values;
    int minimum_value;
    int maximum_value;
    int increment_value;
    short decimal_points;
    unsigned char sb_child_type;
    unsigned char arrow_sensitivity;
    unsigned char position_type;
} XmSpinBoxConstraintPart, *XmSpinBoxConstraint;

typedef struct _XmSpinBoxConstraintRec
{
    XmManagerConstraintPart manager;
    XmSpinBoxConstraintPart spinBox;
} XmSpinBoxConstraintRec, *XmSpinBoxConstraintPtr;

/* SpinBox class structure */
typedef struct _XmSpinBoxClassPart
{
    XtPointer extension;
} XmSpinBoxClassPart;

typedef struct _XmSpinBoxClassRec
{
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmSpinBoxClassPart spinBox_class;
} XmSpinBoxClassRec;

XMLIBEXPORT extern XmSpinBoxClassRec xmSpinBoxClassRec;

/* SpinBox instance record */
typedef struct _XmSpinBoxPart
{
    Dimension arrow_size;
    Dimension margin_width;
    Dimension margin_height;
    Dimension spacing;
    unsigned int initial_delay;
    unsigned int repeat_delay;
    XtCallbackList modify_verify_cb;
    XtCallbackList value_changed_cb;
    XRectangle up_arrow_rect;
    XRectangle down_arrow_rect;
    Widget textw;
    int boundary;
    int last_hit;
    int make_change;
    Dimension ideal_height;
    Dimension ideal_width;
    GC arrow_gc;
    GC insensitive_gc;
    Mask dim_mask;
    XtIntervalId spin_timer;
    Boolean up_arrow_pressed;
    Boolean down_arrow_pressed;
    unsigned char arrow_layout;
    unsigned char default_arrow_sensitivity;
    Dimension detail_shadow_thickness;
    unsigned char arrow_orientation;
} XmSpinBoxPart;

/* Widget's instance record declaration */
typedef struct _XmSpinBoxRec 
{
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmSpinBoxPart spinBox;
} XmSpinBoxRec;

/* Access macros */
/* For SpinBox resources */
#define	SPB_ArrowSize(w)		(((XmSpinBoxWidget)(w))->spinBox.arrow_size)
#define	SPB_MarginWidth(w)		(((XmSpinBoxWidget)(w))->spinBox.margin_width)
#define	SPB_MarginHeight(w)		(((XmSpinBoxWidget)(w))->spinBox.margin_height)
#define	SPB_Spacing(w)			(((XmSpinBoxWidget)(w))->spinBox.spacing)
#define	SPB_InitialDelay(w)		(((XmSpinBoxWidget)(w))->spinBox.initial_delay)
#define	SPB_RepeatDelay(w)		(((XmSpinBoxWidget)(w))->spinBox.repeat_delay)
#define	SPB_ModifyVerifyCB(w) (((XmSpinBoxWidget)(w))->spinBox.modify_verify_cb)
#define	SPB_ValueChangedCB(w) (((XmSpinBoxWidget)(w))->spinBox.value_changed_cb)
#define	SPB_TextW(w)			(((XmSpinBoxWidget)(w))->spinBox.textw)
#define	SPB_TimerId(w)			(((XmSpinBoxWidget)(w))->spinBox.spin_timer)
#define	SPB_DefaultSensitivity(w) \
				(((XmSpinBoxWidget)(w))->spinBox.default_arrow_sensitivity)
#define	SPB_DetailShadowThickness(w) \
				(((XmSpinBoxWidget)(w))->spinBox.detail_shadow_thickness)

/* Arrows */
#define	SPB_UpRect(w)		(((XmSpinBoxWidget)(w))->spinBox.up_arrow_rect)
#define	SPB_DownRect(w)		(((XmSpinBoxWidget)(w))->spinBox.down_arrow_rect)
#define	SPB_UpX(w)				SPB_UpRect(w).x
#define	SPB_UpY(w)				SPB_UpRect(w).y
#define	SPB_UpWidth(w)			SPB_UpRect(w).width
#define	SPB_UpHeight(w)			SPB_UpRect(w).height
#define	SPB_DownX(w)			SPB_DownRect(w).x
#define	SPB_DownY(w)			SPB_DownRect(w).y
#define	SPB_DownWidth(w)		SPB_DownRect(w).width
#define	SPB_DownHeight(w)		SPB_DownRect(w).height
#define	SPB_ArrowLayout(w)		(((XmSpinBoxWidget)(w))->spinBox.arrow_layout)
#define	SPB_ArrowOrientation(w)		(((XmSpinBoxWidget)(w))->spinBox.arrow_orientation)

#define	SPB_UpArrowPressed(w)	\
					(((XmSpinBoxWidget)(w))->spinBox.up_arrow_pressed)
#define	SPB_DownArrowPressed(w)	\
					(((XmSpinBoxWidget)(w))->spinBox.down_arrow_pressed)

/* For Constraint resources */
/* Note the SPBC_*() need a SpinBox child as argument. */

#define SPB_GetConstraintRec(w) \
    ((XmSpinBoxConstraint)(&((XmSpinBoxConstraintPtr) \
    (w)->core.constraints)->spinBox))

#define	SPBC_Minimum(w)				(SPB_GetConstraintRec(w)->minimum_value)
#define	SPBC_Maximum(w)				(SPB_GetConstraintRec(w)->maximum_value)
#define	SPBC_Increment(w)			(SPB_GetConstraintRec(w)->increment_value)
#define	SPBC_NumValues(w)			(SPB_GetConstraintRec(w)->num_values)
#define	SPBC_Values(w)				(SPB_GetConstraintRec(w)->values)
#define	SPBC_DecimalPoints(w)		(SPB_GetConstraintRec(w)->decimal_points)
#define	SPBC_Position(w)			(SPB_GetConstraintRec(w)->position)
#define	SPBC_ChildType(w)			(SPB_GetConstraintRec(w)->sb_child_type)
#define	SPBC_PositionType(w)			(SPB_GetConstraintRec(w)->position_type)
#define	SPBC_ArrowSensitivity(w)	(SPB_GetConstraintRec(w)->arrow_sensitivity)

/* Stuff to calculate things */

#define	SPBC_Numeric(w) \
	(SPB_GetConstraintRec(w)->sb_child_type == XmNUMERIC)

#define SPBC_CurrentValue(w)								\
    (SPB_GetConstraintRec(w)->minimum_value +				\
		(SPB_GetConstraintRec(w)->position *				\
			SPB_GetConstraintRec(w)->increment_value))

#define SPBC_MaxPosition(w) \
    (((w)->maximum_value - (w)->minimum_value) / (w)->increment_value)

#ifdef __cplusplus
}
#endif

#endif /* _XM_SPINBP_H */
