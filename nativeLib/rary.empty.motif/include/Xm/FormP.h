/** 
 *
 * $Id: FormP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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


#ifndef _XM_FORMP_H
#define _XM_FORMP_H

#include <Xm/XmP.h>
#include <Xm/Form.h>
#include <Xm/BulletinBP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Frankly I don't know where these come from.
 * They don't seem to be present in the OSF/Motif docs anywhere,
 * nor in anything else I found.
 */
#if 0
#define SORTED_LEFT_TO_RIGHT    (1 << 0)
#define SORTED_TOP_TO_BOTTOM    (1 << 1)
#define DONT_USE_BOTTOM         (1 << 2)
#define DONT_USE_RIGHT          (1 << 3)
#define WIDGET_DONT_USE_BOTTOM  (1 << 4)
#define WIDGET_DONT_USE_RIGHT   (1 << 5)
#define LEFT_NOT_MOVABLE        (1 << 6)
#define RIGHT_NOT_MOVABLE       (1 << 7)
#define TOP_NOT_MOVABLE         (1 << 8)
#define BOTTOM_NOT_MOVABLE      (1 << 9)
#define NOT_MOVABLE_RIGHT       (1 << 10)
#define NOT_MOVABLE_DOWN        (1 << 11)
#define DONT_ATTEMPT_MOVE_RIGHT (1 << 12)
#define DONT_ATTEMPT_MOVE_DOWN  (1 << 13)
#endif

/* Define the form instance part */
typedef struct {
    Dimension horizontal_spacing;
    Dimension vertical_spacing;
    int fraction_base;
    Boolean rubber_positioning;
    Widget first_child;
    Boolean initial_width, initial_height;
    Boolean processing_constraints;	   /* rws 22 Dec 1999
    					      constraint_set_values _always_
    					      forces a change in geometry. Xt
    					      picks this up by calling
    					      geometry_manger. This flag is
    					      set in constraint_set_values and
    					      clear when we hit
    					      geometry_manager. It's purpose
    					      seems to be to avoid setting
    					      the preferred width/height.
    					    */
} XmFormPart;

/* define the full instance record */
typedef struct _XmFormRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmBulletinBoardPart bulletin_board;
    XmFormPart form;
} XmFormRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmFormClassPart;

/* Define the full class record */
typedef struct _XmFormClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmBulletinBoardClassPart bulletin_board_class;
    XmFormClassPart form_class;
} XmFormClassRec;

typedef struct _XmFormAttachmentRec
{
    unsigned char type;
    Widget w;
    int percent;	/* rws 22 Dec 1999
    			   The XmN...Position value
    			 */
    int offset;
    int value;		/* rws 22 Dec 1999
    			   TOP    is the top y value
    			   BOTTOM is the bottom y value
    			   LEFT   is the left x value
    			   RIGHT  is the right x value
    			   Currently LessTif is not using these correctly!!!
    			   BOTTOM is the height + borders and
    			   RIGHT  is the width + borders
    			   form/test56 will have to be corrected if we ever
    			   fix this. This will entail _major_ editing of
    			   _XmFormLayout....
    			 */
    int tempValue;	/* rws 22 Dec 1999
    			   I haven't got a clue!!!!
    			   LessTif is using this to calculate the overall
    			   size of a series of XmATTACH_WIDGET's
    			 */
} XmFormAttachmentRec, *XmFormAttachment;

/* Define the Constraint Resources */
typedef struct _XmFormConstraintPart {
    XmFormAttachmentRec	att[4];
    Widget              next_sibling;	/* rws 22 Dec 1999
    					   I suspect that Motif is sorting
    					   the kids, left to right, top to
    					   bottom, to help lay them out.
    					   LessTif is not currently using
    					   this.
    					 */
    Boolean		sorted;		/* rws 22 Dec 1999
    					   related to the above.
    					 */
    Boolean		resizable;
    Dimension		preferred_width; /* rws 22 Dec 1999                 */
    Dimension		preferred_height;/* the size that this widget wants
    					  * to be. (very complicated)
    					  */
#if 0
/* lesstif */
    Position		x, y;
    int			w, h; /* These are not Dimension; can be negative */
    Boolean		width_from_side, height_from_side;	/* See Form.c */
/* lesstif */
#endif
} XmFormConstraintPart, *XmFormConstraint;

typedef struct _XmFormConstraintRec {
    XmManagerConstraintPart manager;
    XmFormConstraintPart form;
} XmFormConstraintRec, *XmFormConstraints, *XmFormConstraintPtr;

XMLIBEXPORT extern XmFormClassRec xmFormClassRec;

#ifdef __cplusplus
}
#endif

#endif /* _XM_FORMP_H */
