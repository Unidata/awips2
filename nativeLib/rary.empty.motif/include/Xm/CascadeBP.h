/**
 *
 * $Id: CascadeBP.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
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


#ifndef _XM_CASCADEBP_H
#define _XM_CASCADEBP_H

#include <Xm/CascadeB.h>
#include <Xm/LabelP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    XtCallbackList activate_callback;
    XtCallbackList cascade_callback;

    Widget submenu;
    Pixmap cascade_pixmap;
    int map_delay;

    Boolean armed;
    XRectangle cascade_rect;
    XtIntervalId timer;
    Pixmap armed_pixmap;
} XmCascadeButtonPart;

/* Define the full instance record */
typedef struct _XmCascadeButtonRec {
    CorePart core;
    XmPrimitivePart primitive;
    XmLabelPart label;
    XmCascadeButtonPart cascade_button;
} XmCascadeButtonRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmCascadeButtonClassPart;

/* Define the full class record */
typedef struct _XmCascadeButtonClassRec {
    CoreClassPart core_class;
    XmPrimitiveClassPart primitive_class;
    XmLabelClassPart label_class;
    XmCascadeButtonClassPart cascade_button_class;
} XmCascadeButtonClassRec;

/* External definition for the class record */

XMLIBEXPORT extern XmCascadeButtonClassRec xmCascadeButtonClassRec;

/*
 * easy access macros
 */
#define XmCB_ARMED_BIT        (1 << 0)
#define XmCB_TRAVERSE_BIT     (1 << 1)


#define CB_Submenu(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.submenu)

#define CB_ActivateCall(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.activate_callback)

#define CB_CascadeCall(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.cascade_callback)

#define CB_CascadePixmap(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.cascade_pixmap)

#define CB_ArmedPixmap(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.armed_pixmap)

#define CB_Cascade_x(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.cascade_rect.x)

#define CB_Cascade_y(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.cascade_rect.y)

#define CB_Cascade_width(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.cascade_rect.width)

#define CB_Cascade_height(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.cascade_rect.height)

#define CB_HasCascade(cb) \
    (((Lab_MenuType(cb) == XmMENU_PULLDOWN)  ||  \
      (Lab_MenuType(cb) == XmMENU_POPUP)) && (CB_Submenu(cb)))

#define CB_IsArmed(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.armed & XmCB_ARMED_BIT)

#define CB_Traversing(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.armed & XmCB_TRAVERSE_BIT)

#define CB_SetBit(byte,bit,v) \
    byte = (byte & (~bit)) | (v ? bit : 0)

#define CB_SetArmed(cb,v) \
    CB_SetBit(((XmCascadeButtonWidget)(cb))->cascade_button.armed, \
                                      XmCB_ARMED_BIT, v)

#define CB_SetTraverse(cb,v) \
    CB_SetBit(((XmCascadeButtonWidget)(cb))->cascade_button.armed, \
                                      XmCB_TRAVERSE_BIT, v)

/*
 * functions
 */
void _XmCBHelp(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmCascadingPopup(Widget cb, XEvent *event, Boolean doCascade);

#ifdef __cplusplus
}
#endif

#endif /* _XM_CASCADEBP_H */
