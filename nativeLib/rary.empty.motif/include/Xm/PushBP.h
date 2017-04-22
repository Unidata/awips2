/**
 *
 * $Id: PushBP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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


#ifndef _XM_PUSHBP_H
#define _XM_PUSHBP_H

#include <Xm/PushB.h>
#include <Xm/LabelP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    Boolean fill_on_arm;
    Dimension show_as_default;
    Pixel arm_color;
    Pixmap arm_pixmap;
    XtCallbackList activate_callback;
    XtCallbackList arm_callback;
    XtCallbackList disarm_callback;
    
    Boolean armed;
    Pixmap unarm_pixmap;
    GC fill_gc;
    GC background_gc;
    XtIntervalId timer;
    unsigned char multiClick;
    int click_count;
    Time armTimeStamp;
    Boolean compatible;
    Dimension default_button_shadow_thickness;
} XmPushButtonPart;

/* Define the full instance record */
typedef struct _XmPushButtonRec {
    CorePart core;
    XmPrimitivePart primitive;
    XmLabelPart label;
    XmPushButtonPart pushbutton;
} XmPushButtonRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmPushButtonClassPart;

/* Define the full class record */
typedef struct _XmPushButtonClassRec {
    CoreClassPart core_class;
    XmPrimitiveClassPart primitive_class;
    XmLabelClassPart label_class;
    XmPushButtonClassPart pushbutton_class;
} XmPushButtonClassRec;

/* External definition for the class record */

XMLIBEXPORT extern XmPushButtonClassRec xmPushButtonClassRec;

void _XmClearBCompatibility(Widget pb);

#ifdef __cplusplus
}
#endif

#endif /* _XM_PUSHB_P_H */
