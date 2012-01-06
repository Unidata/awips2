/**
 *
 * $Id: TearOffBP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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

#ifndef _XM_TEAROFFBP_H
#define _XM_TEAROFFBP_H

#include <Xm/PushBP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _XmTearOffButtonClassPart {
    String translations;
} XmTearOffButtonClassPart;

typedef struct _XmTearOffButtonClassRec {
    CoreClassPart core_class;
    XmPrimitiveClassPart primitive_class;
    XmLabelClassPart label_class;
    XmPushButtonClassPart pushbutton_class;
    XmTearOffButtonClassPart tearoffbutton_class;
} XmTearOffButtonClassRec;
   
typedef struct _XmTearOffButtonClassRec *XmTearOffButtonWidgetClass;
   
XMLIBEXPORT extern XmTearOffButtonClassRec xmTearOffButtonClassRec;
   
typedef struct {
    Dimension margin;
    unsigned char orientation;
    unsigned char separator_type;
    GC separator_GC;
} XmTearOffButtonPart;
   
typedef struct _XmTearOffButtonRec {
    CorePart core;
    XmPrimitivePart primitive;
    XmLabelPart label;
    XmPushButtonPart pushbutton;
    XmTearOffButtonPart tear_off_button;
} XmTearOffButtonRec;

typedef struct _XmTearOffButtonRec *XmTearOffButtonWidget;

XMLIBEXPORT extern WidgetClass xmTearOffButtonWidgetClass;
   
#ifndef XmIsTearOffButton
#define XmIsTearOffButton(w) XtIsSubclass(w, xmTearOffButtonWidgetClass)
#endif
     
#ifdef __cplusplus
}
#endif

#endif /* _XM_TEAROFFBP_H */
