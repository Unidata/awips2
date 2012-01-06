/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/ArrowBP.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
 * 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

#ifndef _XM_ARROWBP_H
#define _XM_ARROWBP_H

#include <Xm/ArrowB.h>
#include <Xm/PrimitiveP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    XtCallbackList activate_callback;
    XtCallbackList arm_callback;
    XtCallbackList disarm_callback;
    unsigned char direction;

    Boolean selected;

    short top_count;
    short cent_count;
    short bot_count;
    XRectangle *top;
    XRectangle *cent;
    XRectangle *bot;

    GC arrow_GC;
    XtIntervalId timer;
    unsigned char multiClick;
    int click_count;
    Time armTimeStamp;
    GC insensitive_GC;

    Dimension detail_shadow_thickness; /* new for 2.0 */

} XmArrowButtonPart;

/* Define the full instance record */
typedef struct _XmArrowButtonRec {
    CorePart core;
    XmPrimitivePart primitive;
    XmArrowButtonPart arrowbutton;
} XmArrowButtonRec;

/* Define class part structure */
typedef struct _XmArrowButtonClassPart {
    XtPointer extension;
} XmArrowButtonClassPart;

/* Define the full class record */
typedef struct _XmArrowButtonClassRec {
    CoreClassPart core_class;
    XmPrimitiveClassPart primitive_class;
    XmArrowButtonClassPart arrowbutton_class;
} XmArrowButtonClassRec;

/* External definition for the class record */

XMLIBEXPORT extern XmArrowButtonClassRec xmArrowButtonClassRec;

#ifdef __cplusplus
}
#endif

#endif /* _XM_ARROWBP_H */
