/**
 *
 * $Id: ScrollBarP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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

#ifndef _XM_SCROLLBARP_H
#define _XM_SCROLLBARP_H

#include <Xm/ScrollBar.h>
#include <Xm/PrimitiveP.h>

#define MIN_SLIDER_THICKNESS	1
#define MIN_SLIDER_LENGTH	6

#ifdef __cplusplus
extern "C" {
#endif

/* define the instance record */
typedef struct {
    int value;
    int minimum;
    int maximum;
    int slider_size;

    unsigned char orientation;
    unsigned char processing_direction;
    Boolean show_arrows;

    int increment;
    int page_increment;

    int initial_delay;
    int repeat_delay;

    XtCallbackList value_changed_callback;
    XtCallbackList increment_callback;
    XtCallbackList decrement_callback;
    XtCallbackList page_increment_callback;
    XtCallbackList page_decrement_callback;
    XtCallbackList to_top_callback;
    XtCallbackList to_bottom_callback;
    XtCallbackList drag_callback;

    /* Obsolete M*tif compatability */
    GC unhighlight_GC;
    /********************************/

    GC foreground_GC;
    Pixel trough_color;

    /* private instance variables... don't look here for a good time*/
    Pixmap pixmap;
    Boolean sliding_on;
    Boolean etched_slider;
    int saved_value;

    unsigned char flags;

    unsigned char change_type;
    XtIntervalId timer;

    short initial_x;
    short initial_y;
    short separation_x;
    short separation_y;

    short slider_x;
    short slider_y;
    short slider_width;
    short slider_height;

    short slider_area_x;
    short slider_area_y;
    short slider_area_width;
    short slider_area_height;

#if 0
/* rws 5 Feb 1998
   changed as per Gilles Seguin report (I also checked)
*/
    int arrow1_x;
    int arrow1_y;
#else
    short arrow1_x;
    short arrow1_y;
#endif
    unsigned char arrow1_orientation;
    Boolean arrow1_selected;
    
#if 0
/* rws 5 Feb 1998
   changed as per Gilles Seguin report (I also checked)
*/
    int arrow2_x;
    int arrow2_y;
#else
    short arrow2_x;
    short arrow2_y;
#endif
    unsigned char arrow2_orientation;
    Boolean arrow2_selected;

#if 0
/* rws 5 Feb 1998
   changed as per Gilles Seguin report (I also checked)
*/
    int arrow_width;
    int arrow_height;
#else
    short arrow_width;
    short arrow_height;
#endif

    /* obsolescent fields that M*tif has, but apparently doesn't use */
    short arrow1_top_count;
    short arrow1_cent_count;
    short arrow1_bot_count;

    XRectangle * arrow1_top;
    XRectangle * arrow1_cent;
    XRectangle * arrow1_bot;

    short arrow2_top_count;
    short arrow2_cent_count;
    short arrow2_bot_count;

    XRectangle * arrow2_top;
    XRectangle * arrow2_cent;
    XRectangle * arrow2_bot;
    /***********/

    GC unavailable_GC;
} XmScrollBarPart;

/* define the full instance record */
typedef struct _XmScrollBarRec {
    CorePart core;
    XmPrimitivePart primitive;
    XmScrollBarPart scrollBar;
} XmScrollBarRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmScrollBarClassPart;

/* define the full class record */

typedef struct _XmScrollBarClassRec {
    CoreClassPart core_class;
    XmPrimitiveClassPart primitive_class;
    XmScrollBarClassPart scrollBar_class;
} XmScrollBarClassRec;

/* External definition for class record */

XMLIBEXPORT extern XmScrollBarClassRec xmScrollBarClassRec;

/* Private function declaration */

XMLIBEXPORT extern void _XmSetEtchedSlider(XmScrollBarWidget);

#ifdef __cplusplus
}
#endif

#endif /* _XM_SCROLLBARP_H */
