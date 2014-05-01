/**
 *
 * $Id: CascadeBGP.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
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

#ifndef _XM_CASCADEBGP_H
#define _XM_CASCADEBGP_H

#include <Xm/CascadeBG.h>
#include <Xm/LabelGP.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * arrow cache record
 */
typedef struct _XmArrowPixmap {
    Dimension height, width;
    unsigned int depth;
    Pixel top_shadow_color, bottom_shadow_color, foreground_color;
    Display *display;
    Screen *screen;
    Pixmap pixmap;
} XmArrowPixmap;

/*
 * gadget class and instance records
 */
typedef struct _XmCascadeButtonGCacheObjClassPart {
    int foo;
} XmCascadeButtonGCacheObjClassPart;

typedef struct _XmCascadeButtonGCacheObjClassRec {
    ObjectClassPart object_class;
    XmExtClassPart ext_class;
    XmLabelGCacheObjClassPart label_class_cache;
    XmCascadeButtonGCacheObjClassPart cascade_button_class_cache;
} XmCascadeButtonGCacheObjClassRec;

XMLIBEXPORT extern XmCascadeButtonGCacheObjClassRec xmCascadeButtonGCacheObjClassRec;

typedef struct _XmCascadeButtonGCacheObjPart {
    Pixmap cascade_pixmap;
    int map_delay;
    Pixmap armed_pixmap;
} XmCascadeButtonGCacheObjPart;

typedef struct _XmCascadeButtonGCacheObjRec {
    ObjectPart object;
    XmExtPart ext;
    XmLabelGCacheObjPart label_cache;
    XmCascadeButtonGCacheObjPart cascade_button_cache;
} XmCascadeButtonGCacheObjRec;


/*
 * cascade button class and instance records
 */
typedef struct {
    XtPointer extension;
} XmCascadeButtonGadgetClassPart;

typedef struct _XmCascadeButtonGadgetClassRec {
    RectObjClassPart rect_class;
    XmGadgetClassPart gadget_class;
    XmLabelGadgetClassPart label_class;
    XmCascadeButtonGadgetClassPart cascade_button_class;
} XmCascadeButtonGadgetClassRec;

XMLIBEXPORT extern XmCascadeButtonGadgetClassRec xmCascadeButtonGadgetClassRec;

typedef struct {
    Widget submenu;
    XtCallbackList activate_callback;
    XtCallbackList cascade_callback;

    Boolean armed;
    XRectangle cascade_rect;
    int map_delay;
    XtIntervalId timer;

    XmCascadeButtonGCacheObjPart *cache;
} XmCascadeButtonGadgetPart;

/* Define the full instance record */
typedef struct _XmCascadeButtonGadgetRec {
    ObjectPart object;
    RectObjPart rectangle;
    XmGadgetPart gadget;
    XmLabelGadgetPart label;
    XmCascadeButtonGadgetPart cascade_button;
} XmCascadeButtonGadgetRec;

/*
 * easy access macros
 */
#define XmCBG_ARMED_BIT       (1 << 0)
#define XmCBG_TRAVERSE_BIT    (1 << 1)

#define CBG_Submenu(cb) \
    (((XmCascadeButtonGadget)cb)->cascade_button.submenu)

#define CBG_ActivateCall(cb) \
    (((XmCascadeButtonGadget)(cb))->cascade_button.activate_callback)

#define CBG_CascadeCall(cb) \
    (((XmCascadeButtonGadget)(cb))->cascade_button.cascade_callback)

#define CBG_Armed(cb) \
    (((XmCascadeButtonGadget)(cb))->cascade_button.armed)

#define CBG_CascadeRect(cb) \
    (((XmCascadeButtonGadget)(cb))->cascade_button.cascade_rect)

#define CBG_Timer(cb) \
    (((XmCascadeButtonGadget)(cb))->cascade_button.timer)

#define CBG_Cascade_x(cb) \
    (((XmCascadeButtonGadget)(cb))->cascade_button.cascade_rect.x)

#define CBG_Cascade_y(cb) \
    (((XmCascadeButtonGadget)(cb))->cascade_button.cascade_rect.y)

#define CBG_Cascade_width(cb) \
    (((XmCascadeButtonGadget)(cb))->cascade_button.cascade_rect.width)

#define CBG_Cascade_height(cb) \
    (((XmCascadeButtonGadget)(cb))->cascade_button.cascade_rect.height)

#define CBG_HasCascade(cb) \
    (((LabG_MenuType(cb) == XmMENU_PULLDOWN)  || \
      (LabG_MenuType(cb) == XmMENU_POPUP) ||     \
      (LabG_MenuType(cb) == XmMENU_OPTION)) &&  (CBG_Submenu(cb)))

#define CBG_IsArmed(cb) \
    (((XmCascadeButtonGadget)(cb))->cascade_button.armed & XmCBG_ARMED_BIT)

#define CBG_Traversing(cb) \
    (((XmCascadeButtonGadget)(cb))->cascade_button.armed & XmCBG_TRAVERSE_BIT)

#define CBG_SetBit(byte,bit,v) \
    byte = (byte & (~bit)) | (v ? bit : 0)

#define CBG_SetArmed(cb,v) \
    CBG_SetBit(((XmCascadeButtonGadget)(cb))->cascade_button.armed, \
                                      XmCBG_ARMED_BIT, v)

#define CBG_SetTraverse(cb,v) \
    CBG_SetBit(((XmCascadeButtonGadget)(cb))->cascade_button.armed, \
                                      XmCBG_TRAVERSE_BIT, v)

#define CBG_CascadePixmap(cb) \
    (((XmCascadeButtonGadget)cb)->cascade_button.cache->cascade_pixmap)

#define CBG_MapDelay(cb) \
    (((XmCascadeButtonGadget)cb)->cascade_button.cache->map_delay)

#define CBG_ArmedPixmap(cb) \
    (((XmCascadeButtonGadget)cb)->cascade_button.cache->armed_pixmap)

#define CBG_Cache(w) \
    (((XmCascadeButtonGadget)(w))->cascade_button.cache)

#define CBG_ClassCachePart(w) \
    (((XmCascadeButtonGadgetClass)xmCascadeButtonGadgetClass)->gadget_class.cache_part)

extern int _XmCascadeBCacheCompare(XtPointer A, XtPointer B);
extern void _XmArrowPixmapCacheDelete(XtPointer data);
extern void _XmCreateArrowPixmaps(Widget wid);

#ifdef __cplusplus
}
#endif

#endif /* _XM_CASCADEBGP_H */
