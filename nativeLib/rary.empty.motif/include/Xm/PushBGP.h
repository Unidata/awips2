/**
 *
 * $Id: PushBGP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_PUSHBGP_H
#define _XM_PUSHBGP_H

#include <Xm/PushBG.h>
#include <Xm/LabelGP.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Gadget cache class and instance records
 */
typedef struct _XmPushButtonGCacheObjClassPart {
    int foo;
} XmPushButtonGCacheObjClassPart;

typedef struct _XmPushButtonGCacheObjClassRec {
    ObjectClassPart object_class;
    XmExtClassPart ext_class;
    XmLabelGCacheObjClassPart label_class_cache;
    XmPushButtonGCacheObjClassPart pushbutton_class_cache;
} XmPushButtonGCacheObjClassRec;

XMLIBEXPORT extern XmPushButtonGCacheObjClassRec xmPushButtonGCacheObjClassRec;

typedef struct _XmPushButtonGCacheObjPart {
    Boolean fill_on_arm;
    Pixel arm_color;
    Pixmap arm_pixmap;
    Pixmap unarm_pixmap;
    unsigned char multiClick;
    Dimension default_button_shadow_thickness;

    GC fill_gc;
    GC background_gc;
    XtIntervalId timer;
} XmPushButtonGCacheObjPart;

typedef struct _XmPushButtonGCacheObjRec {
    ObjectPart object;
    XmExtPart ext;
    XmLabelGCacheObjPart label_cache;
    XmPushButtonGCacheObjPart pushbutton_cache;
} XmPushButtonGCacheObjRec;


/*
 * pushbutton gadget class and instance records
 */
/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmPushButtonGadgetClassPart;

/* Define the full class record */
typedef struct _XmPushButtonGadgetClassRec {
    RectObjClassPart rect_class;
    XmGadgetClassPart gadget_class;
    XmLabelGadgetClassPart label_class;
    XmPushButtonGadgetClassPart pushbutton_class;
} XmPushButtonGadgetClassRec;

XMLIBEXPORT extern XmPushButtonGadgetClassRec xmPushButtonGadgetClassRec;

typedef struct {
    XtCallbackList activate_callback;
    XtCallbackList arm_callback;
    XtCallbackList disarm_callback;

    Dimension show_as_default;
    Boolean armed;
    int click_count;

    Boolean compatible; /* weird motif compatibility flag */

    XmPushButtonGCacheObjPart *cache;
} XmPushButtonGadgetPart;

/* Define the full instance record */
typedef struct _XmPushButtonGadgetRec {
    ObjectPart object;
    RectObjPart rectangle;
    XmGadgetPart gadget;
    XmLabelGadgetPart label;
    XmPushButtonGadgetPart pushbutton;
} XmPushButtonGadgetRec;


#define PBG_ActivateCallback(w) \
    (((XmPushButtonGadget)(w))->pushbutton.activate_callback)

#define PBG_ArmCallback(w) \
    (((XmPushButtonGadget)(w))->pushbutton.arm_callback)

#define PBG_DisarmCallback(w) \
    (((XmPushButtonGadget)(w))->pushbutton.disarm_callback)

#define PBG_Armed(w) \
    (((XmPushButtonGadget)(w))->pushbutton.armed)

#define PBG_ClickCount(w) \
    (((XmPushButtonGadget)(w))->pushbutton.click_count)

#define PBG_Compatible(w) \
    (((XmPushButtonGadget)(w))->pushbutton.compatible)

#define PBG_ShowAsDefault(w) \
    (((XmPushButtonGadget)(w))->pushbutton.show_as_default)

#define PBG_FillOnArm(w) \
    (((XmPushButtonGadget)(w))->pushbutton.cache->fill_on_arm)

#define PBG_ArmColor(w) \
    (((XmPushButtonGadget)(w))->pushbutton.cache->arm_color)

#define PBG_FillGc(w) \
    (((XmPushButtonGadget)(w))->pushbutton.cache->fill_gc)

#define PBG_BackgroundGc(w) \
    (((XmPushButtonGadget)(w))->pushbutton.cache->background_gc)

#define PBG_Timer(w) \
    (((XmPushButtonGadget)(w))->pushbutton.cache->timer)

#define PBG_ArmPixmap(w) \
    (((XmPushButtonGadget)(w))->pushbutton.cache->arm_pixmap)

#define PBG_UnarmPixmap(w) \
    (((XmPushButtonGadget)(w))->pushbutton.cache->unarm_pixmap)

#define PBG_MultiClick(w) \
    (((XmPushButtonGadget)(w))->pushbutton.cache->multiClick)

#define PBG_DefaultButtonShadowThickness(w) \
    (((XmPushButtonGadget)(w))->pushbutton.cache->default_button_shadow_thickness)

#define PBG_Cache(w) \
    (((XmPushButtonGadget)(w))->pushbutton.cache)

#define PBG_ClassCachePart(w) \
    (((XmPushButtonGadgetClass)xmPushButtonGadgetClass)->gadget_class.cache_part)

/* weird Motif backwards compatibility function.  I don't care if it ever
 * gets implemented or not */
XMLIBEXPORT extern void _XmClearBGCompatibility(Widget pbg);

XMLIBEXPORT extern int _XmPushBCacheCompare(XtPointer A, XtPointer B);

#ifdef __cplusplus
}
#endif

#endif /* _XM_PUSHBP_H */
