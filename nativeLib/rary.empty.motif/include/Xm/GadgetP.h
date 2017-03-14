/**
 *
 * $Id: GadgetP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_GADGETP_H
#define _XM_GADGETP_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* The list of event types which are
   selectable for input for a gadget */

#define XmNO_EVENT 0x000             /* No Events */
#define XmENTER_EVENT 0x001          /* Enter event */
#define XmLEAVE_EVENT 0x002          /* Leave event */
#define XmFOCUS_IN_EVENT 0x004       /* Focus In event */
#define XmFOCUS_OUT_EVENT 0x008      /* Focus Out event */
#define XmMOTION_EVENT 0x010         /* Button Motion event */
#define XmARM_EVENT 0x020            /* Button 1 Press event */
#define XmACTIVATE_EVENT 0x040       /* Button 1 Release event */
#define XmHELP_EVENT 0x080           /* Help (F1) key event */
#define XmKEY_EVENT 0x100            /* Any Key event */
#define XmMULTI_ARM_EVENT 0x200      /* Double Button 1 Press event */
#define XmMULTI_ACTIVATE_EVENT 0x400 /* Double Button 1 Release event */
#define XmBDRAG_EVENT 0x800          /* Button 2 Press event */
#define XmALL_EVENT 0xFFF            /* All the above events */

/*
 * cache stuff
 */
typedef struct _XmGadgetCache {
    struct _XmGadgetCache *next;
    struct _XmGadgetCache *prev;
    int ref_count;
} XmGadgetCache, *XmGadgetCachePtr;

typedef struct _XmCacheClassPart {
    XmGadgetCache cache_head;
    XmCacheCopyProc cache_copy;
    XmGadgetCacheProc cache_delete;
    XmCacheCompareProc cache_compare;
} XmCacheClassPart, *XmCacheClassPartPtr;

typedef struct _XmGadgetCacheRef {
    XmGadgetCache cache;
    XtArgVal data;
} XmGadgetCacheRef, *XmGadgetCacheRefPtr;

/* Define the gadget instance part */

typedef struct _XmGadgetPart {
    Dimension shadow_thickness;
    Dimension highlight_thickness;

    XtCallbackList help_callback;
    XtPointer user_data;

    Boolean traversal_on;
    Boolean highlight_on_enter;
    Boolean have_traversal;

    unsigned char unit_type;
    XmNavigationType navigation_type;

    Boolean highlight_drawn;
    Boolean highlighted;
    Boolean visible;

    Mask event_mask; /* the events about which this gadget wants to be notified */

    XmDirection layout_direction; /* new for 2.0 */

} XmGadgetPart;

/* Define the full instance record */
typedef struct _XmGadgetRec {
    ObjectPart object;
    RectObjPart rectangle;
    XmGadgetPart gadget;
} XmGadgetRec;

/*
 * Define class part structures
 */
typedef struct _XmGadgetClassExtRec {
    XtPointer next_extension;
    XrmQuark record_type;
    long version;
    Cardinal record_size;
    XmWidgetBaselineProc widget_baseline;
    XmWidgetDisplayRectProc widget_display_rect;

    XmWidgetMarginsProc widget_margins; /* new for 2.0 */

} XmGadgetClassExtRec, *XmGadgetClassExt;

#define XmGadgetClassExtVersion 1L

#define GCEPTR(wc) \
     ((XmGadgetClassExt *)(&(((XmGadgetClass)(wc))->gadget_class.extension)))
#define _XmGetGadgetClassExtPtr(wc, q) \
    ((*GCEPTR(wc) && (((*GCEPTR(wc))->record_type) == (q))) \
	? GCEPTR(wc) \
	: ((XmGadgetClassExt *) \
	   _XmGetClassExtensionPtr(((XmGenericClassExt *)GCEPTR(wc)), (q))))

typedef struct {
    XtWidgetProc border_highlight;
    XtWidgetProc border_unhighlight;
    XtActionProc arm_and_activate;
    XmWidgetDispatchProc input_dispatch;
    XmVisualChangeProc visual_change;
    XmSyntheticResource *syn_resources;
    int num_syn_resources;
    XmCacheClassPartPtr cache_part;
    XtPointer extension;
} XmGadgetClassPart;

/* Define the full class record */
typedef struct _XmGadgetClassRec {
    RectObjClassPart rect_class;
    XmGadgetClassPart gadget_class;
} XmGadgetClassRec;

/* External definition for class record */
XMLIBEXPORT extern XmGadgetClassRec xmGadgetClassRec;

#define G_ShadowThickness(g) \
       (((XmGadget)(g))->gadget.shadow_thickness)

#define G_HighlightThickness(g) \
       (((XmGadget)(g))->gadget.highlight_thickness)

/*
 * gadget functions
 */
void _XmBuildGadgetResources(WidgetClass c);

#ifdef __cplusplus
}
#endif

#endif /* _XM_GADGETP_H */
