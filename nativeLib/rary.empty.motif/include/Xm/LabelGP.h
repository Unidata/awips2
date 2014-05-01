/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/LabelGP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2000, 2002 LessTif Development Team
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

#ifndef _XM_LABELGP_H
#define _XM_LABELGP_H

#include <Xm/LabelG.h>
#include <Xm/GadgetP.h>
#include <Xm/ExtObjectP.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * cache class and instance record
 */
typedef struct _XmLabelGCacheObjClassPart {
	int foo;
} XmLabelGCacheObjClassPart;

typedef struct _XmLabelGCacheObjClassRec {
	ObjectClassPart object_class;
	XmExtClassPart ext_class;
	XmLabelGCacheObjClassPart label_class_cache;
} XmLabelGCacheObjClassRec;

XMLIBEXPORT extern XmLabelGCacheObjClassRec xmLabelGCacheObjClassRec;

typedef struct _XmLabelGCacheObjPart {
	unsigned char label_type;
	unsigned char alignment;
	XmStringDirection string_direction;

	Dimension margin_height;
	Dimension margin_width;
	Dimension margin_left;
	Dimension margin_right;
	Dimension margin_top;
	Dimension margin_bottom;

	Boolean recompute_size;

	Boolean skipCallback;

	unsigned char menu_type;
} XmLabelGCacheObjPart;

typedef struct _XmLabelGCacheObjRec {
	ObjectPart object;
	XmExtPart ext;
	XmLabelGCacheObjPart label_cache;
} XmLabelGCacheObjRec;

/*
 * gadget class and instance record
 */
typedef struct _XmLabelGadgetClassPart {
	XtWidgetProc setOverrideCallback;
	XmMenuProc menuProcs;
	XtPointer extension;
} XmLabelGadgetClassPart;

/* Define the full class record */
typedef struct _XmLabelGadgetClassRec {
    RectObjClassPart rect_class;
    XmGadgetClassPart gadget_class;
    XmLabelGadgetClassPart label_class;
} XmLabelGadgetClassRec;

XMLIBEXPORT extern XmLabelGadgetClassRec xmLabelGadgetClassRec;

typedef struct _XmLabelGadgetPart {
	_XmString _label;
	_XmString _acc_text;
	KeySym mnemonic;
	String mnemonicCharset;
	String accelerator;
	XmFontList font;		/* both XmNfontList and XmNrenderTable */

	Pixmap pixmap;
	Pixmap pixmap_insen;

	/* private instance variables */
	GC normal_GC;
	GC insensitive_GC;

	XRectangle TextRect;
	XRectangle acc_TextRect;

	XmLabelGCacheObjPart *cache;

	/* 2.x resources */
	Boolean	check_set_render_table;
} XmLabelGadgetPart;

typedef struct _XmLabelGadgetRec {
	ObjectPart object;
	RectObjPart rectangle;
	XmGadgetPart gadget;
	XmLabelGadgetPart label;
} XmLabelGadgetRec;

/* pad between label text and accelerator text */
#define LABELG_ACC_PAD 15

/*
 * access macros
 */
#define LabG_LabelType(w) \
    (((XmLabelGadget)(w))->label.cache->label_type)

#define LabG_Alignment(w) \
    (((XmLabelGadget)(w))->label.cache->alignment)

#define LabG_StringDirection(w) \
    (((XmLabelGadget)(w))->label.cache->string_direction)

#define LabG_MarginHeight(w) \
    (((XmLabelGadget)(w))->label.cache->margin_height)

#define LabG_MarginWidth(w) \
    (((XmLabelGadget)(w))->label.cache->margin_width)

#define LabG_MarginLeft(w) \
    (((XmLabelGadget)(w))->label.cache->margin_left)

#define LabG_MarginRight(w) \
    (((XmLabelGadget)(w))->label.cache->margin_right)

#define LabG_MarginTop(w) \
    (((XmLabelGadget)(w))->label.cache->margin_top)

#define LabG_MarginBottom(w) \
    (((XmLabelGadget)(w))->label.cache->margin_bottom)

#define LabG_RecomputeSize(w) \
    (((XmLabelGadget)(w))->label.cache->recompute_size)

#define LabG_SkipCallback(w) \
    (((XmLabelGadget)(w))->label.cache->skipCallback)

#define LabG_MenuType(w) \
    (((XmLabelGadget)(w))->label.cache->menu_type)

#define LabG_Cache(w) \
    (((XmLabelGadget)(w))->label.cache)

#define LabG_ClassCachePart(w) \
    (((XmLabelGadgetClass)xmLabelGadgetClass)->gadget_class.cache_part)

#define LabG__label(w) \
       (((XmLabelGadget)(w))->label._label)

#define LabG__acceleratorText(w) \
       (((XmLabelGadget)(w))->label._acc_text)

#define LabG_Font(w) \
       (((XmLabelGadget)(w))->label.font)

#define LabG_Mnemonic(w) \
       (((XmLabelGadget)(w))->label.mnemonic)

#define LabG_MnemonicCharset(w) \
       (((XmLabelGadget)(w))->label.mnemonicCharset)

#define LabG_Accelerator(w) \
       (((XmLabelGadget)(w))->label.accelerator)

#define LabG_Pixmap(w) \
       (((XmLabelGadget)(w))->label.pixmap)

#define LabG_PixmapInsensitive(w) \
       (((XmLabelGadget)(w))->label.pixmap_insen)

#define LabG_NormalGC(w) \
       (((XmLabelGadget)(w))->label.normal_GC)

#define LabG_InsensitiveGC(w) \
       (((XmLabelGadget)(w))->label.insensitive_GC)

#define LabG_TextRect(w) \
       (((XmLabelGadget)(w))->label.TextRect)

#define LabG_AccTextRect(w) \
       (((XmLabelGadget)(w))->label.acc_TextRect)

#define LabG_TextRect_x(w) \
       (LabG_TextRect(w).x)

#define LabG_TextRect_y(w) \
       (LabG_TextRect(w).y)

#define LabG_TextRect_width(w) \
       (LabG_TextRect(w).width)

#define LabG_TextRect_height(w) \
       (LabG_TextRect(w).height)

#define LabG_IsText(w) \
       (LabG_LabelType(w) == XmSTRING)

#define LabG_IsPixmap(w) \
       (LabG_LabelType(w) == XmPIXMAP)

#define LabG_Shadow(w) \
       (((XmLabelGadget)(w))->gadget.shadow_thickness)

#define LabG_Highlight(w) \
       (((XmLabelGadget)(w))->gadget.highlight_thickness)

#define LabG_Baseline(w) \
       (XmStringBaseline ( \
               ((XmLabelGadget)(w))->label.font,\
               ((XmLabelGadget)(w))->label._label))

/*
 * a bunch of prototypes.  I don't know why Motif has these, as the access
 * macros essentially render them useless.  Maybe so Label.c doesn't have
 * to include this?  Nah, it has to anyway.
 * Glimmer: ahh, could be that these routines are used when the cache might
 * be affected, and the temp subparts don't exist (MLM)...
 */
extern int  _XmLabelCacheCompare(XtPointer A, XtPointer B);
extern void _XmCalcLabelGDimensions(Widget wid);
extern void _XmReCacheLabG(Widget w);
extern void _XmAssignLabG_MarginHeight(XmLabelGadget lw, Dimension value);
extern void _XmAssignLabG_MarginWidth(XmLabelGadget lw, Dimension value);
extern void _XmAssignLabG_MarginLeft(XmLabelGadget lw, Dimension value);
extern void _XmAssignLabG_MarginRight(XmLabelGadget lw, Dimension value);
extern void _XmAssignLabG_MarginTop(XmLabelGadget lw, Dimension value);
extern void _XmAssignLabG_MarginBottom(XmLabelGadget lw, Dimension value);

extern void _XmProcessDrag(Widget w, XEvent *event, String *params, Cardinal *num_params);

#ifdef __cplusplus
}
#endif

#endif /* _XM_LABELGP_H */
