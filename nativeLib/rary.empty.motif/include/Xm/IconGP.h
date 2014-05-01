/**
 *
 * $Id: IconGP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 * 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1997-2001 LessTif Development Team
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

#ifndef _XM_ICONGP_H
#define _XM_ICONGP_H

#include <Xm/IconG.h>
#include <Xm/GadgetP.h>
#include <Xm/ExtObjectP.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Gadget cache class and instance records
 */
typedef struct _XmIconGCacheObjClassPart {
    XtPointer extension;
} XmIconGCacheObjClassPart;

typedef struct _XmIconGCacheObjClassRec {
    ObjectClassPart object_class;
    XmExtClassPart ext_class;
    XmIconGCacheObjClassPart icon_class_cache;
} XmIconGCacheObjClassRec;

XMLIBEXPORT extern XmIconGCacheObjClassRec xmIconGCacheObjClassRec;

typedef struct _XmIconGCacheObjPart {
    XmRenderTable	render_table;
    GC			selected_gc;
    GC			inverse_gc;
    Pixel		background;
    Pixel		foreground;
    Pixel		top_shadow_color;
    Pixel		bottom_shadow_color;
    Pixel		highlight_color;
    Pixmap		background_pixmap;
    Pixmap		top_shadow_pixmap;
    Pixmap		bottom_shadow_pixmap;
    Pixmap		highlight_pixmap;
    GC			normal_gc;
    GC			background_gc;
    GC			insensitive_gc;
    GC			top_shadow_gc;
    GC			bottom_shadow_gc;
    GC			highlight_gc;
    unsigned char	alignment;
    Dimension		spacing;
    Dimension		margin_width;
    Dimension		margin_height;
} XmIconGCacheObjPart;

typedef struct _XmIconGCacheObjRec {
    ObjectPart object;
    XmExtPart ext;
    XmIconGCacheObjPart icon_cache;
} XmIconGCacheObjRec;

typedef Widget (*XmGetContainerParentProc)(Widget);

/*
 * icon gadget class and instance records
 */
/* Define class part structure */
typedef struct {
    XmGetContainerParentProc get_container_parent;
    XtPointer extension;
} XmIconGadgetClassPart;

/* Define the full class record */
typedef struct _XmIconGadgetClassRec {
    RectObjClassPart rect_class;
    XmGadgetClassPart gadget_class;
    XmIconGadgetClassPart icong_class;
} XmIconGadgetClassRec;

XMLIBEXPORT extern XmIconGadgetClassRec xmIconGadgetClassRec;

typedef struct _XmIconGadgetPart {
    XmString label_string;
    Pixmap large_icon_mask;
    Pixmap large_icon_pixmap;
    Pixmap small_icon_mask;
    Pixmap small_icon_pixmap;
    unsigned char view_type;
    unsigned char visual_emphasis;
    XmStringTable detail;
    Cardinal detail_count;
    Dimension label_rect_width;
    Dimension label_rect_height;
    Dimension large_icon_rect_width;
    Dimension large_icon_rect_height;
    Dimension small_icon_rect_width;
    Dimension small_icon_rect_height;
    String large_pixmap_name;
    String small_pixmap_name;
    XmIconGCacheObjPart *cache;
} XmIconGadgetPart;

/* Define the full instance record */
typedef struct _XmIconGadgetRec {
    ObjectPart object;
    RectObjPart rectangle;
    XmGadgetPart gadget;
    XmIconGadgetPart icong;
} XmIconGadgetRec;


#define IG_VisualEmphasis(w)	(((XmIconGadget)(w))->icong.visual_emphasis)
#define IG_DetailCount(w)	(((XmIconGadget)(w))->icong.detail_count)
#define IG_ViewType(w)		(((XmIconGadget)(w))->icong.view_type)
#define IG_LabelString(w)	(((XmIconGadget)(w))->icong.label_string)
#define IG_NormalGC(w)		(((XmIconGadget)(w))->icong.cache->normal_gc)
#define IG_SelectedGC(w)	(((XmIconGadget)(w))->icong.cache->selected_gc)
#define IG_BackgroundGC(w)	(((XmIconGadget)(w))->icong.cache->background_gc)
#define IG_Cache(w)		(((XmIconGadget)(w))->icong.cache)
#define IG_ClassCachePart(w)	(((XmIconGadgetClass)xmIconGadgetClass)->gadget_class.cache_part)
#define IG_LargeIconMask(w)	(((XmIconGadget)(w))->icong.large_icon_mask)
#define IG_LargeIconPixmap(w)	(((XmIconGadget)(w))->icong.large_icon_pixmap)
#define IG_SmallIconMask(w)	(((XmIconGadget)(w))->icong.small_icon_mask)
#define IG_SmallIconPixmap(w)	(((XmIconGadget)(w))->icong.small_icon_pixmap)
#define IG_RenderTable(w)	(((XmIconGadget)(w))->icong.cache->render_table)
#define IG_Alignment(w)		(((XmIconGadget)(w))->icong.cache->alignment)
#define IG_Spacing(w)		(((XmIconGadget)(w))->icong.cache->spacing)
#define IG_MarginHeight(w)	(((XmIconGadget)(w))->icong.cache->margin_height)
#define IG_MarginWidth(w)	(((XmIconGadget)(w))->icong.cache->margin_width)

/* weird Motif backwards compatibility function.  I don't care if it ever
 * gets implemented or not */
XMLIBEXPORT extern void _XmClearBGCompatibility(Widget pbg);

#ifdef __cplusplus
}
#endif

#endif /* _XM_ICONGP_H */
