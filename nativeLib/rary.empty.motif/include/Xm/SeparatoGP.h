/**
 *
 * $Id: SeparatoGP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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


#ifndef _XM_SEPARATORGP_H
#define _XM_SEPARATORGP_H

#include <Xm/SeparatoG.h>
#include <Xm/GadgetP.h>
#include <Xm/ExtObjectP.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * define the cache class and instance
 */
typedef struct _XmSeparatorGCacheObjClassPart {
    int foo;
} XmSeparatorGCacheObjClassPart;

typedef struct _XmSeparatorGCacheObjClassRec {
    ObjectClassPart object_class;
    XmExtClassPart ext_class;
    XmSeparatorGCacheObjClassPart separator_class_cache;
} XmSeparatorGCacheObjClassRec;

XMLIBEXPORT extern XmSeparatorGCacheObjClassRec xmSeparatorGCacheObjClassRec;

typedef struct _XmSeparatorGCacheObjPart {
    Dimension margin;
    unsigned char orientation;
    unsigned char separator_type;
    GC separator_GC;
} XmSeparatorGCacheObjPart;

typedef struct _XmSeparatorGCacheObjRec {
    ObjectPart object;
    XmExtPart ext;
    XmSeparatorGCacheObjPart separator_cache;
} XmSeparatorGCacheObjRec;


/*
 * Define the separator gadget class and instance record
 */
typedef struct {
    XtPointer extension;
} XmSeparatorGadgetClassPart;

/* Define the full class record */
typedef struct _XmSeparatorGadgetClassRec {
    RectObjClassPart rect_class;
    XmGadgetClassPart gadget_class;
    XmSeparatorGadgetClassPart separator_class;
} XmSeparatorGadgetClassRec;

XMLIBEXPORT extern XmSeparatorGadgetClassRec xmSeparatorGadgetClassRec;

typedef struct _XmSeparatorGadgetPart {
    XmSeparatorGCacheObjPart *cache;
} XmSeparatorGadgetPart;

typedef struct _XmSeparatorGadgetRec {
    ObjectPart object;
    RectObjPart rectangle;
    XmGadgetPart gadget;
    XmSeparatorGadgetPart separator;
} XmSeparatorGadgetRec;


/*
 * convenient access macros
 */
#define SEPG_Margin(w) \
    (((XmSeparatorGadget)(w))->separator.cache->margin)

#define SEPG_Orientation(w) \
    (((XmSeparatorGadget)(w))->separator.cache->orientation)

#define SEPG_SeparatorType(w) \
    (((XmSeparatorGadget)(w))->separator.cache->separator_type)

#define SEPG_SeparatorGC(w) \
    (((XmSeparatorGadget)(w))->separator.cache->separator_GC)

#define SEPG_Cache(w) \
    (((XmSeparatorGadget)(w))->separator.cache)

#define SEPG_ClassCachePart(w) \
    (((XmSeparatorGadgetClass)xmSeparatorGadgetClass)->gadget_class.cache_part)

XMLIBEXPORT extern int _XmSeparatorCacheCompare(XtPointer A, XtPointer B);

#ifdef __cplusplus
}
#endif

#endif /* _XM_SEPARATORGP_H */
