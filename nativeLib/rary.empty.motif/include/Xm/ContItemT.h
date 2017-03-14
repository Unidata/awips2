/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/ContItemT.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
 *
 * Copyright (C) 1999 Free Software Foundation, Inc.
 * Copyright © 1999, 2000, 2001 LessTif Development Team
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


#ifndef _XM_CONTITEMT_H
#define _XM_CONTITEMT_H

#include <X11/Intrinsic.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ContItemAllValid	(0xFFFF)
#define ContItemViewType	(1L<<0)
#define ContItemVisualEmphasis	(1L<<1)
#define ContItemIconWidth	(1L<<2)
#define ContItemDetailCount	(1L<<3)

typedef struct {
	Mask			valueMask;
	unsigned char		view_type;
	unsigned char		visual_emphasis;
	Dimension		icon_width;
	Cardinal		detail_count;
} XmContainerItemDataRec, *XmContainerItemData;

typedef void (*XmContainerItemGetValuesProc)(Widget, XmContainerItemData);
typedef	void (*XmContainerItemSetValuesProc)(Widget, XmContainerItemData);

typedef struct {
	int				version;
	XmContainerItemSetValuesProc	setValues;
	XmContainerItemGetValuesProc	getValues;
} XmContainerItemTraitRec, *XmContainerItemTrait;

XMLIBEXPORT extern XrmQuark	XmQTcontainerItem;

#ifdef __cplusplus
}
#endif

#endif /* _XM_CONTITEMT_H */
