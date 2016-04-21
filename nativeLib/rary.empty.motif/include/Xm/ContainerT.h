/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/ContainerT.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
 *
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


#ifndef _XM_CONTAINERT_H
#define _XM_CONTAINERT_H

#include <X11/Intrinsic.h>

#ifdef __cplusplus
extern "C" {
#endif

XMLIBEXPORT extern XrmQuark XmQTcontainer;

typedef struct {
	Mask		valueMask;
	Cardinal	*detail_order;
	Cardinal	detail_order_count;
	XmTabList	detail_tablist;
	Dimension	first_column_width;
	unsigned char	selection_mode;
	Pixel		select_color;
} XmContainerDataRec, *XmContainerData;

typedef void (*XmContainerGetValuesProc)(Widget, XmContainerData);

typedef struct {
	int				version;
	XmContainerGetValuesProc	getValues;
} XmContainerTraitRec, *XmContainerTrait;

#define ContAllValid             (0xFFFF)
#define ContDetailOrder          (1L<<0)
#define ContDetailTabList        (1L<<1)
#define ContFirstColumnWidth     (1L<<2)
#define ContSelectionMode        (1L<<3)
#define ContSelectColor          (1L<<4)

#ifdef __cplusplus
}
#endif

#endif /* _XM_CONTAINERT_H */
