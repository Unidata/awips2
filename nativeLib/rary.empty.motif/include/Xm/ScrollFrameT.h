/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/ScrollFrameT.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
 * 
 * Copyright (C) 1997-1998 Free Software Foundation, Inc.
 * Copyright (C) 1998-2000 LessTif Development Team
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

#ifndef _XM_SCROLLFRAMET_H
#define _XM_SCROLLFRAMET_H

#ifdef __cplusplus
extern "C" {
#endif

typedef void (* XmScrollFrameInitProc)(
	Widget,
	XtCallbackProc,
	Widget);

typedef Boolean (* XmScrollFrameGetInfoProc)(
	Widget,
	Cardinal *,
	Widget **,
	Cardinal *);

typedef void (* XmScrollFrameAddNavigatorProc)(
	Widget,
	Widget,
	Mask);

typedef void (*XmScrollFrameRemoveNavigatorProc)(
	Widget,
	Widget);

typedef struct _XmScrollFrameTraitRec {
	int version;
	XmScrollFrameInitProc			init;
	XmScrollFrameGetInfoProc		getInfo;
	XmScrollFrameAddNavigatorProc		addNavigator;
	XmScrollFrameRemoveNavigatorProc	removeNavigator;
} XmScrollFrameTraitRec, *XmScrollFrameTrait;

typedef struct _XmScrollFrameDataRec {
	int	dummy;
} XmScrollFrameDataRec, *XmScrollFrameData;

XMLIBEXPORT extern XrmQuark  XmQTscrollFrame;

#ifdef __cplusplus
}
#endif

#endif /* _XM_SCROLLFRAMET_H */
