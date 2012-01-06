/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/ActivatableT.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright (C) 1997-2000 LessTif Development Team
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


#ifndef _XM_ACTIVATABLET_H
#define _XM_ACTIVATABLET_H

#include <X11/Intrinsic.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * XmActivatableCallBackProc :
 *	Widget	w;		the widget
 *	XtCallbackProc	cb;	a callback procedure to be added or removed
 *	XtPointer cd;		client_data associated with the above
 *	Boolean setunset;	if true, cb gets added, otherwise it's removed
 */
typedef void (*XmActivatableCallBackProc)(Widget,
					XtCallbackProc,
					XtPointer,
					Boolean);

typedef struct {
	int				version;
	XmActivatableCallBackProc	changeCB;
} XmActivatableTraitRec, *XmActivatableTrait;

XMLIBEXPORT extern XrmQuark	XmQTactivatable;

#ifdef __cplusplus
}
#endif

#endif /* _XM_ACTIVATABLET_H */
