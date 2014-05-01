/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/AccTextT.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
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


#ifndef _XM_ACCTEXTT_H
#define _XM_ACCTEXTT_H

#include <X11/Intrinsic.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * XmAccessTextualGetValuesProc :
 *	Widget	w;		the widget
 *	int format;		the format in which to return the value
 *	returns :		the value requested, in the format specified.
 */
typedef XtPointer (*XmAccessTextualGetValuesProc)(Widget, int);
/*
 * XmAccessTextualSetValuesProc :
 *	Widget w;		the widget
 *	XtPointer s;	the string passed to the widget
 *	int format;		the format of the string
 *	returns :		-
 */
typedef	void (*XmAccessTextualSetValuesProc)(Widget, XtPointer, int);
/*
 * XmAccessTextualPreferredProc :
 *	Widget w;		the widget
 *	returns :		the preferred format for values for this widget
 */
typedef int (*XmAccessTextualPreferredProc)(Widget);

typedef struct {
	int					version;
	XmAccessTextualGetValuesProc	getValue;
	XmAccessTextualSetValuesProc	setValue;
	XmAccessTextualPreferredProc	preferredFormat;
} XmAccessTextualTraitRec, *XmAccessTextualTrait;

XMLIBEXPORT extern XrmQuark	XmQTaccessTextual;

/* This is the list of values you can pass to "format" */
enum {
	XmFORMAT_XmSTRING,	/* Parameter is an XmString */
	XmFORMAT_MBYTE,		/* Parameter is a multibyte string */
	XmFORMAT_WCS		/* Parameter is a wide character string */
};

#ifdef __cplusplus
}
#endif

#endif /* _XM_ACCTEXTT_H */
