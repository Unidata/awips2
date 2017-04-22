/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/NavigatorT.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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


#ifndef _XM_NAVIGATORT_H
#define _XM_NAVIGATORT_H

#include <X11/Intrinsic.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	int	x, y;
} XmTwoDIntRec, *XmTwoDInt;

typedef struct {
	Mask			valueMask;
	Mask			dimMask;
	XmTwoDIntRec	*value;
	XmTwoDIntRec	*minimum;
	XmTwoDIntRec	*maximum;
	XmTwoDIntRec	*slider_size;
	XmTwoDIntRec	*increment;
	XmTwoDIntRec	*page_increment;
} XmNavigatorDataRec, *XmNavigatorData;

/*
 */
typedef void (*XmNavigatorMoveCBProc)
		(Widget, XtCallbackProc, XtPointer, Boolean);
/*
 */
typedef	void (*XmNavigatorSetValueProc)(Widget, XmNavigatorData, Boolean);
/*
 */
typedef int (*XmNavigatorGetValueProc)(Widget, XmNavigatorData);

typedef struct {
	int			version;
	XmNavigatorMoveCBProc	changeMoveCB;
	XmNavigatorSetValueProc	setValue;
	XmNavigatorGetValueProc	getValue;
} XmNavigatorTraitRec, *XmNavigatorTrait;

XMLIBEXPORT extern XrmQuark	XmQTnavigator;

/* These bit values indicate which fields in the structure are valid. */
enum {
	NavDimMask		= 0x0001,
	NavValue		= 0x0002,
	NavMinimum		= 0x0004,
	NavMaximum		= 0x0008,
	NavSliderSize	= 0x0010,
	NavIncrement	= 0x0020,
	NavPageIncrement	= 0x0040,
	NavAllValid		= 0xFFFF
};

enum {
	NavigDimensionX	= 0x01,
	NavigDimensionY	= 0x02
};

#ifdef __cplusplus
}
#endif

#endif /* _XM_NAVIGATORT_H */
