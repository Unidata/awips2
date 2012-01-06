/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/CareVisualT.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
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


#ifndef _XM_CAREVISUALT_H
#define _XM_CAREVISUALT_H

#include <X11/Intrinsic.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * XmCareVisualRedrawProc :
 *	Widget child;		the widget affected by parent's visual change
 *	Widget parent;		the current parent widget
 *	Widget newparent;	the new parent widget
 *	Mask visualChangeMask;	bitmask representing parent's visual changes
 */
typedef Boolean (*XmCareVisualRedrawProc)(Widget, Widget, Widget, Mask);

typedef struct _XmCareVisualTraitRec {
	int			version;
	XmCareVisualRedrawProc	redraw;
} XmCareVisualTraitRec, *XmCareVisualTrait;

XMLIBEXPORT extern XrmQuark	XmQTcareParentVisual;

#define NoVisualChange                    0L
#define VisualForeground                  (1L<<0)  
#define VisualHighlightPixmap             (1L<<1)                              
#define VisualHighlightColor              (1L<<2)   
#define VisualBottomShadowPixmap          (1L<<3)   
#define VisualBottomShadowColor           (1L<<4)   
#define VisualTopShadowPixmap             (1L<<5)   
#define VisualTopShadowColor              (1L<<6)   
#define VisualBackgroundPixel             (1L<<7)   
#define VisualBackgroundPixmap            (1L<<8)   
#define VisualSelectColor                 (1L<<9)   

#ifdef __cplusplus
}
#endif

#endif /* _XM_CAREVISUALT_H */
