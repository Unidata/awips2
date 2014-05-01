/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/DrawP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

#ifndef _XM_DRAWP_H
#define _XM_DRAWP_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif


void XmeDrawShadows(Display *display,
			   Drawable d,
			   GC top_gc,
			   GC bottom_gc,
			   Position x, Position y,
			   Dimension width, Dimension height,
			   Dimension shad_thick,
			   unsigned int shad_type);

void XmeClearBorder(Display *display,
			   Window w,
			   Position x, Position y,
			   Dimension width, Dimension height,
			   Dimension shadow_thick);

void XmeDrawDiamond(Display *display,
			   Drawable d,
			   GC top_gc,
			   GC bottom_gc,
			   GC center_gc,
			   Position x, Position y,
			   Dimension width, Dimension height,
			   Dimension shadow_thick,
			   Dimension fill);

void XmeDrawSeparator(Display *display,
			    Drawable d,
			    GC top_gc,
			    GC bottom_gc,
			    GC separator_gc,
			    Position x, Position y,
			    Dimension width, Dimension height,
			    Dimension shadow_thick,
			    Dimension margin,
			    unsigned char orientation,
			    unsigned char separator_type);

void XmeDrawHighlight(Display *display,
			     Drawable d,
			     GC gc,
			     Position x, Position y,
			     Dimension width, Dimension height,
			     Dimension highlight_thick);

void XmeDrawArrow(Display *display,
			 Drawable d,
			 GC top_gc,
			 GC bot_gc,
			 GC cent_gc,
			 Position x, Position y,
			 Dimension width, Dimension height,
			 Dimension shadow_thick,
			 unsigned char direction);

void XmeDrawCircle(Display *display,
			  Drawable d,
			  GC top_gc,
			  GC bot_gc,
			  GC cent_gc,
			  Position x, Position y,
			  Dimension width, Dimension height,
			  Dimension shadow_thick,
			  Dimension margin);

void XmeDrawIndicator(Display *display, 
	  		     Drawable d, 
			     GC gc, 
			     Position x, Position y, 
			     Dimension width, Dimension height,
			     Dimension margin, 
			     XtEnum type);

void XmeDrawPolygonShadow(Display *display,
                          Drawable drawable,
                          GC top_gc,
                          GC bottom_gc,
                          XPoint *points,
                          int point_count,
                          Dimension shadow_thickness,
                          unsigned char shadow_type);


Widget
XmeCreateClassDialog(WidgetClass widget_class,
                     Widget parent,
                     String name,
                     ArgList args,
                     Cardinal arg_count);


/* 
 * Obsolete routines from 1.2.x 
 * 
 * The following routines have been provided for backward compatability.
 * BTW, Motif 2.0 defines macros in XmP.h instead of declaring the functions.  
 */

XMLIBEXPORT extern void _XmDrawShadows(Display *display,
			   Drawable d,
			   GC top_gc,
			   GC bottom_gc,
			   Position x, Position y,
			   Dimension width, Dimension height,
			   Dimension shad_thick,
			   unsigned int shad_type);

XMLIBEXPORT extern void _XmDrawShadow(Display *display,
			  Drawable d,
			  GC top_gc,
			  GC bottom_gc,
			  Dimension shad_thick,
			  Position x, Position y,
			  Dimension width, Dimension height);

XMLIBEXPORT extern void _XmClearBorder(Display *display,
			   Window w,
			   Position x, Position y,
			   Dimension width, Dimension height,
			   Dimension shadow_thick);

XMLIBEXPORT extern void _XmDrawSeparator(Display *display,
			    Drawable d,
			    GC top_gc,
			    GC bottom_gc,
			    GC separator_gc,
			    Position x, Position y,
			    Dimension width, Dimension height,
			    Dimension shadow_thick,
			    Dimension margin,
			    unsigned char orientation,
			    unsigned char separator_type);

XMLIBEXPORT extern void _XmDrawDiamond(Display *display,
			   Drawable d,
			   GC top_gc,
			   GC bottom_gc,
			   GC center_gc,
			   Position x, Position y,
			   Dimension width, Dimension height,
			   Dimension shadow_thick,
			   Dimension fill);

XMLIBEXPORT extern void _XmDrawSimpleHighlight(Display *display,
				   Drawable d,
				   GC gc,
				   Position x, Position y,
				   Dimension width, Dimension height,
				   Dimension highlight_thick);

XMLIBEXPORT extern void _XmDrawHighlight(Display *display,
			     Drawable d,
			     GC gc,
			     Position x, Position y,
			     Dimension width, Dimension height,
			     Dimension highlight_thick,
			     int line_style);

XMLIBEXPORT extern void _XmDrawArrow(Display *display,
			 Drawable d,
			 GC top_gc,
			 GC bot_gc,
			 GC cent_gc,
			 Position x, Position y,
			 Dimension width, Dimension height,
			 Dimension shadow_thick,
			 unsigned char direction);

XMLIBEXPORT extern void _XmHighlightBorder(Widget w);
XMLIBEXPORT extern void _XmUnhighlightBorder(Widget w);


#ifdef __cplusplus
}
#endif

#endif /* _XM_DRAWP_H */
