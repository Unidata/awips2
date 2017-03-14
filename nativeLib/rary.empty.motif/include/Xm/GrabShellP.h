/**
 * 
 * $Id: GrabShellP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_GRABSHELLP_H
#define _XM_GRABSHELLP_H

#include <Xm/GrabShell.h>
#include <Xm/XmP.h>
#include <X11/ShellP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _XmGrabShellPart {
   Cursor cursor;
   Dimension shadow_thickness;
   Pixel top_shadow_color;
   Pixmap top_shadow_pixmap;
   Pixel bottom_shadow_color;
   Pixmap bottom_shadow_pixmap;
   GC top_shadow_GC;
   GC bottom_shadow_GC;
   Boolean owner_events;
   int grab_style;
   Time post_time;
   Time unpost_time;
   Boolean mapped;
   Window old_focus;
   int old_revert_to;
} XmGrabShellPart;

typedef struct _XmGrabShellRec {
   CorePart core;
   CompositePart composite;
   ShellPart shell;
   WMShellPart wm_shell;
   VendorShellPart vendor_shell;
   XmGrabShellPart grab_shell;
} XmGrabShellRec, _XmGrabShellWidgetRec, XmGrabShellWidgetRec;

/* The typedefs _XmGrabShellWidgetRec and XmGrabShellWidgetRec are for backwards compatibility. */

typedef struct _XmGrabShellClassPart {
   XtPointer extension;
} XmGrabShellClassPart;

typedef struct _XmGrabShellClassRec {
   CoreClassPart core_class;
   CompositeClassPart composite_class;
   ShellClassPart shell_class;
   WMShellClassPart wm_shell_class;
   VendorShellClassPart vendor_shell_class;
   XmGrabShellClassPart grab_shell_class;
} XmGrabShellClassRec;

XMLIBEXPORT extern XmGrabShellClassRec xmGrabShellClassRec;

#define GS_Cursor(w) \
        (((XmGrabShellWidget)(w))->grab_shell.cursor)

#define GS_ShadowThickness(w) \
        (((XmGrabShellWidget)(w))->grab_shell.shadow_thickness)

#define GS_TopShadowColor(w) \
        (((XmGrabShellWidget)(w))->grab_shell.top_shadow_color)

#define GS_TopShadowPixmap(w) \
        (((XmGrabShellWidget)(w))->grab_shell.top_shadow_pixmap)

#define GS_BottomShadowColor(w) \
        (((XmGrabShellWidget)(w))->grab_shell.bottom_shadow_color)

#define GS_BottomShadowPixmap(w) \
        (((XmGrabShellWidget)(w))->grab_shell.bottom_shadow_pixmap)

#define GS_TopShadowGC(w) \
        (((XmGrabShellWidget)(w))->grab_shell.top_shadow_GC)

#define GS_BottomShadowGC(w) \
        (((XmGrabShellWidget)(w))->grab_shell.bottom_shadow_GC)

#define GS_OwnerEvents(w) \
        (((XmGrabShellWidget)(w))->grab_shell.owner_events)

#define GS_GrabStyle(w) \
        (((XmGrabShellWidget)(w))->grab_shell.grab_style)

#define GS_PostTime(w) \
        (((XmGrabShellWidget)(w))->grab_shell.post_time)

#define GS_UnpostTime(w) \
        (((XmGrabShellWidget)(w))->grab_shell.unpost_time)

#define GS_Mapped(w) \
        (((XmGrabShellWidget)(w))->grab_shell.mapped)

#define GS_OldFocus(w) \
        (((XmGrabShellWidget)(w))->grab_shell.old_focus)

#define GS_OldRevertTo(w) \
        (((XmGrabShellWidget)(w))->grab_shell.old_revert_to)

#ifdef __cplusplus
}
#endif

#endif /* _XM_GRABSHELLP_H */
