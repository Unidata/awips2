/**
 * 
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/PrintSP.h,v 1.9 2004/06/07 20:01:50 dannybackx Exp $
 *
 * Copyright © 2000,2001,2002 Free Software Foundation, Inc.
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

#ifndef _XM_PRINTSP_H
#define _XM_PRINTSP_H

#include <Xm/Xm.h>
#include <X11/ShellP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* New fields for the XmPrintShell widget class record */

typedef struct {
    XtPointer       extension;          /* pointer to extension record      */
} XmPrintShellClassPart;

typedef struct XmPrintShellClassRec {
  	CoreClassPart      core_class;
	CompositeClassPart composite_class;
	ShellClassPart  shell_class;
	WMShellClassPart   wm_shell_class;
	VendorShellClassPart vendor_shell_class;
	TopLevelShellClassPart top_level_shell_class;
	ApplicationShellClassPart application_shell_class;
	XmPrintShellClassPart print_shell_class;
} XmPrintShellClassRec;

XMLIBEXPORT extern XmPrintShellClassRec xmPrintShellClassRec;

/* New fields for the XmPrint shell widget */

typedef struct {
    XtCallbackList	start_job_callback,
			end_job_callback,
			page_setup_callback,
			pdm_notification_callback;
    Dimension		min_x,
			min_y,
			max_x,
			max_y;
    unsigned short	default_pixmap_resolution;
    int			print_resolution;
    Boolean		last_page;
} XmPrintShellPart;

typedef  struct XmPrintShellRec {
	CorePart 	core;
	CompositePart 	composite;
	ShellPart 	shell;
	WMShellPart	wm;
	VendorShellPart	vendor;
	TopLevelShellPart topLevel;
	ApplicationShellPart application;
	XmPrintShellPart print;
} XmPrintShellRec;
XMLIBEXPORT extern WidgetClass xmPrintShellWidgetClass;

#define PS_StartJobCallback(w) \
	(((XmPrintShellWidget) (w))->print.start_job_callback)
#define PS_EndJobCallback(w) \
	(((XmPrintShellWidget) (w))->print.end_job_callback)
#define PS_PageSetupCallback(w) \
	(((XmPrintShellWidget) (w))->print.page_setup_callback)
#define PS_PdmNotificationCallback(w) \
	(((XmPrintShellWidget) (w))->print.pdm_notification_callback)
#define PS_MinX(w) \
	(((XmPrintShellWidget) (w))->print.min_x)
#define PS_MinY(w) \
	(((XmPrintShellWidget) (w))->print.min_y)
#define PS_MaxX(w) \
	(((XmPrintShellWidget) (w))->print.max_x)
#define PS_MaxY(w) \
	(((XmPrintShellWidget) (w))->print.max_y)
#define PS_DefaultPixmapResolution(w) \
	(((XmPrintShellWidget) (w))->print.default_pixmap_resolution)
#define PS_PrintResolution(w) \
	(((XmPrintShellWidget) (w))->print.print_resolution)
#define PS_LastPage(w) \
	(((XmPrintShellWidget) (w))->print.last_page)

#ifdef __cplusplus
}
#endif

#endif /* _XM_PRINTSP_H */
