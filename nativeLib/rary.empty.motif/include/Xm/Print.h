/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/Print.h,v 1.10 2004/06/07 20:01:48 dannybackx Exp $
 * 
 * Copyright © 2000 LessTif Development Team
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

#ifndef _XM_PRINT_H
#define _XM_PRINT_H

#include <X11/extensions/Print.h>
#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C"
{
#endif

typedef struct
{
	int    		reason;
	XEvent		*event;
	XPContext	context;
	Boolean		last_page;
	XtPointer	detail;
} XmPrintShellCallbackStruct;

XMLIBEXPORT extern WidgetClass xmPrintShellWidgetClass;
typedef struct XmPrintShellRec *XmPrintShellWidget;

XMLIBEXPORT XtEnum XmPrintPopupPDM(Widget print_shell,
                       Widget video_transient_for);

XMLIBEXPORT Widget XmPrintSetup(Widget video_widget,
                    Screen *print_screen,
                    String print_shell_name,
                    ArgList args,
                    Cardinal num_args);

XMLIBEXPORT Boolean XmIsPrintShell(Widget w);

XMLIBEXPORT XtEnum XmPrintToFile(Display *dpy,
                     String filename,
                     XPFinishProc finish_proc,
                     XtPointer client_data);
		     
#ifdef __cplusplus
}
#endif

#endif		/* _XM_PRINT_H */
