/**
 *
 * $Id: MenuShellP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
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

#ifndef _XM_MENUSHELLP_H
#define _XM_MENUSHELLP_H

#include <Xm/XmP.h>
#include <Xm/MenuShell.h>
#include <X11/ShellP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* define the menushell instance part */
typedef struct {
    unsigned char focus_policy;
    XmFocusData focus_data;
    Boolean private_shell;
    XmFontList default_font_list;
    XmFontList button_font_list;
    XmFontList label_font_list;

    XmDirection layout_direction; /* new for 2.0 */
} XmMenuShellPart;

/* define the full instance record */
typedef struct _XmMenuShellRec {
    CorePart core;
    CompositePart composite;
    ShellPart shell;
    OverrideShellPart override;
    XmMenuShellPart menu_shell;
} XmMenuShellRec;

/* define class part structure */
typedef struct {
    XtActionProc popdownOne;
    XtActionProc popdownEveryone;
    XtActionProc popdownDone;
    XmMenuPopupProc popupSharedMenupane;
    XtPointer extension;
} XmMenuShellClassPart;

/* define the full class record */
typedef struct _XmMenuShellClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ShellClassPart shell_class;
    OverrideShellClassPart override_shell_class;
    XmMenuShellClassPart menu_shell_class;
} XmMenuShellClassRec;

XMLIBEXPORT extern XmMenuShellClassRec xmMenuShellClassRec;

void _XmEnterRowColumn(Widget widget,
		       XtPointer closure,
		       XEvent *event,
		       Boolean *cont);
void _XmClearTraversal(Widget wid,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params);
void _XmSetLastManagedMenuTime(Widget wid, Time newTime);

#define MS_FocusPolicy(w) \
	(((XmMenuShellWidget)w)->menu_shell.focus_policy)

#ifdef __cplusplus
}
#endif

#endif /* _XM_MENUSHELLP_H */
