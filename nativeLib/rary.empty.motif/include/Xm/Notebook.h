/**
 * 
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/Notebook.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_NOTEBOOK_H
#define _XM_NOTEBOOK_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

XMLIBEXPORT extern WidgetClass xmNotebookWidgetClass;

typedef struct _XmNotebookRec *XmNotebookWidget;
typedef struct _XmNotebookClassRec *XmNotebookWidgetClass;

#ifndef XmIsNotebook
#define XmIsNotebook(w) XtIsSubclass(w, xmNotebookWidgetClass)
#endif

typedef enum {
    XmPAGE_FOUND,
    XmPAGE_INVALID,
    XmPAGE_EMPTY,
    XmPAGE_DUPLICATED
} XmNotebookPageStatus;

typedef struct {
    int	page_number;
    Widget	page_widget;
    Widget	status_area_widget;
    Widget	major_tab_widget;
    Widget	minor_tab_widget;
} XmNotebookPageInfo;

XMLIBEXPORT Widget XmCreateNotebook(Widget parent,
                        String name,
                        ArgList arglist,
                        Cardinal argcount);

XMLIBEXPORT XmNotebookPageStatus XmNotebookGetPageInfo(Widget w,
                                           int page_number,
                                           XmNotebookPageInfo *page_info);

#ifdef __cplusplus
}
#endif

#endif /* _XM_NOTEBOOK_H */
