/**
 *
 * $Id: MainWP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_MAINWP_H
#define _XM_MAINWP_H

#include <Xm/MainW.h>
#include <Xm/ScrolledWP.h>
#include <Xm/SeparatoG.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Define the main window instance part */
typedef struct {
    Dimension AreaWidth, AreaHeight;
    Dimension margin_width, margin_height;
    Widget CommandWindow;
    Widget MenuBar;
    Widget Message;
    unsigned char CommandLoc;

    /* private instance variables */
    XmSeparatorGadget Sep1, Sep2, Sep3;
    Boolean ManagingSep;
    Boolean ShowSep;
} XmMainWindowPart;

/* Define the full instance record */
typedef struct _XmMainWindowRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmScrolledWindowPart swindow;
    XmMainWindowPart mwindow;
} XmMainWindowRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmMainWindowClassPart;

/* Defint the full class record */
typedef struct _XmMainWindowClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmScrolledWindowClassPart scrolled_window_class;
    XmMainWindowClassPart main_window_class;
} XmMainWindowClassRec;

XMLIBEXPORT extern XmMainWindowClassRec xmMainWindowClassRec;

#ifdef __cplusplus
}
#endif

#endif /* _XM_MAINWP_H */
