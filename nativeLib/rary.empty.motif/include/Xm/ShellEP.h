/**
 *
 * $Id: ShellEP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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

#ifndef _XM_SHELLEP_H
#define _XM_SHELLEP_H

#include <Xm/DesktopP.h>

#ifdef __cplusplus
extern "C" {
#endif

#define XmInheritEventHandler		((XtEventHandler)_XtInherit)

#define _XmRAW_MAP	0
#define _XmPOPUP_MAP	1
#define _XmMANAGE_MAP	2

#ifndef XmIsShellExt
#define XmIsShellExt(w)	XtIsSubclass(w, xmShellExtObjectClass)
#endif

typedef struct _XmShellExtRec *XmShellExtObject;
typedef struct _XmShellExtClassRec *XmShellExtObjectClass;
#if 0
XMLIBEXPORT extern WidgetClass xmShellExtObjectClass;
#else
XMLIBEXPORT extern WidgetClass xmShellExtClass;
#endif

typedef struct _XmShellExtClassPart {
    XtEventHandler structureNotifyHandler;
    XtPointer extension;
} XmShellExtClassPart, *XmShellExtClassPartPtr;

typedef struct _XmShellExtClassRec {
    ObjectClassPart object_class;
    XmExtClassPart ext_class;
    XmDesktopClassPart desktop_class;
    XmShellExtClassPart shell_class;
} XmShellExtClassRec;

typedef struct {
    unsigned long lastConfigureRequest;
    Boolean useAsyncGeometry;
} XmShellExtPart, *XmShellExtPartPtr;

XMLIBEXPORT extern XmShellExtClassRec xmShellExtClassRec;

typedef struct _XmShellExtRec {
    ObjectPart object;
    XmExtPart ext;
    XmDesktopPart desktop;
    XmShellExtPart shell;
} XmShellExtRec;

#ifdef __cplusplus
}
#endif

#endif /* _XM_SHELLEP_H */
