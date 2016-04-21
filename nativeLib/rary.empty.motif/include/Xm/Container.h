/**
 * 
 * $Id: Container.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright © 1997, 2000, 2001 LessTif Development Team
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

#ifndef _XM_CONTAINER_H
#define _XM_CONTAINER_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

XMLIBEXPORT extern WidgetClass xmContainerWidgetClass;

typedef struct _XmContainerRec *XmContainerWidget;
typedef struct _XmContainerClassRec *XmContainerWidgetClass;

#ifndef XmIsContainer
#define XmIsContainer(w) XtIsSubclass(w, xmContainerWidgetClass)
#endif

XMLIBEXPORT Widget XmCreateContainer(Widget parent, String name, ArgList arglist, Cardinal argcount);
XMLIBEXPORT int XmContainerGetItemChildren(Widget w, Widget item, WidgetList *item_children);
XMLIBEXPORT void XmContainerRelayout(Widget w);
XMLIBEXPORT void XmContainerReorder(Widget w, WidgetList cwid_list, int cwid_count);
XMLIBEXPORT Boolean XmContainerCut(Widget w, Time timestamp);
XMLIBEXPORT Boolean XmContainerCopy(Widget w, Time timestamp);
XMLIBEXPORT Boolean XmContainerPaste(Widget w);
XMLIBEXPORT Boolean XmContainerCopyLink(Widget w, Time timestamp);
XMLIBEXPORT Boolean XmContainerPasteLink(Widget w);

#ifdef __cplusplus
}
#endif

#endif /* _XM_CONTAINER_H */
