/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/SSpinB.h,v 1.5 2004/06/07 20:01:51 dannybackx Exp $
 * 
 * Copyright © 2000, 2001, 2002 LessTif Development Team
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

#ifndef _XM_SSPINB_H
#define _XM_SSPINB_H

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C"
{
#endif


XMLIBEXPORT extern WidgetClass xmSimpleSpinBoxWidgetClass;

typedef struct _XmSpinBoxClassRec *XmSimpleSpinBoxWidgetClass;
typedef struct _XmSpinBoxRec *XmSimpleSpinBoxWidget;

XMLIBEXPORT Widget XmCreateSimpleSpinBox(Widget parent, String name,
                             ArgList arglist, Cardinal argcount);

XMLIBEXPORT void XmSimpleSpinBoxAddItem(Widget w, XmString item, int pos);

XMLIBEXPORT void XmSimpleSpinBoxDeletePos(Widget w, int pos);

XMLIBEXPORT void XmSimpleSpinBoxSetItem(Widget w, XmString item);

#ifdef __cplusplus
}
#endif

#endif		/* _XM_SSPINB_H */
