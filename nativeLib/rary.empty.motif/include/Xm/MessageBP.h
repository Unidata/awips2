/**
 *
 * $Id: MessageBP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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

#ifndef _XM_MESSAGEBP_H
#define _XM_MESSAGEBP_H

#include <Xm/BulletinBP.h>
#include <Xm/MessageB.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Define the paned window instance part */
typedef struct {
    unsigned char dialog_type;
    unsigned char default_type;
    Boolean internal_pixmap;
    Boolean minimize_buttons;

    unsigned char message_alignment;
    XmString message_string;
    Widget message_wid;

    Pixmap symbol_pixmap;
    Widget symbol_wid;

    XmString ok_label_string;
    XtCallbackList ok_callback;
    Widget ok_button;

    XmString cancel_label_string;
    XtCallbackList cancel_callback;
    
    XmString help_label_string;
    Widget help_button;

    Widget separator;
} XmMessageBoxPart;

/* Define the full instance record */
typedef struct _XmMessageBoxRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmBulletinBoardPart bulletin_board;
    XmMessageBoxPart message_box;
} XmMessageBoxRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmMessageBoxClassPart;

/* Defint the full class record */
typedef struct _XmMessageBoxClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmBulletinBoardClassPart bulletin_board_class;
    XmMessageBoxClassPart messagebox_class;
} XmMessageBoxClassRec;

XMLIBEXPORT extern XmMessageBoxClassRec xmMessageBoxClassRec;


XmGeoMatrix _XmMessageBoxGeoMatrixCreate(Widget wid, Widget instigator,
					 XtWidgetGeometry *desired);
Boolean _XmMessageBoxNoGeoRequest(XmGeoMatrix geoSpec);

#ifdef __cplusplus
}
#endif

#endif /* _XM_MESSAGEBP_H */
