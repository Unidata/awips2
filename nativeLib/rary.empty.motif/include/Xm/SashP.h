/**
 *
 * $Id: SashP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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

#ifndef _XM_SASHP_H
#define _XM_SASHP_H

#include <Xm/PrimitiveP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Define these constants here, as the Sash widget doesn't have any
 * public header file.
 */

XMLIBEXPORT extern WidgetClass xmSashWidgetClass;
typedef struct _XmSashRec *XmSashWidget;
typedef struct _XmSashClassRec *XmSashWidgetClass;

typedef struct {
    XtCallbackList sash_action;
    Boolean has_focus;
} XmSashPart;


/*
 * This structure contains the call_data passed to functions
 * registered with the sash_action_callback.
 */
typedef struct {
    XEvent *event;
    String *params;
    Cardinal num_params;
} SashCallDataRec, *SashCallData;

/* Define the full instance record */
typedef struct _XmSashRec {
    CorePart core;
    XmPrimitivePart primitive;
    XmSashPart sash;
} XmSashRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmSashClassPart;

/* Define the full class record */
typedef struct _XmSashClassRec {
    CoreClassPart core_class;
    XmPrimitiveClassPart primitive_class;
    XmSashClassPart sash_class;
} XmSashClassRec;

/* External definition for class record */

XMLIBEXPORT extern XmSashClassRec xmSashClassRec;

#ifndef XmIsSash
#define XmIsSash(w) XtIsSubclass((w), xmSashWidgetClass)
#endif


#ifdef __cplusplus
}
#endif

#endif /* _XM_SASH_P_H */
