/**
 *
 * $Id: ProtocolsP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
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
#ifndef _XM_PROTOCOLS_P_H
#define _XM_PROTOCOLS_P_H

#include <Xm/Protocols.h>
#include <Xm/ExtObjectP.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef XmIsProtocol
#define XmIsProtocol(w) XtIsSubclass(w, xmProtocolObjectClass)
#endif

typedef struct _XmProtocolClassPart {
    XtPointer extension;
} XmProtocolClassPart;

typedef struct _XmProtocolClassRec {
    ObjectClassPart object_class;
    XmExtClassPart ext_class;
    XmProtocolClassPart	protocol_class;
} XmProtocolClassRec, *XmProtocolObjectClass;

typedef struct _XmProtocolPart {
    XtCallbackRec pre_hook, post_hook;
    XtCallbackList callbacks;
    Atom atom;
    Boolean active;
} XmProtocolPart, *XmProtocolPartPtr;

typedef struct _XmProtocolRec {
    ObjectPart object;
    XmExtPart ext;
    XmProtocolPart protocol;
} XmProtocolRec, *XmProtocol, **XmProtocolList;

XMLIBEXPORT extern XmProtocolClassRec xmProtocolClassRec;
XMLIBEXPORT extern WidgetClass xmProtocolObjectClass;

typedef struct _XmProtocolMgrRec {
    Atom property;
    XmProtocolList protocols;
    Cardinal num_protocols;
    Cardinal max_protocols;
} XmProtocolMgrRec, *XmProtocolMgr, **XmProtocolMgrList;

typedef struct _XmAllProtocolsMgrRec {
  XmProtocolMgrList protocol_mgrs;
  Cardinal num_protocol_mgrs;
  Cardinal max_protocol_mgrs;
  Widget shell;
} XmAllProtocolsMgrRec, *XmAllProtocolsMgr;
    
XMLIBEXPORT extern void _XmInstallProtocols(Widget w);

#ifdef __cplusplus
}
#endif

#endif /* _XM_PROTOCOLSP_H */
