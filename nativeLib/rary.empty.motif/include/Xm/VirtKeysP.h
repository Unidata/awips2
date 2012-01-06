/**
 *
 * $Id: VirtKeysP.h,v 1.1 2004/08/28 19:23:27 dannybackx Exp $
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

#ifndef _XM_VIRTKEYSP_H
#define _XM_VIRTKEYSP_H

#include <Xm/VirtKeys.h>

#ifdef __cplusplus
extern "C" {
#endif

#define XmKEYCODE_TAG_SIZE 32

/*
 * Only the first record structure is visible outside the LessTif toolkit
 * code. The least two structures are used solely inside LessTif.
 */
typedef struct _XmKeyBindingRec {
    KeySym       keysym;
    unsigned int modifiers;
} XmKeyBindingRec, *XmKeyBinding;

typedef struct _XmVirtualKeysymRec {
    String       name;
    KeySym       keysym;
} XmVirtualKeysymRec, *XmVirtualKeysym;

typedef struct _XmDefaultBindingStringRec {
    String       vendorName;
    String       defaults;
} XmDefaultBindingStringRec, *XmDefaultBindingString;


XMLIBEXPORT extern void _XmVirtKeysInitialize(Widget w);
XMLIBEXPORT extern void _XmVirtKeysDestroy(Widget widget);
XMLIBEXPORT extern void _XmVirtKeysHandler(Widget widget,
			       XtPointer client_data,
			       XEvent *event,
			       Boolean *dontSwallow); 
XMLIBEXPORT extern void _XmVirtualToActualKeysym(Display *Dsp, KeySym VirtualKeysym,
                                     KeySym *RealKeysymReturn,
                                     Modifiers *ModifierReturn);
XMLIBEXPORT extern void _XmVirtKeysStoreBindings(Widget shell, String binding);
XMLIBEXPORT extern Boolean _XmVirtKeysLoadFileBindings(String filename, String *binding);
XMLIBEXPORT extern int _XmVirtKeysLoadFallbackBindings(Display *Dsp, String *Bindings);

#ifdef __cplusplus
}
#endif

#endif /* _XM_VIRTKEYS_P_H */
