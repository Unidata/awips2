/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/MenuT.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 *
 * Copyright (C) 2000 Free Software Foundation, Inc.
 * Copyright (C) 2000 LessTif Development Team
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


#ifndef _XM_MENUT_H
#define _XM_MENUT_H

#ifdef __cplusplus
extern "C" {
#endif

XMLIBEXPORT extern XrmQuark XmQTmenuSystem;
XMLIBEXPORT extern XrmQuark XmQTmenuSavvy;

typedef enum {
	XmDISABLE_ACTIVATE,
	XmENABLE_ACTIVATE
} XmActivateState;

typedef int (*XmMenuSystemWidgetProc)(Widget);
typedef Boolean (*XmMenuSystemVerifyProc)(Widget, XEvent*);
typedef void (*XmMenuSystemControlTraversalProc)(Widget, Boolean);
typedef void (*XmMenuSystemCascadeProc)(Widget, Widget, XEvent*);
typedef void (*XmMenuSystemPositionProc)(Widget, XEvent*);
typedef Boolean (*XmMenuSystemPopdownProc)(Widget, XEvent*);
typedef void (*XmMenuSystemEntryCallbackProc)(Widget, Widget, XtPointer);
typedef Boolean (*XmMenuSystemUpdateHistoryProc)(Widget, Widget, Boolean);
typedef void (*XmMenuSystemUpdateBindingsProc)(Widget, int);
typedef void (*XmMenuSystemRecordPostFromWidgetProc)(Widget, Widget, Boolean);
typedef void (*XmMenuSystemDisarmProc)(Widget);
typedef Widget (*XmMenuSystemPopupPostedProc)(Widget);          

typedef void (*XmMenuSavvyDisableProc)(Widget, XmActivateState);
typedef char* (*XmMenuSavvyGetAcceleratorProc)(Widget);
typedef KeySym (*XmMenuSavvyGetMnemonicProc)(Widget);
typedef char* (*XmMenuSavvyGetActivateCBNameProc)(void);

typedef int (*XmMenuSystemTypeProc)(Widget);
typedef int (*XmMenuSystemStatusProc)(Widget);
typedef void (*XmMenuSystemGetPostedFromWidgetProc)(Widget);
typedef void (*XmMenuSystemArmProc)(Widget);
typedef void (*XmMenuSystemMenuBarCleanupProc)(Widget);
typedef void (*XmMenuSystemTearOffArmProc)(Widget);
typedef void (*XmMenuSystemReparentProc)(Widget, XEvent*);
typedef void (*XmMenuSystemPopdownAllProc)(Widget, XEvent*);
typedef void (*XmMenuSystemChildFocusProc)(Widget);

typedef struct _XmMenuSystemTraitRec
{
	int					version;
	XmMenuSystemTypeProc			type;
	XmMenuSystemStatusProc			status;
	XmMenuSystemCascadeProc			cascade;
	XmMenuSystemVerifyProc			verifyButton;
	XmMenuSystemControlTraversalProc	controlTraversal;
	XmMenuSystemMenuBarCleanupProc		menuBarCleanup;
	XmMenuSystemPopdownProc			popdown;
	XmMenuSystemPopdownProc			buttonPopdown;
	XmMenuSystemReparentProc		reparentToTearOffShell;
	XmMenuSystemReparentProc		reparentToMenuShell;
	XmMenuSystemArmProc			arm;
	XmMenuSystemDisarmProc			disarm;
	XmMenuSystemTearOffArmProc		tearOffArm;
	XmMenuSystemEntryCallbackProc		entryCallback;
	XmMenuSystemUpdateHistoryProc		updateHistory;
	XmMenuSystemGetPostedFromWidgetProc	getLastSelectToplevel;
	XmMenuSystemPositionProc		position;
	XmMenuSystemUpdateBindingsProc		updateBindings;
	XmMenuSystemRecordPostFromWidgetProc	recordPostFromWidget;
	XmMenuSystemPopdownAllProc		popdownEveryone;
	XmMenuSystemChildFocusProc		childFocus;
	XmMenuSystemPopupPostedProc		getPopupPosted;
} XmMenuSystemTraitRec, *XmMenuSystemTrait;


typedef struct _XmMenuSavvyTraitRec {
	int					version;
	XmMenuSavvyDisableProc			disableCallback;
	XmMenuSavvyGetAcceleratorProc		getAccelerator;
	XmMenuSavvyGetMnemonicProc		getMnemonic;
	XmMenuSavvyGetActivateCBNameProc	getActivateCBName;
} XmMenuSavvyTraitRec, *XmMenuSavvyTrait;

#ifdef __cplusplus
}
#endif

#endif /* _XM_MENUT_H */
