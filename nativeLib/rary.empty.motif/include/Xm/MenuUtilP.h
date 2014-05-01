/**
 *
 * $Id: MenuUtilP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

#ifndef _XM_MENUUTILP_H
#define _XM_MENUUTILP_H

#include <Xm/XmP.h>
#include <Xm/RowColumnP.h>

#ifdef __cplusplus
extern "C" {
#endif

void _XmMenuArmItem(Widget w);

void _XmMenuDisarmItem(Widget w);

Boolean _XmIsActiveTearOff(Widget w);

Widget _XmGetRC_PopupPosted(Widget);

void _XmMenuEscape(Widget, XEvent *, String *, Cardinal *);
void _XmMenuTraverseLeft(Widget, XEvent *, String *, Cardinal *);
void _XmMenuTraverseRight(Widget, XEvent *, String *, Cardinal *);
void _XmMenuTraverseUp(Widget, XEvent *, String *, Cardinal *);
void _XmMenuTraverseDown(Widget, XEvent *, String *, Cardinal *);

void _XmRC_GadgetTraverseDown(Widget, XEvent *, String *, Cardinal *);

void _XmRC_GadgetTraverseUp(Widget, XEvent *, String *, Cardinal *);
void _XmRC_GadgetTraverseLeft(Widget, XEvent *, String *, Cardinal *);
void _XmRC_GadgetTraverseRight(Widget, XEvent *, String *, Cardinal *);

Boolean _XmGetInDragMode(Widget w);
void _XmSetInDragMode(Widget w, Boolean flag);

void _XmRecordEvent(XEvent *event);
Boolean _XmIsEventUnique(XEvent *event);

int _XmGrabPointer(Widget w,
			  int owner_events,
			  unsigned int event_mask,
			  int pointer_mode,
			  int keyboard_mode,
			  Window confine_to,
			  Cursor cursor,
			  Time time);

void _XmUngrabPointer(Widget w, Time t);

int _XmGrabKeyboard(Widget widget,
			   int owner_events,
			   int pointer_mode,
			   int keyboard_mode,
			   Time time);

void _XmUngrabKeyboard(Widget w, Time t);

XtPointer _XmGetMenuProcContext(void);
void _XmSaveMenuProcContext(XtPointer address);

void _XmMenuTraversalHandler(Widget w, Widget pw,
				    XmTraversalDirection direction);

void _XmMenuSetInPMMode(Widget wid, Boolean flag);

void _XmSetMenuTraversal(Widget wid, Boolean traversalOn);

void _XmLeafPaneFocusOut(Widget wid);

void _XmSaveCoreClassTranslations(Widget widget);
void _XmRestoreCoreClassTranslations(Widget widget);

XmMenuState _XmGetMenuState(Widget widget);

#ifdef __cplusplus
}
#endif

#endif /* _XM_MENUUTILP_H */
