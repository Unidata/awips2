/**
 *
 * $Id: TextF.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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

#ifndef _XM_TEXTF_H
#define _XM_TEXTF_H

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

XMLIBEXPORT extern WidgetClass xmTextFieldWidgetClass;

typedef struct _XmTextFieldRec *XmTextFieldWidget;
typedef struct _XmTextFieldClassRec *XmTextFieldWidgetClass;

XMLIBEXPORT extern Widget XmCreateTextField(Widget parent,
				char *name,
				Arg *arglist,
				Cardinal argCount);

#ifndef XmIsTextField
#define XmIsTextField(w) XtIsSubclass((w), xmTextFieldWidgetClass)
#endif

XMLIBEXPORT void XmTextFieldClearSelection(Widget widget, Time time);
XMLIBEXPORT Boolean XmTextFieldCopy(Widget widget, Time time);
XMLIBEXPORT Boolean XmTextFieldCut(Widget widget, Time time);
XMLIBEXPORT int XmTextFieldGetBaseline(Widget widget);
XMLIBEXPORT XmTextPosition XmTextFieldGetCursorPosition(Widget widget);
XMLIBEXPORT Boolean XmTextFieldGetEditable(Widget widget);
XMLIBEXPORT XmTextPosition XmTextFieldGetInsertionPosition(Widget widget);
XMLIBEXPORT XmTextPosition XmTextFieldGetLastPosition(Widget widget);
XMLIBEXPORT int XmTextFieldGetMaxLength(Widget widget);
XMLIBEXPORT char *XmTextFieldGetSelection(Widget widget);
XMLIBEXPORT Boolean XmTextFieldGetSelectionPosition(Widget widget, XmTextPosition *left, XmTextPosition *right);
XMLIBEXPORT wchar_t *XmTextFieldGetSelectionWcs(Widget widget);
XMLIBEXPORT char *XmTextFieldGetString(Widget widget);
XMLIBEXPORT wchar_t *XmTextFieldGetStringWcs(Widget widget);
XMLIBEXPORT int XmTextFieldGetSubstring(Widget widget, XmTextPosition start, int num_chars, int buffer_size, char *buffer);
XMLIBEXPORT int XmTextFieldGetSubstringWcs(Widget widget, XmTextPosition start, int num_chars, int buffer_size, wchar_t *buffer);
XMLIBEXPORT void XmTextFieldInsert(Widget widget, XmTextPosition position, char *string);
XMLIBEXPORT void XmTextFieldInsertWcs(Widget widget, XmTextPosition position, wchar_t *wcstring);
XMLIBEXPORT Boolean XmTextFieldPaste(Widget widget);
XMLIBEXPORT Boolean XmTextFieldPosToXY(Widget widget, XmTextPosition position, Position *x, Position *y);
XMLIBEXPORT Boolean XmTextFieldRemove(Widget widget);
XMLIBEXPORT void XmTextFieldReplace(Widget widget, XmTextPosition from_pos, XmTextPosition to_pos, char *value);
XMLIBEXPORT void XmTextFieldReplaceWcs(Widget widget, XmTextPosition from_pos, XmTextPosition to_pos, wchar_t *wcstring);
XMLIBEXPORT void XmTextFieldSetAddMode(Widget widget, Boolean state);
XMLIBEXPORT void XmTextFieldSetCursorPosition(Widget widget, XmTextPosition position);
XMLIBEXPORT void XmTextFieldSetEditable(Widget widget, Boolean editable);
XMLIBEXPORT void XmTextFieldSetHighlight(Widget widget, XmTextPosition left, XmTextPosition right, XmHighlightMode mode);
XMLIBEXPORT void XmTextFieldSetInsertionPosition(Widget widget, XmTextPosition position);
XMLIBEXPORT void XmTextFieldSetMaxLength(Widget widget, int max_length);
XMLIBEXPORT void XmTextFieldSetSelection(Widget widget, XmTextPosition first, XmTextPosition last, Time time);
XMLIBEXPORT void XmTextFieldSetString(Widget widget, char *value);
XMLIBEXPORT void XmTextFieldSetStringWcs(Widget widget, wchar_t *wcstring);
XMLIBEXPORT void XmTextFieldShowPosition(Widget widget, XmTextPosition position);
XMLIBEXPORT XmTextPosition XmTextFieldXYToPos(Widget widget, Position x, Position y);

#if XmVERSION >= 2
XMLIBEXPORT Boolean XmTextFieldCopyLink(Widget widget,
                            Time time);
#endif


#ifdef __cplusplus
}
#endif

#endif /* _XM_TEXTF_H */
