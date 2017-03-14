/**
 *
 * $Id: TextStrSoP.h,v 1.1 2004/08/28 19:23:27 dannybackx Exp $
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

#ifndef _XM_TEXTSTRSOP_H
#define _XM_TEXTSTRSOP_H

#include <Xm/XmP.h>
#include <Xm/Text.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    EditDone,
    EditError,
    EditReject
} XmTextStatus;

typedef enum {
    XmsdLeft,
    XmsdRight
} XmTextScanDirection;
    
typedef struct _XmSourceDataRec {
    XmTextSource source;
    XmTextWidget *widgets;
    XmTextPosition left, right;
    char *ptr;
    char *value;
    char *gap_start;
    char *gap_end;
    char *PSWC_NWLN;
    int length;
    int maxlength;
    int old_length;
    int numwidgets;
    int maxallowed;
    Time prim_time;
    Boolean hasselection;
    Boolean editable;
} XmSourceDataRec, *XmSourceData;

typedef void (*AddWidgetProc)(XmTextSource, XmTextWidget);
typedef int (*CountLinesProc)(XmTextSource, XmTextPosition, unsigned long);
typedef void (*RemoveWidgetProc)(XmTextSource, XmTextWidget);
typedef XmTextPosition (*ReadProc)(XmTextSource,
				   XmTextPosition,
				   XmTextPosition,
				   XmTextBlock);
typedef XmTextStatus (*ReplaceProc)(XmTextWidget,
				    XEvent *,
				    XmTextPosition *,
				    XmTextPosition *,
				    XmTextBlock,
				    Boolean);
typedef XmTextPosition (*ScanProc)(XmTextSource,
				   XmTextPosition,
				   XmTextScanType,
				   XmTextScanDirection,
				   int,
				   Boolean);
typedef Boolean (*GetSelectionProc)(XmTextSource,
				    XmTextPosition *,
				    XmTextPosition *);
typedef void (*SetSelectionProc)(XmTextSource,
				 XmTextPosition,
				 XmTextPosition,
				 Time);


typedef struct _XmTextSourceRec {
    struct _XmSourceDataRec *data;
    AddWidgetProc AddWidget;
    CountLinesProc CountLines;
    RemoveWidgetProc RemoveWidget;
    ReadProc ReadSource;
    ReplaceProc Replace;
    ScanProc Scan;
    GetSelectionProc GetSelection;
    SetSelectionProc SetSelection;
} XmTextSourceRec;


XMLIBEXPORT extern XmTextSource _XmStringSourceCreate(char *value, Boolean is_wchar);
XMLIBEXPORT extern void _XmStringSourceDestroy(XmTextSource source);
XMLIBEXPORT extern char *_XmStringSourceGetValue(XmTextSource source, Boolean want_wchar);
XMLIBEXPORT extern void _XmStringSourceSetValue(XmTextWidget widget, char *value);
XMLIBEXPORT extern Boolean _XmStringSourceHasSelection(XmTextSource source);
XMLIBEXPORT extern Boolean _XmStringSourceGetEditable(XmTextSource source);
XMLIBEXPORT extern void _XmStringSourceSetEditable(XmTextSource source, Boolean editable);
XMLIBEXPORT extern int _XmStringSourceGetMaxLength(XmTextSource source);
XMLIBEXPORT extern void _XmStringSourceSetMaxLength(XmTextSource source, int max);
XMLIBEXPORT extern char *_XmStringSourceGetString(XmTextWidget tw,
				      XmTextPosition from,
				      XmTextPosition to,
				      Boolean want_wchar);
XMLIBEXPORT extern Boolean _XmTextFindStringBackwards(Widget w,
					  XmTextPosition start,
					  char *search_string,
					  XmTextPosition *position);
XMLIBEXPORT extern Boolean _XmTextFindStringForwards(Widget w,
					 XmTextPosition start,
					 char *search_string,
					 XmTextPosition *position);
XMLIBEXPORT extern Boolean _XmStringSourceFindString(Widget w,
					 XmTextPosition start,
					 char *string,
					 XmTextPosition *position);
XMLIBEXPORT extern void _XmStringSourceSetGappedBuffer(XmSourceData data,
					   XmTextPosition position);
XMLIBEXPORT extern Boolean _XmTextModifyVerify(XmTextWidget initiator,
				   XEvent *event,
				   XmTextPosition *start,
				   XmTextPosition *end,
				   XmTextPosition *cursorPos,
				   XmTextBlock block,
				   XmTextBlock newblock,
				   Boolean *freeBlock);

#ifdef __cplusplus
}
#endif

#endif /* _XM_TEXTSTRSOP_H */
