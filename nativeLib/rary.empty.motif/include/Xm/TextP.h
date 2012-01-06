/**
 *
 * $Id: TextP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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

#ifndef _XM_TEXTP_H
#define _XM_TEXTP_H

#include <Xm/PrimitiveP.h>
#include <Xm/Text.h>
#include <Xm/TextOutP.h>
#include <Xm/TextInP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _InputRec *Input;
typedef struct _OutputRec *Output;

#define MAXINT	INT_MAX
#define NODELTA	MAXINT
#define TEXTWIDGETCLASS	"Text"

#define GetSrc(widget)	(((XmTextWidget)(widget))->text.source)

typedef struct {
    XmTextPosition start;
    Boolean changed;
    XmTextPosition changed_position;
    Boolean past_end;
    LineTableExtra extra;
} LineRec, *Line;

typedef struct _XmTextLineTableRec {
    unsigned int start_pos : 31,
		 virt_line : 1;
} XmTextLineTableRec, *XmTextLineTable;

typedef struct {
    XmTextPosition from, to;
} RangeRec;

typedef struct {
    Cardinal number;
    Cardinal maximum;
    RangeRec *range;
} Ranges;

typedef struct {
    XmTextSource source;
    XtCallbackList activate_callback;
    XtCallbackList focus_callback;
    XtCallbackList losing_focus_callback;
    XtCallbackList value_changed_callback;
    XtCallbackList modify_verify_callback;
    XtCallbackList wcs_modify_verify_callback;
    XtCallbackList motion_verify_callback;
    XtCallbackList gain_primary_callback;
    XtCallbackList lose_primary_callback;

    char *value;
    wchar_t *wc_value; 
    Dimension margin_height;
    Dimension margin_width;
    Dimension cursor_position_x;

    OutputCreateProc output_create;
    InputCreateProc input_create;

    XmTextPosition top_character;
    XmTextPosition bottom_position;
    XmTextPosition cursor_position;
    int max_length;
    int edit_mode;

    Boolean auto_show_cursor_position;
    Boolean editable;
    Boolean verify_bell;
    Boolean add_mode;
    Boolean traversed;
    Boolean in_redisplay;
    Boolean needs_redisplay;
    Boolean in_refigure_lines;
    Boolean needs_refigure_lines;
    Boolean in_setvalues;
    Boolean in_resize;
    Boolean in_expose;
    Boolean highlight_changed;
    Boolean pendingoff;

    char char_size;
    OnOrOff on_or_off;

    Output output;
    Input input;

    XmTextPosition first_position;
    XmTextPosition last_position;
    XmTextPosition forget_past;
    XmTextPosition force_display;
    XmTextPosition new_top;
    XmTextPosition last_top_char;
    XmTextPosition dest_position;

    int disable_depth;
    int pending_scroll;
    int total_lines;
    int top_line;
    int vsbar_scrolling;

    Cardinal number_lines;
    Cardinal maximum_lines;
    Line line;

    Ranges repaint;
    _XmHighlightData highlight;
    _XmHighlightData old_highlight;
    Widget inner_widget;

    XmTextLineTable line_table;
    unsigned int table_size;
    unsigned int table_index;
} XmTextPart;

/* Define the full instance record */
typedef struct _XmTextRec {
    CorePart core;
    XmPrimitivePart primitive;
    XmTextPart text;
} XmTextRec;

/* Define class part structure */
typedef struct {
    XtPointer extension;
} XmTextClassPart;

/* Define the full class record */
typedef struct _XmTextClassRec {
    CoreClassPart core_class;
    XmPrimitiveClassPart primitive_class;
    XmTextClassPart text_class;
} XmTextClassRec;

/* External definition for class record */

XMLIBEXPORT extern XmTextClassRec xmTextClassRec;


/* Inner Widget: base object for the Output and Input "subparts".  This really
 * is only to hold the values for resource specification.  An Output and an
 * input object will be created later that will copy the information from
 * these records into their own.
 */

typedef struct {
    OutputDataRec out;
    InputDataRec in;
} XmTextInnerPart;

/* Define the full instance record */
typedef struct _XmTextInnerRec {
    ObjectPart object;
    XmTextInnerPart inner;
} XmTextInnerRec;

/* Define the full class record */
typedef struct _XmTextInnerClassRec {
    ObjectClassPart object_class;
    XmTextClassPart text_src_class; /* just a dummy */
} XmTextInnerClassRec;

/* External definition for class record */

XMLIBEXPORT extern XmTextInnerClassRec xmTextInnerClassRec;
typedef struct _XmTextInnerRec *XmTextInnerWidget;


XMLIBEXPORT extern int _XmTextGetTotalLines(Widget widget);
XMLIBEXPORT extern XmTextLineTable _XmTextGetLineTable(Widget widget,
					   int *total_lines);
XMLIBEXPORT extern void _XmTextRealignLineTable(XmTextWidget widget,
				    XmTextLineTable *temp_table,
				    int *temp_table_size,
				    register unsigned int cur_index,
				    register XmTextPosition cur_start,
				    register XmTextPosition cur_end);
XMLIBEXPORT extern unsigned int _XmTextGetTableIndex(XmTextWidget widget,
					 XmTextPosition pos);
XMLIBEXPORT extern void _XmTextUpdateLineTable(Widget widget,
				   XmTextPosition start,
				   XmTextPosition end,
				   XmTextBlock block,
				   Boolean update);
XMLIBEXPORT extern void _XmTextSetCursorPosition(Widget w,
				     XmTextPosition pos);
XMLIBEXPORT extern void _XmTextLineInfo(XmTextWidget widget,
			    LineNum line,
			    XmTextPosition *startpos,
			    LineTableExtra *extra);
XMLIBEXPORT extern LineNum _XmTextPosToLine(XmTextWidget widget,
				XmTextPosition position);
XMLIBEXPORT extern void _XmTextMarkRedraw(XmTextWidget widget,
			      XmTextPosition left,
			      XmTextPosition right);
XMLIBEXPORT extern void _XmTextDisableRedisplay(XmTextWidget widget,
				    Boolean losesbackingstore);
XMLIBEXPORT extern void _XmTextEnableRedisplay(XmTextWidget widget) ;
XMLIBEXPORT extern void _XmTextDelete(XmTextWidget w, XEvent *ev,
		XmTextPosition start, XmTextPosition end);
XMLIBEXPORT extern void _XmTextSetTopCharacter ( Widget w, XmTextPosition position);
XMLIBEXPORT extern LineNum _XmTextNumLines( XmTextWidget widget);
void _XmTextInvalidate(XmTextWidget w, XmTextPosition position,
		       XmTextPosition topos, long delta);

#define Text_Source(w) (((XmTextWidget)(w))->text.source)
#define Text_ActivateCallback(w) (((XmTextWidget)(w))->text.activate_callback)
#define Text_FocusCallback(w) (((XmTextWidget)(w))->text.focus_callback)
#define Text_LosingFocusCallback(w) (((XmTextWidget)(w))->text.losing_focus_callback)
#define Text_ValueChangedCallback(w) (((XmTextWidget)(w))->text.value_changed_callback)
#define Text_ModifyVerifyCallback(w) (((XmTextWidget)(w))->text.modify_verify_callback)
#define Text_WcsModifyVerifyCallback(w) (((XmTextWidget)(w))->text.wcs_modify_verify_callback)
#define Text_MotionVerifyCallback(w) (((XmTextWidget)(w))->text.motion_verify_callback)
#define Text_GainPrimaryCallback(w) (((XmTextWidget)(w))->text.gain_primary_callback)
#define Text_LosePrimaryCallback(w) (((XmTextWidget)(w))->text.lose_primary_callback)
#define Text_Value(w) (((XmTextWidget)(w))->text.value)
#define Text_WcsValue(w) (((XmTextWidget)(w))->text.wc_value)
#define Text_MarginWidth(w) (((XmTextWidget)(w))->text.margin_width)
#define Text_MarginHeight(w) (((XmTextWidget)(w))->text.margin_height)
#define Text_CursorPositionX(w) (((XmTextWidget)(w))->text.cursor_position_x)
#define Text_OutputCreate(w) (((XmTextWidget)(w))->text.output_create)
#define Text_InputCreate(w) (((XmTextWidget)(w))->text.input_create)
#define Text_TopPos(w) (((XmTextWidget)(w))->text.top_character)
#define Text_BottomPos(w) (((XmTextWidget)(w))->text.bottom_position)
#define Text_CursorPos(w) (((XmTextWidget)(w))->text.cursor_position)
#define Text_FirstPos(w) (((XmTextWidget)(w))->text.first_position)
#define Text_LastPos(w) (((XmTextWidget)(w))->text.last_position)
#define Text_MaxLength(w) (((XmTextWidget)(w))->text.max_length)
#define Text_EditMode(w) (((XmTextWidget)(w))->text.edit_mode)
#define Text_AutoShowCursorPosition(w) (((XmTextWidget)(w))->text.auto_show_cursor_position)
#define Text_Editable(w) (((XmTextWidget)(w))->text.editable)
#define Text_VerifyBell(w) (((XmTextWidget)(w))->text.verify_bell)
#define Text_AddMode(w) (((XmTextWidget)(w))->text.add_mode)
#define Text_Traversed(w) (((XmTextWidget)(w))->text.traversed)
#define Text_InRedisplay(w) (((XmTextWidget)(w))->text.in_redisplay)
#define Text_NeedsRedisplay(w) (((XmTextWidget)(w))->text.needs_redisplay)
#define Text_InRefigureLines(w) (((XmTextWidget)(w))->text.in_refigure_lines)
#define Text_NeedsRefigureLines(w) (((XmTextWidget)(w))->text.needs_refigure_lines)
#define Text_InSetValues(w) (((XmTextWidget)(w))->text.in_setvalues)
#define Text_InResize(w) (((XmTextWidget)(w))->text.in_resize)
#define Text_InExpose(w) (((XmTextWidget)(w))->text.in_expose)
#define Text_HighlightChanged(w) (((XmTextWidget)(w))->text.highlight_changed)
#define Text_PendingOff(w) (((XmTextWidget)(w))->text.pendingoff)
#define Text_CharSize(w) (((XmTextWidget)(w))->text.char_size)
#define Text_OnOrOff(w) (((XmTextWidget)(w))->text.on_or_off)
#define Text_Output(w) (((XmTextWidget)(w))->text.output)
#define Text_OutputData(w) (Text_Output(w)->data)
#define Text_Input(w) (((XmTextWidget)(w))->text.input)
#define Text_InputData(w) (Text_Input(w)->data)
#define Text_ForgetPast(w) (((XmTextWidget)(w))->text.forget_past)
#define Text_ForceDisplay(w) (((XmTextWidget)(w))->text.force_display)
#define Text_NewTop(w) (((XmTextWidget)(w))->text.new_top)
#define Text_LastTopChar(w) (((XmTextWidget)(w))->text.last_top_char)
#define Text_DestPosition(w) (((XmTextWidget)(w))->text.dest_position)
#define Text_DisableDepth(w) (((XmTextWidget)(w))->text.disable_depth)
#define Text_PendingScroll(w) (((XmTextWidget)(w))->text.pending_scroll)
#define Text_TotalLines(w) (((XmTextWidget)(w))->text.total_lines)
#define Text_TopLine(w) (((XmTextWidget)(w))->text.top_line)
#define Text_VSBarScrolling(w) (((XmTextWidget)(w))->text.vsbar_scrolling)
#define Text_Line(w) (((XmTextWidget)(w))->text.line)
#define Text_LineCount(w) (((XmTextWidget)(w))->text.number_lines)
#define Text_LineMax(w) (((XmTextWidget)(w))->text.maximum_lines)
#define Text_Repaint(w) (((XmTextWidget)(w))->text.repaint)
#define Text_Highlight(w) (((XmTextWidget)(w))->text.highlight)
#define Text_OldHighlight(w) (((XmTextWidget)(w))->text.old_highlight)
#define Text_InnerWidget(w) (XmTextInnerWidget)(((XmTextWidget)(w))->text.inner_widget)
#define Text_LineTable(w) (((XmTextWidget)(w))->text.line_table)
#define Text_TableSize(w) (((XmTextWidget)(w))->text.table_size)
#define Text_TableIndex(w) (((XmTextWidget)(w))->text.table_index)

#ifdef __cplusplus
};
#endif

#endif /* _XM_TEXTP_H */
