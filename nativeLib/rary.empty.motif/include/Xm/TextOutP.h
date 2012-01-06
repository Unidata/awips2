/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/TextOutP.h,v 1.2 2005/03/29 15:28:00 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1997, 1999, 2000, 2001, 2002 LessTif Development Team
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

#ifndef _XM_TEXTOUTP_H
#define _XM_TEXTOUTP_H

#include <Xm/XmP.h>
#include <Xm/Text.h>
#include <limits.h>  /* for INT_MAX */

#ifdef __cplusplus
extern "C" {
#endif

#define NOLINE		30000
#define PASTENDPOS	INT_MAX

typedef struct _LineTableExtraRec {
  Dimension width;
  Boolean wrappedbychar;
} LineTableExtraRec, *LineTableExtra;

typedef unsigned int LineNum;
typedef enum {
	on,
	off
} OnOrOff;

typedef struct _OutputDataRec {
	XmFontList fontlist;
	unsigned int blinkrate;
	Boolean wordwrap;
	Boolean cursor_position_visible;
	Boolean autoshowinsertpoint;
	Boolean hasfocus;
	Boolean has_rect;
	Boolean handlingexposures;
	Boolean exposevscroll;
	Boolean exposehscroll;
	Boolean resizewidth, resizeheight;
	Boolean scrollvertical, scrollhorizontal;
	Boolean scrollleftside, scrolltopside;
	Boolean ignorevbar;
	Boolean ignorehbar;
	short int cursor_on;
	Boolean refresh_ibeam_off;
	Boolean suspend_hoffset;
	Boolean use_fontset;
	Boolean have_inverted_image_gc;
	OnOrOff blinkstate;
	Position insertx, inserty;
	int number_lines;
	int leftmargin, rightmargin;
	int topmargin, bottommargin;
	int scrollwidth;
	int vsliderSize;
	int hoffset;
	int averagecharwidth;
	int tabwidth;
	short columns, rows;
	Dimension lineheight;
	Dimension minwidth, minheight;
	Dimension prevW;
	Dimension prevH;
	Dimension cursorwidth, cursorheight;
	Dimension font_ascent;
	Dimension font_descent;
	XtIntervalId timerid;
	Pixmap cursor;
	Pixmap add_mode_cursor;
	Pixmap ibeam_off;
	Pixmap stipple_tile;
	GC gc, imagegc;
	Widget vbar, hbar;
	XFontStruct *font;
	GC save_gc;
	short columns_set, rows_set;

	/* rwmcm: Extra stuff that I'm not sure I will need */
	Boolean fontlist_created;

	/* Stuff to support anti-aliased fonts through Xft */
	XmFontType	font_type;
	XtPointer	xft_font;	/* Should be 'XftFont *' but this would require an
					 * additional include file. */
	Boolean		check_set_render_table;
} OutputDataRec, *OutputData;

typedef void (*OutputCreateProc)(Widget, ArgList, Cardinal);
typedef XmTextPosition (*XYToPosProc)(XmTextWidget, Position, Position);
typedef void (*GetPreferredSizeProc)(Widget, Dimension *, Dimension *);
typedef void (*GetValuesProc)(Widget, ArgList, Cardinal);
typedef Boolean (*SetValuesProc)(Widget, Widget, Widget, ArgList, Cardinal *);
typedef void (*DrawInsertionPointProc)(XmTextWidget, XmTextPosition, OnOrOff);
typedef void (*MakePositionVisibleProc)(XmTextWidget, XmTextPosition);
typedef Boolean (*MoveLinesProc)(XmTextWidget, LineNum, LineNum, LineNum);
typedef Boolean (*PosToXYProc)(XmTextWidget, XmTextPosition, Position *, Position *);
typedef Boolean (*MeasureLineProc)(XmTextWidget,
		LineNum,
		XmTextPosition,
		XmTextPosition *,
		LineTableExtraRec **);
typedef void (*DrawProc)(XmTextWidget,
		LineNum,
		XmTextPosition,
		XmTextPosition,
		XmHighlightMode) ;
typedef void (*InvalidateProc)(XmTextWidget,
		XmTextPosition,
		XmTextPosition,
		long);


typedef struct _OutputRec {
	struct _OutputDataRec *data;
	XYToPosProc XYToPos;
	PosToXYProc PosToXY;
	MeasureLineProc MeasureLine;
	DrawProc Draw;
	DrawInsertionPointProc DrawInsertionPoint;
	MakePositionVisibleProc MakePositionVisible;
	MoveLinesProc MoveLines;
	InvalidateProc Invalidate;
	GetPreferredSizeProc GetPreferredSize;
	GetValuesProc GetValues;
	SetValuesProc SetValues;
	XmRealizeOutProc realize;
	XtWidgetProc destroy;
	XmResizeFlagProc resize;
	XtExposeProc expose;
} OutputRec;


void _XmTextDrawDestination(XmTextWidget widget);
void _XmTextClearDestination(XmTextWidget widget, Boolean ignore_sens);
void _XmTextDestinationVisible(Widget w, Boolean turn_on);
void _XmTextChangeBlinkBehavior(XmTextWidget widget, Boolean newvalue);
void _XmTextOutputCreate(Widget wid, ArgList args, Cardinal num_args);
void _XmTextAdjustGC(XmTextWidget tw);
Boolean _XmTextShouldWordWrap(XmTextWidget widget);
Boolean _XmTextScrollable(XmTextWidget widget);
void _XmTextOutputGetSecResData(XmSecondaryResourceData *secResDataRtn);
int _XmTextGetNumberLines(XmTextWidget widget);
Boolean _XmTextGetDisplayRect(Widget w, XRectangle *display_rect);
void _XmTextMarginsProc(Widget w, XmBaselineMargins *margins_rec);
void _XmTextChangeHOffset(XmTextWidget widget, int length);
void _XmTextToggleCursorGC(Widget widget);
void _XmTextFreeContextData(Widget w,
		XtPointer clientData,
		XtPointer callData);
void _XmTextResetClipOrigin(XmTextWidget tw,
		XmTextPosition position,
		Boolean clip_mask_reset) ;
XmTextPosition _XmTextFindLineEnd(XmTextWidget w,
		XmTextPosition pos,
		LineTableExtra *extra);
void _XmTextMovingCursorPosition(XmTextWidget w, XmTextPosition pos);
Boolean _XmTextGetBaselines(Widget widget,
		Dimension **baselines,
		int *line_count) ;
int _XmOut_FontTextWidth(OutputData o, char *s, int l);
int _XmOut_FontMaxWidth(OutputData o);


#define Out_XOffset(o) (o->hoffset)
/*CP:The next 2 macros are badly named and are not used correctly in the sources */
#define Out_XDraw(o) (o->prevW)
#define Out_YDraw(o) (o->prevH)
#define Out_PrevW(o) (o->prevW)
#define Out_PrevH(o) (o->prevH)
/*
	#define Out_FontHeight(o) (o->lineheight)
 */
#ifdef	USE_XFT
# define Out_FontHeight(o)	\
	(Out_XftFont(o) ? Out_XftFont(o)->height : o->lineheight)
#define Out_FontAscent(o)	\
	(Out_XftFont(o) ? Out_XftFont(o)->ascent : o->font->max_bounds.ascent)
#define Out_FontDescent(o)	\
	(Out_XftFont(o) ? Out_XftFont(o)->descent : o->font->max_bounds.descent)
#else
# define Out_FontHeight(o) (o->lineheight)
# define Out_FontAscent(o) (o->font->max_bounds.ascent)
# define Out_FontDescent(o) (o->font->max_bounds.descent)
#endif
#define Out_CursorSave(o) (o->ibeam_off)
#define Out_CursorSaveValid(o) (o->refresh_ibeam_off)
#define Out_CursorIBeam(o) (o->cursor)
#define Out_CursorAddMode(o) (o->add_mode_cursor)
#define Out_CursorStipple(o) (o->stipple_tile)
#define Out_CursorX(o) (o->insertx)
#define Out_CursorY(o) (o->inserty)
#define Out_CursorPositionVisible(o) (o->cursor_position_visible)
#define Out_CursorHeight(o) (o->cursorheight)
#define Out_CursorWidth(o) (o->cursorwidth)

#define Out_Font(o) (o->font)
#define Out_FontList(o) (o->fontlist)
#define Out_FontListCreated(o) (o->fontlist_created)
#define	Out_FontType(o)		(o->font_type)
/*
 * This macro is obsolete
 * #define Out_FontTextWidth(o,s,l) (int)XTextWidth(Out_Font(o), s, l)
 */
#define Out_FontAverageWidth(o) (o->averagecharwidth)
#define Out_Font_Ascent(o) (o->font_ascent)
#define Out_Font_Descent(o) (o->font_descent)
#define Out_DrawGC(o) (o->gc)		/* drawing and copying */
#define Out_DrawGCInverted(o) (o->have_inverted_image_gc)
#define Out_CursorGC(o) (o->imagegc)	/* highlighting text */
#define Out_CopyGC(o) (o->save_gc) /* cursor (not clipped) */

#define Out_BlinkRate(o) (o->blinkrate)
#define Out_BlinkState(o) (o->blinkstate)
#define Out_BlinkOn(o) (o->cursor_on)
#define Out_Columns(o) (o->columns)
#define Out_ColumnsSet(o) (o->columns_set)
#define Out_HasFocus(o) (o->hasfocus)
#define Out_ResizeWidth(o) (o->resizewidth)
#define Out_ResizeHeight(o) (o->resizeheight)
#define Out_MinWidth(o) (o->minwidth)
#define Out_MinHeight(o) (o->minheight)
#define Out_OldLength(o) (o->old_length)
#define Out_Rows(o) (o->rows)
#define Out_RowsSet(o) (o->rows_set)
#define Out_TimerId(o) (o->timerid)
#define Out_TabWidth(o) (o->tabwidth)
#define Out_ScrollVertical(o) (o->scrollvertical)
#define Out_ScrollHorizontal(o) (o->scrollhorizontal)
#define Out_ScrollLeftSide(o) (o->scrollleftside)
#define Out_ScrollTopSide(o) (o->scrolltopside)
#define Out_Vbar(o) (o->vbar)
#define Out_Hbar(o) (o->hbar)
#define Out_ScrollWidth(o) (o->scrollwidth)
#define Out_NumberLines(o) (o->number_lines)
#define Out_WordWrap(o) (o->wordwrap)
#define Out_LeftMargin(o) (o->leftmargin)
#define Out_RightMargin(o) (o->rightmargin)
#define Out_TopMargin(o) (o->topmargin)
#define Out_BottomMargin(o) (o->bottommargin)
#define Out_ExposeVScroll(o) (o->exposevscroll)
#define Out_ExposeHScroll(o) (o->exposehscroll)
#define Out_HandlingExposures(o) (o->handlingexposures)
#define Out_UseFontSet(o) (o->use_fontset)
#define	Out_XftFont(o)	((XftFont *)(o->xft_font))

#ifdef __cplusplus
}
#endif

#endif /* _XM_TEXTOUTP_H */
