/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/TextFP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2001, 2002 LessTif Development Team
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

#ifndef _XM_TEXTFP_H
#define _XM_TEXTFP_H

#include <Xm/PrimitiveP.h>
#include <Xm/TextF.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	Boolean Echo;		/* Echo characters to window? */
	Boolean AllowSelection;	/* Allow selections to be copied/pasted? */

	  /* Stuff that will go away with the Clipboard routines */
	char *SelectionText;	/* pointer to text selection, when needed */
	int SelectionLen;		/* length */
	
	XmTextPosition OldHighlightStart;	/* save data */
	XmTextPosition OldHighlightEnd;
	XmTextPosition OldCursorPos;	/* previous position */
	int OldCursorX;		/* previous pixel pos of cursor */

	int FastInsertCursorStart;	/* data storage for some text optimization */
	int FastInsertTextLen;

	Dimension FontHeight;
	int XDraw;			/* x location of text drawing area */
	int YDraw;			/* y location of text drawing area */
	int XOffset;		/* offset from x=0 to start of text string */
	int OldXOffset;
	int YOffset;		/* y pixel offset to baseline of font */
	int TextWidth;		/* char width of text */
	int OldTextWidth;
} XmTextFieldPartLesstifExtension;
	
typedef struct {
	XtCallbackList activate_callback;	/* Command activate callback */
	XtCallbackList focus_callback;		/* Verify losing focus callback */
	XtCallbackList losing_focus_callback;	/* Verify losing focus callback */
	XtCallbackList modify_verify_callback;	/* Verify value to change callback */
	XtCallbackList wcs_modify_verify_callback; /* Verify value to change callback */
	XtCallbackList motion_verify_callback; /* Verify insert cursor position to
						  change callback */
	XtCallbackList gain_primary_callback; /* Gained ownership of Primary Selection */
	XtCallbackList lose_primary_callback; /* Lost ownership of Primary Selection */
	XtCallbackList value_changed_callback; /* Notify that value has change callback */
	char * value;		/* pointer to widget value stored as char* */
	wchar_t * wc_value;		/* pointer to widget value stored as wchar_t* */

	XmFontList font_list;	/* Uses only the font portion of fontlist */
	XFontStruct *font;	        /* font retrieved from the fontlist */
	XmTextScanType *selection_array; /* Description of what to cycle
					     through on selections */
	_XmHighlightData highlight;    /* Info on the highlighting regions. */

	GC gc;			/* Normal GC for drawing text and cursor */
	GC image_gc;		/* Image GC for drawing text cursor*/
	GC save_gc;                 /* GC for saving/restoring under IBeam */

	Pixmap ibeam_off;		/* pixmap for area under the IBeam */
	Pixmap add_mode_cursor;	/* The add mode cursor pixmap */
	Pixmap cursor;		/* The ibeam cursor stencil */
	Pixmap putback;		/* AVAILABLE: was in 1.1 but not really used */
	Pixmap stipple_tile;	/* The tile pattern for the stippled I-beam */
	Pixmap image_clip;		/* The clip rect needed for image gc */

	XmTextPosition cursor_position;/* CursorPos - text cursor position */
	XmTextPosition new_h_offset;/* Used in setvaluesalmost proc */
	XmTextPosition h_offset;  	/* The x position of the first character
	                               (relative to left edge of the widget) */
	XmTextPosition orig_left;     /* unused */
	XmTextPosition orig_right;    /* unused */
	XmTextPosition prim_pos_left; /* HighlightStart */
	XmTextPosition prim_pos_right; /* HighlightEnd */
	XmTextPosition prim_anchor;	/* HighlightPivot */

	XmTextPosition sec_pos_left; /* usused */
	XmTextPosition sec_pos_right; /* usused */
	XmTextPosition sec_anchor;	/* usused */

	XmTextPosition stuff_pos;	/* Position to stuff the primary selection */

	Position select_pos_x;    /* x position for timer-based scrolling */

	Time prim_time;             /* Timestamp of primary selection */
	Time dest_time;             /* Timestamp of destination selection */
	Time sec_time;              /* Timestamp of secondary selection */
	Time last_time;             /* Time of last selection event */

	XtIntervalId timer_id;	/* Blinking cursor timer */
	XtIntervalId select_id;     /* Timer based scrolling identifier */

	int blink_rate;		/* Rate of blinking text cursor in msec */
	int selection_array_count;  /* Selection array count */
	int threshold;		/* Selection threshold */
	int size_allocd;		/* Size allocated for value string */
	int string_length;          /* The number of characters in the string 
				   (including the trailing NULL) */
	int cursor_height;		/* Save cursor dimensions */
	int cursor_width;		/* Save cursor dimensions */
	int sarray_index;		/* Index into selection array */
	int max_length;		/* Maximum number of character that can be
				   inserted into the text field widget */

	int max_char_size;          /* Max bytes per character in cur locale */
	short columns;		/* The number of characters in the width */

	Dimension margin_width;	/* Height between text borders and text */
	Dimension margin_height;	/* Width between text borders and text */
	Dimension average_char_width;/* Average character width based on font */
	Dimension margin_top;   /* Height between text borders and top of text */
	Dimension margin_bottom;/* Height between text borders and bottom of text */
	Dimension font_ascent;  /* Ascent of font or fontset used by widget */
	Dimension font_descent;  /* Descent of font or fontset used by widget */

	Boolean resize_width;	/* Allows the widget to grow horizontally
				   when borders are reached */
	Boolean pending_delete;	/* Delete primary selection on insert when
				   set to True */
	Boolean editable;		/* Sets editablility of text */
	Boolean verify_bell;          /* Determines if bell is sounded when verify
	                               *  callback returns doit - False
	                               */
	Boolean cursor_position_visible;	/* Sets visibility of insert cursor */

	Boolean traversed;          /* Flag used with losing focus verification to
	                               indicate a traversal key pressed event */
	Boolean add_mode;		/* Add mode for cursor movement */
	Boolean has_focus;		/* Flag that indicates whether the widget
			           has input focus */
	Boolean blink_on;		/* State of Blinking insert cursor */
	short int cursor_on;	/* Indicates whether the cursor is visible */
	Boolean refresh_ibeam_off;	/* Indicates whether the area under IBeam needs
				   to be re-captured */
	Boolean have_inverted_image_gc;  /* fg/bg of image gc have been swapped */
	Boolean has_primary;	/* Indicates that is has the
				   primary selection */
	Boolean has_secondary;	/* Indicates that is has the
				   secondary selection */
	Boolean has_destination;	/* Indicates that is has the
				   destination selection */
	Boolean sec_drag;           /* Indicates a secondary drag was made */ 
	Boolean selection_move;	/* Indicates that the action requires a
				   secondary move (i.e. copy & cut) */
	Boolean pending_off;	/* indicates pending delete state */
	Boolean fontlist_created;   /* Indicates that the text field widget created
				   it's own fontlist */
	Boolean has_rect;		/* currently has clipping rectangle */
	Boolean do_drop;		/* Indicates that the widget the recieved the
				   button release, did not have a previous
	                               button press, so it is o.k. to request
				   the MOTIF_DROP selection. */
	Boolean cancel;		/* Cancels selection actions when true */
	Boolean extending;		/* Indicates extending primary selection */
	Boolean sec_extending;      /* Indicates extending secondary selection */
	Boolean changed_visible;    /* Indicates whether the dest_visible flag
	                               is in a temporary changed state */
	Boolean have_fontset;       /* The widgets font is a fontset, not a 
				 * fontstruct... use R5 draw routines */
	Boolean in_setvalues;	/* used to disable unnecessary redisplays */
	Boolean do_resize;		/* used to prevent inappropriate resizes */
	Boolean redisplay;		/* used to set redisplay flag in setvalues */
	Boolean overstrike;		/* overstrike mode for character input */
	Boolean sel_start;		/* overstrike mode for character input */
	XtPointer extension;	/* Pointer to extension record. */

#ifdef BIDI
/* BIDI: Add the following for bi-directional text support */
	XmTextMode text_mode;       /* mode is VISUAL, IMPLICIT */
	XmCsdMode  csd_mode;        /* csd is AUTO,INITIAL,MIDDLE,FINAL, etc.*/
	XmNssMode  nss_mode;        /* nss is BILINGUAL,ARABIC.HINDU or PASSTHR */
	Boolean symmetric_swap;     /* should we perform symmetric swapping? */
	Boolean expand_tail;        /* should we expand the tail for seen family */
	Boolean word_break;
	Boolean text_compose;       /* composed tashkeel over letter */
/* end BIDI */
#endif

	Boolean	check_set_render_table;
	Boolean	use_xft;
} XmTextFieldPart;

/* Define the full instance record */
typedef struct _XmTextFieldRec {
	CorePart core;
	XmPrimitivePart primitive;
	XmTextFieldPart text;
} XmTextFieldRec;

/* Define class part structure */
typedef struct {
	XtPointer extension;
} XmTextFieldClassPart;

/* Define the full class record */
typedef struct _XmTextFieldClassRec {
	CoreClassPart core_class;
	XmPrimitiveClassPart primitive_class;
	XmTextFieldClassPart textfield_class;
} XmTextFieldClassRec;

/* External definition for class record */

XMLIBEXPORT extern XmTextFieldClassRec xmTextFieldClassRec;

#define TF_ALLOC_SIZE 256

#define TextF__(w) ((XmTextFieldWidget)(w))->text
#define TextF__E__(w) ((XmTextFieldPartLesstifExtension *) (((XmTextFieldWidget)(w))->text.extension))

#define TextF_Alloc(w) (TextF__(w).size_allocd)
#define TextF_BlinkRate(w) (TextF__(w).blink_rate)
#define TextF_BlinkOn(w) (TextF__(w).blink_on)
#define TextF_Columns(w) (TextF__(w).columns)
#define TextF_CursorPos(w) (TextF__(w).cursor_position)
#define TextF_CursorPositionVisible(w) (TextF__(w).cursor_position_visible)
#define TextF_Editable(w) (TextF__(w).editable)
#define TextF_Font(w) (TextF__(w).font)
#define TextF_FontList(w) (TextF__(w).font_list)
#define TextF_FontListCreated(w) (TextF__(w).fontlist_created)
#define TextF_HasFocus(w) (TextF__(w).has_focus)
#define TextF_HighlightStart(w) (TextF__(w).prim_pos_left)
#define TextF_HighlightEnd(w) (TextF__(w).prim_pos_right)
#define TextF_HighlightPivot(w) (TextF__(w).prim_anchor)
#define TextF_SecondaryStart(w) (TextF__(w).sec_pos_left)
#define TextF_SecondaryEnd(w) (TextF__(w).sec_pos_right)
#define TextF_SecondaryPivot(w) (TextF__(w).sec_anchor)
#define TextF_MarginHeight(w) (TextF__(w).margin_height)
#define TextF_MarginWidth(w) (TextF__(w).margin_width)
#define TextF_MaxLength(w) (TextF__(w).max_length)
#define TextF_Length(w) (TextF__(w).string_length)
#define TextF_PendingDelete(w) (TextF__(w).pending_delete)
#define TextF_ResizeWidth(w) (TextF__(w).resize_width)
#define TextF_SelectId(w) (TextF__(w).select_id)
#define TextF_SelectionArray(w) (TextF__(w).selection_array)
#define TextF_SelectionArrayCount(w) (TextF__(w).selection_array_count)
#define TextF_SelectThreshold(w) (TextF__(w).select_threshold)
#define TextF_TimerId(w) (TextF__(w).timer_id)
#define TextF_Value(w) (TextF__(w).value)
#define TextF_ValueWcs(w) (TextF__(w).wc_value)
#define TextF_VerifyBell(w) (TextF__(w).verify_bell)
#define TextF_ViewHeight(w) (TextF__(w).margin_top)
#define TextF_ViewWidth(w) (TextF__(w).margin_bottom)

#define TextF_HighlightMode(w) (TextF__(w).highlight.list->mode)


	/* Lesstif Extensions */
#define TextF_XOffset(w) (TextF__E__(w)->XOffset)
#define TextF_YOffset(w) (TextF__E__(w)->YOffset)
#define TextF_OldXOffset(w) (TextF__E__(w)->OldXOffset)
#define TextF_OldCursorPos(w) (TextF__E__(w)->OldCursorPos)
#define TextF_OldCursorX(w) (TextF__E__(w)->OldCursorX)
#define TextF_Echo(w) (TextF__E__(w)->Echo)
#define TextF_AllowSelection(w) (TextF__E__(w)->AllowSelection)
#define TextF_SelectionText(w) (TextF__E__(w)->SelectionText)
#define TextF_SelectionLen(w) (TextF__E__(w)->SelectionLen)
#define TextF_OldHighlightStart(w) (TextF__E__(w)->OldHighlightStart)
#define TextF_OldHighlightEnd(w) (TextF__E__(w)->OldHighlightEnd)
#define TextF_FastInsertCursorStart(w) (TextF__E__(w)->FastInsertCursorStart)
#define TextF_FastInsertTextLen(w) (TextF__E__(w)->FastInsertTextLen)
#define TextF_FontHeight(w) (TextF__E__(w)->FontHeight)
#define TextF_XDraw(w) (TextF__E__(w)->XDraw)
#define TextF_YDraw(w) (TextF__E__(w)->YDraw)
#define TextF_TextWidth(w) (TextF__E__(w)->TextWidth)
#define TextF_OldTextWidth(w) (TextF__E__(w)->OldTextWidth)

#define TextF_ActivateCallback(w) (TextF__(w).activate_callback)
#define TextF_FocusCallback(w) (TextF__(w).focus_callback)
#define TextF_LosingFocusCallback(w) (TextF__(w).losing_focus_callback)
#define TextF_ModifyVerifyCallback(w) (TextF__(w).modify_verify_callback)
#define TextF_WcsModifyVerifyCallback(w) (TextF__(w).wcs_modify_verify_callback)
#define TextF_MotionVerifyCallback(w) (TextF__(w).motion_verify_callback)
#define TextF_GainPrimaryCallback(w) (TextF__(w).gain_primary_callback)
#define TextF_LosePrimaryCallback(w) (TextF__(w).lose_primary_callback)
#define TextF_ValueChangedCallback(w) (TextF__(w).value_changed_callback)

	/* GC stuff */  
#define TextF_DrawGC(w) (TextF__(w).gc)		/* drawing and copying */
#define TextF_DrawGCInverted(w) (TextF__(w).have_inverted_image_gc)
#define TextF_CursorGC(w) (TextF__(w).image_gc) /* highlighting text */
#define TextF_CopyGC(w) (TextF__(w).save_gc)     /* cursor (not clipped) */
#define TextF_CursorSave(w) (TextF__(w).ibeam_off)
#define TextF_CursorSaveValid(w) (TextF__(w).refresh_ibeam_off)
#define TextF_CursorIBeam(w) (TextF__(w).cursor)
#define TextF_CursorAddMode(w) (TextF__(w).add_mode_cursor)
#define TextF_AddMode(w) (TextF__(w).add_mode)
#define TextF_CursorStipple(w) (TextF__(w).stipple_tile)

	/* Font functions */
#define TextF_FontTextWidth(w,s,l) (int)XTextWidth(TextF__(w).font, s, l)
#define TextF_FontAverageWidth(w) (TextF__(w).average_char_width)

#ifdef	USE_XFT
#define TextF_FontAscent(w)			\
	(TextF_UseXft(w) ? 			\
		(TextF_XftFont(w)->ascent) :	\
		(TextF__(w).font->max_bounds.ascent))

#define TextF_FontDescent(w)			\
	(TextF_UseXft(w) ?			\
		(TextF_XftFont(w)->descent) :	\
		(TextF__(w).font->max_bounds.descent))
#else
#define TextF_FontAscent(w) (TextF__(w).font->max_bounds.ascent)
#define TextF_FontDescent(w) (TextF__(w).font->max_bounds.descent)
#endif

#define	TextF_UseXft(w)	(((XmTextFieldWidget)(w))->text.use_xft)
#define	TextF_XftFont(w)	(((XmTextFieldWidget)(w))->text.font_list->renditions[0]->xft_font)

/* Highlight */
#define TextF_Highlight(w) (TextF__(w).highlight)

#ifdef __cplusplus
}
#endif

#endif /* _XM_TEXTFP_H */
