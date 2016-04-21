/**
 *
 * $Id: BulletinBP.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
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

#ifndef _XM_BULLETINBP_H
#define _XM_BULLETINBP_H

#include <Xm/BulletinB.h>
#include <Xm/ManagerP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* The bulletin board constraint part */
typedef struct _XmBulletinBoardConstraintPart {
   char empty;
} XmBulletinBoardConstraintPart, *XmBulletinBoardConstraint;

/* Define the bulletin board instance part */
typedef struct {
    Dimension margin_width;
    Dimension margin_height;

    Widget default_button;
    Widget dynamic_default_button;
    Widget cancel_button;
    Widget dynamic_cancel_button;

    XtCallbackList focus_callback;
    XtCallbackList map_callback;
    XtCallbackList unmap_callback;

    XtTranslations text_translations;

    XmFontList button_font_list;
    XmFontList label_font_list;
    XmFontList text_font_list;

    Boolean allow_overlap;
    Boolean default_position;
    Boolean auto_unmanage;
    unsigned char resize_policy;

    Dimension old_width;
    Dimension old_height;
    Dimension old_shadow_thickness;

    unsigned char shadow_type;
    Boolean in_set_values;
    Boolean initial_focus;

    Boolean no_resize;
    unsigned char dialog_style;
    XmString dialog_title;
    Widget shell;
    Widget stupid_dead_field;

    XmGeoMatrix geo_cache;
} XmBulletinBoardPart;

/* Define the full instance record */
typedef struct _XmBulletinBoardRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmBulletinBoardPart bulletin_board;
} XmBulletinBoardRec;

/* Define class part structure */
typedef struct {
    Boolean		always_install_accelerators;
    XmGeoCreateProc	geo_matrix_create;
    XmFocusMovedProc	focus_moved_proc;
    XtPointer		extension;
} XmBulletinBoardClassPart;

/* Define the full class record */
typedef struct _XmBulletinBoardClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmBulletinBoardClassPart bulletin_board_class;
} XmBulletinBoardClassRec;

XMLIBEXPORT extern XmBulletinBoardClassRec xmBulletinBoardClassRec;

#define XmDIALOG_SUFFIX                "_popup"

#define BB_CancelButton(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.cancel_button)

#define BB_DynamicCancelButton(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.dynamic_cancel_button)

#define BB_DefaultButton(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.default_button)

#define BB_DynamicDefaultButton(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.dynamic_default_button)

#define BB_MarginHeight(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.margin_height)

#define BB_MarginWidth(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.margin_width)

#define BB_ButtonFontList(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.button_font_list)

#define BB_LabelFontList(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.label_font_list)

#define BB_TextFontList(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.text_font_list)

#define BB_StringDirection(w) \
	(((XmBulletinBoardWidget)(w))->manager.string_direction)

#define BB_ResizePolicy(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.resize_policy)

#define BB_InSetValues(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.in_set_values)

#define BB_InitialFocus(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.initial_focus)

Widget _XmBB_CreateButtonG(Widget bb, XmString l_string, char *name);
Widget _XmBB_CreateLabelG( Widget bb, XmString l_string, char *name);
void _XmBulletinBoardSizeUpdate(Widget wid);
void _XmBulletinBoardFocusMoved(Widget wid,
				XtPointer client_data,
				XtPointer data);
void _XmBulletinBoardReturn(Widget wid, XEvent *event,
			    String *params, Cardinal *numParams);
void _XmBulletinBoardCancel(Widget wid, XEvent *event,
			    String *params, Cardinal *numParams);
void _XmBulletinBoardMap(Widget wid);
void _XmBulletinBoardUnmap(Widget wid);
void _XmBulletinBoardSetDefaultShadow(Widget button);
void _XmBulletinBoardSetDynDefaultButton(Widget wid,
					 Widget newDefaultButton);
void _XmBBUpdateDynDefaultButton(Widget bb);


#ifdef __cplusplus
}
#endif

#endif /* _XM_BULLETINBP_H */
