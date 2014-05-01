/**
 *
 * $Id: SelectioBP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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

#ifndef _XM_SELECTIOBP_H
#define _XM_SELECTIOBP_H

#include <Xm/BulletinBP.h>
#include <Xm/SelectioB.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef XmUNSPECFIED
#define XmUNSPECIFIED	(~0)
#endif

/* Define the selection box instance part */
typedef struct {
    Widget list_label;
    XmString list_label_string;

    Widget list;
    XmStringTable list_items;
    int list_item_count;
    int list_visible_item_count;
    int list_selected_item_position;

    Widget selection_label;
    XmString selection_label_string;

    Widget text;
    XmString text_string;
    short text_columns;

    Widget work_area;

    Widget separator;

    Widget ok_button;
    XmString ok_label_string;

    Widget apply_button;
    XmString apply_label_string;

    XmString cancel_label_string;

    Widget help_button;
    XmString help_label_string;

    XtCallbackList ok_callback;
    XtCallbackList apply_callback;
    XtCallbackList cancel_callback;
    XtCallbackList no_match_callback;

    XtAccelerators text_accelerators;

    Boolean must_match;
    Boolean adding_sel_widgets;	 /* anyone know what this is for ? */
    Boolean minimize_buttons;

    unsigned char dialog_type;
    unsigned char child_placement;

} XmSelectionBoxPart;

/* Define the full instance record */
typedef struct _XmSelectionBoxRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmBulletinBoardPart bulletin_board;
    XmSelectionBoxPart selection_box;
} XmSelectionBoxRec;

/* Define class part structure */
typedef struct {
    XtCallbackProc list_callback;
    XtPointer extension;
} XmSelectionBoxClassPart;

/* Defint the full class record */
typedef struct _XmSelectionBoxClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmBulletinBoardClassPart bulletin_board_class;
    XmSelectionBoxClassPart selection_box_class;
} XmSelectionBoxClassRec;

XMLIBEXPORT extern XmSelectionBoxClassRec xmSelectionBoxClassRec;

#define SB_ListLabel(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.list_label)

#define SB_List(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.list)

#define SB_SelectionLabel(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.selection_label)

#define SB_Text(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.text)

#define SB_WorkArea(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.work_area)

#define SB_Separator(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.separator)

#define SB_OkButton(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.ok_button)

#define SB_ApplyButton(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.apply_button)

#define SB_CancelButton(w) \
	(((XmSelectionBoxWidget) (w))->bulletin_board.cancel_button)

#define SB_HelpButton(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.help_button)

#define SB_DefaultButton(w) \
	(((XmSelectionBoxWidget) (w))->bulletin_board.default_button)

#define SB_MarginHeight(w) \
	(((XmSelectionBoxWidget) (w))->bulletin_board.margin_height)

#define SB_MarginWidth(w) \
	(((XmSelectionBoxWidget) (w))->bulletin_board.margin_width)

#define SB_ButtonFontList(w) \
	(((XmSelectionBoxWidget) (w))->bulletin_board.button_font_list)

#define SB_LabelFontList(w) \
	(((XmSelectionBoxWidget) (w))->bulletin_board.label_font_list)

#define SB_TextFontList(w) \
	(((XmSelectionBoxWidget) (w))->bulletin_board.text_font_list)

#define SB_StringDirection(w) \
	(((XmSelectionBoxWidget)(w))->manager.string_direction)

#define SB_AddingSelWidgets(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.adding_sel_widgets)

#define SB_TextAccelerators(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.text_accelerators)

#define SB_ListItemCount(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.list_item_count)

#define SB_ListSelectedItemPosition(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.list_selected_item_position)

#define SB_ListVisibleItemCount(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.list_visible_item_count)

#define SB_TextColumns(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.text_columns)

#define SB_MinimizeButtons(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.minimize_buttons)

#define SB_MustMatch(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.must_match)

/*
 * internal functions
 */
void _XmSelectionBoxCreateListLabel(XmSelectionBoxWidget sel);
void _XmSelectionBoxCreateSelectionLabel(XmSelectionBoxWidget sel);
void _XmSelectionBoxCreateList(XmSelectionBoxWidget sel);
void _XmSelectionBoxCreateText(XmSelectionBoxWidget sel);
void _XmSelectionBoxCreateSeparator(XmSelectionBoxWidget sel);
void _XmSelectionBoxCreateOkButton(XmSelectionBoxWidget sel);
void _XmSelectionBoxCreateApplyButton(XmSelectionBoxWidget sel);
void _XmSelectionBoxCreateCancelButton(XmSelectionBoxWidget sel);
void _XmSelectionBoxCreateHelpButton(XmSelectionBoxWidget sel);
XmGeoMatrix _XmSelectionBoxGeoMatrixCreate(Widget wid, Widget instigator,
					   XtWidgetGeometry *desired);
Boolean _XmSelectionBoxNoGeoRequest(XmGeoMatrix geoSpec);
void _XmSelectionBoxGetSelectionLabelString(Widget wid, int resource_offset,
					    XtArgVal *value);
void _XmSelectionBoxGetListLabelString(Widget wid, int resource_offset,
				       XtArgVal *value);
void _XmSelectionBoxGetTextColumns(Widget wid, int resource_offset,
				   XtArgVal *value);
void _XmSelectionBoxGetTextString(Widget wid, int resource_offset,
				  XtArgVal *value);
void _XmSelectionBoxGetListItems(Widget wid, int resource_offset,
				 XtArgVal *value);
void _XmSelectionBoxGetListItemCount(Widget wid, int resource_offset,
				     XtArgVal *value);
void _XmSelectionBoxGetListVisibleItemCount(Widget wid, int resource_offset,
					    XtArgVal *value);
void _XmSelectionBoxGetOkLabelString(Widget wid, int resource_offset,
				     XtArgVal *value);
void _XmSelectionBoxGetApplyLabelString(Widget wid, int resource_offset,
					XtArgVal *value);
void _XmSelectionBoxGetCancelLabelString(Widget wid, int resource_offset,
					 XtArgVal *value);
void _XmSelectionBoxGetHelpLabelString(Widget wid, int resource_offset,
				       XtArgVal *value);
void _XmSelectionBoxUpOrDown(Widget wid, XEvent *event,
			     String *argv, Cardinal *argc);
void _XmSelectionBoxRestore(Widget wid, XEvent *event,
			    String *argv, Cardinal *argc);

#ifdef __cplusplus
}
#endif

#endif /* _XM_SELECTIOBP_H */
