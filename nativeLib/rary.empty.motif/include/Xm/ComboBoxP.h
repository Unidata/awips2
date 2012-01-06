/**
 * 
 * $Id: ComboBoxP.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright (C) 1997-2000 LessTif Development Team
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

#ifndef _XM_COMBOBOXP_H
#define _XM_COMBOBOXP_H

#include <Xm/XmP.h>
#include <Xm/ManagerP.h>
#include <Xm/ComboBox.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ======================================================================== */
/* ComboBox Widget */

/* Define the instance part */
typedef struct _XmComboBoxPart {
   unsigned char type;
   unsigned char match_behavior;
   Dimension highlight_thickness;
   Dimension arrow_size;
   Dimension arrow_spacing;
   Dimension margin_width;
   Dimension margin_height;
   XtCallbackList selection_callback;
   XmString selected_item;
   int selected_position;
   XmFontList render_table; /* also XmNfontList */

   Widget list_shell;
   Widget list;
   Widget scrolled_w;
   Widget vsb;
   Widget hsb;
   int ideal_ebheight;
   int ideal_ebwidth;
   GC arrow_GC;
   XRectangle hit_rect;
   Dimension arrow_shadow_width;
   Boolean arrow_pressed;
   Boolean highlighted;
   Boolean scrolling;
   XtEnum shell_state;    /* unused ?! */
   Boolean text_changed;  /* unused ?! */
#if XmVersion >= 2001
   XtEnum position_mode;
#endif   
} XmComboBoxPart;

/* Define the full instance record */
typedef struct _XmComboBoxRec {
   CorePart core;
   CompositePart composite;
   ConstraintPart constraint;
   XmManagerPart manager;
   XmComboBoxPart combo_box;
} XmComboBoxRec;

/* Define class part structure */
typedef struct _XmComboBoxClassPart {
   XtPointer extension;
} XmComboBoxClassPart;

/* Define the full class record */
typedef struct _XmComboBoxClassRec {
   CoreClassPart core_class;
   CompositeClassPart composite_class;
   ConstraintClassPart constraint_class;
   XmManagerClassPart manager_class;
   XmComboBoxClassPart combo_box_class;
} XmComboBoxClassRec;

XMLIBEXPORT extern XmComboBoxClassRec xmComboBoxClassRec;

#define CB_ArrowPressed(w) \
        (((XmComboBoxWidget)(w))->combo_box.arrow_pressed)

#define CB_ArrowSize(w) \
        (((XmComboBoxWidget)(w))->combo_box.arrow_size)

#define CB_ArrowSpacing(w) \
        (((XmComboBoxWidget)(w))->combo_box.arrow_spacing)

#define CB_EditBox(w) \
        (((XmComboBoxWidget)(w))->composite.children[0])

#define CB_HighlightThickness(w) \
        (((XmComboBoxWidget)(w))->combo_box.highlight_thickness)

#define CB_Highlighted(w) \
        (((XmComboBoxWidget)(w))->combo_box.highlighted)

#define CB_HitRect(w) \
        (((XmComboBoxWidget)(w))->combo_box.hit_rect)

#define CB_List(w) \
        (((XmComboBoxWidget)(w))->combo_box.list)

#define CB_ListShell(w) \
        (((XmComboBoxWidget)(w))->combo_box.list_shell)

#define CB_MarginHeight(w) \
        (((XmComboBoxWidget)(w))->combo_box.margin_height)

#define CB_MarginWidth(w) \
        (((XmComboBoxWidget)(w))->combo_box.margin_width)

#define CB_MatchBehavior(w) \
        (((XmComboBoxWidget)(w))->combo_box.match_behavior)

#if XmVersion >= 2001
#define CB_PositionMode(w) \
        (((XmComboBoxWidget)(w))->combo_box.position_modeXmCpositionMode)
#endif

#define CB_RenderTable(w) \
        (((XmComboBoxWidget)(w))->combo_box.render_table)

#define CB_ScrolledW(w) \
        (((XmComboBoxWidget)(w))->combo_box.scrolled_w)

#define CB_SelectedItem(w) \
        (((XmComboBoxWidget)(w))->combo_box.selected_item)

#define CB_SelectedPosition(w) \
        (((XmComboBoxWidget)(w))->combo_box.selected_position)

#define CB_SelectionCB(w) \
        (((XmComboBoxWidget)(w))->combo_box.selection_callback)

#if XmVersion == 2000
#define CB_ShellState(w) \
        (((XmComboBoxWidget)(w))->combo_box.shell_state)
#endif

#define CB_TextChanged(w) \
        (((XmComboBoxWidget)(w))->combo_box.text_changed)

#define CB_Type(w) \
        (((XmComboBoxWidget)(w))->combo_box.type)

#ifdef __cplusplus
}
#endif

#endif /* _XM_COMBOBOXP_H */
