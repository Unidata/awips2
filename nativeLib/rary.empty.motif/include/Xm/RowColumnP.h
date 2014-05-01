/**
 *
 * $Id: RowColumnP.h,v 1.1 2004/08/28 19:23:26 dannybackx Exp $
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
 
#ifndef _XM_ROWCOLUMNP_H
#define _XM_ROWCOLUMNP_H

#include <Xm/RowColumn.h>
#include <Xm/ManagerP.h>

#ifdef __cplusplus
extern "C" {
#endif

/* used by the popup menu code. */
typedef struct _XmButtonEventStatusRec {
    Time time;
    Boolean verified;
    Boolean waiting_to_be_managed;
    XButtonEvent event;
} XmButtonEventStatusRec;

/* Also not used in lesstif.. will be someday */
typedef struct _XmReplayInfoRec {
    Time time;
    Widget toplevel_menu;
} XmReplayInfoRec;

typedef struct _XmMenuFocusRec {
    Widget oldWidget;
    Window oldFocus;
    int oldRevert;
} XmMenuFocusRec;

/* per screen state information dealing with menus */
typedef struct _XmMenuStateRec {
    Widget RC_LastSelectToplevel;
    XmButtonEventStatusRec RC_ButtonEventStatus;
    XmReplayInfoRec RC_ReplayInfo;

    Widget RC_activeItem;
    XmMenuFocusRec RC_menuFocus;

    Boolean RC_allowAcceleratedInsensitiveUnmanagedMenuItems;
    Time MS_LastManagedMenuTime;
    Boolean MU_InDragMode;
    Widget MU_CurrentMenuChild;
    Boolean MU_InPMMode;
} XmMenuStateRec, *XmMenuState;

/* geometry information for children of the row column.  Used to
   simplify the layout routines */
typedef struct _XmRCKidGeometryRec
{
    Widget kid;
    XtWidgetGeometry box;
    Dimension margin_top;
    Dimension margin_bottom;
    Dimension baseline;
} XmRCKidGeometryRec, *XmRCKidGeometry;

/* Define the rowcolumn instance part */
typedef struct {
    Dimension margin_height;
    Dimension margin_width;

    Dimension spacing;

    Dimension entry_border;

    Widget help_pushbutton;
    Widget cascadeBtn;

    XmString option_label;
    Widget option_submenu;

    XmRCKidGeometry boxes;

    WidgetClass entry_class;

    XtCallbackList entry_callback;
    XtCallbackList map_callback;
    XtCallbackList unmap_callback;

    Widget memory_subwidget;

    short num_columns;

    String menuPost;

    unsigned int postButton;
    int postEventType;
    unsigned int postModifiers;

    String menu_accelerator;
    KeySym mnemonic;
    XmStringCharSet mnemonicCharSet;

    unsigned char entry_alignment;

    unsigned char packing;

    unsigned char type;

    unsigned char orientation;

    Boolean armed;
    Boolean adjust_margin;
    Boolean adjust_last;
    Boolean do_alignment;
    Boolean radio;
    Boolean radio_one;
    Boolean homogeneous;
    Boolean resize_width;
    Boolean resize_height;

    Boolean popup_enabled;
    
    Dimension old_width;
    Dimension old_height;
    Dimension old_shadow_thickness;

    Widget *postFromList;
    int postFromCount;
    int postFromListSize;

    Widget lastSelectToplevel;
    Widget popupPosted;

    unsigned char oldFocusPolicy;

    unsigned char TearOffModel;
    Widget ParentShell;
    Widget tear_off_control;
    Boolean to_state;
    
    XtCallbackList tear_off_activated_callback;
    XtCallbackList tear_off_deactivated_callback;
    Widget tear_off_lastSelectToplevel;
    Widget tear_off_focus_item;

    unsigned char entry_vertical_alignment;
    unsigned char popup_menu_click;
    XtIntervalId popup_timeout_timer;
} XmRowColumnPart;

/* Define the full instance record */
typedef struct _XmRowColumnRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmRowColumnPart row_column;
} XmRowColumnRec;

/* Define class part structure */
typedef struct {
    XmMenuProc menuProcedures;   /* proc to interface with menu widgets */
    XtActionProc armAndActivate; /* proc triggered by accelerator */
    XmMenuTraversalProc traversalHandler; /* proc to handle menu traversal */
    XtPointer extension;
} XmRowColumnClassPart;

/* Define the full class record */
typedef struct _XmRowColumnClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmRowColumnClassPart row_column_class;
} XmRowColumnClassRec;

/* Define the Constraint Resources */
typedef struct _XmRowColumnConstraintPart {
    Boolean was_managed;
    Dimension margin_top;
    Dimension margin_bottom;
    Dimension baseline;
    short position_index;
} XmRowColumnConstraintPart;

typedef struct _XmRowColumnConstraintRec {
    XmManagerConstraintPart manager;
    XmRowColumnConstraintPart row_column;
} XmRowColumnConstraintRec, *XmRowColumnConstraints;

#define XmRC_ARMED_BIT        (1 << 0)  
#define XmRC_BEING_ARMED_BIT  (1 << 1)
#define XmRC_EXPOSE_BIT       (1 << 2)
#define XmRC_WINDOW_MOVED_BIT (1 << 3)
#define XmRC_WIDGET_MOVED_BIT (1 << 4)
#define XmRC_POPPING_DOWN_BIT (1 << 5)
#define XmRC_FROM_RESIZE_BIT  (1 << 6)

#define RC_IsArmed(m)    (((XmRowColumnWidget)(m))->row_column.armed & XmRC_ARMED_BIT)
#define RC_BeingArmed(m) (((XmRowColumnWidget)(m))->row_column.armed & XmRC_BEING_ARMED_BIT)
#define RC_WidgetHasMoved(m) (((XmRowColumnWidget)(m))->row_column.armed & XmRC_WINDOW_MOVED_BIT)
#define RC_WindowHasMoved(m) (((XmRowColumnWidget)(m))->row_column.armed & XmRC_WIDGET_MOVED_BIT)
#define RC_PoppingDown(m) (((XmRowColumnWidget)(m))->row_column.armed & XmRC_POPPING_DOWN_BIT)
#define RC_FromResize(m) (((XmRowColumnWidget)(m))->row_column.armed & XmRC_FROM_RESIZE_BIT)

#define RC_SetBit(byte,bit,v) byte = (byte & (~bit)) | (v ? bit : 0)

#define RC_SetArmed(m,v)  RC_SetBit (((XmRowColumnWidget)(m))->row_column.armed, XmRC_ARMED_BIT, v)
#define RC_SetBeingArmed(m,v)  RC_SetBit (((XmRowColumnWidget)(m))->row_column.armed, XmRC_BEING_ARMED_BIT, v)
#define RC_SetExpose(m,v) RC_SetBit (((XmRowColumnWidget)(m))->row_column.armed, XmRC_EXPOSE_BIT, v)
#define RC_SetWidgetMoved(m,v) RC_SetBit(((XmRowColumnWidget)(m))->row_column.armed, XmRC_WIDGET_MOVED_BIT,v)
#define RC_SetWindowMoved(m,v) RC_SetBit(((XmRowColumnWidget)(m))->row_column.armed, XmRC_WINDOW_MOVED_BIT,v)
#define RC_SetPoppingDown(m,v) RC_SetBit(((XmRowColumnWidget)(m))->row_column.armed, XmRC_POPPING_DOWN_BIT,v)
#define RC_SetFromResize(m,v) RC_SetBit(((XmRowColumnWidget)(m))->row_column.armed, XmRC_FROM_RESIZE_BIT,v)

XMLIBEXPORT extern XmRowColumnClassRec xmRowColumnClassRec;

/*
 * easy access macros
 */
#define RC_MarginW(m) \
    (((XmRowColumnWidget)(m))->row_column.margin_width)

#define RC_MarginH(m) \
    (((XmRowColumnWidget)(m))->row_column.margin_height)

#define RC_TearOffActivate_cb(m) \
    (((XmRowColumnWidget)(m))->row_column.tear_off_activated_callback)

#define RC_TearOffDeactivate_cb(m) \
    (((XmRowColumnWidget)(m))->row_column.tear_off_deactivated_callback)

#define RC_Entry_cb(m) \
    (((XmRowColumnWidget)(m))->row_column.entry_callback)

#define RC_Map_cb(m) \
    (((XmRowColumnWidget)(m))->row_column.map_callback)

#define RC_Unmap_cb(m) \
    (((XmRowColumnWidget)(m))->row_column.unmap_callback)

#define RC_Orientation(m) \
    (((XmRowColumnWidget)(m))->row_column.orientation)

#define RC_Spacing(m) \
    (((XmRowColumnWidget)(m))->row_column.spacing)

#define RC_EntryBorder(m) \
    (((XmRowColumnWidget)(m))->row_column.entry_border)

#define RC_HelpPb(m) \
    (((XmRowColumnWidget)(m))->row_column.help_pushbutton)

#define RC_DoMarginAdjust(m) \
    (((XmRowColumnWidget)(m))->row_column.adjust_margin)

#define RC_EntryAlignment(m) \
    (((XmRowColumnWidget)(m))->row_column.entry_alignment)

#define RC_EntryVerticalAlignment(m) \
    (((XmRowColumnWidget)(m))->row_column.entry_vertical_alignment)

#define RC_Packing(m) \
    (((XmRowColumnWidget)(m))->row_column.packing)

#define RC_NCol(m) \
    (((XmRowColumnWidget)(m))->row_column.num_columns)

#define RC_AdjLast(m) \
    (((XmRowColumnWidget)(m))->row_column.adjust_last)

#define RC_AdjMargin(m) \
    (((XmRowColumnWidget)(m))->row_column.adjust_margin)

#define RC_MemWidget(m) \
    (((XmRowColumnWidget)(m))->row_column.memory_subwidget)

#define RC_CascadeBtn(m) \
    (((XmRowColumnWidget)(m))->row_column.cascadeBtn)

#define RC_OptionLabel(m) \
    (((XmRowColumnWidget)(m))->row_column.option_label)

#define RC_OptionSubMenu(m) \
    (((XmRowColumnWidget)(m))->row_column.option_submenu)

#define RC_RadioBehavior(m) \
    (((XmRowColumnWidget)(m))->row_column.radio)

#define RC_RadioAlwaysOne(m) \
    (((XmRowColumnWidget)(m))->row_column.radio_one)

#define RC_PopupPosted(m) \
    (((XmRowColumnWidget)(m))->row_column.popupPosted)

#define RC_ResizeHeight(m) \
    (((XmRowColumnWidget)(m))->row_column.resize_height)

#define RC_ResizeWidth(m) \
    (((XmRowColumnWidget)(m))->row_column.resize_width)

#define RC_Type(m) \
    (((XmRowColumnWidget)(m))->row_column.type)

#define RC_EntryClass(m) \
    (((XmRowColumnWidget)(m))->row_column.entry_class)

#define RC_IsHomogeneous(m) \
    (((XmRowColumnWidget)(m))->row_column.homogeneous)

#define RC_Boxes(m) \
    (((XmRowColumnWidget)(m))->row_column.boxes)

#define RC_PopupEnabled(m) \
    (((XmRowColumnWidget)(m))->row_column.popup_enabled)

#define RC_MenuAccelerator(m) \
    (((XmRowColumnWidget)(m))->row_column.menu_accelerator)

#define RC_Mnemonic(m) \
    (((XmRowColumnWidget)(m))->row_column.mnemonic)

#define RC_MnemonicCharSet(m) \
    (((XmRowColumnWidget)(m))->row_column.mnemonicCharSet)

#define RC_MenuPost(m) \
    (((XmRowColumnWidget)(m))->row_column.menuPost)

#define RC_PostModifiers(m) \
    (((XmRowColumnWidget)(m))->row_column.postModifiers)

#define RC_PostButton(m) \
    (((XmRowColumnWidget)(m))->row_column.postButton)

#define RC_PostEventType(m) \
    (((XmRowColumnWidget)(m))->row_column.postEventType)

#define RC_OldFocusPolicy(m) \
    (((XmRowColumnWidget)(m))->row_column.oldFocusPolicy)

#define	RC_ParentShell(w) \
    (((XmRowColumnWidget)(w))->row_column.ParentShell)

#define	RC_TearOffLastSelectToplevel(w) \
    (((XmRowColumnWidget)(w))->row_column.tear_off_lastSelectToplevel)

#define	RC_TearOffFocusItem(w) \
    (((XmRowColumnWidget)(w))->row_column.tear_off_focus_item)

#define RC_TearOffControl(m) \
    (((XmRowColumnWidget)(m))->row_column.tear_off_control)

#define RC_TearOffModel(m) \
    (((XmRowColumnWidget)(m))->row_column.TearOffModel)

#define XmTO_TORN_OFF_BIT       (1 << 0)
#define XmTO_FROM_INIT_BIT      (1 << 1)
#define XmTO_VISUAL_DIRTY_BIT   (1 << 2)
#define XmTO_ACTIVE_BIT         (1 << 3)

#define RC_SetTornOff(m,v) \
    RC_SetBit(((XmRowColumnWidget)(m))->row_column.to_state, XmTO_TORN_OFF_BIT,v)

#define RC_TornOff(m) \
    (((XmRowColumnWidget)(m))->row_column.to_state & XmTO_TORN_OFF_BIT)

#define RC_SetFromInit(m,v) \
    RC_SetBit(((XmRowColumnWidget)(m))->row_column.to_state, XmTO_FROM_INIT_BIT,v)

#define RC_FromInit(m) \
    (((XmRowColumnWidget)(m))->row_column.to_state & XmTO_FROM_INIT_BIT)

#define RC_SetTearOffDirty(m,v) \
    RC_SetBit(((XmRowColumnWidget)(m))->row_column.to_state, XmTO_VISUAL_DIRTY_BIT,v)

#define RC_TearOffDirty(m) \
    (((XmRowColumnWidget)(m))->row_column.to_state & XmTO_VISUAL_DIRTY_BIT)

#define RC_TearOffActive(m) \
    (((XmRowColumnWidget)(m))->row_column.to_state & XmTO_ACTIVE_BIT)

#define RC_SetTearOffActive(m,v) \
    RC_SetBit(((XmRowColumnWidget)(m))->row_column.to_state, XmTO_ACTIVE_BIT,v)

/*
 * for _XmProcessMenuTree()
 *	Don't use an enum as this will conflict with one for render tables
 *	in Xm.h.
 */
#define	XmADD		0
#define	XmDELETE	1
#define	XmREPLACE	2

/*
 * Defines used when calling _XmMenuIsAccessible()
 */
enum {
    XmWEAK_CHECK = 1,
    XmMEDIUM_CHECK,
    XmSTRONG_CHECK
};

enum {
    XmMENU_BEGIN,
    XmMENU_MIDDLE,
    XmMENU_END
};

/*
 * Defines used when calling find_first_managed_child()
 */
enum {
    ANY_CHILD,
    FIRST_BUTTON
};

void _LtRCEntryCallback(Widget w, XtCallbackList cb_list, XtPointer cbs);

void _XmPostPopupMenu(Widget wid, XEvent *event);
void _XmSetPopupMenuClick(Widget wid, Boolean popupMenuClick);
Boolean _XmGetPopupMenuClick(Widget wid);
void _XmAllowAcceleratedInsensitiveUnmanagedMenuItems(Widget wid,
						      Boolean allowed);
void _XmSetSwallowEventHandler(Widget widget, Boolean add_handler);
void _XmMenuFocus(Widget w, int operation, Time _time);
void _XmGetActiveTopLevelMenu(Widget wid, Widget *rwid);
Boolean _XmMatchBSelectEvent(Widget wid, XEvent *event);
Boolean _XmMatchBDragEvent(Widget wid, XEvent *event);
void _XmHandleMenuButtonPress(Widget wid, XEvent *event);
void _XmMenuBtnDown(Widget wid, XEvent *event,
			   String *params, Cardinal *num_params);
void _XmMenuBtnUp(Widget wid, XEvent *event,
		  String *params, Cardinal *num_params);
void _XmCallRowColumnMapCallback(Widget wid, XEvent *event);
void _XmCallRowColumnUnmapCallback(Widget wid, XEvent *event);
void _XmMenuPopDown(Widget w, XEvent *event, Boolean *popped_up);
Boolean _XmIsActiveTearOff(Widget w);
void _XmMenuHelp(Widget wid, XEvent *event,
		 String *params, Cardinal *num_params);
Widget _XmMenuNextItem(Widget menu, Widget current_item);

#ifdef __cplusplus
}
#endif

#endif /* _XM_ROWCOLUMN_P_H */
