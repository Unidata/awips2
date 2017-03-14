/**
 *
 * $Id: ManagerP.h,v 1.1 2004/08/28 19:23:25 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1997, 1999, 2000, 2001 LessTif Development Team
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

#ifndef _XM_MANAGERP_H
#define _XM_MANAGERP_H 

#include <Xm/XmP.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MCEPTR(cl) \
    ((XmManagerClassExt *)(&(((XmManagerWidgetClass)(cl))->manager_class.extension)))
#define _XmGetManagerClassExtPtr(cl, o) \
    ((*MCEPTR(cl) && (((*MCEPTR(cl))->record_type) == (o))) \
        ? MCEPTR(cl) \
        : ((XmManagerClassExt *)_XmGetClassExtensionPtr(((XmGenericClassExt *)MCEPTR(cl)), (o))))

/* structure used for storing accelerator and mnemonic information */
typedef struct {
    unsigned int eventType;
    KeySym	keysym;
    KeyCode	key;
    unsigned int modifiers;
    Widget	component;
    Boolean	needGrab;
    Boolean	isMnemonic;
} XmKeyboardData;

/* Define the manager instance part */
typedef struct {
    Pixel foreground;

    Dimension shadow_thickness;
    Pixel top_shadow_color;
    Pixmap top_shadow_pixmap;
    Pixel bottom_shadow_color;
    Pixmap bottom_shadow_pixmap;

    Pixel highlight_color;
    Pixmap highlight_pixmap;

    XtCallbackList help_callback;
    XtPointer user_data;

    Boolean traversal_on;
    unsigned char unit_type;
    XmNavigationType navigation_type;

    /* private instance variables */
    Boolean event_handler_added;
    Widget active_child; 
    Widget highlighted_widget;
    Widget accelerator_widget;

    Boolean has_focus;

    XmStringDirection string_direction;		/* 2.x */

    XmKeyboardData *keyboard_list;
    short num_keyboard_entries;
    short size_keyboard_list;

    XmGadget selected_gadget;
    XmGadget eligible_for_multi_button_event;

    GC background_GC;
    GC highlight_GC;
    GC top_shadow_GC;
    GC bottom_shadow_GC;

    Widget initial_focus;

    XtCallbackList popup_handler_callback;  /* new for 2.0 */
} XmManagerPart;

typedef struct {
    XtPointer next_extension;
    XrmQuark record_type;
    long version;
    Cardinal record_size;
    XmTraversalChildrenProc traversal_children;
} XmManagerClassExtRec, *XmManagerClassExt;

#define XmManagerClassExtVersion 1L

/* Define the full instance record */
typedef struct _XmManagerRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
} XmManagerRec;

/* Define class part structure */
typedef struct {
    String translations;
    XmSyntheticResource *syn_resources; 
    int num_syn_resources;
    XmSyntheticResource *syn_constraint_resources;
    int num_syn_constraint_resources;
    XmParentProcessProc parent_process;
    XtPointer extension;
} XmManagerClassPart;

#define XmInheritParentProcess ((XmParentProcessProc) _XtInherit)

/* Define the full class record */
typedef struct _XmManagerClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
} XmManagerClassRec;

/* manager constraints */

typedef struct _XmManagerConstraintPart {
    int unused;
} XmManagerConstraintPart;

typedef struct _XmManagerConstraintRec {
    XmManagerConstraintPart manager;
} XmManagerConstraintRec, *XmManagerConstraintPtr;

XMLIBEXPORT extern XmManagerClassRec xmManagerClassRec;

/*
 * easy access macros -- FOR USE BY GADGET CODE ONLY
 */
#define XmParentTopShadowGC(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.top_shadow_GC)

#define XmParentBottomShadowGC(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.bottom_shadow_GC)

#define XmParentHighlightGC(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.highlight_GC)

#define XmParentBackgroundGC(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.background_GC)

/*
 * easy access macros -- FOR USE BY MANAGERS.
 */
#define MGR_SizeKeyboardList(m) (((XmManagerWidget)(m))->manager.size_keyboard_list)
#define MGR_ShadowThickness(m)	(((XmManagerWidget)(m))->manager.shadow_thickness)
#define MGR_KeyboardList(m)	(((XmManagerWidget)(m))->manager.keyboard_list)
#define MGR_NumKeyboardEntries(m) (((XmManagerWidget)(m))->manager.num_keyboard_entries)

/* Motif 2.* */
#define MGR_LayoutDirection(m)	(((XmManagerWidget)(m))->manager.string_direction)

/* internal manager routines */

Boolean _XmParentProcess(Widget widget, XmParentProcessData data);
void _XmDestroyParentCallback ( Widget w, XtPointer client, XtPointer call );
/* What the F is this? */
void _XmSocorro(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmClearShadowType(Widget w,
			Dimension old_width, Dimension old_height,
			Dimension old_shadow_thickness,
			Dimension old_highlight_thickness);

/* action routine prototypes */
void _XmGadgetButtonMotion(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetArm(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetMultiArm(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetMultiActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetDrag(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetSelect(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmParentActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmParentCancel(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraversePrevTabGroup(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseNextTabGroup(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraversePrev(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseNext(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseUp(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseDown(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseLeft(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseRight(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseHome(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetHelp(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetKeyInput(Widget w, XEvent *event, String *params, Cardinal *num_params);

void _XmManagerHelp(Widget w, XEvent *event, String *params, Cardinal *num_params);

#ifdef __cplusplus
}
#endif

#endif /* _XM_MANAGERP_H */
