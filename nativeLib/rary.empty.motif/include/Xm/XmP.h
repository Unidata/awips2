/**
 *
 * $Id: XmP.h,v 1.2 2005/03/26 06:12:17 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
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

#ifndef _XM_XMP_H
#define _XM_XMP_H

#include <X11/IntrinsicP.h>
#include <Xm/Xm.h>
#include <Xm/DrawP.h>

#ifndef XmConst
#define XmConst	const
#endif /* !XmConst */


#ifdef __cplusplus
extern "C" {
#endif

/*
 * shorthand macros
 */
#ifdef XtDisplay
#undef XtDisplay
#endif
#define XtDisplay(widget) \
    (XtIsWidget(widget) \
	? ((Widget)(widget))->core.screen->display \
	: ((Object)(widget))->object.parent->core.screen->display)

#ifdef XtScreen
#undef XtScreen
#endif
#define XtScreen(widget) \
    (XtIsWidget(widget) \
	? ((Widget)(widget))->core.screen \
	: ((Object)(widget))->object.parent->core.screen)

#ifdef XtWindow
#undef XtWindow
#endif
#define XtWindow(widget) \
    (XtIsWidget(widget) \
	? ((Widget)(widget))->core.window \
	: ((Object)(widget))->object.parent->core.window)

#ifdef XtName
#undef XtName
#endif
#define XtName(widget) \
    XrmQuarkToString(((Object)(widget))->object.xrm_name)

#ifdef XtClass
#undef XtClass
#endif
#define XtClass(widget) \
    (((Object)(widget))->object.widget_class)

#ifdef XtSuperclass
#undef XtSuperclass
#endif
#define XtSuperclass(widget) \
    (XtClass(widget)->core_class.superclass)

#ifdef XtIsRealized
#undef XtIsRealized
#endif
#define XtIsRealized(widget) \
    (XtIsWidget(widget) \
	? ((Widget)(widget))->core.window \
	: ((Object)(widget))->object.parent->core.window)

#ifdef XtIsManaged
#undef XtIsManaged
#endif
#define XtIsManaged(widget) \
    (((XmGadget)(widget))->rectangle.managed)

#ifdef XtParent
#undef XtParent
#endif
#define XtParent(widget) \
    (((Object)(widget))->object.parent)

/*
 * #defines for useful core record variables
 */

#define XtWidth(w)	 (((Widget)(w))->core.width)
#define XtHeight(w)	 (((Widget)(w))->core.height)
#define XtX(w)		 (((Widget)(w))->core.x)
#define XtY(w)		 (((Widget)(w))->core.y)
#define XtBackground(w)	 (((Widget)(w))->core.background_pixel)
#define XtBorderWidth(w) (((Widget)(w))->core.border_width)
#define XtSensitive(w)	 (((Widget)(w))->core.sensitive && \
			  ((Widget)(w))->core.ancestor_sensitive)
#define XtCoreProc(w,p)  (((Widget)(w))->core.widget_class->core_class.p)

/*
 * menu values
 */
enum {
    XmMENU_POPDOWN,
    XmMENU_PROCESS_TREE, 
    XmMENU_TRAVERSAL, 
    XmMENU_SHELL_POPDOWN,
    XmMENU_CALLBACK,
    XmMENU_BUTTON,
    XmMENU_CASCADING,
    XmMENU_SUBMENU,
    XmMENU_ARM,
    XmMENU_DISARM,
    XmMENU_BAR_CLEANUP,
    XmMENU_STATUS,
    XmMENU_MEMWIDGET_UPDATE,
    XmMENU_BUTTON_POPDOWN,
    XmMENU_RESTORE_EXCLUDED_TEAROFF_TO_TOPLEVEL_SHELL,
    XmMENU_RESTORE_TEAROFF_TO_TOPLEVEL_SHELL,
    XmMENU_RESTORE_TEAROFF_TO_MENUSHELL,
    XmMENU_GET_LAST_SELECT_TOPLEVEL,
    XmMENU_TEAR_OFF_ARM
};

#define XmMENU_TORN_BIT                         (1 << 0)
#define XmMENU_TEAR_OFF_SHELL_DESCENDANT_BIT    (1 << 1)
#define XmMENU_POPUP_POSTED_BIT                 (1 << 2)

#define XmIsTorn(m) \
    ((m) & XmMENU_TORN_BIT)
#define XmPopupPosted(m) \
    ((m) & XmMENU_POPUP_POSTED_BIT)
#define XmIsTearOffShellDescendant(m) \
    ((m) & XmMENU_TEAR_OFF_SHELL_DESCENDANT_BIT)

/*
 * constants used in button/SimpleMenu communication
 */
typedef struct _XmSimpleMenuRec {
    int count;
    int post_from_button;
    XtCallbackProc callback;
    XmStringTable label_string;
    String *accelerator;
    XmStringTable accelerator_text;
    XmKeySymTable mnemonic;
    XmStringCharSetTable mnemonic_charset;
    XmButtonTypeTable button_type;
    int button_set;
    XmString option_label;
    KeySym option_mnemonic;
} XmSimpleMenuRec, *XmSimpleMenu;

/* For MapEvent: _XmMatchBtnEvent */
#define XmIGNORE_EVENTTYPE      -1

/* Default minimum Toggle indicator dimension */
#define XmDEFAULT_INDICATOR_DIM   9

/* DefaultButtonShadow stuff */
#define Xm3D_ENHANCE_PIXEL              2
#define XmDEFAULT_TOP_MARGIN            0
#define XmDEFAULT_BOTTOM_MARGIN         0


/*
 * synthetic resource stuff
 */

typedef enum { 
    XmSYNTHETIC_NONE, 
    XmSYNTHETIC_LOAD 
} XmImportOperator;

typedef void (*XmExportProc)(Widget, int, XtArgVal *);
typedef XmImportOperator (*XmImportProc)(Widget, int, XtArgVal*);

typedef struct _XmSyntheticResource {
    String resource_name;
    Cardinal resource_size;
    Cardinal resource_offset;
    XmExportProc export_proc;
    XmImportProc import_proc;
} XmSyntheticResource;

/*
 * ParentProcess structures
 */

enum {
    XmPARENT_PROCESS_ANY, 
    XmINPUT_ACTION
};

enum {
    XmPARENT_ACTIVATE,
    XmPARENT_CANCEL
};

#define XmRETURN XmPARENT_ACTIVATE
#define XmCANCEL XmPARENT_CANCEL

typedef struct {
    int process_type;
} XmParentProcessAnyRec;

typedef struct {
    int process_type;
    XEvent *event;
    int action;
    String *params;
    Cardinal *num_params;
} XmParentInputActionRec;

typedef union {
    XmParentProcessAnyRec any;
    XmParentInputActionRec input_action;
} XmParentProcessDataRec, *XmParentProcessData;

#define XmINVALID_DIMENSION (0xFFFF)

enum {
    XmBASELINE_GET,
    XmBASELINE_SET
};

typedef struct _XmBaselineMargins {
    unsigned char get_or_set;
    Dimension margin_top;
    Dimension margin_bottom;
    Dimension shadow;
    Dimension highlight;
    Dimension text_height;
    Dimension text_width;
    Dimension margin_height;
} XmBaselineMargins;

typedef enum {
    XmFOCUS_IN,
    XmFOCUS_OUT,
    XmENTER,
    XmLEAVE
} XmFocusChange;

typedef enum{
    XmNOT_NAVIGABLE,
    XmCONTROL_NAVIGABLE,
    XmTAB_NAVIGABLE,
    XmDESCENDANTS_NAVIGABLE,
    XmDESCENDANTS_TAB_NAVIGABLE
} XmNavigability;

#define XmVoidProc      XtProc

typedef Boolean (*XmParentProcessProc)(Widget, XmParentProcessData);
typedef void    (*XmWidgetDispatchProc)
			(Widget gadget, XEvent *event, Mask event_mask);
typedef void    (*XmMenuPopupProc)(Widget, Widget, XEvent *);
typedef void    (*XmMenuTraversalProc)( Widget, Widget, XmTraversalDirection);
typedef void    (*XmResizeFlagProc)(Widget, Boolean);
typedef void    (*XmRealizeOutProc)(Widget, Mask *, XSetWindowAttributes *);
typedef Boolean (*XmVisualChangeProc)
			(Widget gadget, Widget cur_mgr, Widget new_mgr);
typedef void    (*XmTraversalProc)(Widget, XtPointer, XtPointer, int);
typedef void    (*XmFocusMovedProc)( Widget, XtPointer, XtPointer) ;
typedef void    (*XmCacheCopyProc)(XtPointer, XtPointer, size_t);
typedef int     (*XmCacheCompareProc)(XtPointer, XtPointer);
typedef Boolean (*XmWidgetBaselineProc)
			(Widget w, Dimension **baselines, int *num_baselines);
typedef Boolean (*XmWidgetDisplayRectProc)(Widget w, XRectangle *rect);
typedef void    (*XmWidgetMarginsProc)(Widget w, XmBaselineMargins *margins);
typedef XmNavigability    (*XmWidgetNavigableProc)(Widget w);
typedef void    (*XmFocusChangeProc)(Widget w, XmFocusChange change);

typedef void    (*XmMenuProc)(int function, Widget widget, ...);

typedef void    (*XmGadgetCacheProc)(XtPointer);
typedef Boolean (*XmTraversalChildrenProc)
			(Widget mw, Widget **children, Cardinal *num_children);

/*
 * virtkey stuff
 */

typedef struct {
    Modifiers mod;
    char      *key;
    char      *action;
} _XmBuildVirtualKeyStruct;


/*
 * stuff needed by the Text and TextField widgets to do their rendering
 */

typedef struct {
    XmTextPosition position;
    XmHighlightMode mode;
} _XmHighlightRec;

typedef struct {
    Cardinal number;
    Cardinal maximum;
    _XmHighlightRec *list;
} _XmHighlightData;

typedef struct {
    Atom selection;
    Atom target;
} _XmTextInsertPair;

typedef enum {
    XmDEST_SELECT,
    XmPRIM_SELECT
} XmSelectType;

typedef struct {
    Boolean done_status;
    Boolean success_status;
    XmSelectType select_type;
    XSelectionRequestEvent *event;
} _XmInsertSelect;

typedef struct {
    XEvent *event;
    String *params;
    Cardinal *num_params;
} _XmTextActionRec;

typedef struct {
    Widget widget;
    XmTextPosition insert_pos;
    int num_chars;
    Time timestamp;
    Boolean move;
} _XmTextDropTransferRec;

typedef struct {
    XmTextPosition position;
    Atom target;
    Time time;
    int num_chars;
    int ref_count;
} _XmTextPrimSelect;

typedef struct {
    Screen *screen;
    XContext context;
    unsigned char type;
} XmTextContextDataRec, *XmTextContextData;

enum {
    _XM_IS_DEST_CTX,
    _XM_IS_GC_DATA_CTX,
    _XM_IS_PIXMAP_CTX
};

#define XmTEXT_DRAG_ICON_WIDTH  64
#define XmTEXT_DRAG_ICON_HEIGHT 64
#define XmTEXT_DRAG_ICON_X_HOT  10
#define XmTEXT_DRAG_ICON_Y_HOT   4

/*
 * geometry stuff, used in GeoUtils.c
 */

enum{
    XmGET_ACTUAL_SIZE = 1,
    XmGET_PREFERRED_SIZE,
    XmGEO_PRE_SET,
    XmGEO_POST_SET
};

/* fill modes for the GeoLayoutRec's below */
enum {
    XmGEO_EXPAND,
    XmGEO_CENTER,
    XmGEO_PACK
};

/* fit modes for the GeoLayoutRec's below */
enum {
    XmGEO_PROPORTIONAL,
    XmGEO_AVERAGING,
    XmGEO_WRAP
};

enum {
    XmGEO_ROW_MAJOR,
    XmGEO_COLUMN_MAJOR
};

typedef struct _XmGeoMatrixRec *XmGeoMatrix;
typedef union _XmGeoMajorLayoutRec *XmGeoMajorLayout;
typedef struct _XmKidGeometryRec {
    Widget kid;
    XtWidgetGeometry box;
} XmKidGeometryRec, *XmKidGeometry;

typedef void (*XmGeoArrangeProc)(XmGeoMatrix, Position, Position,
				 Dimension *, Dimension *);
typedef Boolean (*XmGeoExceptProc)(XmGeoMatrix);
typedef void (*XmGeoExtDestructorProc)(XtPointer);
typedef void (*XmGeoSegmentFixUpProc)(XmGeoMatrix, int,
				      XmGeoMajorLayout, XmKidGeometry);

typedef struct {
    Boolean end;
    XmGeoSegmentFixUpProc fix_up;
    Dimension even_width;
    Dimension even_height;
    Dimension min_height;
    Boolean stretch_height;
    Boolean uniform_border;
    Dimension border;
    unsigned char fill_mode;
    unsigned char fit_mode;
    Boolean sticky_end;
    Dimension space_above;
    Dimension space_end;
    Dimension space_between;
    Dimension max_box_height;
    Dimension boxes_width;
    Dimension fill_width;
    Dimension box_count;
} XmGeoRowLayoutRec, *XmGeoRowLayout;

typedef struct {
    Boolean end;
    XmGeoSegmentFixUpProc fix_up;
    Dimension even_height;
    Dimension even_width;
    Dimension min_width;
    Boolean stretch_width;
    Boolean uniform_border;
    Dimension border;
    unsigned char fill_mode;
    unsigned char fit_mode;
    Boolean sticky_end;
    Dimension space_left;
    Dimension space_end;
    Dimension space_between;
    Dimension max_box_width;
    Dimension boxed_height;
    Dimension fill_height;
    Dimension box_count;
} XmGeoColumnLayoutRec, *XmGeoColumnLayout;

typedef union _XmGeoMajorLayoutRec {
    XmGeoRowLayoutRec row;
    XmGeoColumnLayoutRec col;
} XmGeoMajorLayoutRec;

typedef struct _XmGeoMatrixRec {
    Widget composite;
    Widget instigator;
    XtWidgetGeometry instig_request;
    XtWidgetGeometry parent_request;
    XtWidgetGeometry *in_layout;
    XmKidGeometry boxes;  /* there is a NULL pointer add the end of each row */
    XmGeoMajorLayout layouts;
    Dimension margin_w;
    Dimension margin_h;
    Boolean stretch_boxes;
    Boolean uniform_border;
    Dimension border;
    Dimension max_major;
    Dimension boxes_minor;
    Dimension fill_minor;
    Dimension width;
    Dimension height;
    XmGeoExceptProc set_except;
    XmGeoExceptProc almost_except;
    XmGeoExceptProc no_geo_request;
    XtPointer extension;
    XmGeoExtDestructorProc ext_destructor;
    XmGeoArrangeProc arrange_boxes;
    unsigned char major_order;
} XmGeoMatrixRec;

typedef XmGeoMatrix (*XmGeoCreateProc)(Widget, Widget, XtWidgetGeometry *);

/*
 * inheritance stuff
 */

#define XmInheritCallbackProc      ((XtCallbackProc) _XtInherit)
#define XmInheritTraversalProc     ((XmTraversalProc) _XtInherit)
#define XmInheritParentProcess     ((XmParentProcessProc) _XtInherit)
#define XmInheritWidgetProc        ((XtWidgetProc) _XtInherit)
#define XmInheritMenuProc          ((XmMenuProc) _XtInherit)
#define XmInheritTranslations      XtInheritTranslations
#define XmInheritCachePart         ((XtCacheClassPartPtr) _XtInherit)
#define XmInheritBaselineProc      ((XmWidgetBaselineProc) _XtInherit)
#define XmInheritDisplayRectProc   ((XmWidgetDisplayRectProc) _XtInherit)
#define XmInheritMarginsProc ((XmWidgetMarginsProc) _XtInherit)
#define XmInheritGeoMatrixCreate   ((XmGeoCreateProc) _XtInherit)
#define XmInheritFocusMovedProc    ((XmFocusMovedProc) _XtInherit)
#define XmInheritClass             ((WidgetClass) &_XmInheritClass)
#define XmInheritInitializePrehook ((XtInitProc) _XtInherit)
#define XmInheritSetValuesPrehook  ((XtSetValuesFunc) _XtInherit)
#define XmInheritInitializePosthook ((XtInitProc) _XtInherit)
#define XmInheritSetValuesPosthook ((XtSetValuesFunc) _XtInherit)
#define XmInheritGetValuesPosthook ((XtArgsProc) _XtInherit)
#define XmInheritSecObjectCreate   ((XtInitProc) _XtInherit)
#define XmInheritGetSecResData     ((XmGetSecResDataFunc) _XtInherit)
#define XmInheritInputDispatch     ((XmWidgetDispatchProc) _XtInherit)
#define XmInheritVisualChange      ((XmVisualChangeProc) _XtInherit)
#define XmInheritGetValuesPrehook  ((XtArgsProc) _XtInherit)
#define XmInheritArmAndActivate	   ((XtActionProc) _XtInherit)
#define XmInheritActionProc        ((XtActionProc) _XtInherit)
#define XmInheritFocusChange       ((XmFocusChangeProc) _XtInherit)
#define XmInheritWidgetNavigable   ((XmWidgetNavigableProc) _XtInherit)
#define XmInheritClassPartInitPrehook ((XtWidgetClassProc) _XtInherit)
#define XmInheritClassPartInitPosthook ((XtWidgetClassProc) _XtInherit)
#define XmInheritBorderHighlight   ((XtWidgetProc) _XtInherit)
#define XmInheritBorderUnhighlight ((XtWidgetProc) _XtInherit)

#define XmInheritRealize           ((XtRealizeProc) _XtInherit)
#define XmInheritResize            ((XtWidgetProc) _XtInherit)
#define XmInheritSetOverrideCallback ((XtWidgetProc) _XtInherit)
#define XmInheritTraversalChildrenProc ((XmTraversalChildrenProc) _XtInherit)



/*
 * fast subclassing definitions
 */

enum {
    XmCASCADE_BUTTON_BIT = 1,
    XmCASCADE_BUTTON_GADGET_BIT,
    XmCOMMAND_BOX_BIT,
    XmDIALOG_SHELL_BIT,
    XmLIST_BIT,
    XmFORM_BIT,
    XmTEXT_FIELD_BIT,
    XmGADGET_BIT,
    XmLABEL_BIT,
    XmLABEL_GADGET_BIT,
    XmMAIN_WINDOW_BIT,
    XmMANAGER_BIT,
    XmMENU_SHELL_BIT,
    XmDRAWN_BUTTON_BIT,
    XmPRIMITIVE_BIT,
    XmPUSH_BUTTON_BIT,
    XmPUSH_BUTTON_GADGET_BIT,
    XmROW_COLUMN_BIT,
    XmSCROLL_BAR_BIT,
    XmSCROLLED_WINDOW_BIT,
    XmSELECTION_BOX_BIT,
    XmSEPARATOR_BIT,
    XmSEPARATOR_GADGET_BIT,
    XmTEXT_BIT,
    XmTOGGLE_BUTTON_BIT,
    XmTOGGLE_BUTTON_GADGET_BIT,
    XmDROP_TRANSFER_BIT,
    XmDROP_SITE_MANAGER_BIT,
    XmDISPLAY_BIT,
    XmSCREEN_BIT,
    XmARROW_BUTTON_BIT = 32,
    XmARROW_BUTTON_GADGET_BIT,
    XmBULLETIN_BOARD_BIT,
    XmDRAWING_AREA_BIT,
    XmFILE_SELECTION_BOX_BIT,
    XmFRAME_BIT,
    XmMESSAGE_BOX_BIT,
    XmSASH_BIT,
    XmSCALE_BIT,
    XmPANED_WINDOW_BIT,
    XmVENDOR_SHELL_BIT,
    XmCLIP_WINDOW_BIT,
    XmDRAG_ICON_BIT,
    XmTEAROFF_BUTTON_BIT,
    XmDRAG_OVER_SHELL_BIT,
    XmDRAG_CONTEXT_BIT,

    XmCONTAINER_BIT,
    XmICONGADGET_BIT,
    XmNOTEBOOK_BIT,
    XmCSTEXT_BIT,
    XmGRAB_SHELL_BIT,
    XmCOMBO_BOX_BIT,
    XmSPINBOX_BIT,
    XmICONHEADER_BIT,

    XmICON_GADGET_BIT,

    XmFAST_SUBCLASS_TAIL_BIT,

    XmFIRST_APPLICATION_SUBCLASS_BIT = 192
};

#define XmLAST_FAST_SUBCLASS_BIT (XmFAST_SUBCLASS_TAIL_BIT - 1)

#undef XmIsCascadeButton
#define XmIsCascadeButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmCASCADE_BUTTON_BIT))

#undef XmIsCascadeButtonGadget
#define XmIsCascadeButtonGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmCASCADE_BUTTON_GADGET_BIT))

#undef XmIsCommandBox
#define XmIsCommandBox(w) \
    (_XmIsFastSubclass(XtClass(w), XmCOMMAND_BOX_BIT))

#undef XmIsDialogShell
#define XmIsDialogShell(w) \
    (_XmIsFastSubclass(XtClass(w), XmDIALOG_SHELL_BIT))

#undef XmIsDisplay
#define XmIsDisplay(w) \
    (_XmIsFastSubclass(XtClass(w), XmDISPLAY_BIT))

#undef XmIsList
#define XmIsList(w) \
    (_XmIsFastSubclass(XtClass(w), XmLIST_BIT))

#undef XmIsForm
#define XmIsForm(w) \
    (_XmIsFastSubclass(XtClass(w), XmFORM_BIT))

#undef XmIsTextField
#define XmIsTextField(w) \
    (_XmIsFastSubclass(XtClass(w), XmTEXT_FIELD_BIT))

#undef XmIsGadget
#define XmIsGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmGADGET_BIT))

#undef XmIsLabel
#define XmIsLabel(w) \
    (_XmIsFastSubclass(XtClass(w), XmLABEL_BIT))

#undef XmIsLabelGadget
#define XmIsLabelGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmLABEL_GADGET_BIT))

#undef XmIsIconGadget
#define XmIsIconGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmICON_GADGET_BIT))

#undef XmIsMainWindow
#define XmIsMainWindow(w) \
    (_XmIsFastSubclass(XtClass(w), XmMAIN_WINDOW_BIT))

#undef XmIsManager
#define XmIsManager(w) \
    (_XmIsFastSubclass(XtClass(w), XmMANAGER_BIT))

#undef XmIsMenuShell
#define XmIsMenuShell(w) \
    (_XmIsFastSubclass(XtClass(w), XmMENU_SHELL_BIT))

#undef XmIsDragIcon
#define XmIsDragIcon(w) \
    (_XmIsFastSubclass(XtClass(w), XmDRAG_ICON_BIT))

#undef XmIsDropSiteManager
#define XmIsDropSiteManager(w) \
    (_XmIsFastSubclass(XtClass(w), XmDROP_SITE_MANAGER_BIT))

#undef XmIsDropTransfer
#define XmIsDropTransfer(w) \
    (_XmIsFastSubclass(XtClass(w), XmDROP_TRANSFER_BIT))

#undef XmIsDragOverShell
#define XmIsDragOverShell(w) \
    (_XmIsFastSubclass(XtClass(w), XmDRAG_OVER_SHELL_BIT))

#undef XmIsDragContext
#define XmIsDragContext(w) \
    (_XmIsFastSubclass(XtClass(w), XmDRAG_CONTEXT_BIT))

#undef XmIsDrawnButton
#define XmIsDrawnButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmDRAWN_BUTTON_BIT))

#undef XmIsPrimitive
#define XmIsPrimitive(w) \
    (_XmIsFastSubclass(XtClass(w), XmPRIMITIVE_BIT))

#undef XmIsPushButton
#define XmIsPushButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmPUSH_BUTTON_BIT))

#undef XmIsPushButtonGadget
#define XmIsPushButtonGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmPUSH_BUTTON_GADGET_BIT))

#undef XmIsRowColumn
#define XmIsRowColumn(w) \
    (_XmIsFastSubclass(XtClass(w), XmROW_COLUMN_BIT))

#undef XmIsScreen
#define XmIsScreen(w) \
    (_XmIsFastSubclass(XtClass(w), XmSCREEN_BIT))

#undef XmIsScrollBar
#define XmIsScrollBar(w) \
    (_XmIsFastSubclass(XtClass(w), XmSCROLL_BAR_BIT))

#undef XmIsScrolledWindow
#define XmIsScrolledWindow(w) \
    (_XmIsFastSubclass(XtClass(w), XmSCROLLED_WINDOW_BIT))

#undef XmIsSelectionBox
#define XmIsSelectionBox(w) \
    (_XmIsFastSubclass(XtClass(w), XmSELECTION_BOX_BIT))

#undef XmIsSeparator
#define XmIsSeparator(w) \
    (_XmIsFastSubclass(XtClass(w), XmSEPARATOR_BIT))

#undef XmIsSeparatorGadget
#define XmIsSeparatorGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmSEPARATOR_GADGET_BIT))

#undef XmIsText
#define XmIsText(w) \
    (_XmIsFastSubclass(XtClass(w), XmTEXT_BIT))

#undef XmIsTearOffButton
#define XmIsTearOffButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmTEAROFF_BUTTON_BIT))

#undef XmIsToggleButton
#define XmIsToggleButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmTOGGLE_BUTTON_BIT))

#undef XmIsToggleButtonGadget
#define XmIsToggleButtonGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmTOGGLE_BUTTON_GADGET_BIT))

#undef XmIsArrowButton
#define XmIsArrowButton(w) \
    (_XmIsFastSubclass(XtClass(w), XmARROW_BUTTON_BIT))

#undef XmIsArrowButtonGadget
#define XmIsArrowButtonGadget(w) \
    (_XmIsFastSubclass(XtClass(w), XmARROW_BUTTON_GADGET_BIT))

#undef XmIsBulletinBoard
#define XmIsBulletinBoard(w) \
    (_XmIsFastSubclass(XtClass(w), XmBULLETIN_BOARD_BIT))

#undef XmIsDrawingArea
#define XmIsDrawingArea(w) \
    (_XmIsFastSubclass(XtClass(w), XmDRAWING_AREA_BIT))

#undef XmIsFileSelectionBox
#define XmIsFileSelectionBox(w) \
    (_XmIsFastSubclass(XtClass(w), XmFILE_SELECTION_BOX_BIT))

#undef XmIsFrame
#define XmIsFrame(w) \
    (_XmIsFastSubclass(XtClass(w), XmFRAME_BIT))

#undef XmIsMessageBox
#define XmIsMessageBox(w) \
    (_XmIsFastSubclass(XtClass(w), XmMESSAGE_BOX_BIT))

#undef XmIsSash
#define XmIsSash(w) \
    (_XmIsFastSubclass(XtClass(w), XmSASH_BIT))

#undef XmIsScale
#define XmIsScale(w) \
    (_XmIsFastSubclass(XtClass(w), XmSCALE_BIT))

#undef XmIsPanedWindow
#define XmIsPanedWindow(w) \
    (_XmIsFastSubclass(XtClass(w), XmPANED_WINDOW_BIT))

/*
 * XmPartOffset bogosity
 */

#define XmObjectIndex           0
#define ObjectIndex             XmObjectIndex
#define XmRectObjIndex          (XmObjectIndex + 1)
#define RectObjIndex            XmRectObjIndex
#define XmWindowObjIndex        (XmRectObjIndex + 1)
#define WindowObjIndex          XmWindowObjIndex
#define XmCoreIndex             0
#define CoreIndex               XmCoreIndex
#define XmCompositeIndex        (XmWindowObjIndex + 2)
#define CompositeIndex          XmCompositeIndex
#define XmConstraintIndex       (XmCompositeIndex + 1)
#define ConstraintIndex         XmConstraintIndex
#define XmGadgetIndex           (XmRectObjIndex + 1)
#define XmPrimitiveIndex        (XmWindowObjIndex + 2)
#define XmManagerIndex          (XmConstraintIndex + 1)

#define XmArrowBIndex           (XmPrimitiveIndex + 1)
#define XmArrowButtonIndex      XmArrowBIndex
#define XmLabelIndex            (XmPrimitiveIndex + 1)
#define XmListIndex             (XmPrimitiveIndex + 1)
#define XmScrollBarIndex        (XmPrimitiveIndex + 1)
#define XmSeparatorIndex        (XmPrimitiveIndex + 1)
#define XmTextIndex             (XmPrimitiveIndex + 1)

#define XmCascadeBIndex         (XmLabelIndex + 1)
#define XmCascadeButtonIndex    XmCascadeBIndex
#define XmDrawnBIndex           (XmLabelIndex + 1)
#define XmDrawnButtonIndex      XmDrawnBIndex
#define XmPushBIndex            (XmLabelIndex + 1)
#define XmPushButtonIndex       XmPushBIndex
#define XmToggleBIndex          (XmLabelIndex + 1)
#define XmToggleButtonIndex     XmToggleBIndex
#define XmTearOffButtonIndex    (XmPushBIndex + 1)

#define XmArrowBGIndex          (XmGadgetIndex + 1)
#define XmArrowButtonGadgetIndex XmArrowBGIndex
#define XmLabelGIndex           (XmGadgetIndex + 1)
#define XmLabelGadgetIndex      XmLabelGIndex
#define XmSeparatoGIndex        (XmGadgetIndex + 1)
#define XmSeparatorGadgetIndex  XmSeparatoGIndex

#define XmCascadeBGIndex        (XmLabelGIndex + 1)
#define XmCascadeButtonGadgetIndex XmCascadeBGIndex
#define XmPushBGIndex           (XmLabelGIndex + 1)
#define XmPushButtonGadgetIndex XmPushBGIndex
#define XmToggleBGIndex         (XmLabelGIndex + 1)
#define XmToggleButtonGadgetIndex XmToggleBGIndex

#define XmBulletinBIndex        (XmManagerIndex + 1)
#define XmBulletinBoardIndex    XmBulletinBIndex
#define XmDrawingAIndex         (XmManagerIndex + 1)
#define XmDrawingAreaIndex      XmDrawingAIndex
#define XmFrameIndex            (XmManagerIndex + 1)
#define XmPanedWIndex           (XmManagerIndex + 1)
#define XmPanedWindowIndex      XmPanedWIndex
#define XmSashIndex             (XmPrimitiveIndex + 1)
#define XmRowColumnIndex        (XmManagerIndex + 1)
#define XmScaleIndex            (XmManagerIndex + 1)
#define XmScrolledWIndex        (XmManagerIndex + 1)
#define XmScrolledWindowIndex   XmScrolledWIndex

#define XmFormIndex             (XmBulletinBIndex + 1)
#define XmMessageBIndex         (XmBulletinBIndex + 1)
#define XmMessageBoxIndex       XmMessageBIndex
#define XmSelectioBIndex        (XmBulletinBIndex + 1)
#define XmSelectionBoxIndex     XmSelectioBIndex

#define XmMainWIndex            (XmScrolledWIndex + 1)
#define XmMainWindowIndex       XmMainWIndex

#define XmCommandIndex          (XmSelectioBIndex + 1)
#define XmFileSBIndex           (XmSelectioBIndex + 1)
#define XmFileSelectionBoxIndex XmFileSBIndex

#define XmShellIndex            (XmCompositeIndex + 1)
#define ShellIndex              XmShellIndex
#define XmOverrideShellIndex    (XmShellIndex + 1)
#define OverrideShellIndex      XmOverrideShellIndex
#define XmWMShellIndex          (XmShellIndex + 1)
#define WMShellIndex            XmWMShellIndex
#define XmVendorShellIndex      (XmWMShellIndex + 1)
#define VendorShellIndex        XmVendorShellIndex
#define XmTransientShellIndex   (XmVendorShellIndex + 1)
#define TransientShellIndex     XmTransientShellIndex
#define XmTopLevelShellIndex    (XmVendorShellIndex + 1)
#define TopLevelShellIndex      XmTopLevelShellIndex
#define XmApplicationShellIndex (XmTopLevelShellIndex + 1)
#define ApplicationShellIndex   XmApplicationShellIndex
#define XmDisplayIndex          (XmApplicationShellIndex + 1)

#define XmDialogSIndex          (XmTransientShellIndex + 1)
#define XmDialogShellIndex      XmDialogSIndex
#define XmMenuShellIndex        (XmOverrideShellIndex + 1)

#define XmDragIconIndex         (XmRectObjIndex + 1)
#define XmDropSiteManagerIndex  (XmObjectIndex + 1)
#define XmDropTransferIndex     (XmObjectIndex + 1)
#define XmDragOverShellIndex    (XmVendorShellIndex + 1)
#define XmDragContextIndex      (XmCoreIndex + 1)

#define XmOFFSETBITS (sizeof(Cardinal)*8/2)
#define XmOFFSETMASK ((1<<XmOFFSETBITS)-1)

typedef struct _XmPartResource {
    String resource_name;
    String resource_class;
    String resource_type;
    Cardinal resource_size;
    Cardinal resource_offset;
    String default_type;
    XtPointer default_addr;
} XmPartResource;

#define XmPartOffset(part, variable) \
    ((part##Index) << XmOFFSETBITS) + XtOffsetOf( part##Part, variable)

#define XmConstraintPartOffset(part, var) \
    ((part##Index) << XmOFFSETBITS) + XtOffsetOf(part##ConstraintPart, var)

#define XmGetPartOffset(r, off) \
    ((r)->resource_offset & 0xffff) + \
	(*(off))[(r)->resource_offset >> XmOFFSETBITS];

#define XmField(widget, offsetrecord, part, variable, type) \
    (*(type *)(((char *) (widget)) + offsetrecord[part##Index] + \
                XtOffsetOf( part##Part, variable)))

#define XmConstraintField(widget, offsetrecord, part, variable, type) \
    (*(type *)(((char *) (widget)->core.constraints) + \
		offsetrecord[part##Index] + \
		XtOffsetOf( part##ConstraintPart, variable)))

/*
 * these structures must match those of XRectangle, XRegion
 */

typedef struct {
    short x1, x2, y1, y2;
} XmRegionBox;

typedef struct _XmRegion {
    long	size;
    long	numRects;
    XmRegionBox	*rects;
    XmRegionBox	extents;
} XmRegionRec, *XmRegion;

/*********************** Trait.c ***********************************/
#include <Xm/TraitP.h>

/*********************** GadgetUtils.c *****************************/

XMLIBEXPORT XmGadget _XmInputInGadget(Widget cw, int x, int y);
XMLIBEXPORT XmGadget _XmInputForGadget(Widget cw, int x, int y);
XMLIBEXPORT void _XmConfigureObject(Widget g, Position x, Position y,
			Dimension width, Dimension height,
			Dimension border_width);
XMLIBEXPORT void XmeConfigureObject(Widget g, Position x, Position y,
			Dimension width, Dimension height,
			Dimension border_width);
XMLIBEXPORT void _XmResizeObject(Widget g, Dimension width, Dimension height,
		     Dimension border_width);
XMLIBEXPORT void _XmMoveObject(Widget g, Position x, Position y);
XMLIBEXPORT void _XmRedisplayGadgets(Widget w, XEvent *event, Region region);
XMLIBEXPORT void XmeRedisplayGadgets(Widget w, XEvent *event, Region region);
XMLIBEXPORT void _XmDispatchGadgetInput(Widget g, XEvent *event, Mask mask);
XMLIBEXPORT Time __XmGetDefaultTime(Widget w, XEvent *event);

/************************* ImageCache.c *******************************/

#define _XmCreateImage(IMAGE, DISPLAY, DATA, WIDTH, HEIGHT, BYTE_ORDER) {\
    IMAGE = XCreateImage(DISPLAY,\
			 DefaultVisual(DISPLAY, DefaultScreen(DISPLAY)),\
			 1,\
			 XYBitmap,\
			 0,\
			 DATA,\
			 WIDTH, HEIGHT,\
			 8,\
			 (WIDTH+7) >> 3);\
    IMAGE->byte_order = BYTE_ORDER;\
    IMAGE->bitmap_unit = 8;\
    IMAGE->bitmap_bit_order = LSBFirst;\
}

XMLIBEXPORT Boolean _XmInstallImage(XImage *image, char *image_name,
		        int hot_x, int hot_y);
XMLIBEXPORT Boolean _XmGetImage(Screen *screen, char *image_name, XImage **image);
XMLIBEXPORT Boolean _XmGetPixmapData(Screen *screen,
			 Pixmap pixmap,
			 char **image_name,
			 int *depth,
			 Pixel *foreground,
			 Pixel *background,
			 int *hot_x,
			 int *hot_y,
			 unsigned int *width,
			 unsigned int *height);
XMLIBEXPORT Boolean XmeGetPixmapData(Screen *screen,
			 Pixmap pixmap,
			 char **image_name,
			 int *depth,
			 Pixel *foreground,
			 Pixel *background,
			 int *hot_x,
			 int *hot_y,
			 unsigned int *width,
			 unsigned int *height);
XMLIBEXPORT Pixmap _XmGetPixmap(Screen *screen,
		    char *image_name,
		    int depth,
		    Pixel foreground,
		    Pixel background);
XMLIBEXPORT Boolean _XmInstallPixmap(Pixmap pixmap,
			 Screen *screen,
			 char *image_name,
			 Pixel foreground,
			 Pixel background);

/************************** MapEvent.c *****************************/

XMLIBEXPORT Boolean _XmMapBtnEvent(String str,
		       int *eventType,
		       unsigned int *button,
		       unsigned int *modifiers);
XMLIBEXPORT Boolean _XmMapKeyEvent(String str,
		       int *eventType,
		       unsigned *keysym,
		       unsigned int *modifiers);
XMLIBEXPORT Boolean _XmMatchBtnEvent(XEvent *event,
		         int eventType,
			 unsigned int button,
			 unsigned int modifiers);
XMLIBEXPORT Boolean _XmMatchKeyEvent(XEvent *event,
			 int eventType,
			 unsigned int key,
			 unsigned int modifiers);

/************************** ReadImage.c *****************************/

XMLIBEXPORT XImage *_XmGetImageFromFile(char *filename);
XMLIBEXPORT XImage *_XmGetImageAndHotSpotFromFile(char *filename, int *hot_x, int *hot_y);

/************************* RenderTable.c ****************************/

XMLIBEXPORT Boolean XmeRenderTableGetDefaultFont(XmRenderTable renderTable,
                                     XFontStruct  **fontStruct);

/************************* ResConvert.c *****************************/

enum { XmLABEL_FONTLIST = 1,
       XmBUTTON_FONTLIST,
       XmTEXT_FONTLIST
};

enum { XmLABEL_RENDER_TABLE = 1, 
       XmBUTTON_RENDER_TABLE, 
       XmTEXT_RENDER_TABLE  
};


XMLIBEXPORT void _XmRegisterConverters(void);
#define _XmWarning XmeWarning
XMLIBEXPORT void XmeWarning(Widget w, const char *message, ...);
XMLIBEXPORT Boolean _XmStringsAreEqual(char *in_str, char *text_str);
XMLIBEXPORT Boolean XmeNamesAreEqual(char *in_str, char *text_str);
XMLIBEXPORT XmFontList _XmGetDefaultFontList(Widget w, unsigned char fontListType);
XMLIBEXPORT char *_XmConvertCSToString(XmString cs);
XMLIBEXPORT Boolean _XmCvtXmStringToCT(XrmValue *from, XrmValue *to);
XMLIBEXPORT XmFontList XmeGetDefaultRenderTable(Widget w, unsigned int fontListType);


/**************************** ResInd.c *****************************/

XMLIBEXPORT void _XmBuildResources(XmSyntheticResource **wc_resources_ptr,
		       int *wc_num_resources_ptr,
		       XmSyntheticResource *sc_resources,
		       int sc_num_resources);
XMLIBEXPORT void _XmInitializeSyntheticResources(XmSyntheticResource *resources,
				     int num_resources);
XMLIBEXPORT void _XmPrimitiveGetValuesHook(Widget w,
			       ArgList args,
			       Cardinal *num_args);
XMLIBEXPORT void _XmGadgetGetValuesHook(Widget w,
			    ArgList args,
			    Cardinal *num_args);
XMLIBEXPORT void _XmManagerGetValuesHook(Widget w,
			     ArgList args,
			     Cardinal *num_args);
XMLIBEXPORT void _XmExtGetValuesHook(Widget w,
			 ArgList args,
			 Cardinal *num_args);
XMLIBEXPORT void _XmExtImportArgs(Widget w,
		      ArgList args,
		      Cardinal *num_args);
XMLIBEXPORT void _XmPrimitiveImportArgs(Widget w,
			    ArgList args,
			    Cardinal *num_args);
XMLIBEXPORT void _XmGadgetImportArgs(Widget w,
			 ArgList args,
			 Cardinal *num_args);
XMLIBEXPORT void _XmGadgetImportSecondaryArgs(Widget w,
				  ArgList args,
				  Cardinal *num_args);
XMLIBEXPORT void _XmManagerImportArgs(Widget w,
			  ArgList args,
			  Cardinal *num_args);
XMLIBEXPORT int _XmConvertUnits(Screen *screen,
		    int dimension,
		    int from_type,
		    int from_val,
		    int to_type);
XMLIBEXPORT XmImportOperator _XmToHorizontalPixels(Widget widget,
				       int offset,
				       XtArgVal *value);
XMLIBEXPORT XmImportOperator XmeToHorizontalPixels(Widget widget,
				       int offset,
				       XtArgVal *value);
XMLIBEXPORT XmImportOperator _XmToVerticalPixels(Widget widget,
				     int offset,
				     XtArgVal *value);
XMLIBEXPORT XmImportOperator XmeToVerticalPixels(Widget widget,
				     int offset,
				     XtArgVal *value);
XMLIBEXPORT void _XmFromHorizontalPixels(Widget widget,
			     int offset,
			     XtArgVal *value);
XMLIBEXPORT void XmeFromHorizontalPixels(Widget widget,
			     int offset,
			     XtArgVal *value);
XMLIBEXPORT void _XmFromVerticalPixels(Widget widget,
			   int offset,
			   XtArgVal *value);
XMLIBEXPORT void XmeFromVerticalPixels(Widget widget,
			   int offset,
			   XtArgVal *value);
XMLIBEXPORT void _XmSortResourceList(XrmResource *list[], Cardinal len);
XMLIBEXPORT void _XmUnitTypeDefault(Widget widget,
		        int offset,
		        XrmValue *value);
XMLIBEXPORT unsigned char _XmGetUnitType(Widget widget);


/************************* UniqueEvent.c *****************************/

XMLIBEXPORT Boolean _XmIsEventUnique(XEvent *event);
XMLIBEXPORT void _XmRecordEvent(XEvent *event);

/*************************** Visual.c ********************************/

#define XmLOOK_AT_SCREEN          (1<<0)
#define XmLOOK_AT_CMAP            (1<<1)
#define XmLOOK_AT_BACKGROUND      (1<<2)
#define XmLOOK_AT_FOREGROUND      (1<<3)
#define XmLOOK_AT_TOP_SHADOW      (1<<4)
#define XmLOOK_AT_BOTTOM_SHADOW   (1<<5)
#define XmLOOK_AT_SELECT          (1<<6)

#define XmBACKGROUND     ((unsigned char) (1<<0))
#define XmFOREGROUND     ((unsigned char) (1<<1))
#define XmTOP_SHADOW     ((unsigned char) (1<<2))
#define XmBOTTOM_SHADOW  ((unsigned char) (1<<3))
#define XmSELECT         ((unsigned char) (1<<4))

typedef struct _XmColorData {
   Screen * screen;
   Colormap color_map;
   unsigned char allocated;
   XColor background;
   XColor foreground;
   XColor top_shadow;
   XColor bottom_shadow;
   XColor select;
} XmColorData;

XMLIBEXPORT void _XmRegisterPixmapConverters(void);
XMLIBEXPORT char *_XmGetBGPixmapName(void);
XMLIBEXPORT void _XmClearBGPixmap(void);
XMLIBEXPORT void XmeGetDefaultPixel(Widget widget, int type, int offset, XrmValue *value);
XMLIBEXPORT void _XmForegroundColorDefault(Widget widget, int offset, XrmValue *value);
XMLIBEXPORT void _XmHighlightColorDefault(Widget widget, int offset, XrmValue *value);
XMLIBEXPORT void _XmBackgroundColorDefault(Widget widget, int offset, XrmValue *value);
XMLIBEXPORT void _XmTopShadowColorDefault(Widget widget, int offset, XrmValue *value);
XMLIBEXPORT void _XmBottomShadowColorDefault(Widget widget, int offset, XrmValue *value);
XMLIBEXPORT void _XmPrimitiveTopShadowPixmapDefault(Widget widget, int offset,
				        XrmValue *value);
XMLIBEXPORT void _XmManagerTopShadowPixmapDefault(Widget widget, int offset,
				      XrmValue *value);
XMLIBEXPORT void _XmPrimitiveHighlightPixmapDefault(Widget widget, int offset,
					XrmValue *value);
XMLIBEXPORT void _XmManagerHighlightPixmapDefault(Widget widget, int offset,
				      XrmValue *value);
XMLIBEXPORT void _XmGetDefaultThresholdsForScreen(Screen *screen);
XMLIBEXPORT String _XmGetDefaultBackgroundColorSpec(Screen *screen);
XMLIBEXPORT void _XmSetDefaultBackgroundColorSpec(Screen *screen, String new_color_spec);
XMLIBEXPORT XmColorData *_XmGetDefaultColors(Screen *screen, Colormap color_map);
XMLIBEXPORT Boolean _XmSearchColorCache(unsigned int which, XmColorData *values,
			    XmColorData **ret);
XMLIBEXPORT XmColorData *_XmAddToColorCache(XmColorData *new_rec);
XMLIBEXPORT Pixel _XmBlackPixel(Screen *screen, Colormap colormap, XColor blackcolor);
XMLIBEXPORT Pixel _XmWhitePixel(Screen *screen, Colormap colormap, XColor whitecolor);
XMLIBEXPORT Pixel _XmAccessColorData(XmColorData *cd, unsigned char which);
XMLIBEXPORT XmColorData *_XmGetColors(Screen *screen, Colormap color_map, Pixel background);
XMLIBEXPORT void _XmSelectColorDefault(Widget w, int offset, XrmValue *val);

/**************************** XmString.c **********************************/

XMLIBEXPORT XFontStruct *_XmGetFirstFont(XmFontListEntry entry);
XMLIBEXPORT Boolean _XmFontListGetDefaultFont(XmFontList fontlist,
				  XFontStruct **font_struct);
XMLIBEXPORT Boolean _XmFontListSearch(XmFontList fontlist, XmStringCharSet charset,
			  short *indx, XFontStruct **font_struct);
XMLIBEXPORT Boolean _XmStringIsXmString(XmString string);
XMLIBEXPORT Boolean _XmStringInitContext(_XmStringContext *context, _XmString string);
XMLIBEXPORT Boolean _XmStringGetNextSegment(_XmStringContext context,
				XmStringCharSet *charset,
				XmStringDirection *direction,
				char **text,
				short *char_count,
				Boolean *separator) ;
XMLIBEXPORT void _XmStringFreeContext(_XmStringContext context) ;
XMLIBEXPORT Dimension _XmStringWidth(XmFontList fontlist, _XmString string) ;
XMLIBEXPORT Dimension _XmStringHeight(XmFontList fontlist, _XmString string) ;
XMLIBEXPORT void _XmStringExtent(XmFontList fontlist, _XmString string,
		     Dimension *width, Dimension *height) ;
XMLIBEXPORT Boolean _XmStringEmpty(_XmString string);
XMLIBEXPORT void _XmStringDraw(Display *d, Window w, XmFontList fontlist, _XmString string,
		   GC gc, Position x, Position y, Dimension width,
		   unsigned char align, unsigned char lay_dir,
		   XRectangle *clip);
XMLIBEXPORT void _XmStringDrawImage(Display *d, Window w,
			XmFontList fontlist, _XmString string,
			GC gc, Position x, Position y, Dimension width,
			unsigned char align, unsigned char lay_dir,
			XRectangle *clip);
XMLIBEXPORT void _XmStringDrawUnderline(Display *d, Window w, XmFontList f, _XmString s,
			    GC gc, Position x, Position y, Dimension width,
			    unsigned char align, unsigned char lay_dir,
			    XRectangle *clip, _XmString u);
XMLIBEXPORT void _XmStringDrawMnemonic(Display *d, Window w,
                           XmFontList fontlist, _XmString string,
                           GC gc, Position x, Position y, Dimension width,
                           unsigned char alignment,
                           unsigned char layout_direction,
                           XRectangle *clip,
                           String mnemonic, XmStringCharSet charset);
XMLIBEXPORT _XmString _XmStringCreate(XmString cs);
XMLIBEXPORT void _XmStringFree(_XmString string);
XMLIBEXPORT char *_XmStringGetCurrentCharset(void) ;
XMLIBEXPORT char *_XmCharsetCanonicalize(String charset);
XMLIBEXPORT _XmString _XmStringCopy(_XmString string);
XMLIBEXPORT Boolean _XmStringByteCompare(_XmString a, _XmString b);
XMLIBEXPORT Boolean _XmStringHasSubstring(_XmString string, _XmString substring);
XMLIBEXPORT XmString _XmStringCreateExternal(XmFontList fontlist, _XmString cs);
XMLIBEXPORT Dimension _XmStringBaseline(XmFontList fontlist, _XmString string);
XMLIBEXPORT int _XmStringLineCount(_XmString string);
XMLIBEXPORT char * _XmStringGetTextConcat(XmString string);
XMLIBEXPORT Boolean _XmStringIsCurrentCharset(XmStringCharSet c);
XMLIBEXPORT Boolean _XmStringSingleSegment(XmString str, char **pTextOut,
			       XmStringCharSet *pCharsetOut);

XMLIBEXPORT void _XmStringUpdateWMShellTitle(XmString xmstr, Widget shell);
XMLIBEXPORT void XmeStringUpdateWMShellTitle(XmString xmstr, Widget shell);

XMLIBEXPORT XmIncludeStatus XmeGetNextCharacter(XtPointer *text_in_out,
                                    XtPointer text_end,
                                    XmTextType type,
                                    XmStringTag tag,
                                    XmParseMapping entry,
                                    int pattern_length,
                                    XmString *str_include,
                                    XtPointer call_data);

XMLIBEXPORT XmIncludeStatus XmeGetDirection(XtPointer *text_in_out,
                                XtPointer text_end,
                                XmTextType type,
                                XmStringTag tag,
                                XmParseMapping entry,
                                int pattern_length,
                                XmString *str_include,
                                XtPointer call_data);



/************************* Traversal.c ********************************/

#define XmTAB_ANY	((XmNavigationType)255)
#define XmNONE_OR_BC	((XmNavigationType)254)

typedef struct _XmFocusMovedCallbackStruct {
    int 	reason;
    XEvent  	*event;
    Boolean 	cont;
    Widget	old_focus;
    Widget	new_focus;
    unsigned char focus_policy;
} XmFocusMovedCallbackStruct, *XmFocusMovedCallback;

typedef struct _XmFocusDataRec *XmFocusData;

XMLIBEXPORT XmFocusData _XmCreateFocusData(void);
XMLIBEXPORT void _XmDestroyFocusData(XmFocusData focusData);
XMLIBEXPORT void _XmSetActiveTabGroup(XmFocusData focusData, Widget tabGroup);
XMLIBEXPORT Widget _XmGetActiveItem(Widget w);
XMLIBEXPORT void _XmNavigInitialize(Widget request, Widget new_wid,
			ArgList args, Cardinal *num_args);
XMLIBEXPORT Boolean _XmNavigSetValues(Widget current, Widget request, Widget new_wid,
			  ArgList args, Cardinal *num_args);
XMLIBEXPORT void _XmNavigChangeManaged(Widget wid);
XMLIBEXPORT void XmeNavigChangeManaged(Widget wid);
XMLIBEXPORT void _XmNavigResize(Widget wid);
XMLIBEXPORT void _XmValidateFocus(Widget wid);
XMLIBEXPORT void _XmNavigDestroy(Widget wid);
XMLIBEXPORT Boolean _XmCallFocusMoved(Widget old, Widget new_wid, XEvent *event);
XMLIBEXPORT Boolean _XmMgrTraversal(Widget wid, XmTraversalDirection direction);
XMLIBEXPORT void _XmClearFocusPath(Widget wid);
XMLIBEXPORT Boolean _XmFocusIsHere(Widget w);
XMLIBEXPORT unsigned char _XmGetFocusPolicy(Widget w);
XMLIBEXPORT Widget _XmFindTopMostShell(Widget w);
XMLIBEXPORT void _XmFocusModelChanged(Widget wid,
			  XtPointer client_data, XtPointer call_data);
XMLIBEXPORT Boolean _XmGrabTheFocus(Widget w, XEvent *event);
XMLIBEXPORT XmFocusData _XmGetFocusData(Widget wid);
XMLIBEXPORT Boolean _XmCreateVisibilityRect(Widget w, XRectangle *rectPtr);
XMLIBEXPORT void _XmSetRect(XRectangle *rect, Widget w);
XMLIBEXPORT int _XmIntersectRect(XRectangle *srcRectA, Widget widget, XRectangle *dstRect);
XMLIBEXPORT int _XmEmptyRect(XRectangle *r);
XMLIBEXPORT void _XmClearRect(XRectangle *r);
XMLIBEXPORT Boolean _XmIsNavigable(Widget wid);
XMLIBEXPORT void _XmWidgetFocusChange(Widget wid, XmFocusChange change);
XMLIBEXPORT Widget _XmNavigate(Widget wid, XmTraversalDirection direction);
XMLIBEXPORT Widget _XmFindNextTabGroup(Widget wid);
XMLIBEXPORT Widget _XmFindPrevTabGroup(Widget wid);
XMLIBEXPORT void _XmSetInitialOfTabGroup(Widget tab_group, Widget init_focus);
XMLIBEXPORT void _XmResetTravGraph(Widget wid);
XMLIBEXPORT Boolean _XmFocusIsInShell(Widget wid);
XMLIBEXPORT Boolean XmeFocusIsInShell(Widget wid);
XMLIBEXPORT Boolean _XmShellIsExclusive(Widget wid);
XMLIBEXPORT Widget _XmGetFirstFocus(Widget wid);
				
/*********************** TravAct.c ******************************/

XMLIBEXPORT void _XmTrackShellFocus(Widget wid,
			XtPointer client_data,
			XEvent *event,
			Boolean *dontSwallow);

XMLIBEXPORT void _XmManagerEnter(Widget wid,
		     XEvent *event_in,
		     String *params,
		     Cardinal *num_params);

XMLIBEXPORT void _XmManagerLeave(Widget wid,
		     XEvent *event_in,
		     String *params,
		     Cardinal *num_params);

XMLIBEXPORT void _XmManagerFocusInInternal(Widget wid,
			       XEvent *event,
			       String *params,
			       Cardinal *num_params);

XMLIBEXPORT void _XmManagerFocusIn(Widget mw,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params);

XMLIBEXPORT void _XmManagerFocusOut(Widget wid,
		        XEvent *event,
		        String *params,
		        Cardinal *num_params);

XMLIBEXPORT void _XmManagerUnmap(Widget mw,
		     XEvent *event,
		     String *params,
		     Cardinal *num_params);

XMLIBEXPORT void _XmPrimitiveEnter(Widget w, 
		       XEvent *event, 
		       String *params, 
		       Cardinal *num_params);

XMLIBEXPORT void _XmPrimitiveLeave(Widget w, 
		       XEvent *event, 
		       String *params, 
		       Cardinal *num_params);

XMLIBEXPORT void _XmPrimitiveFocusOut(Widget w, 
			  XEvent *event, 
			  String *params, 
			  Cardinal *num_params);

XMLIBEXPORT void _XmPrimitiveFocusIn(Widget w, 
			 XEvent *event, 
			 String *params, 
			 Cardinal *num_params);

XMLIBEXPORT void _XmPrimitiveUnmap(Widget w,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params);

XMLIBEXPORT void _XmEnterGadget(Widget w,
		    XEvent *event,
		    String *params,
		    Cardinal *num_params);

XMLIBEXPORT void _XmLeaveGadget(Widget w,
		    XEvent *event,
		    String *params,
		    Cardinal *num_params);

XMLIBEXPORT void _XmFocusInGadget(Widget w,
		      XEvent *event,
		      String *params,
		      Cardinal *num_params);

XMLIBEXPORT void _XmFocusOutGadget(Widget w,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params);

/************************ GeoUtils.c ***************************/

XMLIBEXPORT void _XmGeoAdjustBoxes(XmGeoMatrix geoSpec);
XMLIBEXPORT void _XmGeoArrangeBoxes(XmGeoMatrix geoSpec, Position x, Position y,
			       Dimension *pW, Dimension *pH);
XMLIBEXPORT Dimension _XmGeoBoxesSameWidth(XmKidGeometry rowPtr, Dimension width);
XMLIBEXPORT Dimension _XmGeoBoxesSameHeight(XmKidGeometry rowPtr, Dimension height);
XMLIBEXPORT void _XmGeoClearRectObjAreas(RectObj r, XWindowChanges *old);
XMLIBEXPORT int _XmGeoCount_kids(CompositeWidget c);
XMLIBEXPORT void _XmGeoGetDimensions(XmGeoMatrix geoSpec);
XMLIBEXPORT XmKidGeometry _XmGetKidGeo(Widget wid, Widget instigator,
			   XtWidgetGeometry *request, 
			   int uniform_border, Dimension border,
			   int uniform_width_margins, 
			   int uniform_height_margins,
			   Widget help, int geo_type);
XMLIBEXPORT void _XmGeoLoadValues(Widget wid, int geoType, Widget instigator, 
		      XtWidgetGeometry *request,
		      XtWidgetGeometry *geoResult);
XMLIBEXPORT XmGeoMatrix _XmGeoMatrixAlloc(unsigned int numRows,
			      unsigned int numBoxes,
			      unsigned int extSize);
XMLIBEXPORT void _XmGeoMatrixFree(XmGeoMatrix geo_spec);
XMLIBEXPORT void _XmGeoMatrixGet(XmGeoMatrix geoSpec, int geoType);
XMLIBEXPORT void _XmGeoMatrixSet(XmGeoMatrix geoSpec);
XMLIBEXPORT Boolean _XmGeoReplyYes(Widget wid, XtWidgetGeometry *desired,
				   XtWidgetGeometry *response);
XMLIBEXPORT Boolean _XmGeoSetupKid(XmKidGeometry geo, Widget kidWid);
XMLIBEXPORT Boolean _XmGeometryEqual(Widget wid, XtWidgetGeometry *geoA,
				     XtWidgetGeometry *geoB);
XMLIBEXPORT void _XmHandleSizeUpdate(Widget wid, unsigned char policy,
			 XmGeoCreateProc createMatrix);
XMLIBEXPORT XtGeometryResult _XmHandleQueryGeometry(Widget wid,
				        XtWidgetGeometry *intended,
				        XtWidgetGeometry *desired, 
				        unsigned char policy,
				        XmGeoCreateProc createMatrix);
XMLIBEXPORT XtGeometryResult _XmHandleGeometryManager(Widget wid, Widget instigator,
					  XtWidgetGeometry *desired, 
					  XtWidgetGeometry *allowed,
					  unsigned char policy, 
					  XmGeoMatrix *cachePtr,
					  XmGeoCreateProc createMatrix);
XMLIBEXPORT XtGeometryResult _XmMakeGeometryRequest(Widget w,
					XtWidgetGeometry *geom);
XMLIBEXPORT void _XmMenuBarFix(XmGeoMatrix geoSpec, int action,
		   XmGeoMajorLayout layoutPtr, XmKidGeometry rowPtr);
XMLIBEXPORT void _XmSeparatorFix(XmGeoMatrix geoSpec, int action,
		     XmGeoMajorLayout layoutPtr, XmKidGeometry rowPtr);
XMLIBEXPORT void _XmSetKidGeo(XmKidGeometry kg, Widget instigator);
XMLIBEXPORT XtGeometryResult XmeReplyToQueryGeometry(Widget w,
	XtWidgetGeometry *request, XtWidgetGeometry *reply);


/*************************** Region.c ***************************/
XMLIBEXPORT XmRegion _XmRegionCreate(void);
XMLIBEXPORT XmRegion _XmRegionCreateSize(long size);
XMLIBEXPORT void _XmRegionComputeExtents(XmRegion r);
XMLIBEXPORT void _XmRegionGetExtents(XmRegion r, XRectangle *rect);
XMLIBEXPORT void _XmRegionUnionRectWithRegion(XRectangle *rect,
				  XmRegion source,
				  XmRegion dest);
XMLIBEXPORT void _XmRegionIntersectRectWithRegion(XRectangle *rect,
				      XmRegion source,
				      XmRegion dest);
XMLIBEXPORT long _XmRegionGetNumRectangles(XmRegion r);
XMLIBEXPORT void _XmRegionGetRectangles(XmRegion r,
			    XRectangle **rects,
			    long *nrects);
XMLIBEXPORT void _XmRegionSetGCRegion(Display *dpy,
			  GC gc,
			  int x_origin,
			  int y_origin,
			  XmRegion r);
XMLIBEXPORT void _XmRegionDestroy(XmRegion r);
XMLIBEXPORT void _XmRegionOffset(XmRegion pRegion, int x, int y);
XMLIBEXPORT void _XmRegionIntersect(XmRegion reg1, XmRegion reg2, XmRegion newReg);
XMLIBEXPORT void _XmRegionUnion(XmRegion reg1, XmRegion reg2, XmRegion newReg);
XMLIBEXPORT void _XmRegionSubtract(XmRegion regM, XmRegion regS, XmRegion regD);
XMLIBEXPORT Boolean _XmRegionIsEmpty(XmRegion r);
XMLIBEXPORT Boolean _XmRegionEqual(XmRegion r1, XmRegion r2);
XMLIBEXPORT Boolean _XmRegionPointInRegion(XmRegion pRegion, int x, int y);
XMLIBEXPORT void _XmRegionClear(XmRegion r );
XMLIBEXPORT void _XmRegionShrink(XmRegion r, int dx, int dy);
XMLIBEXPORT void _XmRegionDrawShadow(Display *display,
			 Drawable d,
			 GC top_gc,
			 GC bottom_gc,
			 XmRegion region,
			 Dimension border_thick,
			 Dimension shadow_thick,
			 unsigned int shadow_type);

/****************************** Dest.c ***************************/

XMLIBEXPORT void _XmSetDestination(Display *dpy, Widget w);

/***************************** XmIm.c ****************************/

XMLIBEXPORT void _XmImChangeManaged(Widget vw);
XMLIBEXPORT void _XmImRealize(Widget vw);
XMLIBEXPORT void _XmImResize(Widget vw);
XMLIBEXPORT void _XmImRedisplay(Widget vw);

/************************* DragBS.c ******************************/

XMLIBEXPORT void _XmInitAtomPairs(Display *display);
XMLIBEXPORT void _XmInitTargetsTable(Display *display);
XMLIBEXPORT Cardinal _XmIndexToTargets(Widget shell, Cardinal t_index, Atom **targetsRtn);
XMLIBEXPORT Cardinal _XmTargetsToIndex(Widget shell, Atom *targets, Cardinal numTargets);
XMLIBEXPORT Atom _XmAllocMotifAtom(Widget shell, Time time);
XMLIBEXPORT void _XmFreeMotifAtom(Widget shell, Atom atom);
XMLIBEXPORT void _XmDestroyMotifWindow(Display *dpy);
XMLIBEXPORT Window _XmGetDragProxyWindow(Display *display);

/************************* DragOverS.c ***************************/

XMLIBEXPORT void _XmDragOverHide(Widget w, Position clipOriginX, Position clipOriginY,
		     XmRegion clipRegion);
XMLIBEXPORT void _XmDragOverShow(Widget w, Position clipOriginX, Position clipOriginY,
		     XmRegion clipRegion);
XMLIBEXPORT void _XmDragOverMove(Widget w, Position x, Position y);
XMLIBEXPORT void _XmDragOverChange(Widget w, unsigned char dropSiteStatus);
XMLIBEXPORT void _XmDragOverFinish(Widget w, unsigned char completionStatus);
XMLIBEXPORT Cursor _XmDragOverGetActiveCursor( Widget w);
XMLIBEXPORT void _XmDragOverSetInitialPosition(Widget w,
				   Position initialX, Position initialY);

/************************** DragUnder.c *******************************/

XMLIBEXPORT void _XmDragUnderAnimation(Widget w, XtPointer clientData, XtPointer callData);

/************************** Xme.c **************************************/

XMLIBEXPORT void XmeVirtualToActualKeysym(Display *Dsp, KeySym VirtualKeysym,
                                     KeySym *RealKeysymReturn,
                                     Modifiers *ModifierReturn);

XMLIBEXPORT void XmeResolvePartOffsets(WidgetClass widget_class,
                           XmOffsetPtr *offset,
                           XmOffsetPtr *constraint_offset);

XMLIBEXPORT Cursor XmeGetNullCursor(Widget w);

XMLIBEXPORT void XmeQueryBestCursorSize(Widget widget, Dimension *width, Dimension *height);

XMLIBEXPORT void XmeSetWMShellTitle(XmString xmstr, Widget shell);
			   
/************************* Xmos.c *************************************/

XMLIBEXPORT extern int XmeMicroSleep(long secs);

XMLIBEXPORT XmString XmeGetLocalizedString(char *reserved, Widget widget,                                                                                  
                               char *resource, String string);



/****************** THESE AREN'T SUPPOSED TO BE USED ******************/

#ifdef XM_1_1_BC

#define XmVPANED_BIT		XmPANED_WINDOW_BIT

#define LOOK_AT_SCREEN		(1<<0)
#define LOOK_AT_CMAP		(1<<1)
#define LOOK_AT_BACKGROUND	(1<<2)
#define LOOK_AT_FOREGROUND	(1<<3)
#define LOOK_AT_TOP_SHADOW	(1<<4)
#define LOOK_AT_BOTTOM_SHADOW	(1<<5)
#define LOOK_AT_SELECT		(1<<6)

#define XmStrlen(s)		((s) ? strlen(s) : 0)

#define DEFAULT_INDICATOR_DIM	9

#ifndef MAX
#define MAX(a,b)		((a) > (b) ? (a) : (b))
#endif

#define RX(r)		(((RectObj)(r))->rectangle.x)
#define RY(r)		(((RectObj)(r))->rectangle.y)
#define RWidth(r)	(((RectObj)(r))->rectangle.width)
#define RHeight(r)	(((RectObj)(r))->rectangle.height)
#define RBorder(r)	(((RectObj)(r))->rectangle.border_width)

#define GMode(g)	((g)->request_mode)
#define IsX(g)		(GMode(g) & CWX)
#define IsY(g)		(GMode(g) & CWY)
#define IsWidth(g)	(GMode(g) & CWWidth)
#define IsHeight(g)	(GMode(g) & CWHeight)
#define IsBorder(g)	(GMode(g) & CWBorderWidth)
#define IsWidthHeight(g) ((GMode(g) & CWWidth) || (GMode(g) & CWHeight))
#define IsQueryOnly(g)	(GMode(g) & XtCWQueryOnly)

#define MAXDIMENSION    ((1 << 31)-1)

#define Max(x, y)       (((x) > (y)) ? (x) : (y))
#define Min(x, y)       (((x) < (y)) ? (x) : (y))
#define AssignMax(x, y) if ((y) > (x)) x = (y)
#define AssignMin(x, y) if ((y) < (x)) x = (y)


#define DIALOG_SUFFIX "_popup"
#define DIALOG_SUFFIX_SIZE 6

#define XM_3D_ENHANCE_PIXEL 2
#define XM_DEFAULT_TOP_MARGIN 0
#define XM_DEFAULT_BOTTOM_MARGIN 0

XMLIBEXPORT extern WidgetClass xmWorldObjectClass;
XMLIBEXPORT extern WidgetClass xmDesktopObjectClass;
XMLIBEXPORT extern WidgetClass xmDisplayObjectClass;
XMLIBEXPORT extern WidgetClass xmScreenObjectClass;

#endif /* XM_1_1_BC */

/**********************************************************************/

#ifdef __cplusplus
}
#endif


#include <Xm/VendorSP.h>
#include <Xm/ManagerP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/GadgetP.h>
#include <Xm/BaseClassP.h> 


#endif /* _XM_XMP_H */
