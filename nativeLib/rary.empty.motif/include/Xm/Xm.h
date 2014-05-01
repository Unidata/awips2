/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/Xm.h.in,v 1.38 2005/04/16 21:31:44 dannybackx Exp $
 *
 * Copyright © 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2005 LessTif Development Team
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


#ifndef _XM_XM_H
#define _XM_XM_H

#include <stddef.h>    /* for  wchar_t, etc. */

#if defined(_MFC_VER) && !defined(STATIC_XM)
/* If Lesstif is built as a DLL with a Microsoft compiler, we need to use the
 *	__declspec mechanism to import and export variables and functions.
 */
#if XMDLL	/* We're compiling the DLL */
#define XMLIBEXPORT __declspec(dllexport)
#else		/* We're including this in an application that will link to the DLL */
#define XMLIBEXPORT __declspec(dllimport)
#endif
#else
#define XMLIBEXPORT /* Nada */
#endif

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xatom.h>

/* handy little define */

#if NeedFunctionPrototypes
#define _XmANSI_ARGS_(args) args
#else
#define _XmANSI_ARGS_(args) ()
#endif

/* Version Information */
#define LESSTIF_VERSION  _MAJOR_VERSION_
#define LESSTIF_REVISION _MINOR_VERSION_
#define	LESSTIF_UPDATE_LEVEL	_PICO_VERSION_

#define LesstifVersion (LESSTIF_VERSION * 1000 + LESSTIF_REVISION)
#define LesstifVERSION_STRING "@(#)GNU/LessTif Version 2.1 Release _MAJOR_VERSION_._MINOR_VERSION_._PICO_VERSION_"

/* since we're replacing Motif 2.1... */
#define XmVERSION 2
#define XmREVISION 1
#define XmVersion (XmVERSION * 1000 + XmREVISION)
#define XmVERSION_STRING LesstifVERSION_STRING
#define XmUPDATE_LEVEL 0

/* A function to report our version */
extern char *XmVersionString(void);

/* Our headers may use the #defines made above! */
#include <Xm/XmStrDefs.h>
#include <Xm/VirtKeys.h>
#include <Xm/Transfer.h>
#include <Xm/Primitive.h>
#include <Xm/Manager.h>
#include <Xm/Gadget.h>

#ifdef __cplusplus
extern "C" {
#endif

XMLIBEXPORT extern int xmUseVersion;

/* pixmap stuff */

#define XmUNSPECIFIED_PIXMAP	2

/* define for easy checking for unset resources */

#define XmUNSPECIFIED (~0)

/*
 * vendor defines (hey, GNU is a vendor)
 */
#define XmSTRING_OS_CHARSET             XmSTRING_ISO8859_1
#ifndef XmFALLBACK_CHARSET
#define XmFALLBACK_CHARSET              XmSTRING_ISO8859_1
#endif

#define XmDEFAULT_FONT                  _XmSDEFAULT_FONT
#define XmDEFAULT_BACKGROUND            _XmSDEFAULT_BACKGROUND
#define XmDEFAULT_DARK_THRESHOLD        20
#define XmDEFAULT_LIGHT_THRESHOLD       93
#define XmDEFAULT_FOREGROUND_THRESHOLD  70

XMLIBEXPORT extern char    _XmSDEFAULT_FONT[];
XMLIBEXPORT extern char    _XmSDEFAULT_BACKGROUND[];

typedef unsigned char XmDirection;

#define XmDIRECTION_IGNORED 0x30

#define XmRIGHT_TO_LEFT_MASK 0x01
#define XmLEFT_TO_RIGHT_MASK 0x02
#define XmHORIZONTAL_MASK 0x03
#define XmTOP_TO_BOTTOM_MASK 0x04
#define XmBOTTOM_TO_TOP_MASK 0x08
#define XmVERTICAL_MASK 0x0c
#define XmPRECEDENCE_HORIZ_MASK 0x40
#define XmPRECEDENCE_VERT_MASK 0x80
#define XmPRECEDENCE_MASK 0xc0

enum {
    XmRIGHT_TO_LEFT_TOP_TO_BOTTOM =
        XmRIGHT_TO_LEFT_MASK | XmTOP_TO_BOTTOM_MASK | XmPRECEDENCE_HORIZ_MASK,
    XmLEFT_TO_RIGHT_TOP_TO_BOTTOM =
        XmLEFT_TO_RIGHT_MASK | XmTOP_TO_BOTTOM_MASK | XmPRECEDENCE_HORIZ_MASK,
    XmRIGHT_TO_LEFT_BOTTOM_TO_TOP =
        XmRIGHT_TO_LEFT_MASK | XmBOTTOM_TO_TOP_MASK | XmPRECEDENCE_HORIZ_MASK,
    XmLEFT_TO_RIGHT_BOTTOM_TO_TOP =
        XmLEFT_TO_RIGHT_MASK | XmBOTTOM_TO_TOP_MASK | XmPRECEDENCE_HORIZ_MASK,
    XmTOP_TO_BOTTOM_RIGHT_TO_LEFT =
        XmRIGHT_TO_LEFT_MASK | XmTOP_TO_BOTTOM_MASK | XmPRECEDENCE_VERT_MASK,
    XmTOP_TO_BOTTOM_LEFT_TO_RIGHT =
        XmLEFT_TO_RIGHT_MASK | XmTOP_TO_BOTTOM_MASK | XmPRECEDENCE_VERT_MASK,
    XmBOTTOM_TO_TOP_RIGHT_TO_LEFT =
        XmRIGHT_TO_LEFT_MASK | XmBOTTOM_TO_TOP_MASK | XmPRECEDENCE_VERT_MASK,
    XmBOTTOM_TO_TOP_LEFT_TO_RIGHT =
        XmLEFT_TO_RIGHT_MASK | XmBOTTOM_TO_TOP_MASK | XmPRECEDENCE_VERT_MASK,
    XmTOP_TO_BOTTOM =
        XmTOP_TO_BOTTOM_MASK | XmHORIZONTAL_MASK | XmPRECEDENCE_MASK,
    XmBOTTOM_TO_TOP =
        XmBOTTOM_TO_TOP_MASK | XmHORIZONTAL_MASK | XmPRECEDENCE_MASK,
    XmRIGHT_TO_LEFT =
        XmRIGHT_TO_LEFT_MASK | XmVERTICAL_MASK | XmPRECEDENCE_MASK,
    XmLEFT_TO_RIGHT =
        XmLEFT_TO_RIGHT_MASK | XmVERTICAL_MASK | XmPRECEDENCE_MASK,
    XmDEFAULT_DIRECTION = 0xff
};
  
/*
 * XmString stuff
 */

typedef enum {
    XmFONT_IS_FONT,
    XmFONT_IS_FONTSET,
    XmFONT_IS_XOC,	/* IBM's BiDi extension to Motif */
    XmFONT_IS_XFT	/* FontConfig */
} XmFontType;

enum {
    XmSTRING_DIRECTION_L_TO_R, 
    XmSTRING_DIRECTION_R_TO_L,
    XmSTRING_DIRECTION_UNSET = 3,
    XmSTRING_DIRECTION_DEFAULT = XmDEFAULT_DIRECTION
};

typedef unsigned char XmStringDirection;
typedef unsigned char *XmString;
typedef XmString *XmStringTable;
typedef char *XmStringCharSet;
typedef unsigned char XmStringComponentType;
typedef char *XmStringTag;

typedef struct __XmStringContextRec *_XmStringContext;
typedef struct __XmStringRec *_XmString;
typedef struct _XmtStringContextRec *XmStringContext;
typedef struct _XmFontListContextRec *XmFontContext;

#define	_MOTIF_DEFAULT_LOCALE	"_MOTIF_DEFAULT_LOCALE"
/*
 * Rendering stuff
 *	The definitions for this replace some older XmFontList* stuff.
 *	Here's the old stuff for reference :
 *	typedef struct _XmFontListRec *XmFontListEntry, *XmFontList;
 */
typedef struct __XmRenditionRec		*XmFontListEntry;
typedef	struct __XmRenderTableRec	*XmFontList;

typedef struct __XmRenditionRec		*XmRendition;
typedef struct __XmRenderTableRec	*XmRenderTable;

enum {
    XmSTRING_COMPONENT_UNKNOWN,		/* 0 */
    XmSTRING_COMPONENT_CHARSET,		/* 1 */
    XmSTRING_COMPONENT_TEXT,		/* 2 */
    XmSTRING_COMPONENT_DIRECTION,	/* 3 */
    XmSTRING_COMPONENT_SEPARATOR,	/* 4 */
    XmSTRING_COMPONENT_LOCALE_TEXT,	/* 5 */
    XmSTRING_COMPONENT_LOCALE,		/* 6 */
    XmSTRING_COMPONENT_WIDECHAR_TEXT,	/* 7 */
    XmSTRING_COMPONENT_LAYOUT_PUSH,	/* 8 */
    XmSTRING_COMPONENT_LAYOUT_POP,	/* 9 */
    XmSTRING_COMPONENT_RENDITION_BEGIN,	/* 10 */
    XmSTRING_COMPONENT_RENDITION_END,	/* 11 */
    XmSTRING_COMPONENT_TAB		/* 12 */
    /* 13 - 125 is said to be reserved */
};

#define XmSTRING_COMPONENT_FONTLIST_ELEMENT_TAG XmSTRING_COMPONENT_CHARSET

#define XmSTRING_COMPONENT_TAG          XmSTRING_COMPONENT_CHARSET

#define XmSTRING_COMPONENT_END          ((XmStringComponentType) 126)
/* anybody know what 127 is? */
#define XmSTRING_COMPONENT_USER_BEGIN   ((XmStringComponentType) 128)
/* 128-255 are user tags */
#define XmSTRING_COMPONENT_USER_END     ((XmStringComponentType) 255)

typedef enum {
    XmCHARSET_TEXT,
    XmMULTIBYTE_TEXT,
    XmWIDECHAR_TEXT,
    XmNO_TEXT
} XmTextType;

typedef enum {
    XmOUTPUT_ALL,
    XmOUTPUT_BETWEEN,
    XmOUTPUT_BEGINNING,
    XmOUTPUT_END,
    XmOUTPUT_BOTH
} XmParseModel;

/* Real definition hidden in XmI/XmI.h */
typedef struct __XmParseMappingRec *XmParseMapping;
typedef XmParseMapping             *XmParseTable;
typedef unsigned char XmIncludeStatus;

typedef XmIncludeStatus (*XmParseProc) (
	XtPointer	*text_in_out,
	XtPointer	text_end,
	XmTextType	type,
	XmStringTag	tag,
	XmParseMapping	entry,
	int		pattern_length,
	XmString	*str_include,
	XtPointer	call_data);

enum {
    XmINSERT,
    XmTERMINATE,
    XmINVOKE
};

typedef enum {
    XmSTYLE_STRING = XStringStyle,
    XmSTYLE_COMPOUND = XCompoundTextStyle,
    XmSTYLE_TEXT = XTextStyle,
    XmSTYLE_STANDARD_ICC_TEXT = XStdICCTextStyle,
    XmSTYLE_LOCALE = 32,
    XmSTYLE_COMPOUND_STRING
} XmICCEncodingStyle;

/* tab list stuff */
typedef enum {
    XmABSOLUTE,
    XmRELATIVE
} XmOffsetModel;

typedef struct _XmTabRec *XmTab;
typedef struct _XmTabListRec  *XmTabList;

/* render table stuff */

typedef enum {
    XmSKIP,
    XmMERGE_REPLACE,
    XmMERGE_OLD,
    XmMERGE_NEW,
    XmDUPLICATE
} XmMergeMode;

#define XmAS_IS 255
#define XmFORCE_COLOR 1

#define XmUNSPECIFIED_PIXEL ((Pixel) (~0))
#define XmDEFAULT_SELECT_COLOR XmUNSPECIFIED_PIXEL
#define XmREVERSED_GROUND_COLORS (XmDEFAULT_SELECT_COLOR - 1)
#define XmHIGHLIGHT_COLOR (XmREVERSED_GROUND_COLORS - 1)

enum {
    XmUNSPECIFIED_LOAD_MODEL,
    XmLOAD_DEFERRED,
    XmLOAD_IMMEDIATE
};

/* size policy stuff */

enum {
    XmCHANGE_ALL,
    XmCHANGE_NONE,
    XmCHANGE_WIDTH,
    XmCHANGE_HEIGHT
};

/* unitType stuff */

enum {
	XmPIXELS,
	Xm100TH_MILLIMETERS,
	Xm1000TH_INCHES,
	Xm100TH_POINTS,
	Xm100TH_FONT_UNITS,
	XmINCHES,
	XmCENTIMETERS,
	XmMILLIMETERS,
	XmPOINTS,
	XmFONT_UNITS
};

/* delete Responses for VendorShell */

enum {
    XmDESTROY,
    XmUNMAP,
    XmDO_NOTHING
};

/* keyboard focus policies for VendorShell */

enum {
    XmEXPLICIT,
    XmPOINTER
};

/* Navigation stuff */

enum {
    XmNONE,
    XmTAB_GROUP,
    XmSTICKY_TAB_GROUP,
    XmEXCLUSIVE_TAB_GROUP
};

/* Audible Warning types for VendorShell */

enum {
/* implied
 *  XmNONE
 */
    XmBELL = 1
};

/* Input Manager types */
enum {
    XmPER_SHELL,
    XmPER_WIDGET,
    XmINHERIT_POLICY = 255
};

/* Note: The define in XmI.h was changed to XmIMInputPolicy. */
typedef unsigned char XmInputPolicy;

/* various widgets' orientation, menu definitions */

enum {
    XmNO_ORIENTATION,
    XmVERTICAL,
    XmHORIZONTAL
};

/* row column types */

enum {
    XmWORK_AREA,
    XmMENU_BAR,
    XmMENU_PULLDOWN,
    XmMENU_POPUP,
    XmMENU_OPTION
};

/* row column packing strategies */

enum {
    XmNO_PACKING,
    XmPACK_TIGHT,
    XmPACK_COLUMN,
    XmPACK_NONE
};

enum {
/* implied
 *  XmALIGNMENT_BASELINE_TOP,
 *  XmALIGNMENT_CENTER,
 *  XmALIGNMENT_BASELINE_BOTTOM,
 */
    XmALIGNMENT_CONTENTS_TOP = 3,
    XmALIGNMENT_CONTENTS_BOTTOM
};

enum {
    XmTEAR_OFF_ENABLED,
    XmTEAR_OFF_DISABLED
};

enum {
    XmUNPOST,
    XmUNPOST_AND_REPLAY
};

/* XmPanedWindow positioning */
enum {
	XmLAST_POSITION = -1,
	XmFIRST_POSITION
};

/* XmComboBox types */
enum {
   XmCOMBO_BOX = 0,
   XmDROP_DOWN_COMBO_BOX,
   XmDROP_DOWN_LIST
};

/* XmComboBox: XmNpositionMode resource */
enum {
  XmZERO_BASED,
  XmONE_BASED
};

enum {
   XmQUICK_NAVIGATE = 1
};

/* Label and Frame alignments */

enum {
    XmALIGNMENT_BEGINNING,
    XmALIGNMENT_CENTER,
    XmALIGNMENT_END
};

enum {
    XmALIGNMENT_BASELINE_TOP,
/* implied
 *  XmALIGNMENT_CENTER
 */
    XmALIGNMENT_BASELINE_BOTTOM = 2,
    XmALIGNMENT_WIDGET_TOP,
    XmALIGNMENT_WIDGET_BOTTOM
};

#define XmALIGNMENT_CHILD_TOP XmALIGNMENT_WIDGET_BOTTOM
#define XmALIGNMENT_CHILD_BOTTOM XmALIGNMENT_WIDGET_TOP 

/* Frame Child Types */

enum {
    XmFRAME_GENERIC_CHILD,
    XmFRAME_WORKAREA_CHILD,
    XmFRAME_TITLE_CHILD
};

/* For toggle button stuff */
enum {
    XmN_OF_MANY = 1,
    XmONE_OF_MANY,
    XmONE_OF_MANY_ROUND,
    XmONE_OF_MANY_DIAMOND
};

enum { XmUNSET, XmSET, XmINDETERMINATE };
enum { XmTOGGLE_BOOLEAN, XmTOGGLE_INDETERMINATE };

typedef unsigned char XmToggleButtonState;

/* Form attachments */

enum {
    XmATTACH_NONE,
    XmATTACH_FORM,
    XmATTACH_OPPOSITE_FORM,
    XmATTACH_WIDGET,
    XmATTACH_OPPOSITE_WIDGET,
    XmATTACH_POSITION,
    XmATTACH_SELF
};

/* resize policies for some manager widgets */

enum {
    XmRESIZE_NONE,
    XmRESIZE_GROW,
    XmRESIZE_ANY
};

/* callback reasons */

enum {
    XmCR_NONE,				/* 0 */
    XmCR_HELP,				/* 1 */
    XmCR_VALUE_CHANGED,			/* 2 */
    XmCR_INCREMENT,			/* 3 */
    XmCR_DECREMENT,			/* 4 */
    XmCR_PAGE_INCREMENT,		/* 5 */
    XmCR_PAGE_DECREMENT,		/* 6 */
    XmCR_TO_TOP,			/* 7 */
    XmCR_TO_BOTTOM,			/* 8 */
    XmCR_DRAG,				/* 9 */
    XmCR_ACTIVATE,			/* 10 */
    XmCR_ARM,				/* 11 */
    XmCR_DISARM,			/* 12 */
    XmCR_DUMMY13,			/* 13 */
    XmCR_DUMMY14,			/* 14 */
    XmCR_DUMMY15,			/* 15 */
    XmCR_MAP,				/* 16 */
    XmCR_UNMAP,				/* 17 */
    XmCR_FOCUS,				/* 18 */
    XmCR_LOSING_FOCUS,			/* 19 */
    XmCR_MODIFYING_TEXT_VALUE,		/* 20 */
    XmCR_MOVING_INSERT_CURSOR,		/* 21 */
    XmCR_EXECUTE,			/* 22 */
    XmCR_SINGLE_SELECT,			/* 23 */
    XmCR_MULTIPLE_SELECT,		/* 24 */
    XmCR_EXTENDED_SELECT,		/* 25 */
    XmCR_BROWSE_SELECT,			/* 26 */
    XmCR_DEFAULT_ACTION,		/* 27 */
    XmCR_CLIPBOARD_DATA_REQUEST,	/* 28 */
    XmCR_CLIPBOARD_DATA_DELETE,		/* 29 */
    XmCR_CASCADING,			/* 30 */
    XmCR_OK,				/* 31 */
    XmCR_CANCEL,			/* 32 */
    XmCR_DUMMY33,			/* 33 */
    XmCR_APPLY,				/* 34 */
    XmCR_NO_MATCH,			/* 35 */
    XmCR_COMMAND_ENTERED,		/* 36 */
    XmCR_COMMAND_CHANGED,		/* 37 */
    XmCR_EXPOSE,			/* 38 */
    XmCR_RESIZE,			/* 39 */
    XmCR_INPUT,				/* 40 */
    XmCR_GAIN_PRIMARY,			/* 41 */
    XmCR_LOSE_PRIMARY,			/* 42 */
    XmCR_CREATE,			/* 43 */
    XmCR_TEAR_OFF_ACTIVATE,		/* 44 */
    XmCR_TEAR_OFF_DEACTIVATE,		/* 45 */
    XmCR_OBSCURED_TRAVERSAL,		/* 46 */
    XmCR_FOCUS_MOVED,			/* 47 */
    XmCR_DUMMY48,			/* 48 */
    XmCR_DUMMY49,			/* 49 */
    XmCR_DUMMY50,			/* 50 */
    XmCR_DUMMY51,			/* 51 */
    XmCR_DUMMY52,			/* 52 */
    XmCR_DUMMY53,			/* 53 */
    XmCR_REPOST,			/* 54 */
    XmCR_COLLAPSED,			/* 55 */
    XmCR_EXPANDED,			/* 56 */
    XmCR_SELECT,			/* 57 */
    XmCR_DRAG_START,			/* 58 */
    XmCR_NO_FONT,			/* 59 */
    XmCR_NO_RENDITION,			/* 60 */
    XmCR_POST,				/* 61 */
    XmCR_SPIN_NEXT,			/* 62 */
    XmCR_SPIN_PRIOR,			/* 63 */
    XmCR_SPIN_FIRST,			/* 64 */
    XmCR_SPIN_LAST,			/* 65 */
    XmCR_PAGE_SCROLLER_INCREMENT,	/* 66 */
    XmCR_PAGE_SCROLLER_DECREMENT,	/* 67 */
    XmCR_MAJOR_TAB,			/* 68 */
    XmCR_MINOR_TAB,			/* 69 */
	XmCR_START_JOB,
	XmCR_END_JOB,
	XmCR_PAGE_SETUP,
    XmCR_PDM_NONE,                      /* ?? */
    XmCR_PDM_START_VXAUTH,              /* ?? */
    XmCR_PDM_START_PXAUTH,              /* ?? */
    XmCR_PDM_UP,                        /* ?? */
    XmCR_PDM_OK,                        /* ?? */
    XmCR_PDM_CANCEL,                    /* ?? */
    XmCR_PDM_START_ERROR,               /* ?? */
    XmCR_PDM_EXIT_ERROR,                /* ?? */
    XmCR_PROTOCOLS = 6666		/* Note: this was 47 in Motif 1.2x */
};

/*
 * callback structures
 */

typedef struct {
    int reason;
    XEvent *event;
} XmAnyCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    int click_count;
} XmArrowButtonCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    XmString item_or_text;
    int item_position;
} XmComboBoxCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    XmString value;
    int length;
} XmCommandCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    Widget item;
    unsigned char new_outline_state;
} XmContainerOutlineCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    WidgetList selected_items;
    int selected_item_count;
    unsigned char auto_selection_type;
} XmContainerSelectCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    XmRendition rendition;
    char *font_name;
    XmRenderTable render_table;
    XmStringTag tag;
} XmDisplayCallbackStruct;

typedef struct _XmDragStartCallbackStruct {
    int reason;
    XEvent *event;
    Widget widget;
    Boolean doit;
} XmDragStartCallbackStruct, *XmDragStartCallback;

typedef struct {
    int reason;
    XEvent *event;
    Window window;
} XmDrawingAreaCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    Window window;
    int click_count;
} XmDrawnButtonCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    XmString value;
    int length;
    XmString mask;
    int mask_length;
    XmString dir;
    int dir_length;
    XmString pattern;
    int pattern_length;
} XmFileSelectionBoxCallbackStruct;

typedef struct {
    int reason;                    /* reason callback was called */
    XEvent *event;                 /* points to event structure */
    XmString item;                 /* item most recently selected */
    int item_length;               /* number of bytes in item member */
    int item_position;             /* item's position in XmNitems */
    XmString *selected_items;      /* list of items selected */
    int selected_item_count;       /* number of items in selected_items */
    int *selected_item_positions;  /* array of ints marking selected items */
    char selection_type;           /* type of most recent selection */
    unsigned char auto_selection_type;  /* type of automatic selection callback */
} XmListCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    int page_number;
    Widget page_widget;
    int prev_page_number;
    Widget prev_page_widget;
} XmNotebookCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    Widget menuToPost;
    Boolean postIt;
    Widget target;
} XmPopupHandlerCallbackStruct;

typedef struct {
    int reason;                   /* reason callback was called */
    XEvent *event;
    int click_count;
} XmPushButtonCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    Widget widget;
    char *data;
    char *callbackstruct;
} XmRowColumnCallbackStruct;


typedef struct {
    int reason;                   /* reason callback was called */
    XEvent *event;
    int value;
} XmScaleCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    int value;
    int pixel;
} XmScrollBarCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    XmString value;
    int length;
} XmSelectionBoxCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    Widget widget;
    Boolean doit;
    int position;
    XmString value;
    Boolean crossed_boundary;
} XmSimpleSpinBoxCallbackStruct;

typedef struct {
    int reason;
    XEvent *event;
    Widget widget;
    Boolean doit;
    int position;
    XmString value;
    Boolean crossed_boundary;
} XmSpinBoxCallbackStruct;

typedef struct {
    int reason;                   /* reason callback was called */
    XEvent *event;
    int set;
} XmToggleButtonCallbackStruct;

/* multiclick */

enum {
    XmMULTICLICK_DISCARD,
    XmMULTICLICK_KEEP
};

/* Drawn button overrides some of the ShadowTypes */
enum {
    XmSHADOW_IN = 7,
    XmSHADOW_OUT
};

/* Arrow button directions */
enum {
    XmARROW_UP,
    XmARROW_DOWN,
    XmARROW_LEFT,
    XmARROW_RIGHT
};

/* Shadow/Separator types */

enum {
    XmNO_LINE,
    XmSINGLE_LINE,
    XmDOUBLE_LINE,
    XmSINGLE_DASHED_LINE,
    XmDOUBLE_DASHED_LINE,
    XmSHADOW_ETCHED_IN,
    XmSHADOW_ETCHED_OUT,
    XmSHADOW_ETCHED_IN_DASH,
    XmSHADOW_ETCHED_OUT_DASH,
    XmINVALID_SEPARATOR_TYPE
};

/* XmLabel types */

enum {
    XmPIXMAP = 1,
    XmSTRING
};

/* DragDrop */
enum {
    XmWINDOW,
/* implied
 *  XmPIXMAP
 */
    XmCURSOR = 2
};

/* maximum value resources */

enum {
    XmMAX_ON_TOP,
    XmMAX_ON_BOTTOM,
    XmMAX_ON_LEFT,
    XmMAX_ON_RIGHT
};

/* list selection policies */

enum {
    XmSINGLE_SELECT,
    XmMULTIPLE_SELECT,
    XmEXTENDED_SELECT,
    XmBROWSE_SELECT
};

enum {
    XmSTATIC,
    XmDYNAMIC
};

enum {
    XmNORMAL_MODE,
    XmADD_MODE
};

/* container stuff */

/* for XmRAutomaticSelection */
enum {
    XmNO_AUTO_SELECT,
    XmAUTO_SELECT
};

/* auto_selection_type */
enum {
   XmAUTO_UNSET,
   XmAUTO_BEGIN,
   XmAUTO_MOTION,
   XmAUTO_CANCEL,
   XmAUTO_NO_CHANGE,
   XmAUTO_CHANGE
};

/* for XmRLineStyle */
enum { 
 /* XmNO_LINE */
    XmSINGLE = 1
};

/* for XmREntryViewType */
enum {
 /* XmLARGE_ICON */
 /* XmSMALL_ICON */
    XmANY_ICON = 2
};

/* for XmRSpatialIncludeModel */
enum {
    XmAPPEND,
    XmCLOSEST,
    XmFIRST_FIT
};

/* for XmRLayoutType */
enum {
    XmOUTLINE,
    XmSPATIAL,
    XmDETAIL
};

/* for XmNoutlineButtonPolicy */
enum {
    XmOUTLINE_BUTTON_PRESENT,
    XmOUTLINE_BUTTON_ABSENT
};

/* for XmRSpatialPlaceStyle */
enum {
 /* XmNONE */
    XmGRID = 1,
    XmCELLS
};

/* for XmRPrimaryOwnership */
enum {
    XmOWN_NEVER,
    XmOWN_ALWAYS,
    XmOWN_MULTIPLE,
    XmOWN_POSSIBLE_MULTIPLE
};

/* for XmRSpatialResizeModel */
enum {
    XmGROW_MINOR,
    XmGROW_MAJOR,
    XmGROW_BALANCED
};

/* for XmRSelectionTechnique */
enum {
    XmMARQUEE,
    XmMARQUEE_EXTEND_START,
    XmMARQUEE_EXTEND_BOTH,
    XmTOUCH_ONLY,
    XmTOUCH_OVER
};

/* for XmRSpatialSnapModel */
enum {
 /* XmNONE */
    XmSNAP_TO_GRID = 1,
    XmCENTER
};

/* for XmROutlineState */
enum {
    XmCOLLAPSED,
    XmEXPANDED
};

/* icon gadget stuff */

/* for XmRViewType */
enum {
    XmLARGE_ICON,
    XmSMALL_ICON
};

/* for XmRVisualEmphasis */
enum {
    XmSELECTED,
    XmNOT_SELECTED
};

/* notebook stuff */

#define XmUNSPECIFIED_PAGE_NUMBER       (-32768)

/* for XmRBindingType */
enum {  
 /* XmNONE */
 /* XmPIXMAP */
    XmSOLID = 2,
    XmSPIRAL,
    XmPIXMAP_OVERLAP_ONLY
};

/* for XmRNBChildType */
enum {
 /* XmNONE */
    XmPAGE = 1,
    XmMAJOR_TAB,
    XmMINOR_TAB,
    XmSTATUS_AREA,
    XmPAGE_SCROLLER
};

/* spin box stuff */

/* for XmNarrowLayout */
enum {
    XmARROWS_END,
    XmARROWS_BEGINNING,
    XmARROWS_SPLIT,
    XmARROWS_FLAT_END,
    XmARROWS_FLAT_BEGINNING
};

/* XmSpinBox XmNarrowOrientation */
enum {
	XmARROWS_HORIZONTAL,
	XmARROWS_VERTICAL
};

/* for XmNarrowSensitivity or XmNdefaultArrowSensitivity */
enum {
    XmARROWS_INSENSITIVE,
    XmARROWS_INCREMENT_SENSITIVE,
    XmARROWS_DECREMENT_SENSITIVE,
    XmARROWS_SENSITIVE,
    XmARROWS_DEFAULT_SENSITIVITY
};

/* for XmNspinBoxChildType */
enum {
    XmNUMERIC = 3
};

/* XmNpositionType for XmSpinBox */
enum {
	XmPOSITION_VALUE,
	XmPOSITION_INDEX
};

/* XmSpinBoxValidatePosition() return values */
enum{
   XmVALID_VALUE,
   XmCURRENT_VALUE,
   XmMINIMUM_VALUE,
   XmMAXIMUM_VALUE,
   XmINCREMENT_VALUE
};


/* scrolled window policy stuff */

enum {
    XmVARIABLE,
    XmCONSTANT,
    XmRESIZE_IF_POSSIBLE 
};

enum {
    XmAUTOMATIC,
    XmAPPLICATION_DEFINED
};

enum {
/* implied
 *  XmSTATIC,
 */
    XmAS_NEEDED = 1
};

enum {
    XmBOTTOM_RIGHT,
    XmTOP_RIGHT,
    XmBOTTOM_LEFT,
    XmTOP_LEFT
};

/* main window command window locations */

enum {
    XmCOMMAND_ABOVE_WORKSPACE,
    XmCOMMAND_BELOW_WORKSPACE
};

/* edit modes for text widgets */

enum {
    XmMULTI_LINE_EDIT,
    XmSINGLE_LINE_EDIT
};

/* text directions */

typedef enum {
    XmTEXT_FORWARD,
    XmTEXT_BACKWARD
} XmTextDirection;

typedef long XmTextPosition;
typedef Atom XmTextFormat;

#define XmFMT_8_BIT	((XmTextFormat) XA_STRING)
#define XmFMT_16_BIT	((XmTextFormat) 2)  /* they _mean_ XA_SECONDARY ??? */

/*
 * something about backwards compatibility... besides, xmcd needs these
 */
#define FMT8BIT  XmFMT_8_BIT
#define FMT16BIT XmFMT_16_BIT

#define XmFMT_8_BIT	((XmTextFormat) XA_STRING)
#define XmFMT_16_BIT	((XmTextFormat) 2)  /* they _mean_ XA_SECONDARY ??? */

/* Stuff for Text Widgets */
typedef enum {
    XmSELECT_POSITION,
    XmSELECT_WHITESPACE,
    XmSELECT_WORD,
    XmSELECT_LINE,
    XmSELECT_ALL,
    XmSELECT_PARAGRAPH,
    XmSELECT_OUT_LINE
} XmTextScanType;

/* highlight mode for text and textfield widgets */

typedef enum {
    XmHIGHLIGHT_NORMAL,
    XmHIGHLIGHT_SELECTED,
    XmHIGHLIGHT_SECONDARY_SELECTED
} XmHighlightMode;

typedef struct {
    char *ptr;
    int length;
    XmTextFormat format;
} XmTextBlockRec, *XmTextBlock;

/* keep the members comma separated, as that can be endian dependent */
typedef struct {
    int  reason;   
    XEvent *event;
    Boolean doit;
    XmTextPosition currInsert, newInsert;
    XmTextPosition startPos, endPos;
    XmTextBlock text;
} XmTextVerifyCallbackStruct, *XmTextVerifyPtr;

typedef struct {
    wchar_t *wcsptr;
    int length;
} XmTextBlockRecWcs, *XmTextBlockWcs;

typedef struct {
    int  reason;   
    XEvent *event;
    Boolean doit;
    XmTextPosition currInsert, newInsert;
    XmTextPosition startPos, endPos;
    XmTextBlockWcs text;
} XmTextVerifyCallbackStructWcs, *XmTextVerifyPtrWcs;

#define XmCOPY_FAILED           0
#define XmCOPY_SUCCEEDED        1
#define XmCOPY_TRUNCATED        2

/* dialog child types */

enum {
    XmDIALOG_NONE,
    XmDIALOG_APPLY_BUTTON,
    XmDIALOG_CANCEL_BUTTON,
    XmDIALOG_DEFAULT_BUTTON,
    XmDIALOG_OK_BUTTON,
    XmDIALOG_FILTER_LABEL,
    XmDIALOG_FILTER_TEXT,
    XmDIALOG_HELP_BUTTON,
    XmDIALOG_LIST,
    XmDIALOG_LIST_LABEL,
    XmDIALOG_MESSAGE_LABEL,
    XmDIALOG_SELECTION_LABEL,
    XmDIALOG_SYMBOL_LABEL,
    XmDIALOG_TEXT,
    XmDIALOG_SEPARATOR,
    XmDIALOG_DIR_LIST,
    XmDIALOG_DIR_LIST_LABEL
};

#define XmDIALOG_COMMAND_TEXT    XmDIALOG_TEXT
#define XmDIALOG_FILE_LIST       XmDIALOG_LIST
#define XmDIALOG_FILE_LIST_LABEL XmDIALOG_LIST_LABEL
#define XmDIALOG_HISTORY_LIST    XmDIALOG_LIST
#define XmDIALOG_PROMPT_LABEL    XmDIALOG_SELECTION_LABEL
#define XmDIALOG_VALUE_TEXT      XmDIALOG_TEXT

/* dialog styles */
enum {
    XmDIALOG_MODELESS,
    XmDIALOG_PRIMARY_APPLICATION_MODAL,
    XmDIALOG_FULL_APPLICATION_MODAL,
    XmDIALOG_SYSTEM_MODAL
};

/* this is obsolete.  Use XmDIALOG_PRIMARY_APPLICATION_MODAL */
#define XmDIALOG_APPLICATION_MODAL XmDIALOG_PRIMARY_APPLICATION_MODAL

/* child placements (for selection boxes) */

enum {
    XmPLACE_TOP,
    XmPLACE_ABOVE_SELECTION,
    XmPLACE_BELOW_SELECTION
};

/* file type masks for filesb */
#define XmFILE_DIRECTORY    (1 << 0)
#define XmFILE_REGULAR      (1 << 1)
#define XmFILE_ANY_TYPE     (XmFILE_DIRECTORY | XmFILE_REGULAR)

/* defines for selection dialog type: */

enum {
    XmDIALOG_WORK_AREA,
    XmDIALOG_PROMPT,
    XmDIALOG_SELECTION,
    XmDIALOG_COMMAND,
    XmDIALOG_FILE_SELECTION
};

/* dialog types */
enum {
    XmDIALOG_TEMPLATE,
    XmDIALOG_ERROR,
    XmDIALOG_INFORMATION,
    XmDIALOG_MESSAGE,
    XmDIALOG_QUESTION,
    XmDIALOG_WARNING,
    XmDIALOG_WORKING
};

/* traversal stuff */

typedef enum {
    XmVISIBILITY_UNOBSCURED,
    XmVISIBILITY_PARTIALLY_OBSCURED,
    XmVISIBILITY_FULLY_OBSCURED
} XmVisibility;

typedef enum {
    XmTRAVERSE_CURRENT,
    XmTRAVERSE_NEXT,
    XmTRAVERSE_PREV,
    XmTRAVERSE_HOME,
    XmTRAVERSE_NEXT_TAB_GROUP,
    XmTRAVERSE_PREV_TAB_GROUP,
    XmTRAVERSE_UP,
    XmTRAVERSE_DOWN,
    XmTRAVERSE_LEFT,
    XmTRAVERSE_RIGHT
} XmTraversalDirection;

typedef struct {
    int reason;
    XEvent *event;
    Widget traversal_destination;
    XmTraversalDirection direction;
} XmTraverseObscuredCallbackStruct;

typedef unsigned char XmNavigationType;

/* simple menu stuff */

typedef unsigned char XmButtonType;
typedef XmButtonType * XmButtonTypeTable;
typedef KeySym * XmKeySymTable;
typedef XmStringCharSet * XmStringCharSetTable;

enum {
    XmPUSHBUTTON = 1,
    XmTOGGLEBUTTON,
    XmRADIOBUTTON,
    XmCASCADEBUTTON,
    XmSEPARATOR,
    XmDOUBLE_SEPARATOR,
    XmTITLE
};

#define XmCHECKBUTTON	XmTOGGLEBUTTON

/* Stuff needed by the base class stuff in BaseClass.c */

typedef XtPointer (*XmResourceBaseProc)(Widget w, XtPointer);

typedef struct _XmSecondaryResourceDataRec {
    XmResourceBaseProc base_proc;
    XtPointer client_data;
    String name;
    String res_class;
    XtResourceList resources;
    Cardinal num_resources;
} XmSecondaryResourceDataRec, *XmSecondaryResourceData;

XMLIBEXPORT Cardinal XmGetSecondaryResourceData(WidgetClass wc,
				    XmSecondaryResourceData **resData);

/************************ Manager.c ***************************/
XMLIBEXPORT Widget XmObjectAtPoint(Widget cw, Position x, Position y);

/************************ ImageCache.c ***************************/

XMLIBEXPORT Pixmap XmGetPixmap(Screen *screen, char *image_name,
                   Pixel foreground, Pixel background);
XMLIBEXPORT Pixmap XmGetPixmapByDepth(Screen *screen, char *image_name,
                          Pixel foreground, Pixel background, int depth);
XMLIBEXPORT Boolean XmDestroyPixmap(Screen *screen, Pixmap pixmap);
XMLIBEXPORT Boolean XmInstallImage(XImage *image, char *image_name);
XMLIBEXPORT Boolean XmUninstallImage(XImage *image);
XMLIBEXPORT XtEnum XmGetScaledPixmap(Widget widget, String image_name,
                         Pixel foreground, Pixel background,
                         int depth, double scaling_ratio);

/************************** Manager.c *****************************/

XMLIBEXPORT void XmUpdateDisplay(Widget w);

/************************* Primitive.c ****************************/

typedef long XmOffset;
typedef XmOffset *XmOffsetPtr;

XMLIBEXPORT void XmResolvePartOffsets(WidgetClass w_class, XmOffsetPtr *offset);
XMLIBEXPORT void XmResolveAllPartOffsets(WidgetClass w_class, XmOffsetPtr *offset,
			     XmOffsetPtr *constraint_offset);
XMLIBEXPORT Boolean XmWidgetGetBaselines(Widget wid, Dimension **baselines,
			     int *line_count);
XMLIBEXPORT Boolean XmWidgetGetDisplayRect(Widget wid, XRectangle *displayrect);

/************************** RenderTable.c ****************************/

XMLIBEXPORT XmRenderTable XmRenderTableAddRenditions(XmRenderTable        oldtable,
					 XmRendition    *renditions,
					 Cardinal       rendition_count,
					 XmMergeMode    merge_mode);

XMLIBEXPORT XmRenderTable XmRenderTableCopy(XmRenderTable table,
				XmStringTag     *tags,
				int             tag_count);

XMLIBEXPORT XmRenderTable XmRenderTableCvtFromProp(Widget         widget,
				       char             *property,
				       unsigned int     length);

XMLIBEXPORT unsigned int XmRenderTableCvtToProp(Widget            widget,
				    XmRenderTable       table,
				    char                **prop_return);

XMLIBEXPORT void XmRenderTableFree(XmRenderTable table);

XMLIBEXPORT XmRendition XmRenderTableGetRendition(XmRenderTable   table,
				      XmStringTag       tag);

XMLIBEXPORT XmRendition *XmRenderTableGetRenditions(XmRenderTable table,
					XmStringTag     *tags,
					Cardinal        tag_count);

XMLIBEXPORT int XmRenderTableGetTags(XmRenderTable        table,
			 XmStringTag    **tag_list);

XMLIBEXPORT XmRenderTable XmRenderTableRemoveRenditions(XmRenderTable     oldtable,
					    XmStringTag *tags,
					    int         tag_count);

XMLIBEXPORT Boolean XmeRenderTableGetDefaultFont(XmRenderTable    renderTable,
				     XFontStruct        **fontStruct);

XMLIBEXPORT XmString XmStringGenerate(XtPointer   text,
			  XmStringTag   tag,
			  XmTextType    type,
			  XmStringTag   rendition);
  
XMLIBEXPORT XmRendition XmRenditionCreate(Widget widget,
			      XmStringTag tag,
			      ArgList arglist,
			      Cardinal argcount);
  
XMLIBEXPORT void XmRenditionFree(XmRendition rendition);

XMLIBEXPORT void XmRenditionUpdate(XmRendition rendition, 
		       ArgList arglist, 
		       Cardinal argcount);

XMLIBEXPORT void XmRenditionRetrieve (XmRendition rendition,
                          ArgList arglist,
			  Cardinal argcount);

/************************ ResConvert.c ****************************/

XMLIBEXPORT void XmRegisterConverters(void);
XMLIBEXPORT void XmCvtStringToUnitType(XrmValuePtr args, Cardinal *num_args, XrmValue *from_val, XrmValue *to_val);
XMLIBEXPORT char *XmRegisterSegmentEncoding(char *fontlist_tag, char *ct_encoding);
XMLIBEXPORT char *XmMapSegmentEncoding(char *fontlist_tag);
XMLIBEXPORT XmString XmCvtCTToXmString(char *text);
XMLIBEXPORT Boolean XmCvtTextToXmString(Display *display, XrmValuePtr args,
			    Cardinal *num_args, XrmValue *from_val,
			    XrmValue *to_val, XtPointer *converter_data);
XMLIBEXPORT char *XmCvtXmStringToCT(XmString string);
XMLIBEXPORT Boolean XmCvtXmStringToText(Display *display, XrmValuePtr args,
			    Cardinal *num_args, XrmValue *from_val,
			    XrmValue *to_val, XtPointer *converter_data);
XMLIBEXPORT int XmConvertStringToUnits(Screen *screen, String spec, int orientation,
	int to_type, XtEnum *parse_error);
XMLIBEXPORT unsigned int XmCvtXmStringToByteStream(XmString string,
                                       unsigned char **prop_return);
XMLIBEXPORT XmString XmCvtByteStreamToXmString(unsigned char *property);
XMLIBEXPORT int XmCvtXmStringTableToTextProperty (Display *display,
                                      XmStringTable string_table,
                                      int count,
                                      XmICCEncodingStyle style,
                                      XTextProperty *text_prop_return);

/************************** ResInd.c ******************************/

XMLIBEXPORT int XmConvertUnits(Widget widget, int orientation,
		   int from_unit_type, int from_value, int to_unit_type);
XMLIBEXPORT int  XmCvtToHorizontalPixels(Screen *screen, int from_val, int from_type);
XMLIBEXPORT int  XmCvtToVerticalPixels(Screen *screen, int from_val, int from_type);
XMLIBEXPORT int  XmCvtFromHorizontalPixels(Screen *screen, int from_val, int to_type);
XMLIBEXPORT int  XmCvtFromVerticalPixels(Screen *screen, int from_val, int to_type);
XMLIBEXPORT void XmSetFontUnits(Display *display, int h_value, int v_value);
XMLIBEXPORT void XmSetFontUnit(Display *display, int value);

/************************* MenuUtil.c *****************************/

XMLIBEXPORT void XmSetMenuCursor(Display *display, Cursor cursorId);
XMLIBEXPORT Cursor XmGetMenuCursor(Display *display);

/************************** Simple.c ******************************/

XMLIBEXPORT Widget XmCreateSimpleCheckBox(Widget parent, char *name,
			      Arg *arglist, Cardinal argcount);
XMLIBEXPORT Widget XmCreateSimpleMenuBar(Widget parent, char *name,
			     Arg *arglist, Cardinal argcount);
XMLIBEXPORT Widget XmCreateSimpleOptionMenu(Widget parent, char *name,
				Arg *arglist, Cardinal argcount);
XMLIBEXPORT Widget XmCreateSimplePopupMenu(Widget parent, char *name,
			       Arg *arglist, Cardinal argcount);
XMLIBEXPORT Widget XmCreateSimplePulldownMenu(Widget parent, char *name,
				  Arg *arglist, Cardinal argcount);
XMLIBEXPORT Widget XmCreateSimpleRadioBox(Widget parent, char *name,
			      Arg *arglist, Cardinal argcount);

/************************* VaSimple.c *****************************/

XMLIBEXPORT Widget XmVaCreateSimpleCheckBox(Widget parent, String name,
				XtCallbackProc callback, ...);
XMLIBEXPORT Widget XmVaCreateSimpleMenuBar(Widget parent, String name,
			       ...);
XMLIBEXPORT Widget XmVaCreateSimpleOptionMenu(Widget parent, String name,
				  XmString option_label,
				  KeySym option_mnemonic,
				  int button_set,
				  XtCallbackProc callback, ...);
XMLIBEXPORT Widget XmVaCreateSimplePopupMenu(Widget parent, String name,
				 XtCallbackProc callback, ...);
XMLIBEXPORT Widget XmVaCreateSimplePulldownMenu(Widget parent, String name,
				    int post_from_button,
				    XtCallbackProc callback, ...);
XMLIBEXPORT Widget XmVaCreateSimpleRadioBox(Widget parent, String name,
				int button_set,
				XtCallbackProc callback, ...);

/************************** TrackLoc.c *****************************/

XMLIBEXPORT Widget XmTrackingEvent(Widget widget, Cursor cursor, Boolean confine_to,
		       XEvent *event_return);
XMLIBEXPORT Widget XmTrackingLocate(Widget widget, Cursor cursor, Boolean confine_to);

/**************************** Visual.c *****************************/

typedef void (*XmColorProc)(XColor *bg_color, XColor *fg_color,
			    XColor *sel_color, XColor *ts_color, XColor *bs_color);


XMLIBEXPORT XmColorProc XmSetColorCalculation(XmColorProc proc);
XMLIBEXPORT XmColorProc XmGetColorCalculation(void);
XMLIBEXPORT void XmGetColors(Screen *screen, Colormap color_map,
			Pixel background, Pixel *foreground_ret,
			Pixel *top_shadow_ret, Pixel *bottom_shadow_ret,
			Pixel *select_ret);
XMLIBEXPORT void XmChangeColor(Widget widget, Pixel background);

/*************************** XmString.c ****************************/

XMLIBEXPORT Dimension XmStringBaseline(XmFontList fontlist, XmString string);
XMLIBEXPORT Boolean XmStringByteCompare(XmString s1, XmString s2);
XMLIBEXPORT Boolean XmStringCompare(XmString s1, XmString s2);
XMLIBEXPORT XmString XmStringConcat(XmString s1, XmString s2);
XMLIBEXPORT XmString XmStringConcatAndFree(XmString s1, XmString s2);
XMLIBEXPORT XmString XmStringCreate(char *text, char *tag);
XMLIBEXPORT XmString XmStringCreateLtoR(char *text, char *tag);
XMLIBEXPORT XmString XmStringLtoRCreate(char *text, char *tag);
XMLIBEXPORT XmString XmStringCreateLocalized(char *text);
XMLIBEXPORT XmString XmStringCreateSimple(char *text);
XMLIBEXPORT XmString XmStringDirectionCreate(XmStringDirection direction);
XMLIBEXPORT void XmStringDraw(Display *d, 
		  Window w,
		  XmFontList fontlist,
		  XmString string,
		  GC gc,
		  Position x,
		  Position y,
		  Dimension width,
		  unsigned char alignment,
		  unsigned char layout_direction,
		  XRectangle *clip);
XMLIBEXPORT void XmStringDrawImage(Display *d, Window w,
		       XmFontList fontlist,
		       XmString string,
		       GC gc,
		       Position x,
		       Position y,
		       Dimension width,
		       unsigned char alignment,
		       unsigned char layout_direction,
		       XRectangle *clip);
XMLIBEXPORT void XmStringDrawUnderline(Display *d, Window w,
			   XmFontList fontlist, XmString string,
			   GC gc, Position x, Position y, Dimension width,
			   unsigned char alignment,
			   unsigned char layout_direction,
			   XRectangle *clip,
			   XmString underline);
XMLIBEXPORT Boolean XmStringEmpty(XmString s1);
XMLIBEXPORT void XmStringExtent(XmFontList fontlist, 
		    XmString string,
		    Dimension *width,
		    Dimension *height);
XMLIBEXPORT void XmStringFree(XmString string);
XMLIBEXPORT void XmStringFreeContext(XmStringContext context);
XMLIBEXPORT Boolean XmStringGetLtoR(XmString string,
			XmStringCharSet tag,
			char **text);
XMLIBEXPORT XmStringComponentType XmStringGetNextComponent(XmStringContext context,
					       char **text,
					       XmStringCharSet *tag,
					       XmStringDirection *direction,
					       XmStringComponentType *unknown_tag,
					       unsigned short *unknown_length,
					       unsigned char **unknown_value);
XMLIBEXPORT Boolean XmStringGetNextSegment(XmStringContext context,
			       char **text,
			       XmStringCharSet *tag,
			       XmStringDirection *direction,
			       Boolean *separator);
XMLIBEXPORT Boolean XmStringHasSubstring(XmString string,
			     XmString substring);
XMLIBEXPORT Dimension XmStringHeight(XmFontList fontlist, XmString string);
XMLIBEXPORT Boolean XmStringInitContext(XmStringContext *context,
			    XmString string);
XMLIBEXPORT int XmStringLength(XmString s1);
XMLIBEXPORT int XmStringLineCount(XmString string);
XMLIBEXPORT XmString XmStringNConcat(XmString s1, XmString s2, int num_bytes);
XMLIBEXPORT XmString XmStringCopy(XmString s);
XMLIBEXPORT XmString XmStringNCopy(XmString s1, int num_bytes);
XMLIBEXPORT XmStringComponentType XmStringPeekNextComponent(XmStringContext context);
XMLIBEXPORT XmString XmStringSegmentCreate(char *text, char *tag, 
			       XmStringDirection direction,
			       Boolean separator);
XMLIBEXPORT XmString XmStringSeparatorCreate(void);

XMLIBEXPORT Dimension XmStringWidth(XmFontList fontList, XmString string);

XMLIBEXPORT XtPointer XmStringUnparse(XmString string,
                          XmStringTag tag,
                          XmTextType tag_type,
                          XmTextType output_type,
                          XmParseTable parse_table,
                          Cardinal parse_count,
                          XmParseModel parse_model);


XMLIBEXPORT Cardinal XmStringToXmStringTable(XmString string,
                                 XmString break_comp,
                                 XmStringTable *table);

XMLIBEXPORT XmString XmStringTableToXmString(XmStringTable table,
                                 Cardinal count,
                                 XmString break_component);

XMLIBEXPORT XtPointer *XmStringTableUnparse(XmStringTable table,
                                Cardinal count,
                                XmStringTag tag,
                                XmTextType tag_type,
                                XmTextType output_type,
                                XmParseTable parse,
                                Cardinal parse_count,
                                XmParseModel parse_model);

XMLIBEXPORT XmStringTable XmStringTableParseStringArray(XtPointer *strings,
                                            Cardinal count,
                                            XmStringTag tag,
                                            XmTextType type,
                                            XmParseTable parse,
                                            Cardinal parse_count,
                                            XtPointer call_data);

XMLIBEXPORT XmString XmStringPutRendition(XmString string,
                              XmStringTag rendition);

XMLIBEXPORT XmParseMapping XmParseMappingCreate(ArgList arg_list,
                                    Cardinal arg_count);

XMLIBEXPORT void XmParseMappingSetValues(XmParseMapping parse_mapping,
                             ArgList arg_list,
                             Cardinal arg_count);

XMLIBEXPORT void XmParseMappingGetValues(XmParseMapping parse_mapping,
                             ArgList arg_list,
                             Cardinal arg_count);

XMLIBEXPORT void XmParseMappingFree(XmParseMapping parse_mapping);

XMLIBEXPORT void XmParseTableFree(XmParseTable parse_table,
                      Cardinal parse_count);

XMLIBEXPORT XmStringComponentType XmStringGetNextTriple(XmStringContext context,
                                            unsigned int *length,
                                            XtPointer *value);

XMLIBEXPORT XmStringComponentType XmStringPeekNextTriple(XmStringContext context);

XMLIBEXPORT XmString XmStringComponentCreate(XmStringComponentType ctype,
                                 unsigned int length,
                                 XtPointer value);

XMLIBEXPORT Boolean XmStringIsVoid(XmString s1);

XMLIBEXPORT XmString XmStringParseText(XtPointer text, XtPointer *text_end, XmStringTag tag,
	XmTextType type, XmParseTable parse_table, Cardinal parse_count,
	XtPointer call_data);

/*************************** FontList.c *****************************/

XMLIBEXPORT XmFontList XmFontListAppendEntry(XmFontList oldlist,
				 XmFontListEntry entry);
XMLIBEXPORT XmFontList XmFontListCreate(XFontStruct *font,
			    XmStringCharSet charset);
XMLIBEXPORT XmFontList XmFontListAdd(XmFontList old,
			 XFontStruct *font,
			 XmStringCharSet charset);
XMLIBEXPORT XmFontList XmFontListCopy(XmFontList fontlist);
XMLIBEXPORT XmFontListEntry XmFontListEntryCreate(char *tag,
				      XmFontType type,
				      XtPointer font);
XMLIBEXPORT void XmFontListEntryFree(XmFontListEntry *entry);
XMLIBEXPORT XtPointer XmFontListEntryGetFont(XmFontListEntry entry,
				 XmFontType *type_return);
XMLIBEXPORT char *XmFontListEntryGetTag(XmFontListEntry entry);
XMLIBEXPORT XmFontListEntry XmFontListEntryLoad(Display *display,
				    char *font_name,
				    XmFontType type,
				    char *tag);
XMLIBEXPORT void XmFontListFree(XmFontList list);
XMLIBEXPORT void XmFontListFreeFontContext(XmFontContext context);
XMLIBEXPORT Boolean XmFontListInitFontContext(XmFontContext *context,
				  XmFontList fontlist);
XMLIBEXPORT XmFontListEntry XmFontListNextEntry(XmFontContext context);
XMLIBEXPORT Boolean XmFontListGetNextFont(XmFontContext context, XmStringCharSet *charset, XFontStruct **font);
XMLIBEXPORT XmFontList XmFontListRemoveEntry(XmFontList oldlist,
				 XmFontListEntry entry);

XMLIBEXPORT XmFontList XmStringCreateFontList( XFontStruct *font, XmStringCharSet charset);

/************************** Dest.c ***********************************/

XMLIBEXPORT Widget XmGetDestination(Display *display);

/*************************** Traversal.c *****************************/

XMLIBEXPORT Boolean XmIsTraversable(Widget widget);
XMLIBEXPORT XmVisibility XmGetVisibility(Widget widget);
XMLIBEXPORT void XmAddTabGroup(Widget widget);
XMLIBEXPORT void XmRemoveTabGroup(Widget widget);
XMLIBEXPORT Widget XmGetTabGroup(Widget widget);
XMLIBEXPORT Boolean XmProcessTraversal(Widget widget, XmTraversalDirection direction);
XMLIBEXPORT Widget XmGetFocusWidget(Widget widget);

/**************************** Direction.c *********************************/

XMLIBEXPORT Boolean XmDirectionMatch(XmDirection dir1, XmDirection dir2);
XMLIBEXPORT Boolean XmDirectionMatchPartial(XmDirection dir1, XmDirection dir2, XmDirection dir_mask);
XMLIBEXPORT XmStringDirection XmDirectionToStringDirection(XmDirection dir);
XMLIBEXPORT XmDirection XmStringDirectionToDirection(XmStringDirection sdir);

/******************************* Xme.c ************************************/

XMLIBEXPORT void XmeConvertMerge(XtPointer data, Atom type,
                     int format, unsigned long length,
                     XmConvertCallbackStruct *call_data);

#ifdef __cplusplus
}
#endif


#include <Xm/VendorS.h>
#include <Xm/XmIm.h>


#ifdef __cplusplus
extern "C" {
#endif

/* The following is for backwards compability (bc) */
#define XmINDICATOR_3D_BOX 0x01
#define XmINDICATOR_FLAT_BOX 0x02
#define XmINDICATOR_CHECK_GLYPH 0x10
#define XmINDICATOR_CROSS_GLYPH 0x20

enum {
    XmINDICATOR_NONE = 0,
    XmINDICATOR_FILL = XmINDICATOR_3D_BOX,
    XmINDICATOR_CHECK = XmINDICATOR_CHECK_GLYPH,
    XmINDICATOR_CHECK_BOX = XmINDICATOR_CHECK_GLYPH + XmINDICATOR_3D_BOX,
    XmINDICATOR_CROSS = XmINDICATOR_CROSS_GLYPH,
    XmINDICATOR_CROSS_BOX = XmINDICATOR_CROSS_GLYPH + XmINDICATOR_3D_BOX
};

/* A slider can either be a slider or a thermometer */
enum { XmSLIDER, XmTHERMOMETER };

#define XmTextSetTopPosition    XmTextSetTopCharacter 

enum {
	XmPOPUP_DISABLED = 0,
	XmPOPUP_KEYBOARD,
	XmPOPUP_AUTOMATIC,
	XmPOPUP_AUTOMATIC_RECURSIVE
};

/* sliderVisual */
enum {
	XmBACKGROUND_COLOR,
	XmFOREGROUND_COLOR,
	XmTROUGH_COLOR,
	XmSHADOWED_BACKGROUND,
	XmFLAT_FOREGROUND
};

/* showValue */
enum {
	XmNEAR_SLIDER = 1,
        XmNEAR_BORDER
};

/* sliderMark */
enum {
	/* XmNONE */
	XmETCHED_LINE = 1,
	XmTHUMB_MARK,
	XmROUND_MARK
};

/* showArrows */
enum {
	/* XmNONE */
	XmEACH_SIDE = 1,
	XmMAX_SIDE,
	XmMIN_SIDE
};

/* XmString */
XmString XmStringGenerate(XtPointer   text, XmStringTag tag,
                          XmTextType  type, XmStringTag rendition);

/* XmFileSelectionBox */
enum {
	XmPATH_MODE_FULL,
	XmPATH_MODE_RELATIVE
};

enum {
	XmFILTER_NONE,
	XmFILTER_HIDDEN_FILES
};

/* RowColumn */
#define XmInheritMenuTraversalProc ((XmMenuTraversalProc) _XtInherit)

/* XmTabList */
XmTabList XmTabListCopy(XmTabList tablist,
                        int offset,
                        Cardinal count);

void XmTabListFree(XmTabList tablist);

XmTab XmTabListGetTab(XmTabList tablist,
                      Cardinal position);

XmTabList XmTabListInsertTabs(XmTabList oldlist,
                              XmTab *tabs,
                              Cardinal tab_count,
                              int position);

XmTabList XmTabListRemoveTabs(XmTabList oldlist,
                              Cardinal *position_list,
                              Cardinal position_count);

XmTabList XmTabListReplacePositions(XmTabList oldlist,
                                    Cardinal *position_list,
                                    XmTab *tabs,
                                    Cardinal tab_count);

Cardinal XmTabListTabCount(XmTabList tablist);

void XmTabSetValue(XmTab tab,
                   float value);

XmTab XmTabCreate(float value,
                  unsigned char units,
                  XmOffsetModel offset_model,
                  unsigned char alignment,
                  char *decimal);

void XmTabFree(XmTab tab);

float XmTabGetValues(XmTab tab,
                     unsigned char *units,
                     XmOffsetModel *offset,
                     unsigned char *alignment,
                     char **decimal);

XmTabList XmStringTableProposeTablist(XmStringTable strings,
                                      Cardinal num_strings,
                                      Widget widget,
                                      float pad_value,
                                      XmOffsetModel offset_model);

/*
 * XmPrint API
 */
enum {
        XmPDM_NOTIFY_FAIL,
        XmPDM_NOTIFY_SUCCESS
};

void XmRedisplayWidget(Widget w);

#ifdef __cplusplus
}
#endif

#endif /* _XM_XM_H */
