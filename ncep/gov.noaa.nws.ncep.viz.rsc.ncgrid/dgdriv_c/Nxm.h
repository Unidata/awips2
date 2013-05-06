/************************************************************************
 * Nxm.h                                              			*
 *                                                                      *
 * Header file for the application using nxmlib.    			*
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		05/94						*
 * C. Lin/EAI		01/95	update the changes			*
 *				add variable NXManimationStatus		*
 *				add NxmQueryAnimationStatus()		*
 *				NxmStopAnimation(),NxmRestartAnimation()*
 * S. Wang/GSC		05/97	add NxmBxmBtn_*'s			*
 * G. Krueger/EAI	08/97	removed UTIL.C's NfileFullName()	*
 * S. Wang/GSC		09/97	add NxmColrP() func. declarations and	*
 *				_NXMattr, NxmColrP_t structures         *
 * G. Krueger/EAI	09/97	NxmExitDialog->NxmExit_create		*
 * G. Krueger/EAI	09/97	Changed NxmWarning -> NxmWarn_show	*
 * G. Krueger/EAI	09/97	Changed _NxmClosePopupCallback 		*
 *				-> NxmClose_popupCb			*
 * S. Wang/GSC		09/97	add NxmMarkA()				*
 * G. Krueger/EAI	10/97	NxmControlBtn->NxmCtlBtn_create 	*
 * G. Krueger/EAI	11/97	NxmPromptPopupCreate->NxmPrompt_create	*
 * G. Krueger/EAI   	11/97	Renamed NxmHelp functions		*
 * W. Li/EAI		12/97	Changed on_off to state in NxmMarkA_t.	*
 * W. Li/EAI		06/98	Added text size in NxmMarkA_t.		*
 * C. Lin/EAI		08/98	Change txt_size in NxmMarkA_t to float	*
 * W. Li/EAI		01/99	Added BOX, SIZE, FONT, ........., ROTN	*
 * W. Li/EAI		01/99	Moved BOX, ... , ROTN to pgtxt & pgedit *
 * T. Piper/SAIC	10/01	Removed #include <X11/Xatom.h>		*
 * T. Piper/SAIC	01/04	Added CLR_DIR, CLR_TBL			*
 * T. Piper/SAIC	01/04	Removed MAX_COLOR			*
 * T. Piper/SAIC	07/04	Moved CLR_DIR and CLR_TBL to color.h	*
 * H. Zeng/SAIC		08/04	Added NxmScaleA_t			*
 * T. Piper/SAIC	03/06	Changed which_widget to long for 64-bit	*
 * T. Piper/SAIC	03/06	Changed mnemonic from char to KeySym	*
 ***********************************************************************/

#ifndef NXM_HH
#define NXM_HH

#include "color.h"

#ifndef NXMSUCCESS
#define NXMSUCCESS 0
#endif  /* NXMSUCCESS */


typedef struct {
	int total;  /* total number of pixmaps in the loop */
	int current;/* index of current pixmap in the loop */
} _NXMpixmapData;

typedef struct {
	Boolean loopfrwd;
	Boolean loopback;
	Boolean loopfrwdbkwd;
} _NXManimationFlags;

typedef struct {
	unsigned long first;
	unsigned long loop;
	unsigned long last;
	float         max_dwell;
} _NXManimationDwell;

typedef struct {
	unsigned long bgColor;
	unsigned long armColor;
	unsigned long topShadowColor;
	unsigned long bottomShadowColor;
} _NXMbuttonColor;

typedef struct _NXMmenuItem_{
	char			*label;		/* the label for the item */
	WidgetClass		*class;		/* pushbutton, label, separator... */
	KeySym			mnemonic;	/* mnemonic */
	char			*accelerator;	/* accelerator */
	char			*accel_text;	/* string */
	XtCallbackProc		callback;	/* callback function */
	long			which_widget;	/* the data passed into the callback */
	struct _NXMmenuItem_	*subitems;	/* submenus */
	WidgetList		sub_buttons;
} _NXMmenuItem;

enum NXManimationStatus_t {  NXM_NOLOOP,
        NXM_LOOPFRWD,
        NXM_LOOPBACK,
        NXM_LOOPFRWDBKWD
};

struct  bxmInfo {
	char    fgcolor[30];            /* foreground color name        */
	char    bgcolor[30];            /* background color name        */
	char    *sens_bits;
	char    *insens_bits;
};

struct pxmBuf {
	Pixmap  snstv;
	Pixmap  insnstv;
};

typedef struct {
        int     color;
        int     style;
        int     width;
        }       _NXMattr;       /* data structure of attribute           */

typedef struct {
        Widget  colrFrame[GRAPH_COLORS];
        int     selectedFrame;
} NxmColrP_t;      /* data structure of color editing module */

typedef struct {
    char    ext_name[20];    /* external cursor name */
    char    int_name[25];    /* internal cursor name */
    int     id;              /* standard cursor symbol id */
}cursortyp_t;     /* data structure for cursor type*/

typedef struct {
    int           ntyp;      /* total # of cursor types */
    cursortyp_t*  curtyps;   /* array of cursor type names */
}curtypTbl_t;  /* data structure for cursor type table    */

typedef struct {
    char    ref_name[20];    /* cursor referenc name  */
    char    typ_name[20];    /* selected cursor type  */
    char    color[10];       /* selected cursor color */
}cursorref_t;     /* data structure for cursor reference*/

typedef struct {
    int           nref;      /* total # of cursor references */
    cursorref_t*  currefs;   /* array of cursor references */
}currefTbl_t;  /* data structure for cursor reference table  */

typedef struct {
        int     color;
        int     style;
        int     width;
} NxmLineA_t;       /* line attribute structure  */

typedef struct {
	int     state;
	int	type_id;
	float	size;
	int	width;
	int     color;
	float	txt_size;
} NxmMarkA_t;		/* marker attribute structure  */

typedef struct {
	int	color;
	int	unit;
	int	lat_opt;
	float	lat;
	int     val_opt;
	char	value_txt[64];
	int     pos;
	int     font;
	float	size;
	int	style;
} NxmScaleA_t;		/* scale attribute structure  */


#include "proto_nxmlib.h"

#endif /* NXM_HH */
