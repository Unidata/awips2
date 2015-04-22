
/************************************************************************
 * Nxmlib.h                                                             *
 *                                                                      *
 * Header file inside the nxmlib library.                        	*
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * C. Lin/EAI        1/95  add variable NXManimationStatus              *
 *                         add NxmQueryAnimationStatus()                *
 *                             NxmStopAnimation(),NxmRestartAnimation() *
 * S. Wang/GSC      05/96  add structures bxmInfo and pxmBuf            *
 * S. Wang/GSC      05/97  add NxmBxmBtn_*'s				*
 * S. Wang/GSC      08/97  add NxmLineA() and NxmColrP()		*
 * G. Krueger/EAI    9/97  Remove NxmExitDialog				*
 * S. Wang/GSC      09/97  add NxmMarkA()				*
 * G. Krueger/EAI   10/97  NxmControlBtn->NxmCtlBtn_create 		*
 ***********************************************************************/

#ifndef Nxmlib_HH
#define Nxmlib_HH

#include <stdio.h>
#include <Xm/Xm.h>
#include <X11/Xatom.h>

#define NXMSUCCESS 0
#define MAX_COLOR  33


enum NXManimationStatus_t {  NXM_NOLOOP, 
        NXM_LOOPFRWD,
	NXM_LOOPBACK,
	NXM_LOOPFRWDBKWD
	};

typedef struct {
        int total;  /* total number of pixmaps in the loop */
        int current;/* index of current pixmap in the loop */
        } _NXMpixmapData;

typedef struct {
        Boolean loopfrwd;
        Boolean loopback;
/*  NEW  */
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
  	char                 *label;        /* the label for the item */
  	WidgetClass          *class;        /* pushbutton, label, separator... */
  	char                 *mnemonic;     /* mnemonic */
  	char                 *accelerator;  /* accelerator  */
  	char                 *accel_text;   /* string */
  	void                 (*callback)(); /* callback function */
  	int                  which_widget;  /* the data passed into the callback */
  	struct _NXMmenuItem_ *subitems;     /* submenus */
  	WidgetList    	     sub_buttons;
        } _NXMmenuItem;


struct  bxmInfo {
	char    fgcolor[30];		/* foreground color name 	*/
	char    bgcolor[30];		/* background color name	*/
	char    *sens_bits;	
	char    *insens_bits;
};

struct pxmBuf {
	Pixmap  snstv;
	Pixmap  insnstv;
};

typedef struct {
        Widget  colrFrame[MAX_COLOR];
        int     selectedFrame;
} NxmColrP_t;                 


extern Widget NxmCtlBtn_create();
extern Widget NxmBxmBtn_create();
extern Widget NxmBxmBtn_createMulti();
extern void   NxmBxmBtn_setLabel();
extern Widget NxmLineA_create();
extern void   NxmLineA_popUp();
extern int    NxmColrP_getColorPixel();
extern void   NxmColrP_setColor();
extern void   NxmColrP_deselectAll();
extern NxmColrP_t *NxmColrP_create();
extern Widget NxmMarkA_create();
extern void   NxmMarkA_popup();

#endif

