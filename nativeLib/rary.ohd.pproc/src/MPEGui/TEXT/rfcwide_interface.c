/*=========================================================================*/
/*                         FILE NAME:   rfcwide_interface.c                */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   create_rfcwide_interface()         */
/*                                      create_rfcwide_legend()            */
/*                                      create_time_lapse_popup_RFCW       */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <X11/cursorfont.h>
#include <Xm/Protocols.h>
#include <Xm/CascadeB.h>

#include "add_pseudo_RFCW.h"
#include "display_bias_table.h"
#include "display_field_data_RFCW.h"
#include "display_precip_data.h"
#include "drawa.h"
#include "draw_precip_poly_RFCW.h"
#include "libXs.h"
#include "newhour_RFCW.h"
#include "overlay.h"
#include "post_functions.h"
#include "quit_rfcwide.h"
#include "rerun_rfcwgen.h"
#include "rfcwide.h"
#include "rfcwide_callbacks.h"
#include "rfcwide_interface.h"
#include "save_rfcwide.h"
#include "select_site_rfcwide.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "time_lapse_RFCW.h"
#include "TSutils.h"
#include "xs_create_menu_buttons.h"

extern time_lapse_struct tldata;

/***************************************************************************/
/*  FUNCTION NAME:   create_time_lapse_popup_RFCW()                        */
/*       FUNCTION:   popup time lapse slider bar                           */
/***************************************************************************

Function type:

Called by function:

Functions called:
   (callback) read_tl_scale
   (callback) popdown_shell
   (callback) time_lapse

*********************************** BEGIN create_time_lapse_popup_RFCW *******/

void create_time_lapse_popup_RFCW(widgetStruct)
   rfcwide_widget_struct *widgetStruct;
{
   Widget               shell, form, scale, bb, scale_bb, separator;
   Widget               ok_button, cancel_button;
   //Widget help_button;
   Arg                  wargs[10];
   int                  n, background_color;
   XmString             xstr ;

 shell = XtCreatePopupShell("Time Lapse Interval", transientShellWidgetClass, 
 	toplevel, NULL, 0);
 widgetStruct->time_lapse_shell = shell;

 form = XtCreateManagedWidget("time_lapse_form", xmFormWidgetClass, shell, 
 			NULL, 0);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 scale_bb = XtCreateManagedWidget("time_lapse_scale_bb", 
 			xmBulletinBoardWidgetClass, form, wargs, n);

 n = 0;
 xstr = XmStringCreate( "Hours before current time." ,  
                        XmSTRING_DEFAULT_CHARSET) ; 
 XtSetArg(wargs[n], XmNtitleString, xstr ) ; n++;
 XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
 XtSetArg(wargs[n], XmNshowValue, TRUE); n++;
 XtSetArg(wargs[n], XmNminimum, 1); n++;
 XtSetArg(wargs[n], XmNmaximum, 24); n++;
 XtSetArg(wargs[n], XmNvalue, 1); n++;
 scale = XtCreateManagedWidget("time_lapse_scale", xmScaleWidgetClass, 
 			scale_bb, wargs, n);
 XmStringFree ( xstr ) ;

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, scale_bb); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 separator = XtCreateManagedWidget("time_lapse_separator", 
 			xmSeparatorWidgetClass, form, wargs, n);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, separator); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM); n++;
 bb = XtCreateManagedWidget("time_lapse_bb", xmBulletinBoardWidgetClass, 
 			form, wargs, n);

 /* -------------------------------------------------------------------- */
 /* Get the bulletin board background color...                           */
 /* -------------------------------------------------------------------- */

 XtSetArg(wargs[0], XtNbackground, &background_color);
 XtGetValues(bb, wargs, 1);

 n = 0;
 xstr = XmStringCreate ( "OK" , XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg ( wargs[n] , XmNlabelString , xstr ) ; n++ ;
 XtSetArg(wargs[n], XmNshowAsDefault, 1); n++;
 XtSetArg(wargs[n], XtNwidth, 80); n++;
 ok_button = XtCreateManagedWidget("time_lapse_ok", xmPushButtonWidgetClass, 
 			bb, wargs, n);
 XmStringFree ( xstr ) ;
 XtAddCallback(ok_button, XmNactivateCallback, read_tl_scale, scale);
 XtAddCallback(ok_button, XmNactivateCallback, popdown_shell, shell);
 XtAddCallback(ok_button, XmNactivateCallback, time_lapse_RFCW, &tldata);
 XtSetKeyboardFocus(form, ok_button);

 n = 0;
 xstr = XmStringCreate ( "Cancel", XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg(wargs[n], XmNlabelString , xstr ) ; n++ ;
 XtSetArg(wargs[n], XtNwidth, 80); n++;
 XtSetArg(wargs[n], XtNborderWidth, 4); n++;
 XtSetArg(wargs[n], XtNborderColor, background_color); n++;
 cancel_button = XtCreateManagedWidget("time_lapse_cancel", 
 			xmPushButtonWidgetClass, bb, wargs, n);
 XmStringFree ( xstr ) ;
 XtAddCallback(cancel_button, XmNactivateCallback, popdown_shell, shell);

 n = 0;
/* xstr = XmStringCreate ( "Help" , XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg(wargs[n], XmNlabelString, xstr ) ; n++;
 XtSetArg(wargs[n], XtNwidth, 80); n++;
 XtSetArg(wargs[n], XtNborderWidth, 4); n++;
 XtSetArg(wargs[n], XtNborderColor, background_color); n++;
 help_button = XtCreateManagedWidget("time_lapse_help", 
 			xmPushButtonWidgetClass, bb, wargs, n);
 XmStringFree ( xstr ) ;
*/
// XtAddCallback(help_button, XmNactivateCallback,
//	   popup_help_window,  "TIMELAPSE");


}

/************************************** END create_time_lapse_popup_RFCW ******/

