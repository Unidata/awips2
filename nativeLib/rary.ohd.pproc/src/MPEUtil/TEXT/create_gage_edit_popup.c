#include <stdlib.h>
#include "stage3_interface.h"
#include "stage3.h"
#include "drawa.h"
#include "stage3_globals.h"


/***************************************************************************/
/*  FUNCTION NAME:   create_gage_edit_popup                                */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:

Called by function:

Functions called:

Local variables:

******************************************** BEGIN create_gage_edit_popup***/

void create_gage_edit_popup( w,gagenum,call_data)
   Widget w;
   int gagenum;
   caddr_t *call_data;
{
   Widget               shell, form, scale, bb, scale_bb, separator;
   Widget               ok_button, cancel_button, missing_button;
   extern Widget        toplevel ;
   Arg                  wargs[12];
   int                  n, background_color;
   gage_edit_struct     *edit_struct;
   XmString		xm_str;

 edit_struct = (gage_edit_struct *)malloc(sizeof(gage_edit_struct));

 shell = XtCreatePopupShell("gage_edit_shell", transientShellWidgetClass,
		toplevel, NULL, 0);

 form = XtCreateManagedWidget("gage_edit_form", xmFormWidgetClass, shell,
		NULL, 0);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 scale_bb = XtCreateManagedWidget("gage_edit_bb",
		  xmBulletinBoardWidgetClass, form, wargs, n);

 n = 0;
 xm_str = XmStringCreate("New Accumulation (inches)", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNtitleString, xm_str); n++;
 XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
 XtSetArg(wargs[n], XmNshowValue, TRUE); n++;
 XtSetArg(wargs[n], XmNminimum, 0); n++;
 XtSetArg(wargs[n], XmNmaximum, 500); n++;
 XtSetArg(wargs[n], XmNprocessingDirection,XmMAX_ON_RIGHT); n++;
 XtSetArg(wargs[n], XmNdecimalPoints, (short)2); n++;
 XtSetArg(wargs[n], XmNvalue,0); n++;
 scale = XtCreateManagedWidget("gage_edit_scale", xmScaleWidgetClass, scale_bb, wargs, n);
 edit_struct->w = scale;
 edit_struct->num = gagenum;
 XmStringFree(xm_str);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, scale_bb); n++;
 XtSetArg(wargs[n], XmNy, 65); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 separator = XtCreateManagedWidget("gage_edit_separator", xmSeparatorWidgetClass, form, wargs, n);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, separator); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM); n++;
 bb = XtCreateManagedWidget("gage_edit_bb", xmBulletinBoardWidgetClass, form, wargs, n);

 /* -------------------------------------------------------------------- */
 /* Get the bulletin board background color...                           */
 /* -------------------------------------------------------------------- */

 XtSetArg(wargs[0], XtNbackground, &background_color);
 XtGetValues(bb, wargs, 1);

 n = 0;
 xm_str = XmStringCreate("OK", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, xm_str); n++;
 XtSetArg(wargs[n], XmNshowAsDefault, 1); n++;
 XtSetArg(wargs[n], XtNwidth, 90); n++;
 XtSetArg(wargs[n], XtNx, 10); n++;
 ok_button = XtCreateManagedWidget("pseudo_ok", xmPushButtonWidgetClass, bb, wargs, n);
 XtAddCallback(ok_button, XmNactivateCallback, read_gage_edit_scale, edit_struct);
 XtAddCallback(ok_button, XmNactivateCallback, popdown_shell, shell);
 XtSetKeyboardFocus(form, ok_button);
 XmStringFree(xm_str);

 n = 0;
 xm_str = XmStringCreate("Cancel", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, xm_str); n++;
 XtSetArg(wargs[n], XtNwidth, 90); n++;
 XtSetArg(wargs[n], XtNx, 125); n++;
 XtSetArg(wargs[n], XtNborderWidth, 4); n++;
 XtSetArg(wargs[n], XtNborderColor, background_color); n++;
 cancel_button = XtCreateManagedWidget("gage_edit_cancel", 
 		xmPushButtonWidgetClass, bb, wargs, n);
 XtAddCallback(cancel_button, XmNactivateCallback, popdown_shell, shell);
 XmStringFree(xm_str);
 
 n = 0;
 xm_str = XmStringCreate("Set Missing", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, xm_str); n++;
 XtSetArg(wargs[n], XtNwidth, 90); n++;
 XtSetArg(wargs[n], XtNx, 240); n++;
 XtSetArg(wargs[n], XtNborderWidth, 4); n++;
 XtSetArg(wargs[n], XtNborderColor, background_color); n++;
 missing_button = XtCreateManagedWidget("gage_edit_help", 
 		xmPushButtonWidgetClass, bb, wargs, n);
 XtAddCallback(missing_button, XmNactivateCallback, gage_edit_missing,
	       (caddr_t) gagenum);
 XmStringFree(xm_str);
 
 XtAddCallback(missing_button, XmNactivateCallback, popdown_shell, shell);
 XtPopup(shell, XtGrabNone);

}

/*************************************** END create_gage_edit_popup ************/
