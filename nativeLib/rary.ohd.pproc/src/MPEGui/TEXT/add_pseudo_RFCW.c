
/*******************************************************************************
* FILENAME:            add_pseudo_RFCW.c   
* NUMBER OF MODULES:   5
* GENERAL INFORMATION:
*   MODULE 1:          locate_pseudo_RFCW
* DESCRIPTION:         This routine locate position of pseudo gage on the 
*                      screen
*
*   MODULE 2:          create_pseudo_popup_RFCW
* DESCRIPTION:         This routine creates pseudo gage popup window, 
*                      and needed pseudo gage value could be inserted
*                      using the slider bar
*
*   MODULE 3:          write_pseudo_RFCW
* DESCRIPTION:         This routine insert pseudo gage record into 
*                      PseudoGageRadarVal table. If insert is successful,     
*                      then update num_pseudo_gages field in RWResult table 
*                                             
* 
*   MODULE 4:          add_pseudo_RFCW
* DESCRIPTION:         This routine  defines add_pseudo_flag - whether                                   
*                      the user wants to add gage or not: 
*	               flag is equal to 0 -- mouse click on Hydromap 
*		       (Zoom option);               
*                      flag is equal to 1 -- Add Pseudo Gage 
*		       menu item was choosen from MpeControl menu 
* 
*   MODULE 5:          popdown_pseudo_RFCW
* DESCRIPTION:         This routine closes pseudo gage popup window
*
* ORIGINAL AUTHOR:     Bryon Lawrence
* CREATION DATE:       February 8, 2002
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP-UX / Dell-Linux
* MODIFICATION HISTORY:
*   MODULE #  DATE             PROGRAMMER        DESCRIPTION/REASON
*          1  February 8, 2002 Bryon Lawrence    Original Coding 
*        ALL  March   19, 2002 Moria Shebsovich  Changed calculation of the 
*                                                HRAP and latitude, added
*                                                add_pseudo_flag. 
********************************************************************************
*/

#include <math.h>
#include <string.h>
#include <X11/cursorfont.h>

#include "add_pseudo_RFCW.h"
#include "drawa.h"
#include "help.h"
#include "insert_pseudo_RFCW.h"
#include "map_defines.h"      /* for clicks structure */
#include "map_library.h"
#include "read_numpseudo_RFCW.h"
#include "rfcwide.h"
#include "rfcwide_callbacks.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "Xtools.h"

extern int add_pseudo_flag ;

/*****************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   locate_pseudo_RFCW   
* PURPOSE:       This routine locates position of pseudo gage on the screen 
*
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XEvent       event      An informational structure specific to this
*                                type of call back.
* Input  Boolean      continue_to_dispatch_return.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
******************************************************************************
*
* Function type:
*    void
* 
* Called by function:
* 
* Functions called:
*    create_pseudo_popup_RFCW
*
********************************** BEGIN locate_pseudo_RFCW*********/

void locate_pseudo_RFCW ( Widget w, XtPointer clientdata,  XEvent * event , 
			Boolean *continue_to_dispatch_return )  
{
 float lat, lon ;
 
 draw_struct * data = (draw_struct *) clientdata ;
  
 clicks * mouse_event = (clicks * ) event ;

 /* get cursor location in screen coordinates and convert 
    screen coordinates to HRAP coordinates */
  
  mConvertXY2LatLon ( mouse_event -> x , mouse_event -> y, &lat , &lon ) ;
  
  pseudo.hrap = LatLongToHrapMpe (lat , (-1) * lon ) ;
  pseudo.lt = lat;
  pseudo.ln = (-1)*lon;
  
  XUndefineCursor(XtDisplay(data->w), XtWindow(XtParent(data->w)));

  create_pseudo_popup_RFCW(&pseudo);

}

/*****************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   create_pseudo_popup_RFCW    
* PURPOSE:       This routine creates pseudo gage popup window  
*
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
*   pseudo_struct    pseudo     Programmer-specified data structure passed 
*				into this routine.
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
******************************************************************************
*
* Function type:
*   void
*
* Called by function:
*   locate_pseudo_RFCW
* Functions called:
*   none
*
************************************* BEGIN create_pseudo_popup_RFCW*******/
void create_pseudo_popup_RFCW ( pseudo_struct * pseudo )  
{
   Widget               shell, form, scale, bb, scale_bb, separator;
   Widget               ok_button, cancel_button; 
   //Widget help_button;
   Arg                  wargs[12];
   int                  n, background_color;
   XmString		xm_str;
   help_struct         *help_data; 

 shell = XtCreatePopupShell("pseudo_shell", transientShellWidgetClass,
		toplevel, NULL, 0);
 SetTitle ( shell , "Add Pseudo Gage" ) ;
 
 form = XtCreateManagedWidget("pseudo_form", xmFormWidgetClass, shell,
		NULL, 0);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 scale_bb = XtCreateManagedWidget("pseudo_scale_bb",
		  xmBulletinBoardWidgetClass, form, wargs, n);

 n = 0;
 xm_str = XmStringCreate("Pseudo Gage Accumulation (inches)",
	   XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNtitleString, xm_str); n++;
 XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
 XtSetArg(wargs[n], XmNshowValue, TRUE); n++;
 XtSetArg(wargs[n], XmNminimum, 0); n++;
 XtSetArg(wargs[n], XmNmaximum, 500); n++;
 XtSetArg(wargs[n], XmNprocessingDirection,XmMAX_ON_RIGHT); n++;
 XtSetArg(wargs[n], XmNdecimalPoints, (short)2); n++;
 XtSetArg(wargs[n], XmNvalue,0); n++;
 scale = XtCreateManagedWidget("pseudo_scale", xmScaleWidgetClass, scale_bb, wargs, n);
 pseudo->scale_widget = scale;
 XmStringFree(xm_str);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, scale_bb); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XtNy, 65); n++;
 separator = XtCreateManagedWidget("pseudo_separator", xmSeparatorWidgetClass, form, wargs, n);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, separator); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM); n++;
 bb = XtCreateManagedWidget("pseudo_bb", xmBulletinBoardWidgetClass, form, wargs, n);

 /* -------------------------------------------------------------------- */
 /* Get the bulletin board background color...                           */
 /* -------------------------------------------------------------------- */

 XtSetArg(wargs[0], XtNbackground, &background_color);
 XtGetValues(bb, wargs, 1);

 n = 0;
 xm_str = XmStringCreate("OK", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, xm_str); n++;
 XtSetArg(wargs[n], XmNshowAsDefault, 1); n++;
 XtSetArg(wargs[n], XtNwidth, 80); n++;
 XtSetArg(wargs[n], XtNx, 10); n++;
 ok_button = XtCreateManagedWidget("pseudo_ok", xmPushButtonWidgetClass, bb, wargs, n);
 XtAddCallback(ok_button, XmNactivateCallback, read_pseudo_scale, pseudo);
 XtAddCallback(ok_button, XmNactivateCallback, popdown_pseudo_RFCW, ( XtPointer) shell);
 XtAddCallback(ok_button, XmNactivateCallback, write_pseudo_RFCW, pseudo);
 XtSetKeyboardFocus(form, ok_button);
 XmStringFree(xm_str);
 
 n = 0;
 xm_str = XmStringCreate("Cancel", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, xm_str); n++;
 XtSetArg(wargs[n], XtNwidth, 80); n++;
 XtSetArg(wargs[n], XtNx, 125); n++;
 XtSetArg(wargs[n], XtNborderWidth, 4); n++;
 XtSetArg(wargs[n], XtNborderColor, background_color); n++;
 cancel_button = XtCreateManagedWidget("pseudo_cancel", xmPushButtonWidgetClass, bb, wargs, n);
 XtAddCallback(cancel_button, XmNactivateCallback, popdown_pseudo_RFCW, ( XtPointer ) shell);
 XmStringFree(xm_str);
 
 n = 0;
 xm_str =  XmStringCreate("Help", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, xm_str); n++;
 XtSetArg(wargs[n], XtNwidth, 80); n++;
 XtSetArg(wargs[n], XtNx, 240); n++;
 XtSetArg(wargs[n], XtNborderWidth, 4); n++;
 XtSetArg(wargs[n], XtNborderColor, background_color); n++;
// help_button = XtCreateManagedWidget("pseudo_help", xmPushButtonWidgetClass, bb, wargs, n);
 XmStringFree(xm_str);

 help_data = (help_struct *) malloc(sizeof(help_struct));
 help_data->parent = bb;
 help_data->message_widget_name = "pseudo_help"; 
 //XtAddCallback(help_button, XmNactivateCallback, popup_help_window, "PSEUDO");

 XtPopup(shell, XtGrabNone);

}

/*****************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   write_pseudo_RFCW   
* PURPOSE:       This routine writes record to PseudoGageRadarVal table of 
*		 the database after adding a pseudo gage precipitation value
* ARGUMENTS:
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
******************************************************************************
*
* Function type:
*   void
*
* Called by function:
*   (callback) from OK button
* Functions called:
*   read_numpseudo_RFCW
*   insert_pseudo_RFCW
*
***************************************** BEGIN write_pseudo_RFCW ***********/
const int pseudo_gage_name_len = 10 ;

void write_pseudo_RFCW ( Widget w, XtPointer clientdata , XtPointer calldata )
  
{
 point hr;
 float ltt, lnn;
 char psid [ pseudo_gage_name_len ];
 
 pseudo_struct * pseudo = (pseudo_struct *) clientdata ;

 num_gage_edit++;
 hr.x = (int) pseudo->hrap.x;
 hr.y = (int) pseudo->hrap.y;
 ltt = pseudo->lt;
 lnn = pseudo->ln;

/*------------------------------------------------------------------------------------*/
/*    check if any pseudo gages have previously been defined for this                 */
/*      date/hour                                                                     */
/*    name of newest pseudo gage is PSEUDOxx                                          */
/*       where xx = (previous number of pseudo gages defined for this date/hour) + 1  */
/*------------------------------------------------------------------------------------*/

read_numpseudo_RFCW(datetime,&numpseudo);
numpseudo++;
sprintf(psid,"PSEUDO%02d",numpseudo);

/*-----------------------------------------------------------------*/
/*   insert pseudo gage record into PseudoGageRadarVal table       */
/*   if insert is successful, then update num_pseudo_gages field   */
/*     in RWResult table                                           */
/*-----------------------------------------------------------------*/

insert_pseudo_RFCW(psid,datetime, ltt, lnn, pseudo->value*25.4);
add_pseudo_flag = 0 ;
}

/********************************** END write_pseudo_RFCW ***********/

/*****************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   add_pseudo_RFCW   
* PURPOSE:       This routine turns on add_pseudo_flag when the option 
*                Gage->AddPseudoGage was chosen from MPEcontrol menu. 
* ARGUMENTS:
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
***************************************************************************
* Function type:
*   void
*
* Called by function:
*   (callback) add pseudo gage button
*
* Functions called:
*   none
*********************************** BEGIN add_pseudo_RFCW ***********/
void add_pseudo_RFCW ( Widget w, XtPointer clientdata , XtPointer calldata ) 
{
  static Cursor pscursor = (Cursor)NULL;
  Display *dpy;
  extern int display_7x7_flag;
  extern int draw_poly_flag;

  draw_struct * data = (draw_struct *) clientdata ; 
  
  dpy = XtDisplay(data->w);

   /*----------------------------------------------------------------*/
   /*  define add_pseudo_flag                                        */
   /*  = 0 -- mouse click on Hydromap (Zoom option)                  */
   /*  = 1 -- Add Pseudo Gage menu item  chosen from MpeControl menu */
   /*----------------------------------------------------------------*/
   
  display_7x7_flag = 0;
  draw_poly_flag = 0;
  add_pseudo_flag = 1;

  if (pscursor == (Cursor) NULL)
     pscursor = XCreateFontCursor(dpy,XC_hand2);
  XDefineCursor(dpy, XtWindow(XtParent(data->w)), pscursor);

}

/*****************************************************************************
* MODULE NUMBER: 5
* MODULE NAME:   popdown_pseudo_RFCW    
* PURPOSE:       This routine closes pseudo gage popup window 

* ARGUMENTS:
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
***************************************************************************
*
* Function type:
*    void
*
* Called by function:
*    (callback) cancel button
* 
* Functions called:
*   none
*
********************************** BEGIN popdown_pseudo_RFCW ***********/

void popdown_pseudo_RFCW ( Widget w, XtPointer clientdata , XtPointer calldata)
  
{

 Widget shell = ( Widget ) clientdata ;
 if(add_pseudo_flag==1)
    add_pseudo_flag = 0;
 XtPopdown(shell);
 
}
/************************************ END popdown_pseudo_RFCW ***********/
