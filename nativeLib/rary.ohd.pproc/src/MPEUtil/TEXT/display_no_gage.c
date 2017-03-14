#include "drawa.h"
#include "menus.h"
#include "post_functions.h"
#include "stage3.h"
#include "stage3_interface.h"
#include "stage3_globals.h"
#include "Xtools.h"

/***************************************************************************/
/*  FUNCTION NAME:   display_no_gage                                       */
/*       FUNCTION:   display shell stating that no gage data is available  */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) gage_table

Functions called:
   (callback) close_no_gage
   (callback) destroy_shell

******************************************** BEGIN display_no_gage *********/

void MPEUtil_display_no_gage()
{
   Widget       grtable, mainbb, msg_w, ok_button;
   extern Widget toplevel ;
   int          n;
   char         str[40];
   XmString     msg;
   Arg          wargs[5];

 /*--------------------------------------------------------------*/
 /*     create shell and bulletin board                          */
 /*--------------------------------------------------------------*/

 grtable = XtCreatePopupShell("no_gage_shell",
			      transientShellWidgetClass,
			      toplevel, NULL, 0);
			      
 SetTitle ( grtable , "No Gage Data" ) ;

 mainbb = XtCreateManagedWidget("mainbb",
	     xmBulletinBoardWidgetClass, grtable, NULL, 0);

 /*--------------------------------------------------------------*/
 /*     create message string                                    */
 /*--------------------------------------------------------------*/

 n=0;
 strcpy(str, "No gage data available");
 msg = XmStringCreate(str,XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n],XmNlabelString,msg); n++;
 XtSetArg(wargs[n],XtNx,20); n++;
 XtSetArg(wargs[n],XtNy,20); n++;
 msg_w = XtCreateManagedWidget("msg_w",xmLabelWidgetClass,
			      mainbb, wargs, n++);
 XmStringFree(msg);

 /*--------------------------------------------------------------*/
 /*     create quit button                                       */
 /*--------------------------------------------------------------*/

 n = 0;
 msg = XmStringCreate("OK", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, msg); n++;
 XtSetArg(wargs[n], XmNshowAsDefault, 1); n++;
 XtSetArg(wargs[n], XtNwidth, 80); n++;
 ok_button = XtCreateManagedWidget("gage_ok",
	     xmPushButtonWidgetClass, mainbb, wargs, n);

 XtAddCallback(ok_button, XmNactivateCallback, close_no_gage, grtable);
 XtAddCallback(ok_button, XmNactivateCallback, destroy_shell, grtable);
 XtSetKeyboardFocus(mainbb, ok_button);
 XmStringFree(msg);

 /*--------------------------------------------------------------*/
 /*     pop up shell                                             */
 /*--------------------------------------------------------------*/

 XtPopup(grtable,XtGrabNone);
}

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/
void close_no_gage ( Widget w , Widget shell , caddr_t call_data )
{
   XtDestroyWidget ( shell ) ;
}

/********************************************* END display_no_gage *********/

