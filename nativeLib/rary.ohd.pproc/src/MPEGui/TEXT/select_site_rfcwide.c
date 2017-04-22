/*=========================================================================*/
/*                              NAME:  select_site_rfcwide.c               */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:  select_site_rfcwide                 */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "create_ss_interface_rfcwide.h"
#include "libXs.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "select_site_rfcwide.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "Xtools.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/***************************************************************************/
/*       FUNCTION:   display window listing available radar sites for      */
/*                    single site display                                  */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) Show single Radar site option

Functions called:
   (callback) select_callback
   (callback) popdown_shell
   (callback) show_single_Radar

******************************************** BEGIN select_site_rfcwide *************/

void select_site_rfcwide ( Widget w, XtPointer clientdata, XtPointer calldata )
   
{
   Widget                shell, list, bb, swin, title;
   Widget                ok_button, cancel_button;
   Arg                   wargs[10];
   int                   i, n;
   XmString             *xmstr, msg;
  
 /*--------------------------------------------------------------*/
 /*     initialize site number and create shell & bulletin board */
 /*--------------------------------------------------------------*/

 ssnum = 1;
 n=0;
 shell = XtCreatePopupShell("Radar Sites", transientShellWidgetClass, toplevel, wargs,n);

 bb = XtCreateManagedWidget("sitebb" ,xmBulletinBoardWidgetClass, shell, NULL, 0);

 /*--------------------------------------------------------------*/
 /*     create scrolled window to display list                   */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
 XtSetArg(wargs[n], XtNy,45); n++;
 XtSetArg(wargs[n], XtNheight, 355 ); n++;
 XtSetArg(wargs[n], XtNwidth, 150 ); n++;
 swin = XtCreateManagedWidget("swin",xmScrolledWindowWidgetClass, bb,wargs,n);

 /*--------------------------------------------------------------*/
 /*     create strings to display in list                        */
 /*--------------------------------------------------------------*/

 xmstr = (XmString *) XtMalloc(sizeof(XmString) * NRADARS);
 for(i = 0; i < NRADARS; i++)
	 xmstr[i] = XmStringCreate(nexrad[i].id,XmSTRING_DEFAULT_CHARSET);

 /*--------------------------------------------------------------*/
 /*     create list widget                                       */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNitems, xmstr); n++;
 XtSetArg(wargs[n], XmNitemCount, NRADARS ); n++;
 XtSetArg(wargs[n], XmNvisibleItemCount, NRADARS ); n++;
 XtSetArg(wargs[n], XmNselectionPolicy, XmSINGLE_SELECT); n++;
 list = XtCreateManagedWidget("list",xmListWidgetClass, swin, wargs, n);
 XtAddCallback(list, XmNsingleSelectionCallback, select_callback, &ssnum);
 XmListSelectPos(list,1,FALSE);

 for(i = 0; i < NRADARS; i++)
 {
    XmStringFree ( xmstr [ i ] ) ;
 }

 XtFree ( ( char * ) xmstr ) ;
 xmstr = NULL ;

 /*--------------------------------------------------------------*/
 /*     display heading for shell                                */
 /*--------------------------------------------------------------*/

 n=0;
 msg= XmStringCreate("Select Site:", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, msg); n++;
 title = XtCreateManagedWidget("title",xmLabelWidgetClass, bb, wargs,n);
 XmStringFree(msg);
 
 /*--------------------------------------------------------------*/
 /*     create ok button, set as default                         */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XtNy,435); n++;
 XtSetArg(wargs[n], XtNwidth,80); n++;
 XtSetArg(wargs[n], XmNshowAsDefault,1); n++;
 ok_button = XtCreateManagedWidget("ok",xmPushButtonWidgetClass, bb, wargs,n);
 XtSetKeyboardFocus(bb,ok_button);
 XtAddCallback(ok_button, XmNactivateCallback, popdown_shell, shell);
 XtAddCallback(ok_button, XmNactivateCallback, create_ss_interface_rfcwide, NULL);

 /*--------------------------------------------------------------*/
 /*     create cancel button                                     */
 /*--------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XtNy,438); n++;
 XtSetArg(wargs[n], XtNx,130); n++;
 XtSetArg(wargs[n], XtNwidth,80); n++;
 cancel_button = XtCreateManagedWidget("cancel",xmPushButtonWidgetClass, 
 		bb, wargs, n);
 XtAddCallback(cancel_button, XmNactivateCallback, popdown_shell, shell);
 XtAddCallback(cancel_button, XmNactivateCallback, cancel_window_single_site, 
 		NULL);
 /*--------------------------------------------------------------*/
 /*     display shell                                            */
 /*--------------------------------------------------------------*/

 XtPopup(shell, XtGrabNonexclusive);
}

/********************************************* END select_site_rfcwide *************/
