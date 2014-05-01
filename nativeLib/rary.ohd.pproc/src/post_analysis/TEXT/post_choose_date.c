/*==================================================================*/
/*                         FILE NAME:   post_choose_date.c                 */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   display_date_window()              */
/*                                      popup_workingDialog()              */
/*                                      ok_callback()                      */
/*                                      select_callback()                  */
/*                                      pop_down()                         */
/*                                      un_map_shell()                     */
/*                                      set_duration()                     */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <Xm/Protocols.h>
#include <Xm/Xm.h>
#include <Xm/ToggleBG.h>
#include <stdlib.h>

#include "postX.h"
#include "post_stage3.h"
#include "postanalysis_functions.h"
#include "post_stage3_interface.h"
#include "post_stage3_globals.h"


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/***************************************************************************/
/*  FUNCTION NAME:   display_date_window()                                 */
/*       FUNCTION: defines and pops up choose dates window                 */
/***************************************************************************

Function type:
   void

Called by function:
   main

Functions called:
   (callback) select_callback
   get_pixel_by_name
   (callback) set_duration
   (callback) ok_callback
   (callback) un_map_shell
   (callback) popup_workingDialog
   (callback) post_analysis
   (callback) pop_down

******************************************** BEGIN display_date_window *****/

void display_date_window()
{
   Widget       shell, list, bb, swin, title;
   Widget       ok_button, cancel_button, help_button;
   Widget       durationBox, hr6, hr24;
/*   Widget       hr48; */
   Arg          wargs[10];
   int          i,n;
   int          color;
   XmString    *xmstr, msg;
   Atom         wmAtom;

 durcode = 24;

 n=0;
 XtSetArg(wargs[n], XtNwidth, 400 ); n++;
 shell = XtCreatePopupShell("Choose date/hour", transientShellWidgetClass, toplevel, wargs,n);

 bb = XtCreateManagedWidget("datebb" , xmBulletinBoardWidgetClass, shell, NULL, 0);

 n=0;
 XtSetArg(wargs[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
 XtSetArg(wargs[n], XtNy,75); n++;
 XtSetArg(wargs[n], XtNheight, 150 ); n++;
 XtSetArg(wargs[n], XtNwidth, 125 ); n++;
 swin = XtCreateManagedWidget("swin", xmScrolledWindowWidgetClass, bb, wargs, n);

 xmstr = (XmString *) XtMalloc(sizeof(XmString) * NUMHRS - 1);

 for(i = 0; i < NUMHRS; i++)
    xmstr[i] = XmStringCreate(dstring[i], XmSTRING_DEFAULT_CHARSET);

 iselect=1;
 n=0;
 XtSetArg(wargs[n], XmNitems, xmstr); n++;
 XtSetArg(wargs[n], XmNitemCount, NUMHRS ); n++;
 XtSetArg(wargs[n], XmNvisibleItemCount, NUMHRS ); n++;
 XtSetArg(wargs[n], XmNselectionPolicy, XmSINGLE_SELECT); n++;
 list = XtCreateManagedWidget("list", xmListWidgetClass, swin, wargs, n);
 XtAddCallback(list, XmNsingleSelectionCallback, post_analysis_select_callback, &iselect);

 XmListSelectPos(list, 1, FALSE);

 n=0;
 msg= XmStringCreate("Select date & hour for Post Analysis run", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, msg); n++;
 title = XtCreateManagedWidget("title", xmLabelWidgetClass, bb, wargs, n);

 n=0;
 msg= XmStringCreate("End of Run", XmSTRING_DEFAULT_CHARSET);
 XtSetArg(wargs[n], XmNlabelString, msg); n++;
 XtSetArg(wargs[n], XtNy, 60); n++;
 title = XtCreateManagedWidget("title", xmLabelWidgetClass, bb, wargs, n);

 n=0;
 XtSetArg(wargs[n], XtNx, 200); n++;
 XtSetArg(wargs[n], XtNy, 100); n++;
 durationBox = XmCreateRadioBox(bb, "durationBox", wargs, n);
 color = get_pixel_by_name(shell, "firebrick");

 n=0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate("6 hour", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNselectColor, color); n++;
 hr6 = XmCreateToggleButtonGadget(durationBox, "hr6", wargs, n);

/* n=0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate("48 hour", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNselectColor, color); n++;
 hr48 = XmCreateToggleButtonGadget(durationBox, "hr48", wargs, n);
*/
 n=0;
 XtSetArg(wargs[n], XmNlabelString, XmStringCreate("24 hour", XmSTRING_DEFAULT_CHARSET)); n++;
 XtSetArg(wargs[n], XmNset, TRUE); n++;
 XtSetArg(wargs[n], XmNselectColor, color); n++;
 hr24 = XmCreateToggleButtonGadget(durationBox, "hr24", wargs, n);

 XtManageChild(durationBox);
 XtManageChild(hr6);
 XtManageChild(hr24);
 /*XtManageChild(hr48);*/
 XtAddCallback(hr6, XmNvalueChangedCallback, set_duration, (caddr_t)6);
 /*XtAddCallback(hr48, XmNvalueChangedCallback, set_duration, (caddr_t)48);*/
 XtAddCallback(hr24, XmNvalueChangedCallback, set_duration, (caddr_t)24);
 
 n=0;
 XtSetArg(wargs[n], XtNy,250); n++;
 XtSetArg(wargs[n], XtNwidth,80); n++;
 XtSetArg(wargs[n], XmNshowAsDefault,1); n++;
 ok_button = XtCreateManagedWidget("ok", xmPushButtonWidgetClass, bb, wargs, n);
 XtSetKeyboardFocus(bb, ok_button);

 XtAddCallback(ok_button, XmNactivateCallback, ok_callback,        &iselect);
 XtAddCallback(ok_button, XmNactivateCallback, un_map_shell,        shell);
 XtAddCallback(ok_button, XmNactivateCallback, popup_workingDialog, shell);
 XtAddCallback(ok_button, XmNactivateCallback, post_analysis,       NULL);

 n=0;
 XtSetArg(wargs[n], XtNy,253); n++;
 XtSetArg(wargs[n], XtNx,125); n++;
 XtSetArg(wargs[n], XtNwidth,80); n++;
 cancel_button = XtCreateManagedWidget("cancel", xmPushButtonWidgetClass, bb, wargs, n);
 XtAddCallback(cancel_button, XmNactivateCallback, post_analysis_pop_down, NULL);

 n=0;
 XtSetArg(wargs[n], XtNy,253); n++;
 XtSetArg(wargs[n], XtNx,250); n++;
 XtSetArg(wargs[n], XtNwidth,80); n++;
 help_button = XtCreateManagedWidget("help", xmPushButtonWidgetClass, bb, wargs, n);
 XtAddCallback(help_button, XmNactivateCallback,
     popup_help_window, "POSTDATES");

 /*--------------------------------*/
 /*  Add window manager callbacks  */
 /*--------------------------------*/

 wmAtom = XmInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", FALSE);
 XmAddWMProtocolCallback(shell, wmAtom, quit_post_from_WM, NULL);
 /*-----------------------------------------------------------------------------*/

 XtPopup(shell, XtGrabNonexclusive);
}

/********************************************* END display_date_window *****/


/***************************************************************************/
/*  FUNCTION NAME:   popup_workingDialog()                                 */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:

******************************************** BEGIN popup_workingDialog *****/

void popup_workingDialog(w, shell, call_data)
   Widget               w;
   Widget               shell;
   XmAnyCallbackStruct *call_data;
{

   Widget               button;
   XEvent               event;
   Arg                  wargs[3];
   int                  n, loop, Message;

 n=0;
 XtSetArg(wargs[n], XmNmessageString,
	  XmStringCreateLtoR("Preparing Post Analysis data for display",
			     XmSTRING_DEFAULT_CHARSET));n++;
 XtSetArg(wargs[n], XmNdefaultPosition, FALSE); n++;
 working_shell = XmCreateWorkingDialog(toplevel, "working", wargs, n);
 
 /*working_shell is defined in postX.h*/
 
 button = XmMessageBoxGetChild(working_shell, XmDIALOG_OK_BUTTON);
 XtUnmanageChild(button);

 button = XmMessageBoxGetChild(working_shell, XmDIALOG_HELP_BUTTON);
 XtUnmanageChild(button);

 button = XmMessageBoxGetChild(working_shell, XmDIALOG_CANCEL_BUTTON);
 XtUnmanageChild(button);


 if (w != NULL)
    {
    XtPopdown(shell);
    XtManageChild(working_shell);
    XFlush(XtDisplay(w));
    /*-------------------------------------------------------------------------*/
    /*     We need to process all events to get the message to appear          */
    /*     in the information box.                                             */
    /*-------------------------------------------------------------------------*/

    loop = TRUE;
    Message = TRUE;
    XSynchronize(XtDisplay(w), 1);

    while(loop==TRUE)
       {
       if (XtPending())
	  {
	  XtNextEvent(&event);
	  if (event.type == ClientMessage) Message=TRUE;
	  if (Message==TRUE && event.type==Expose) loop=FALSE;
	  XtDispatchEvent(&event);
	  }
       }
    XSynchronize(XtDisplay(w), 0);
    }

}

/********************************************* END popup_workingDialog *****/



/***************************************************************************/
/*  FUNCTION NAME:   ok_callback()                                         */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   none

******************************************** BEGIN ok_callback *************/

void ok_callback(w, client_data, call_data)
   Widget               w;
   int                 *client_data;
   XmAnyCallbackStruct *call_data;
   
{
 /*dates is defined in stage3.h */
 
 if (dbg) printf("In ok_callback\n");
 if (dbg) printf("iselect = %d\n",*client_data);
 date_time = dates[*client_data-1];

 sprintf(datetime,"%d-%02d-%02d %02d:00:00",date_time.year,date_time.month,
                  date_time.day,date_time.hour);
 printf("The selected date/hour is %s\n", datetime);		 

}

/********************************************* END ok_callback *************/



/***************************************************************************/
/*  FUNCTION NAME:   select_callback()                                     */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   none

******************************************** BEGIN select_callback *********/

void post_analysis_select_callback(w, client_data, call_data)
   Widget               w;
   int                  *client_data;
   XmListCallbackStruct *call_data;
{
   char                 *text;

 XmStringGetLtoR (call_data->item, XmSTRING_DEFAULT_CHARSET, &text);
 *client_data = call_data->item_position;
 if (dbg) printf("%s iselect = %d\n", text, *client_data);
}

/********************************************* END select_callback *********/



/***************************************************************************/
/*  FUNCTION NAME:   pop_down()                                            */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   none

******************************************** BEGIN pop_down ****************/

void post_analysis_pop_down(w, client_data, call_data)
   Widget               w;
   caddr_t             *client_data;
   XmAnyCallbackStruct *call_data;
{
 if (dbg) printf("In pop_down\n");
 XtCloseDisplay(XtDisplay(w));
 exit(0);
}

/********************************************* END pop_down ****************/



/***************************************************************************/
/*  FUNCTION NAME:   un_map_shell()                                        */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:
   diplay_date_window (callback)

Functions called:
   none

******************************************** BEGIN un_map_shell ************/

void un_map_shell(w, shell, call_data)
   Widget               w;
   Widget               shell;
   XmAnyCallbackStruct *call_data;
{
 XtPopdown(shell);
}

/********************************************* END un_map_shell ************/



/***************************************************************************/
/*  FUNCTION NAME:   set_duration()                                        */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:
   callback from duration choice buttons (6hr, 24hr, 48hr)

Functions called:
   none

******************************************** BEGIN set_duration ************/

void set_duration(w, code, call_data)
   Widget       w;
   short int    code;
   caddr_t     *call_data;
{
 
 /*durcode is defined in siii_shared.h as short int*/
 
 durcode = code;

 /*-------------------------------------------------------*/
 /*  define cv_duration used for colorvalue table lookup  */
 /*  cv_duration = 86400 is default set in main           */
 /*-------------------------------------------------------*/
 
 if(durcode == 6)
   cv_duration = 21600;
 else if (durcode == 24)
   cv_duration = 86400;  
 else if(durcode == 48)
   cv_duration = 172800;

 if (dbg) printf("in set duration, durcode = %d\n", durcode);


}

/********************************************* END set_duration ************/
