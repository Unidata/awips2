/*=================================================================*/
/*  FUNCTIONS CONTAINED IN THIS FILE:  create_editbias_popup_RFCW  */
/*                                     write_editbias_RFCW         */
/*                                     popdown_editbias            */
/*                                     free_editbias_memory        */
/*                                     read_editbias_scale         */
/*=================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <string.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <X11/cursorfont.h>

#include "create_ss_interface_rfcwide.h"
#include "drawa.h"
#include "edit_bias_RFCW.h"
#include "help.h"
#include "mpe_log_utils.h"
#include "post_functions.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "update_bias_RFCW.h"
#include "Xtools.h"

/******************************************************************/
/*  FUNCTION NAME:   create_editbias_popup_RFCW                   */
/*       FUNCTION:   create editbias popup                        */
/******************************************************************

Function type:
   void

Called by function:

Functions called:
   none

***************************** BEGIN create_editbias_popup_RFCW ***********/

extern draw_struct * ds_array [ 4 ] ;

void create_editbias_popup_RFCW ( Widget w , XtPointer clientdata ,
                                  XtPointer calldata )
    
{
   Atom                 wmAtom;
   editbias_struct *    pEditBias = NULL ;

   
   Widget               shell, form, scale, bb, scale_bb, separator;
   Widget               ok_button, cancel_button, help_button;
   Arg                  wargs[12];
   int                  n, background_color ;
   int                  ss_number = ( int ) clientdata ;
   char                 title[100];
   XmString             xstring ;

   /* Allocate the editbias structure locally.  Be sure to free it when the
      window is closed. */
   pEditBias = ( editbias_struct * ) malloc ( sizeof ( editbias_struct ) ) ;

   if ( pEditBias == NULL )
   {
      flogMessage ( stderr , "\nIn routine 'create_editbias_popup_RFCW':\n"
                         "Could not allocate memory for the editbias_struct\n"
                         "structure.  The edit bias popup GUI cannot be\n"
                         "launched.\n" ) ;
      return ;
   }

 pEditBias->ss_number = ss_number ;

 shell = XtVaCreatePopupShell("editbias_shell", transientShellWidgetClass,
		              toplevel, XmNdeleteResponse, XmDO_NOTHING, 
                              NULL);
 SetTitle ( shell, "Edit Bias Value");
 form = XtCreateManagedWidget("editbias_form", xmFormWidgetClass, shell,
		NULL, 0);

 /* Add Protocall Callbacks for the close button on the popup shell's frame. */
 wmAtom = XmInternAtom ( ( XtDisplay ( shell ) ), "WM_DELETE_WINDOW", 
                         False );
 XmAddWMProtocolCallback ( shell, wmAtom, popdown_editbias, NULL );
 XmAddWMProtocolCallback ( shell, wmAtom, free_editbias_memory, pEditBias );

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 scale_bb = XtCreateManagedWidget("editbias_scale_bb",
		  xmBulletinBoardWidgetClass, form, wargs, n);

/*  create title for edit bias popup  */

sprintf(title," bias value used =%.2f" , siibiasu [ ss_number ] ) ;

 n = 0;
 xstring = XmStringCreate ( title , XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg(wargs[n], XmNtitleString, xstring ); n++;
 XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
 XtSetArg(wargs[n], XmNshowValue, TRUE); n++;
 XtSetArg(wargs[n], XmNminimum, 0); n++;
 XtSetArg(wargs[n], XmNmaximum, 500); n++;
 XtSetArg(wargs[n], XmNprocessingDirection,XmMAX_ON_RIGHT); n++;
 XtSetArg(wargs[n], XmNdecimalPoints, (short)2); n++;
 XtSetArg(wargs[n], XmNvalue,0); n++;
 scale = XtCreateManagedWidget("editbias_scale", xmScaleWidgetClass, scale_bb, 
                               wargs, n);
 pEditBias->scale_widget = scale;
 XmStringFree ( xstring ) ;

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, scale_bb); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XtNy, 65); n++;
 separator = XtCreateManagedWidget("editbias_separator", xmSeparatorWidgetClass, form, wargs, n);

 n = 0;
 XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
 XtSetArg(wargs[n], XmNtopWidget, separator); n++;
 XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
 XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM); n++;
 bb = XtCreateManagedWidget("editbias_bb", xmBulletinBoardWidgetClass, form, wargs, n);

 /* -------------------------------------------------------------------- */
 /* Get the bulletin board background color...                           */
 /* -------------------------------------------------------------------- */

 XtSetArg(wargs[0], XtNbackground, &background_color);
 XtGetValues(bb, wargs, 1);

 n = 0;
 xstring = XmStringCreate ( "OK" , XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg(wargs[n], XmNlabelString, xstring ); n++;
 XtSetArg(wargs[n], XmNshowAsDefault, 1); n++;
 XtSetArg(wargs[n], XtNwidth, 80); n++;
 XtSetArg(wargs[n], XtNx, 10); n++;
 ok_button = XtCreateManagedWidget("editbias_ok", xmPushButtonWidgetClass, 
                                    bb, wargs, n);
 XtAddCallback ( ok_button , XmNactivateCallback , read_editbias_scale , 
                 pEditBias ) ;
 XtAddCallback ( ok_button , XmNactivateCallback , write_editbias_RFCW , 
                 pEditBias ) ;
 XtAddCallback ( ok_button , XmNactivateCallback , popdown_editbias , 
                 shell ) ;
 XtAddCallback ( ok_button , XmNactivateCallback , free_editbias_memory, 
                 pEditBias ) ;
 XtSetKeyboardFocus ( form , ok_button ) ;
 XmStringFree ( xstring ) ;

 n = 0 ;
 xstring = XmStringCreate ( "Cancel" , XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg(wargs[n], XmNlabelString,  xstring ); n++;
 XtSetArg(wargs[n], XtNwidth, 80); n++;
 XtSetArg(wargs[n], XtNx, 125); n++;
 XtSetArg(wargs[n], XtNborderWidth, 4); n++;
 XtSetArg(wargs[n], XtNborderColor, background_color); n++;
 cancel_button = XtCreateManagedWidget("editbias_cancel", 
                                       xmPushButtonWidgetClass, 
                                       bb, wargs, n ) ;
 XtAddCallback(cancel_button, XmNactivateCallback, popdown_editbias, shell);
 XtAddCallback(cancel_button, XmNactivateCallback, free_editbias_memory, 
               pEditBias);
 XmStringFree ( xstring ) ;

 n = 0;
 xstring = XmStringCreate( "Help" , XmSTRING_DEFAULT_CHARSET ) ;
 XtSetArg(wargs[n], XmNlabelString , xstring ); n++;
 XtSetArg(wargs[n], XtNwidth, 80); n++;
 XtSetArg(wargs[n], XtNx, 240); n++;
 XtSetArg(wargs[n], XtNborderWidth, 4); n++;
 XtSetArg(wargs[n], XtNborderColor, background_color); n++;
 help_button = XtCreateManagedWidget("editbias_help", xmPushButtonWidgetClass, 
                                     bb, wargs, n);

// XtAddCallback(help_button, XmNactivateCallback, popup_help_window, "EDITBIAS");
 XmStringFree ( xstring ) ;

 XtPopup(shell, XtGrabNone);

}


/******************************************************************/
/*  FUNCTION NAME:   read_editbias_scale                          */
/*       FUNCTION:   read value of bias from scale                */
/******************************************************************

Function type:
   void

Called by function:

Functions called:
   none

***************************** BEGIN read_editbias_scale ***********/
void read_editbias_scale ( Widget w , XtPointer clientdata , 
                           XtPointer calldata )

{
   int          value;
   editbias_struct * pEditBias = ( editbias_struct * ) clientdata ;
   
   XmScaleGetValue ( pEditBias->scale_widget , & value ) ;
   pEditBias->value = ( float ) value / 100. ;
}

/***************************** END read_editbias_scale ***********/

/******************************************************************************/
/*  FUNCTION NAME:   write_editbias_RFCW                                      */
/*       FUNCTION:   update rw_bias_val_used field in RWRadarResult record    */
/******************************************************************************

Function type:
   void

Called by function:

Functions called:
   update_bias_RFCW

********************************** BEGIN write_editbias_RFCW ***********/
void write_editbias_RFCW ( Widget w, XtPointer clientdata , 
                           XtPointer calldata )
{
   editbias_struct * pEditBias = ( editbias_struct * ) clientdata ;
   
  logMessage("updating RWRadarResult table record for radar=%s  new bias\n"
          "value=%.2f\n" ,
          nexrad [ pEditBias->ss_number ].id , pEditBias->value ) ;
   update_bias_RFCW (nexrad [ pEditBias->ss_number ].id, datetime,
                     &pEditBias->value);
}

/********************************** END write_editbias_RFCW ***********/

/******************************************************************/
/*  FUNCTION NAME:   popdown_editbias                             */
/*       FUNCTION:   popdown editbias popup                       */
/******************************************************************

Function type:
   void

Called by function:
   (callback) cancel button

Functions called:
   none

********************************** BEGIN popdown_editbias ***********/

void popdown_editbias ( Widget w, XtPointer clientdata , XtPointer calldata)
{

   Widget shell = ( Widget ) clientdata ;

   XtPopdown(shell);
   XtDestroyWidget(shell);
}

/************************************ END popdown_editbias ***********/

/******************************************************************/
/*  FUNCTION NAME:   free_editbias_memory                         */
/*       FUNCTION:   Deallocates the memory used by the editbias  */
/*                   struture.                                    */ 
/******************************************************************

Function type:
   void

Called by function:
   (callback) cancel button

Functions called:
   none

********************************** BEGIN popdown_editbias ***********/

void free_editbias_memory ( Widget w, XtPointer clientdata , 
                            XtPointer calldata)
{
   editbias_struct * pEditBias = NULL ;
   
   pEditBias = ( editbias_struct * ) clientdata ;

   if ( pEditBias != NULL )
   {
      free ( pEditBias ) ;
      pEditBias = NULL ;
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

