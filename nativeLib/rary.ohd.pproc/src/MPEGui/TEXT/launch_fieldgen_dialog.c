
/*******************************************************************************
* FILENAME:             launch_fieldgen_dialog.c
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:           launch_fieldgen_dialog
* DESCRIPTION:          This routine creates and "pops up" the "Rerun
*                       FieldGen" dialog box.   It also contains callback
*                       routines to support the "Yes" and "No" button
*                       button callbacks.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        April 26 , 2002
* ORGANIZATION:         OHD / HSEB
* MACHINE:              HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        4/26/2002    Bryon Lawrence    Original Coding
*          1        12/7/2004    Bryon Lawrence    Removed the option to
*                                                  rerun siipp from the
*                                                  Rerun FieldGen Dialog
*                                                  box.
*                                                  Also, reduced the size
*                                                  and the amount of 
*                                                  wasted space in the
*                                                  dialog box.
********************************************************************************
*/

#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/ToggleB.h>
#include <Xm/Xm.h>

#include "GeneralUtil.h"  /* Included for the "get_apps_defaults" routine. */
#include "map_resource.h"
#include "rerun_rfcwgen.h"

static Widget dialog_shell ;
static Widget dialog_form ;
static Widget dialog_label ;
static Widget no_button ;
static Widget yes_button ;

static int button_width = 50 ;
static int button_height = 40 ;
static int height = 90 ;
static int width = 280 ;
static int top_offset = 20 ;

/*******************************************************************************
* MODULE NUMBER:  1
* MODULE NAME:    launch_fieldgen_dialog 
* PURPOSE:        This routine creates and "pops up" the "Rerun
*                 FieldGen GUI.  This GUI validates that the user
*                 wants to rerun FieldGen, which can be a very time 
*                 consuming process, especially at some of the 
*                 larger, data dense RFCs.
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME         DESCRIPTION/UNITS
*   Input  Widget      w            The widget generating the callback.
*   Input  XtPointer   clientdata   Data supplied by the user.
*   Input  XtPointer   calldata     Data supplied by the system.
*
* RETURNS:
*   Void
*
* APIs UTILIZED:
*   NAME                 HEADER FILE       DESCRIPTION
*   rerun_rfcwgen        rerun_rfcwgen.h   Reruns the MPE Field Generator
*                                          program.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE  NAME           DESCRIPTION
*   char *     dialog_string  The string which asks if the user wants to
*                             rerun fieldgen.
*   char *     dialog_title   The title of the dialog box.
*   char *     no_string      Contains the string "No" for the "No" button 
*                             label.
*   char *     yes_string     Contains the string "Yes" for the "Yes" button
*                             label.
*   int        offset         Used in calculating the horizontal offsets of
*                             the push buttons to create a proper looking 
*                             display.
*   XmString   dialog_xmstring Used in creating a Motif Localized String object
*                              from C-style strings.
*
* DATA FILES AND/OR DATABASE:
*   None
*
* ERROR HANDLING:
*   None
********************************************************************************
*/

void launch_fieldgen_dialog ( Widget widget , XtPointer clientdata ,
                              XtPointer calldata )
{
   char * dialog_string = "Are You Sure You Want To Rerun FieldGen?" ;
   char * dialog_title = "Regenerate Hour Fields Dialog" ;
   char * no_string = "No" ;
   char * yes_string = "Yes" ;
   int offset ;
   XmString dialog_xmstring ;

   /* Create the dialog shell that will contain the launch field gen
      dialog message box. */
   dialog_shell = XtVaCreateWidget ( "rerun_dialog_shell" ,
                                     xmDialogShellWidgetClass ,
                                     widget ,
                                     XmNheight , height ,
                                     XmNwidth , width ,
                                     XmNmaxHeight , height ,
                                     XmNmaxWidth , width ,
                                     XmNminHeight , height ,
                                     XmNminWidth , width ,
                                     XmNtitle , dialog_title ,
                                     XmNx , 50 ,
                                     XmNy , 50 ,
                                     NULL ) ;
                                     
   /* Create the widget manager that will be contained within the dialog
      box. */
   dialog_form = XtVaCreateManagedWidget ( "rerun_dialog_form" ,
                                           xmFormWidgetClass ,
                                           dialog_shell ,
                                           XmNheight , height ,
                                           XmNwidth , width ,
                                           NULL ) ;
   
   /* Start creating and laying out the widgets that will make up the
      dialog box. */
   
   /* Create the label that will ask if the user really wants to rerun
      field gen. */
   dialog_xmstring = XmStringCreateLocalized ( dialog_string ) ;
   dialog_label = XtVaCreateManagedWidget ( "dialog_label" ,
                                            xmLabelWidgetClass ,
                                            dialog_form ,
                                            XmNlabelString ,
                                            dialog_xmstring ,
                                            XmNalignment ,
                                            XmALIGNMENT_CENTER ,
                                            XmNtopAttachment ,
                                            XmATTACH_FORM ,
                                            XmNleftAttachment ,
                                            XmATTACH_FORM ,
                                            XmNrightAttachment ,
                                            XmATTACH_FORM ,
                                            NULL ) ;
   XmStringFree ( dialog_xmstring ) ;
                
   /* Dynamically create left and right offsets for the buttons. */
   offset = ( width - ( 2 * button_width ) ) / 3 ;

   /* Create the "Yes" button. */
   dialog_xmstring = XmStringCreateLocalized ( yes_string ) ;
   yes_button = XtVaCreateManagedWidget ( "yes_button" , 
                                          xmPushButtonWidgetClass ,
                                          dialog_form ,
                                          XmNheight , button_height ,
                                          XmNwidth , button_width ,
                                          XmNlabelString , dialog_xmstring ,
                                          XmNalignment , 
                                          XmALIGNMENT_CENTER ,
                                          XmNtopAttachment ,
                                          XmATTACH_WIDGET ,
                                          XmNtopWidget ,
                                          dialog_label ,
                                          XmNtopOffset ,
                                          top_offset ,
                                          XmNleftAttachment ,
                                          XmATTACH_FORM ,
                                          XmNleftOffset ,
                                          offset ,
                                          NULL ) ;
 

   XtAddCallback ( yes_button , XmNactivateCallback , rerun_rfcwgen ,
                   NULL ) ;
   XmStringFree ( dialog_xmstring ) ;
                                          
   /* Create the "No" button. */
   dialog_xmstring = XmStringCreateLocalized ( no_string ) ;
   no_button = XtVaCreateManagedWidget ( "no_button" , 
                                          xmPushButtonWidgetClass ,
                                          dialog_form ,
                                          XmNheight , button_height ,
                                          XmNwidth , button_width ,
                                          XmNlabelString , dialog_xmstring ,
                                          XmNalignment , 
                                          XmALIGNMENT_CENTER ,
                                          XmNtopAttachment ,
                                          XmATTACH_WIDGET ,
                                          XmNtopWidget ,
                                          dialog_label ,
                                          XmNtopOffset,
                                          top_offset ,
                                          XmNrightAttachment ,
                                          XmATTACH_FORM ,
                                          XmNrightOffset ,
                                          offset ,
                                          NULL ) ;
                                          
   XmStringFree ( dialog_xmstring ) ;
                                          
   /* Popup the DialogShell. */
   XtPopup ( dialog_shell , XtGrabExclusive ) ;
}
