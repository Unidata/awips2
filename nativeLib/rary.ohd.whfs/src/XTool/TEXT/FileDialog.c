/*******************************************************************************
* FILENAME:             FileDialog.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*            MODULE 1:  FileDialog
*         DESCRIPTION:  Contains code to create a generic FileSelectionDialog
*                       GUI.
*
* ORIGINAL AUTHOR:      Bryon Lawrence
* CREATION DATE:        November 1, 2001
* ORGANIZATION:         HSEB / OHD
* MACHINE:       HP-UX 9000 / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        11/1/01      Bryon Lawrence    Original Coding
********************************************************************************
*/
#include <stdio.h>
#include <Xm/FileSB.h>
#include <Xm/SelectioB.h>
#include <Xm/Xm.h>
#include "Xtools.h"

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   FileDialog
* PURPOSE:       This routine creates a generic XmFileSelectionDialog object. 
*                When instantiated into a gui, it can be used to select a 
*                directory and a file to save a file into or load a file
*                from.
*
*                The user must pass in 9 arguments to this routine.
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
*    ERROR CODE                            DESCRIPTION
*
********************************************************************************
*/

Widget FileDialog ( Widget toplevel ,
                    ArgList arglist ,
                    Cardinal argcount ,
                    XtCallbackProc ok_cb ,
                    XtPointer ok_client_data , 
                    XtCallbackProc cancel_cb ,
                    XtPointer cancel_client_data , 
                    XtCallbackProc help_cb ,
                    XtPointer help_client_data ) 
{
   static Widget dialog = NULL ;

   if ( dialog == NULL )
   {
      /* Create a simple FileSelectionDialog. It is up to the user to 
         supply any attributes he wishes to set using the arglist and
         argcount arguments. */
      dialog = XmCreateFileSelectionDialog ( toplevel , "filesb" , arglist ,  
                                             argcount ) ;

      /* By default, this GUI is being made MODAL. */
      XtVaSetValues ( dialog , XmNdialogStyle ,
                      XmDIALOG_FULL_APPLICATION_MODAL , NULL ) ;

      /* Set the callbacks based upon the user supplied routines. */
      XtAddCallback ( dialog , XmNcancelCallback , cancel_cb , 
                      cancel_client_data ) ;
      XtAddCallback ( dialog , XmNokCallback , ok_cb , ok_client_data ) ;
      XtAddCallback ( dialog , XmNhelpCallback , help_cb , help_client_data ) ;

      /* Disable any necessary buttons. */
      if ( help_cb == NULL )
      {
         XtUnmanageChild ( 
             XmSelectionBoxGetChild ( dialog , XmDIALOG_HELP_BUTTON ) ) ; 
      }

      if ( ok_cb == NULL )
      {
         XtUnmanageChild ( 
             XmSelectionBoxGetChild ( dialog , XmDIALOG_OK_BUTTON ) ) ; 
      }

      if ( cancel_cb == NULL )
      {
         XtUnmanageChild ( 
             XmSelectionBoxGetChild ( dialog , XmDIALOG_CANCEL_BUTTON ) ) ; 
      }
         
   }

   return dialog ;
}
