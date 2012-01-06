/*=========================================================================*/
/*                         FILE NAME:   quit_rfcwide.c                     */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   quit_rfcwide                       */
/*                                      quit_rfcwide_from_WM               */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "stage3.h"
#include "drawa.h"
#include "rfcwide.h"
#include "rfcwide_callbacks.h"
#include "libXs.h"
#include "quit_rfcwide.h"

/***************************************************************************/
/*  FUNCTION NAME:   quit_rfcwide()                                       */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

************************************** BEGIN quit_rfcwide **************/

void quit_rfcwide()
{
 Widget message;

 if (DataSaved == TRUE) exit(0);
 else
    {
      message = create_save_data_dialog_RFCW(0);
      XtManageChild(message);
      XtVaSetValues(message, XmNx, 40, XmNy, 40, NULL);
    }
}
/********************************************* END quit_rfcwide ************/


/***************************************************************************/
/*  FUNCTION NAME:   quit_rfcwide_from_WM                                  */
/*       FUNCTION:   callback for exiting from rfcwide from window         */
/*                   manager                                               */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) window manager button

******************************************** BEGIN quit_rfcwide_from_WM ****/

void quit_rfcwide_from_WM()
{

exit(0);

}

/********************************************* END quit_rfcwide_from_WM ****/
