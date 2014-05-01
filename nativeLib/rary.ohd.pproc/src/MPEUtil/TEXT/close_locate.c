#include "stage3_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "drawa.h"
#include <X11/cursorfont.h>


/***************************************************************************/
/*  FUNCTION NAME:   close_locate()                                        */
/*       FUNCTION:   close locator shell                                   */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) locate
   (callback) locate_ss

Functions called:
   none

******************************************** BEGIN close_locate ************/

void MPEUtil_close_locate(w, shell, event)
   Widget       w, shell;
   XEvent      *event;
{

 if (event->xbutton.button != 3) return;
 XtPopdown(shell);
 XtRemoveEventHandler(w, ButtonReleaseMask, FALSE, MPEUtil_close_locate, shell);

}

/********************************************* END close_locate ************/

