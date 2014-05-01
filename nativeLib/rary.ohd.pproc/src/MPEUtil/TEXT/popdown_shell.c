#include "stage3_interface.h"
#include "stage3.h"
#include "stage3_globals.h"


/***************************************************************************/
/*  FUNCTION NAME:   popdown_shell()                                       */
/*       FUNCTION:   callback to popdown shell                             */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) close button

Functions called:
   none

******************************************** BEGIN popdown_shell ***********/

void popdown_shell(w, shell, call_data)
   Widget               w;
   Widget               shell;
   XmAnyCallbackStruct *call_data;
{
 XtPopdown(shell);

}

/********************************************* END popdown_shell ***********/

