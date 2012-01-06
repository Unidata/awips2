

#include "stage3_interface.h"
#include "stage3_globals.h"
#include "stage3.h"
#include "drawa.h"
#include <sys/stat.h>
#include <X11/cursorfont.h>
/******************************************************************/
/*  FUNCTION NAME:   popdown_edit_precip_value                    */
/*       FUNCTION:   popdown edit_precip_value slider bar popup   */
/*******************************************************************

Function type:
   void

Called by function:

********************************** BEGIN popdown_edit_precip_value ***********/

void popdown_edit_precip_value(w, shell, call_data)
   Widget               w;
   Widget               shell;
   XmAnyCallbackStruct *call_data;
{
 XtPopdown(shell);

}
/************************************ END popdown_edit_precip_value ***********/

