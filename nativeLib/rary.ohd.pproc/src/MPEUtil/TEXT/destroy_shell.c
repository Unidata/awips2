

#include "stage3_interface.h"
#include "stage3.h"
#include "overlay.h"
#include "stage3_globals.h"
#include "drawa.h"
#include "math.h"
#include <sys/stat.h>
#include <X11/cursorfont.h>
#include <time.h>



/***************************************************************************/
/*  FUNCTION NAME:   destroy_shell()                                       */
/*       FUNCTION:   callback to destroy shell                             */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) close button

Functions called:
   none

******************************************** BEGIN destroy_shell ***********/

void destroy_shell(w, shell, call_data)
   Widget               w;
   Widget               shell;
   XmAnyCallbackStruct *call_data;
{

 XtDestroyWidget(shell);
}

/********************************************* END destroy_shell ***********/


