#include <stdlib.h>           
#include "stage3_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "drawa.h"
#include <X11/cursorfont.h>


/***************************************************************************/
/*  FUNCTION NAME:   pop_down()                                            */
/*       FUNCTION:   callback to pop down dates shell and close process    */
/*                   when cancel is selected                               */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) cancel button in dates shell

Functions called:
   none

******************************************** BEGIN pop_down ****************/

void pop_down(w, client_data, call_data)
   Widget               w;
   caddr_t             *client_data;
   XmAnyCallbackStruct *call_data;
{
 XtCloseDisplay(XtDisplay(w));
 exit(100);
}

/********************************************* END pop_down ****************/

