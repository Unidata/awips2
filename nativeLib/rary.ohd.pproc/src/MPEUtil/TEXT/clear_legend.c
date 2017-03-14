#include "stage3_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "drawa.h"
#include <X11/cursorfont.h>

/********************************************************************/
/*  FUNCTION NAME:   clear_legend                                   */
/*       FUNCTION:   clears legend                                  */
/*********************************************************************

Function type:
   void

Called by function:
   (callback) Next Hour option
   (callback) Previous Hour option
   (callback) ok button
   (expose callback) create_stage3_interface

******************************** BEGIN clear_legend *******/

void clear_legend(w, legend, call_data)
    Widget     w, legend;
    caddr_t    *call_data;
{
 XClearArea(XtDisplay(legend),XtWindow(legend),0,0,0,0,TRUE);
}
/******************************** END clear_legend ************/

