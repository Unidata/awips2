#include "stage3_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "drawa.h"
#include <X11/cursorfont.h>

time_lapse_struct tldata;

/***************************************************************************/
/*  FUNCTION NAME:   read_tl_scale()                                       */
/*       FUNCTION:   callback to read selected duration of time loop       */
/***************************************************************************

Function type:
   void

Called by function:
   (callback) time lapse scale ok button

Functions called:
   none

Local variables:
   w - Widget structure; OK button on Time Lapse scale
   value - integer; value read off time lapse scale

******************************************** BEGIN read_tl_scale ***********/

void read_tl_scale(w, scale_widget, call_data)
   Widget       w;
   Widget       scale_widget;
   caddr_t     *call_data;
{
   int          value;

 XmScaleGetValue(scale_widget, &value);
 tldata.nhrs = value;

}

/********************************************* END read_tl_scale ***********/

