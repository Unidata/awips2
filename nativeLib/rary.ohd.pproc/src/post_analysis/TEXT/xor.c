/*=========================================================================*/
/*  FUNCTIONS CONTAINED IN THIS FILE:   xs_create_xor_gc()                 */
/*                                      start_rubber_band()                */
/*                                      track_rubber_band()                */
/*                                      end_rubber_band()                  */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "post_stage3_globals.h"

/*~~~STRUCTURE DEFINITIONS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

typedef struct {
		int     start_x, start_y,
			last_x,  last_y;
		GC      gc;
		Widget  w;
	       } rubber_band_data;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/***************************************************************************/
/*  FUNCTION NAME:   xs_create_xor_gc()                                    */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   GC structure

Called by function:

Functions called:
   get_pixel_by_name

Local variables:
   w - Widget structure;
   color - deref character;
   values - XGCValues structure;
   gc - GC structure;
   wargs - stack-deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned to 10
   n - integer; index (incrementor) for wargs array

******************************************** BEGIN xs_create_xor_gc ********/

GC xs_create_xor_gc(w, color)
   Widget       w;
   char        *color;
{
   XGCValues    values;
   GC           gc;
   Arg          wargs[10];
   int          n;

 /*------------------------------------------------------------------*/
 /* Get the colors used by the widget.                               */
 /*------------------------------------------------------------------*/

 n=0;
 XtSetArg(wargs[n], XmNbackground, &values.background); n++;
 XtGetValues(w, wargs, n);

 /*------------------------------------------------------------------*/
 /* Set the fg to the XOR of the fg and bg, so if it is              */
 /* XOR'ed with the bg, the result will be fg and vice-versa.        */
 /* This effectively achieves inverse video for the line.            */
 /*------------------------------------------------------------------*/

 values.foreground = get_pixel_by_name(w,color);
 values.foreground = values.foreground ^ values.background;

 /*------------------------------------------------------------------*/
 /* Set the rubber band gc to use XOR mode and                       */
 /* draw a dashed line.                                              */
 /*------------------------------------------------------------------*/

 values.line_style = LineSolid;
 values.function   = GXxor;

 gc = XtGetGC(w, GCForeground | GCBackground | GCFunction | GCLineStyle, &values);

 return gc;
}

/********************************************* END xs_create_xor_gc ********/



/***************************************************************************/
/*  FUNCTION NAME:   start_rubber_band()                                   */
/*       FUNCTION:   ----------------------------------------              */
/*                    ---------------------------------------              */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   none

Local variables:
   w - Widget structure;
   data - deref rubber_band_data structure;
   event - deref XEvent structure;
   gc_rect - GC structure;
   values - XGCValues structure;

******************************************** BEGIN start_rubber_band *******/

void start_rubber_band(w, data, event)
   Widget               w;
   rubber_band_data    *data;
   XEvent              *event;
{

 if (event->xbutton.button != 1) return;
 XDrawRectangle(XtDisplay(w), XtWindow(w),
		data->gc,
		data->start_x, data->start_y,
		data->last_x-data->start_x, data->last_y-data->start_y);

 data->last_x = data->start_x = event->xbutton.x;
 data->last_y = data->start_y = event->xbutton.y;
 data->w = w;

 XDrawRectangle(XtDisplay(w), XtWindow(w),
		data->gc,
		data->start_x, data->start_y,
		data->last_x-data->start_x, data->last_y-data->start_y);
}

/********************************************* END start_rubber_band *******/



/***************************************************************************/
/*  FUNCTION NAME:   track_rubber_band()                                   */
/*       FUNCTION:   tracks motion of mouse when right button is pressed   */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   none

Local variables:
   w - Widget structure;
   data - deref rubber_band_data structure;
   event - XEvent structure;

******************************************** BEGIN track_rubber_band *******/

void track_rubber_band(w, data, event)
   Widget               w;
   rubber_band_data    *data;
   XEvent              *event;
{

 /*------------------------------------------------------------------*/
 /* Draw once to clear the previous line.                            */
 /*------------------------------------------------------------------*/

 XDrawRectangle(XtDisplay(w), XtWindow(w), data->gc,
		data->start_x, data->start_y,
		data->last_x-data->start_x, data->last_y-data->start_y);

 /*------------------------------------------------------------------*/
 /* Update the end points.                                           */
 /*------------------------------------------------------------------*/

 data->last_x  = event->xbutton.x;
 data->last_y  = event->xbutton.y;

 /*------------------------------------------------------------------*/
 /* Draw the new line.                                               */
 /*------------------------------------------------------------------*/

 XDrawRectangle(XtDisplay(w), XtWindow(w), data->gc,
		data->start_x, data->start_y,
		data->last_x-data->start_x, data->last_y-data->start_y);
}

/********************************************* END track_rubber_band *******/



/***************************************************************************/
/*  FUNCTION NAME:   end_rubber_band()                                     */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:

Local variables:
   w - Widget structure;
   data - deref rubber_band_data structure;
   event - XEvent structure;

******************************************** BEGIN end_rubber_band *********/

void end_rubber_band(w, data, event)
   Widget               w;
   rubber_band_data    *data;
   XEvent              *event;
{
  if (event->xbutton.button != 1) return;

 /*------------------------------------------------------------------*/
 /* Clear the current line. */
 /*------------------------------------------------------------------*/

 /*+++++++++++++++++++++++++++++ COMMENTED OUT +++++++++++++++++++++++++++++++
  XDrawRectangle(XtDisplay(w), XtWindow(w), data->gc,
	    data->start_x, data->start_y,
	    data->last_x-data->start_x, data->last_y-data->start_y);
 +++++++++++++++++++++++++++++++ COMMENTED OUT +++++++++++++++++++++++++++++*/



}

/********************************************* END end_rubber_band *********/
