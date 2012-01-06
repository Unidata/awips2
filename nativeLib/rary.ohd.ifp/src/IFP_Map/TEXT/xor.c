/*=========================================================================*/
/*                    FILE PATH/NAME:   /home/lef/s3/xor.c                 */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   xs_create_xor_gc()                 */
/*                                      start_rubber_band()                */
/*                                      track_rubber_band()                */
/*                                      end_rubber_band()                  */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/xor.c                                    */
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

GC IFP_Map_xs_create_xor_gc(w, color, LineStyle)
	Widget  w;
	char    *color;
	int     LineStyle;
{
	XGCValues       values;
	GC              gc;
	Pixel           foreground;
	int             n;

 /*------------------------------------------------------------------*/
 /* Get the colors used by the widget.                               */
 /*------------------------------------------------------------------*/

 XtVaGetValues(w, XmNbackground, &values.background,
		  XmNforeground, &values.foreground, NULL);

 /*------------------------------------------------------------------*/
 /* Set the fg to the XOR of the fg and bg, so if it is              */
 /* XOR'ed with the bg, the result will be fg and vice-versa.        */
 /* This effectively achieves inverse video for the line.            */
 /*------------------------------------------------------------------*/

 values.foreground = values.foreground ^ values.background;

 /*------------------------------------------------------------------*/
 /* Set the rubber band gc to use XOR mode and                       */
 /* draw a dashed line.                                              */
 /*------------------------------------------------------------------*/

 values.line_style = LineStyle;
 values.function   = GXxor;

 gc = XtGetGC(w, GCForeground | GCBackground | GCFunction | GCLineStyle, &values);

 foreground = get_pixel_by_name(w, color);
 XSetForeground(XtDisplay(w), gc, (unsigned long)foreground);

 return gc;

}

/********************************************* END xs_create_xor_gc ********/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/xor.c                                    */
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

void IFP_Map_start_rubber_band(w, data, event)
   Widget               w;
   rubber_band_data    *data;
   XEvent              *event;
{
   GC                   gc_rect;
   XGCValues            values;



 if (event->xbutton.button != 1) return;

 XDrawRectangle(XtDisplay(w), XtWindow(w),
		data->gc,
		data->start_x, data->start_y,
		data->last_x-data->start_x, data->last_y-data->start_y);

 data->last_x = data->start_x = event->xbutton.x;
 data->last_y = data->start_y = event->xbutton.y;

 XDrawRectangle(XtDisplay(w), XtWindow(w),
		data->gc,
		data->start_x, data->start_y,
		data->last_x-data->start_x, data->last_y-data->start_y);
}

/********************************************* END start_rubber_band *******/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/xor.c                                    */
/*  FUNCTION NAME:   track_rubber_band()                                   */
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
   event - XEvent structure;

******************************************** BEGIN track_rubber_band *******/

void IFP_Map_track_rubber_band(w, data, event)
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
/* FILE PATH/NAME:   /home/lef/s3/xor.c                                    */
/*  FUNCTION NAME:   end_rubber_band()                                     */
/*       FUNCTION:   (DOES NOTHING)                                        */
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

void IFP_Map_end_rubber_band(w, widget_struct, event)
	Widget                  w;
	the_widget_struct       *widget_struct;
	XEvent                  *event;
{

 Zoom_In_Enabled = TRUE;



/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/xor.c,v $";
 static char rcs_id2[] = "$Id: xor.c,v 1.1 1995/09/08 14:56:02 page Exp $";}
/*  ===================================================  */

}

