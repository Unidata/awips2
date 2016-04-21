/*=========================================================================*/
/*                    FILE PATH/NAME:   /home/lef/s3/fill_pixmap.c         */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   fill_pixmap                        */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "libXifp.h"
#include "globals.h"
#include "struct_defs.h"
#include "read_write_data.h"
#include "drawa.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/



/***************************************************************************/
/* FILE PATH/NAME:   /home/lef/s3/fill_pixmap.c                            */
/*  FUNCTION NAME:   fill_pixmap                                           */
/*       FUNCTION:   initialize data, read parameters, set up X-windows    */
/*                    and call main loop                                   */
/***************************************************************************

Function type:
   void

Called by function:

Functions called:
   get_pixel_by_name
   draw_bound
   add_overlays

Local variables:
   w - Widget structure;
   data - deref draw_struct structure
   call_data - deref XmDrawingAreaCallbackStruct structure;
   i - integer;
   j - integer;
   temp - integer;
   n - integer; index (incrementor) for wargs array
   wargs - stack-deref (array) Arg structure; array of
      arguments used for setting widget resources; dimensioned 10
   width - Dimension structure;
   height - Dimension structure;
   xloc - integer;
   yloc - integer;
   x_pixels_per_bin - unsigned integer;
   y_pixels_per_bin - unsigned integer;
   gcv - XGCValues structure;
   mask - integer;
   backgc - GC structure;
   dpy - deref Display structure;

******************************************** BEGIN fill_pixmap *************/

void fill_pixmap(w, widget_struct, call_data)
	Widget                      w;
	the_widget_struct           *widget_struct;
	XmDrawingAreaCallbackStruct *call_data;
{

	int                  i, j, temp, n;
	Arg                  wargs[10];
	Dimension            width, height;
	int                  xloc, yloc;
	unsigned int         x_pixels_per_bin, y_pixels_per_bin;
	XGCValues            gcv;
	int                  mask = GCForeground;
	GC                   backgc;
	Display              *dpy;

	draw_struct          *data;



 data = widget_struct->overlays;


 /*--------------------------------------------------------------*/
 /*     Get the width and height of the drawing area             */
 /*--------------------------------------------------------------*/


  XtVaGetValues(widget_struct->main_canvas, XmNwidth, &width, XmNheight, &height, NULL);

  x_pixels_per_bin = (float)width/(float)data->maximum_columns;
  y_pixels_per_bin = (float)height/(float)data->maximum_rows;

  if(x_pixels_per_bin > y_pixels_per_bin) x_pixels_per_bin = y_pixels_per_bin;
  else if(y_pixels_per_bin > x_pixels_per_bin) y_pixels_per_bin = x_pixels_per_bin;


 /*--------------------------------------------------------------*/
 /*     Free the old pixmap and create one the size of the       */
 /*     DrawingArea widget's window                              */
 /*--------------------------------------------------------------*/

  if(data->pix) XFreePixmap(XtDisplay(widget_struct->main_canvas), data->pix);
  if(data->pixbase) XFreePixmap(XtDisplay(widget_struct->main_canvas), data->pixbase);


  data->pix = XCreatePixmap(XtDisplay(widget_struct->main_canvas),
			    DefaultRootWindow(XtDisplay(widget_struct->main_canvas)),
			    width, height,
			    DefaultDepthOfScreen(XtScreen(widget_struct->main_canvas)));

  data->pixbase = XCreatePixmap(XtDisplay(widget_struct->main_canvas),
			    DefaultRootWindow(XtDisplay(widget_struct->main_canvas)),
			    width, height,
			    DefaultDepthOfScreen(XtScreen(widget_struct->main_canvas)));

 XSetForeground(XtDisplay(w), data->gc[0], BlackPixelOfScreen(XtScreen(w)));
 XFillRectangle(XtDisplay(w), data->pix, data->gc[0], 0, 0, width, height);

 if (bound != (overlay_struct **) NULL)
    draw_bound(widget_struct->main_canvas, data, x_pixels_per_bin, y_pixels_per_bin);
 else
    puts("No RFC Boundary information available!");
 XCopyArea(XtDisplay(widget_struct->main_canvas), data->pix, data->pixbase, data->gc[0], 0, 0, width, height, 0, 0);


 add_overlays(w, widget_struct, NULL);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/fill_pixmap.c,v $";
 static char rcs_id2[] = "$Id: fill_pixmap.c,v 1.1 1995/09/08 14:55:23 page Exp $";}
/*  ===================================================  */

}

