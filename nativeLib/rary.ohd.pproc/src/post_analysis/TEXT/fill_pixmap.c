/*=========================================================================*/
/*                              NAME:  fill_pixmap.c                       */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:   fill_pixmap                        */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "fill_pixmap.h"
#include "post_stage3_interface.h"
#include "post_stage3_globals.h"
#include "post_stage3.h"
#include "post_drawa.h"
#include "overlay.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


/***************************************************************************/
/*  FUNCTION NAME:   fill_pixmap                                           */
/*       FUNCTION:   create display and add overlays (if any)              */
/***************************************************************************

Function type:
   void

Called by function:
   display_field
   create_rfcwide_interface
   do_zoom      
   create_ss_interface_rfcwide
   time_lapse_RFCW

Functions called:
   get_pixel_by_name
   get_vip_level
   draw_bound
   add_overlays

Local variables:
   temp - integer; color level to display
   width - Dimension structure; pixel width of DrawingArea
   height - Dimension structure;pixel height of DrawingArea
   xloc - integer; x location in pixels
   yloc - integer; y location in pixels
   backgc - GC structure; background color

******************************************** BEGIN fill_pixmap *************/

void fill_postanalysis_pixmap(w, data, call_data)
   Widget                       w;
   draw_struct                 *data;
   XmDrawingAreaCallbackStruct *call_data;
{
   int                          i, j, temp, n;
   Arg                          wargs[10];
   Dimension                    width, height;
   int                          xloc = 0, yloc;
   unsigned int                 x_pixels_per_bin, y_pixels_per_bin;
   XGCValues                    gcv;
   int                          mask = GCForeground;
   GC                           backgc;
   Display                     *dpy;

 /*--------------------------------------------------------------*/
 /*     Get the width and height of the drawing area             */
 /*--------------------------------------------------------------*/

  if (dbg) printf("in fill_pixmap\n");

  n=0;
  XtSetArg(wargs[n], XmNwidth, &width); n++;
  XtSetArg(wargs[n], XmNheight, &height); n++;
  XtGetValues(data->w, wargs, n);
  
  screenWidth = width;
  mapWidth = width;
  mapHeight = height;
  
  x_pixels_per_bin = (float)width/(float)data->maximum_columns;
  y_pixels_per_bin = (float)height/(float)data->maximum_rows;

  if (x_pixels_per_bin > y_pixels_per_bin)
      x_pixels_per_bin = y_pixels_per_bin;
  else if (y_pixels_per_bin > x_pixels_per_bin)
	   y_pixels_per_bin = x_pixels_per_bin;

 /*--------------------------------------------------------------*/
 /*     Clear the window and generate an Expose event for the    */
 /*     entire window                                            */
 /*--------------------------------------------------------------*/

  if(XtIsRealized(data->w))
     XClearArea(XtDisplay(data->w), XtWindow(w), 0, 0, 0, 0, TRUE);

 /*--------------------------------------------------------------*/
 /*     Free the old pixmap and create one the size of the window*/
 /*--------------------------------------------------------------*/

  if(data->pix) 
     XFreePixmap(XtDisplay(data->w), data->pix);
  if(data->pixbase) 
     XFreePixmap(XtDisplay(data->w), data->pixbase);

  data->pix = XCreatePixmap(XtDisplay(data->w),
			    DefaultRootWindow(XtDisplay(data->w)),
			    width, height,
			    DefaultDepthOfScreen(XtScreen(data->w))
			   );
  data->pixbase = XCreatePixmap(XtDisplay(data->w),
			    DefaultRootWindow(XtDisplay(data->w)),
			    width, height,
			    DefaultDepthOfScreen(XtScreen(data->w))
			   );

  gcv.foreground = get_pixel_by_name(w, "dim grey");
  dpy = XtDisplay(data->w);
  backgc = XCreateGC(dpy, DefaultRootWindow(dpy), mask, &gcv);
  XFillRectangle(XtDisplay(data->w), data->pix, backgc, 0, 0, width, height);


 /*--------------------------------------------------------------*/
 /*     Draw each bin in radar field - write rectangles into     */
 /*     pixmap                                                   */
 /*                                                              */
 /*     XFillRectangle locates a rectangle based on the upper    */
 /*      left corner - HRAP coord are based on lower left corner */
 /*      - the -y direction is up therefore -1 from yloc         */
 /*--------------------------------------------------------------*/

 for(i=0; i<data->maximum_columns; i++)
    {
    xloc = i * x_pixels_per_bin;
    for(j=0; j<data->maximum_rows; j++)
       {
       temp =  get_vip_level(data->num_levels, data->levels,
			     data->data_array[i][j]);
       yloc = (data->maximum_rows - j - 1) * y_pixels_per_bin;
       XFillRectangle(XtDisplay(data->w), data->pix,
		      data->gc[temp], xloc, yloc,
		      x_pixels_per_bin, y_pixels_per_bin);
       }
    }

 /*--------------------------------------------------------------*/
 /*  if site boundary is available, then add to display          */
 /*--------------------------------------------------------------*/

 if(overlay_avail.site_boundary == 1) draw_bound(data->w, data, x_pixels_per_bin, y_pixels_per_bin);

 XCopyArea(XtDisplay(data->w), data->pix, data->pixbase,
	   data->gc[0], 0, 0, width, height, 0, 0);
	   
rectangleWidth = xloc;
post_analysis_add_overlays(data);


}

/********************************************* END fill_pixmap *************/
