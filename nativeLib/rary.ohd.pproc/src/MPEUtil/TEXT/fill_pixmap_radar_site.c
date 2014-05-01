/*******************************************************************************
* FILENAME:				fill_pixmap_radar_site.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:  This routine is called to display MPE Single Radar Site.
*                       It's using MPE drawing routines and MPE pixmap 
*			to draw four pane MPE Single Radar Site window. 
*			
*   MODULE 1:		fill_pixmap_radar_site 
* DESCRIPTION:		This routine creates Mpe Single Radar Site display and            
*                   	add overlays (if any).               
*
* ORGANIZATION:         OHD / HSEB
* MACHINE:		HP Unix / Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*	   1     4/03/2002   Moria Shebsovich         Revision
********************************************************************************
*/

/*=========================================================================*/
/*                    FILE PATH/NAME:  mpe_util/fill_pixmap_radar_site     */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN MODULE #1:   fill_pixmap_radar_site             */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "stage3_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "drawa.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


/***************************************************************************/
/* FILE PATH/NAME:   mpe_util/fill_pixmap_radar_site.c                     */
/*  FUNCTION NAME:   fill_pixmap_radar_site                                */
/*       FUNCTION:   creates Mpe Single Radar Site display and             */
/*                   add overlays (if any)                                 */
/***************************************************************************

Function type:
   void

Called by function:
   create_ss_interface_rfcwide
   
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

************************************** BEGIN fill_pixmap_radar_site ***********/

extern int ss_window_changed ;

void fill_pixmap_radar_site ( Widget w, draw_struct *data, 
			XmDrawingAreaCallbackStruct * call_data )
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
  XFreeGC ( XtDisplay ( data->w ) , backgc ) ;


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

 /*draw_bound(data->w, data, x_pixels_per_bin, y_pixels_per_bin);*/
 XCopyArea(XtDisplay(data->w), data->pix, data->pixbase,
	   data->gc[0], 0, 0, width, height, 0, 0);
	   
 rectangleWidth = xloc;
 ss_window_changed = 1 ;

}

/********************************************* END fill_pixmap_radar_site *************/

