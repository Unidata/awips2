
   
/* File: mp_rubberband.c:  rubberband line example
 */

#include "mods_plot.h"

/*
 * mp_rubberband.c:  rubberband line example for mods_plot
 */
  /* For bug r20-21 */
  /* add FirstClick to handle Linux mouse problem.  HP has no problems with mouse events */
  /* the order of callbacks should be Start/Track/End but the Linux always */
  /* starts with Track/Start/End order.    8/30/02 AV                      */
 
int  FirstClick = 0;
void mp_start_rubber_band(w, data, event)
  Widget                w;      /* widget data structure        */
  mods_plot_struct      *data;  /* mods plot data structure pointer     */
  XEvent                *event; /* X event structure pointer    */
{
  int   xpix, ypix;     /* X, y position pixel value    */
  int   its;            /* Time series index    */
  int   ipt;            /* Index position       */
  int   lpt;            /* Left position        */
  int   rpt;            /* Right position       */
  int   lastx;          /* Last x position      */
  int   i;              /* Counter              */
  int   x_offset;       /* X position from the label border     */
  float *x;             /* Pointer to x array           */

  /* Set up x array for the number of the point along the axis */
  
 
  x = (float *)malloc(data->num_pts * sizeof(float));
  for(i = 0; i < data->num_pts; i++)
     x[i] = (float)i;

  /* Determine the x_offset for drawing lines between points */
  x_offset = ((data->end_x - data->origin_x) /
	      (float)(data->num_pts-1) * 0.25) +
	      data->bar_width/2 + (data->ts_index * data->bar_width);
  /* find point index */
  
  data->lastx = data->ipt;
  
  lastx = data->ipt;
  ipt = data->ipt = mp_find_pt_index(data, event);
  its = data->ts_index;
  if(ipt < 0 || ipt > data->num_pts-1) return;
  if(its < 0 || its > data->num_ts_sel) return;

  /* if mouse moving l to r erase last rh rubberband */
  if(data->lastx < data->ipt)
  {   
     XDrawLine(XtDisplay(w), XtWindow(w), data->rb_gc,
	       data->rpt.x, data->rpt.y,
	       data->cpt.x, data->cpt.y);
    
     XDrawLine(XtDisplay(w), data->pix[2], data->rb_gc,
	       data->rpt.x, data->rpt.y,
	       data->cpt.x, data->cpt.y);  
  }
  
/*
  data->lpt.x = data->cpt.x;
  data->lpt.y = data->cpt.y; 
*/
  FirstClick = 1;
  xpix = val_to_pixel(&x[ipt], &data->min_x,
		      &data->max_x,
		      &data->origin_x, &data->end_x);
  ypix = event->xbutton.y;
  data->cpt.x = xpix + x_offset;
  data->cpt.y = ypix;
   
  if(ipt >= data->num_pts - 1)
     rpt = data->num_pts - 1;
  else
     rpt = ipt + 1;

  xpix = val_to_pixel(&x[rpt], &data->min_x,
		      &data->max_x,
		      &data->origin_x, &data->end_x);
                      
  ypix = val_to_pixel(&data->ts_array[its][rpt], &data->min_y,
		      &data->y_axis_max,
		      &data->origin_y, &data->end_y);

  data->rpt.x = xpix + x_offset;
  data->rpt.y = ypix;
  
  if(data->start_end_sw == START ||
    (lastx > data->ipt && data->start_end_sw != START))
  {
     data->start_end_sw = DONE;
     
     if(ipt <= 0)
	lpt = 0;
     else
	lpt = ipt - 1;
     
     xpix = val_to_pixel(&x[lpt], &data->min_x,
			 &data->max_x,
			 &data->origin_x, &data->end_x);
     ypix = val_to_pixel(&data->ts_array[its][lpt], &data->min_y,
			 &data->y_axis_max,
			 &data->origin_y, &data->end_y);
    
     data->lpt.x = xpix + x_offset;
     data->lpt.y = ypix;
 
   }else{
     
        lpt = ipt -1;
        if(ipt <= 0)lpt = 0;  
      
    
   }

  
 
  /* draw left side */
  XDrawLine(XtDisplay(w), XtWindow(w),
	    data->rb_gc, data->lpt.x, data->lpt.y, data->cpt.x, data->cpt.y);           
  
  /* draw right side */
  XDrawLine(XtDisplay(w), XtWindow(w),
	    data->rb_gc, data->rpt.x, data->rpt.y, data->cpt.x, data->cpt.y);

}

void mp_track_rubber_band(w, data, event)
  Widget                 w;
  mods_plot_struct       *data;
  XEvent                 *event;
  
{
  /* add FirstClick to handle Linux mouse problem.  HP has no problem      */
  /* the order of callbacks should be Start/Track/End but the Linux always */
  /* starts with Track/Start/End order.    8/30/02 AV                      */
 
  if(FirstClick == 0 ) return;               

  /*
  * Draw once to clear the previous line.
  */  
  XDrawLine(XtDisplay(w), XtWindow(w),
	    data->rb_gc, data->lpt.x, data->lpt.y, data->cpt.x, data->cpt.y);
           
 
  /* draw right side */
  XDrawLine(XtDisplay(w), XtWindow(w),
	    data->rb_gc, data->rpt.x, data->rpt.y, data->cpt.x, data->cpt.y);
  
 /*
  * Update the end points.
  */
  
  data->cpt.y = event->xbutton.y;
 
 /*
  * Draw the new line.
  */

  XDrawLine(XtDisplay(w), XtWindow(w),
	    data->rb_gc, data->lpt.x, data->lpt.y, data->cpt.x, data->cpt.y);
            
 
 /* draw right side */
  XDrawLine(XtDisplay(w), XtWindow(w),
	    data->rb_gc, data->rpt.x, data->rpt.y, data->cpt.x, data->cpt.y);
            
            
  
}
extern int Yval;
void mp_end_rubber_band(w, data, event)
  Widget                 w;
  mods_plot_struct       *data;
  XEvent                 *event;
{
   int          ypix;
 /*
  * Clear the current line and update the end point info.
  */


  /* add FirstClick to handle Linux mouse problem.  HP has no problem      */
  /* the order of callbacks should be Start/Track/End but the Linux always */
  /* starts with Track/Start/End order.    8/30/02 AV                      */
  FirstClick = 0;
  /*
   * Write last line segments to pixmap -
   * They have already been written to window in track_rubber_band
  */
  XDrawLine(XtDisplay(w), data->pix[2], data->rb_gc,
	    data->lpt.x, data->lpt.y,
	    data->cpt.x, data->cpt.y);
 
  XDrawLine(XtDisplay(w), data->pix[2], data->rb_gc,
	    data->rpt.x, data->rpt.y,
	    data->cpt.x, data->cpt.y);
  /* save previous current mouse click to left point */
  data->lpt.x = data->cpt.x;
  data->lpt.y = data->cpt.y; 
  
  /* assign new value to y coordinate */
  ypix = (int)data->cpt.y;
  data->ts_array[data->ts_index][data->ipt] = pixel_to_val(&ypix,
						   &data->min_y,
						   &data->y_axis_max,
						   &data->origin_y,
						   &data->end_y);

  /* if mouse moved left to right, interpolate intermediate values */
  
  if(data->lastx < data->ipt){
     mp_interpolate(data);
  }

 
}

int mp_find_pt_index(data, event)
  mods_plot_struct       *data;
  XEvent                 *event;
  {
    int         ixval;
    float       fxval;

    ixval = event->xbutton.x;
    fxval = pixel_to_val(&ixval, &data->min_x,
			 &data->max_x,
			 &data->origin_x, &data->end_x);
    ixval = (int)fxval;
    /*
    if(fxval - ixval >= 0.5)
       ixval++;
    */
    if(ixval < 0) ixval = 0;
    if(ixval > data->num_pts-1) ixval = data->num_pts - 1;
    
    return ixval;
  }

void mp_interpolate(data)
   mods_plot_struct       *data;
      {
      int       i;
      float     qqq, Fks, Fes;
   
      for(i = data->lastx + 1; i < data->ipt; i++)
      {
	 Fks = i - data->lastx;
	 Fes = data->ipt - data->lastx;
	 qqq = Fks/Fes;
	 data->ts_array[data->ts_index][i] =
	       data->ts_array[data->ts_index][data->lastx]
	       + qqq*(data->ts_array[data->ts_index][data->ipt]
	       - data->ts_array[data->ts_index][data->lastx]);

       /*
	 if(data->tables->menu_index >= 0)
	    data->tables->changed[data->tables->menu_index][i] = 1;
       */
      }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_rubberband.c,v $";
 static char rcs_id2[] = "$Id: mp_rubberband.c,v 1.3 2003/03/14 18:38:29 dws Exp $";}
/*  ===================================================  */

      }
