/* File: rubberband.c:  rubberband line example
 */
  /* Bug r20-21 */
  /* add First_Click to handle Linux mouse problem.  HP has no problems with mouse events */
  /* the order of callbacks should be Start/Track/End but the Linux always */
  /* starts with Track/Start/End order.    12/10/02 AV                      */
#include "plot.h"
#include "ifp_struct.h"
int  First_Click = 0;
void start_rubber_band(w, data, event)
  Widget                w;      /* widget data structure */
  combined_struct       *data;  /* tables and plot data structure pointer */
  XEvent                *event; /* X event structure pointer */
{
  int   xpix, ypix;       /* x, y position pixel value */
  int   ixval;            /* initial x value */
  int   its;              /* time series index in the plotted ts array */
  int   ipt;              /* index position */
  int   lpt;              /* left position */
  int   rpt;              /* right position */
  int   lastx;            /* last x position */
  int   num_pts_ratio;    /* ratio of number of points in the plot to 
                           * the number of points in the time series
                           */

  /* find point index */
  data->plot->lastx = data->plot->ipt;
  lastx = data->plot->ipt;
  ipt = data->plot->ipt = find_pt_index(data, event);
  its = data->plot->ts_index;
  num_pts_ratio = (*data->plot->num_pts -1) / 
                  (data->plot->end[data->plot->plot_index[its]] -1);
/*if(ipt < 0 || ipt > *data->plot->num_pts-1) return;*/
  if(ipt < 0 || ipt > data->plot->end[data->plot->plot_index[its]]-1) return;
  if(its < 0 || its > *data->plot->num_plotted_ts) return;

  /* if mouse moving l to r erase last rh rubberband */
  if(data->plot->lastx < data->plot->ipt)
  {
     XDrawLine(XtDisplay(w), XtWindow(w), data->plot->rb_gc,
	    data->plot->rpt.x, data->plot->rpt.y,
	    data->plot->cpt.x, data->plot->cpt.y);

     XDrawLine(XtDisplay(w), data->plot->pix[5], data->plot->rb_gc,
	    data->plot->rpt.x, data->plot->rpt.y,
	    data->plot->cpt.x, data->plot->cpt.y);
  }
 
  data->plot->lpt.x = data->plot->cpt.x;
  data->plot->lpt.y = data->plot->cpt.y;
 
  First_Click = 1;
  xpix = val_to_pixel(&data->plot->x[ipt], &data->plot->min_time_disp,
		      &data->plot->max_time_disp,
		      &data->plot->origin_x, &data->plot->h_slider_size);
  /*
  ypix = val_to_pixel(&data->plot->y[its][ipt], data->plot->miny, data->plot->maxy, Y_AX);
  */
  ypix = event->xbutton.y;
  data->plot->cpt.x = xpix * num_pts_ratio;
  data->plot->cpt.y = ypix ;

/*if(ipt >= *data->plot->num_pts - 1)
     rpt = *data->plot->num_pts - 1;
*/
  if(ipt >= data->plot->end[data->plot->plot_index[its]] - 1)
     rpt = data->plot->end[data->plot->plot_index[its]] - 1;
  else
     rpt = ipt + 1;

  xpix = val_to_pixel(&data->plot->x[rpt], &data->plot->min_time_disp,
		      &data->plot->max_time_disp,
		      &data->plot->origin_x, &data->plot->h_slider_size);
  ypix = val_to_pixel(&data->plot->y[its][rpt-1], &data->plot->min_discharge_disp,
		      &data->plot->max_discharge_disp,
		      &data->plot->v_slider_size, &data->plot->end_y);

  data->plot->rpt.x = xpix * num_pts_ratio;
  data->plot->rpt.y = ypix;

  /*
  if(data->plot->start_end_sw == START ||
    (lastx > data->plot->ipt && data->plot->start_end_sw != START))
  */
  if(data->plot->start_end_sw == START ||
    (lastx > data->plot->ipt && data->plot->start_end_sw != START))
     {
     data->plot->start_end_sw = DONE;

     if(ipt <= 0)
	lpt = 0;
     else
	lpt = ipt - 1;

     xpix = val_to_pixel(&data->plot->x[lpt], &data->plot->min_time_disp,
			 &data->plot->max_time_disp,
			 &data->plot->origin_x, &data->plot->h_slider_size);
     ypix = val_to_pixel(&data->plot->y[its][lpt-1],
			 &data->plot->min_discharge_disp,
			 &data->plot->max_discharge_disp,
			 &data->plot->v_slider_size, &data->plot->end_y);

     data->plot->lpt.x = xpix * num_pts_ratio;
     data->plot->lpt.y = ypix;

     }else{  /* new */


          lpt = ipt -1;
          if(ipt <= 0)lpt = 0;
     }


  /* draw left side */
  XDrawLine(XtDisplay(w), XtWindow(w),
	    data->plot->rb_gc, data->plot->lpt.x, data->plot->lpt.y, data->plot->cpt.x, data->plot->cpt.y);

  /* draw right side */
  XDrawLine(XtDisplay(w), XtWindow(w),
	    data->plot->rb_gc, data->plot->rpt.x, data->plot->rpt.y, data->plot->cpt.x, data->plot->cpt.y);
}

void track_rubber_band(w, data, event)
  Widget                w;
  combined_struct       *data;
  XEvent                *event;
{
  /* add First_Click to handle Linux mouse problem.  HP has no problem      */
  /* the order of callbacks should be Start/Track/End but the Linux always */
  /* starts with Track/Start/End order.    12/10/02 AV                      */
 
  if(First_Click == 0 ) return;   
 
 
 /*
  * Draw once to clear the previous line.
  */


  XDrawLine(XtDisplay(w), XtWindow(w), data->plot->rb_gc,
	    data->plot->lpt.x, data->plot->lpt.y,
	    data->plot->cpt.x, data->plot->cpt.y);

  XDrawLine(XtDisplay(w), XtWindow(w), data->plot->rb_gc,
	    data->plot->rpt.x, data->plot->rpt.y,
	    data->plot->cpt.x, data->plot->cpt.y);

 /*
  * Update the end points.
  */
  /*data->plot->cpt.x = event->xbutton.x;*/
  data->plot->cpt.y = event->xbutton.y;
 /*
  * Draw the new line.
  */

  XDrawLine(XtDisplay(w), XtWindow(w), data->plot->rb_gc,
	    data->plot->lpt.x, data->plot->lpt.y,
	    data->plot->cpt.x, data->plot->cpt.y);

  XDrawLine(XtDisplay(w), XtWindow(w), data->plot->rb_gc,
	    data->plot->rpt.x, data->plot->rpt.y,
	    data->plot->cpt.x, data->plot->cpt.y);
}

void end_rubber_band(w, data, event)
  Widget                w;
  combined_struct       *data;
  XEvent                *event;
{
   int          ypix;
 /*
  * Clear the current line and update the end point info.
  */
  
  /* add First_Click to handle Linux mouse problem.  HP has no problem      */
  /* the order of callbacks should be Start/Track/End but the Linux always */
  /* starts with Track/Start/End order.    12/10/02 AV                      */
  First_Click = 0;


/*    printf("in end_rubber_band, start=(%3d, %3d), end=(%3d, %3d)\n",
            data->plot->start_x, data->plot->start_y,
            data->plot->last_x, data->plot->last_y);
*/
/*
 * Write last line segments to pixmap -
 * They have already been written to window in track_rubber_band
 */
  XDrawLine(XtDisplay(w), data->plot->pix[5], data->plot->rb_gc,
	    data->plot->lpt.x, data->plot->lpt.y,
	    data->plot->cpt.x, data->plot->cpt.y);

  XDrawLine(XtDisplay(w), data->plot->pix[5], data->plot->rb_gc,
	    data->plot->rpt.x, data->plot->rpt.y,
	    data->plot->cpt.x, data->plot->cpt.y);

  /* assign new value to y coordinate */
  ypix = (int)data->plot->cpt.y;
  data->plot->y[data->plot->ts_index][data->plot->ipt-1] = pixel_to_val(&ypix,
			     &data->plot->min_discharge_disp,
			     &data->plot->max_discharge_disp,
			     &data->plot->v_slider_size,
			     &data->plot->end_y);

  if(data->tables->menu_index >= 0)
     data->tables->changed[data->tables->menu_index][data->plot->ipt-1] = 1;

  /* if mouse moved left to right, interpolate intermediate values */

  if(data->plot->lastx < data->plot->ipt)
     interpolate(data);
}

int find_pt_index(data, event)
  combined_struct       *data;
  XEvent                *event;
    {
    int         ixval;
    float       fxval;
    int         time_series_num_pts_ratio;
    float       num_pts_ratio;
    int         its;

    ixval = event->xbutton.x;
    fxval = pixel_to_val(&ixval, &data->plot->min_time_disp,
			 &data->plot->max_time_disp,
			 &data->plot->origin_x, &data->plot->h_slider_size);
    its = data->plot->ts_index;
    time_series_num_pts_ratio = (*data->plot->num_pts -1) / 
                                (data->plot->end[data->plot->plot_index[its]] -1);
    num_pts_ratio = (float)time_series_num_pts_ratio;
    fxval = fxval / num_pts_ratio;
    ixval = (int)fxval;
  
    if(fxval - ixval >= 0.5)
       ixval++;
    if(ixval < 0) ixval = 0;
   
/*  if(ixval > *data->plot->num_pts-1) ixval = *data->plot->num_pts - 1; */
    if(ixval > data->plot->end[data->plot->plot_index[its]]-1) 
       ixval = data->plot->end[data->plot->plot_index[its]] - 1; 
    return ixval;
    }

void interpolate(data)
   combined_struct       *data;
      {
      int       i;
      float     qqq, Fks, Fes;

/*    for(i = data->plot->lastx + 1; i < data->plot->ipt; i++) */
      for(i = data->plot->lastx + 1; i < data->plot->ipt; i++)
	 {
	 Fks = i - data->plot->lastx;
	 Fes = data->plot->ipt - data->plot->lastx;
	 qqq = Fks/Fes;
	 data->plot->y[data->plot->ts_index][i-1] =
	       data->plot->y[data->plot->ts_index][data->plot->lastx-1]
	       + qqq*(data->plot->y[data->plot->ts_index][data->plot->ipt-1]
	       - data->plot->y[data->plot->ts_index][data->plot->lastx-1]);
	 if(data->tables->menu_index >= 0)
	    data->tables->changed[data->tables->menu_index][i-1] = 1;
	 }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rubberband.c,v $";
 static char rcs_id2[] = "$Id: rubberband.c,v 1.5 2003/03/14 18:39:54 dws Exp $";}
/*  ===================================================  */

      }
