#include "rating_curve.h"

/* File: copy_one_rc_drawing_area.c
 *
 * This function handles expose events for any drawing area within
 * the graphical rating curve plot display.
 *
 * When the event window matches window of current drawing area
 * and event count eq 0 (i.e., at end of series of exposes),
 * copy from the pixmap to the window.
 *
 * Figures out which drawing area's pixmap to copy by matching
 * the widget w with the drawing_area_widgets in rc_struct.
 *
 * Uses the horiz scrollbar value for rc_x_axis, rc_graph.
 *
 * Uses the vertical scrollbar value for rc_y_axis, and rc_graph.
 *
 */

void copy_one_rc_drawing_area(w, data, call_data)

  Widget                        w;             /* widget data structure */
  rc_struct                     *data;         /* rating curve data structure pointer */
  XmDrawingAreaCallbackStruct   *call_data;    /* window data structure pointer */
{
  XExposeEvent  *event = (XExposeEvent *) call_data->event; /* expose event pointer */
  int           n;                                          /* window resource array index */
  int           horiz_scrollbar_value;                      /* horizontal scrollbar value  */
  int           vertical_scrollbar_value;                   /* veritcal scrollbar value    */
  int           vertical_slider_size, horiz_slider_size;    /* veritcal & horizontal slider size */
  Arg           wargs[5];                                   /* window resource data structure array */
  Dimension     width, height;                              /* window width & height */
  int           pixel_val;                                  /* pixel position        */

  /* printf("in copy_one_rc_drawing_area\n"); */

  if(event->window == XtWindow(w) && event->count == 0)
    {
/*
 * Last event in series.
 * Get width and height of widget's window.
*/
     n=0;
     XtSetArg(wargs[n], XmNwidth, &width); n++;
     XtSetArg(wargs[n], XmNheight, &height); n++;
     XtGetValues(w, wargs, n);

/*
 * Get values of sliders on scrollbars.
 */
     n=0;
     XtSetArg(wargs[n], XmNvalue, &vertical_scrollbar_value); n++;
     XtSetArg(wargs[n], XmNsliderSize, &vertical_slider_size); n++;
     XtGetValues(data->rc_vert_scrollbar_widget, wargs, n);

     n=0;
     XtSetArg(wargs[n], XmNvalue, &horiz_scrollbar_value); n++;
     XtSetArg(wargs[n], XmNsliderSize, &horiz_slider_size); n++;
     XtGetValues(data->rc_horiz_scrollbar_widget, wargs, n);

/*  Reset the minimum and maximum discharge and stage values displayed
    for the new data so mousetracker prints out correct values.
*/
     pixel_val = vertical_scrollbar_value + vertical_slider_size;
     data->min_stage_disp = pixel_to_val(&pixel_val, &data->min_stg,
					 &data->stg_axis_max,
					 &data->origin_y, &data->end_y);

     pixel_val = vertical_scrollbar_value;
     data->max_stage_disp = pixel_to_val(&pixel_val, &data->min_stg,
					 &data->stg_axis_max,
					 &data->origin_y, &data->end_y);

     pixel_val = horiz_scrollbar_value;
     data->min_q_disp = pixel_to_val(&pixel_val, &data->min_q,
				     &data->q_axis_max,
				     &data->origin_x, &data->end_x);

     pixel_val = horiz_scrollbar_value + horiz_slider_size;
     data->max_q_disp = pixel_to_val(&pixel_val, &data->min_q,
				     &data->q_axis_max,
				     &data->origin_x, &data->end_x);

/*
 * Copy appropriate part of pixmap into the window,
 *  depending on which drawing area we are currently copying.
 */
     if(w == data->drawing_area_widget[0])      /* y_axis_label */
       {
	XCopyArea(XtDisplay(w), data->pix[0],
		  XtWindow(w),  data->gc[0],
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[1])     /* y_axis */
       {
	XCopyArea(XtDisplay(w), data->pix[1], XtWindow(w), data->gc[1],
		  0, vertical_scrollbar_value,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[2])     /* x_axis */
       {
	XCopyArea(XtDisplay(w), data->pix[2], XtWindow(w), data->gc[2],
		  horiz_scrollbar_value, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[3])     /* x_axis_label */
       {
	XCopyArea(XtDisplay(w), data->pix[3], XtWindow(w), data->gc[3],
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[4])     /* graph */
       {
	XCopyArea(XtDisplay(w), data->pix[4], XtWindow(w), data->gc[4],
		  horiz_scrollbar_value, vertical_scrollbar_value,
		  width, height,
		  0, 0);
       }
     else
       {
	printf("Exposed widget does not match TulPlot drawing area");
	printf(" -- BIG PROBLEM\n");
	exit(1);
       }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/copy_one_rc_drawing_area.c,v $";
 static char rcs_id2[] = "$Id: copy_one_rc_drawing_area.c,v 1.1 1995/09/08 14:57:06 page Exp $";}
/*  ===================================================  */

}
