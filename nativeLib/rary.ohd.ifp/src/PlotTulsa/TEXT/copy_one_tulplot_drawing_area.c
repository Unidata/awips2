#include "plot.h"
#include "ifp_struct.h"

/* File: copy_one_tulplot_drawing_area.c
 *
 * This function handles expose events for any drawing area within
 *  the graphical Tulsa plot display.
 *
 * When the event window matches window of current drawing area
 *  and event count eq 0 (i.e., at end of series of exposes),
 *  copy from the pixmap to the window.
 *
 * Figure out which drawing area's pixmap to copy by matching
 *  the widget w with the drawing_area_widgets in the cb_struct.
 *
 * Use horiz scrollbar value for px_x_axis, ro_x_axis,
 *  and hydrograph.
 * Use vertical scrollbar value for discharge_axis,
 *  hydrograph, and stage_axis.
 * Sizes of px_y_axis and ro_y_axis don't change based
 *  on scrollbars or scale widgets.
 */

void copy_one_tulplot_drawing_area(w, data, call_data)

  Widget                        w;           /* widget data structure */
  plot_cb_struct                *data;       /* call back graph data structure pointer */
  XmDrawingAreaCallbackStruct   *call_data;  /* call back structure pointer */
{
  XExposeEvent  *event = (XExposeEvent *) call_data->event; /* expose event pointer */
  int           n;                                          /* window resource array index */
  int           horiz_scrollbar_value;                      /* horizontal scrollbar value  */
  int           vertical_scrollbar_value;                   /* veritcal scrollbar value    */
  int           vertical_slider_size, horiz_slider_size;    /* veritcal & horizontal slider size */
  Arg           wargs[5];                                   /* window resource data structure array */
  Dimension     width, height;                              /* window width & height */
  int           pixel_val;                                  /* pixel position        */

/*   printf("in copy_one_tulplot_drawing_area\n"); */

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
     XtGetValues(data->vertical_scrollbar_widget, wargs, n);

     n=0;
     XtSetArg(wargs[n], XmNvalue, &horiz_scrollbar_value); n++;
     XtSetArg(wargs[n], XmNsliderSize, &horiz_slider_size); n++;
     XtGetValues(data->horiz_scrollbar_widget, wargs, n);

/*  Reset the minimum and maximum discharge and time values displayed
    for the new data so mousetracker prints out correct values.
*/
     pixel_val = vertical_scrollbar_value + vertical_slider_size;
     data->min_discharge_disp = pixel_to_val(&pixel_val, data->min_y,
					     data->discharge_axis_max,
					     &data->origin_y, &data->end_y);

     pixel_val = vertical_scrollbar_value;
     data->max_discharge_disp = pixel_to_val(&pixel_val, data->min_y,
					     data->discharge_axis_max,
					     &data->origin_y, &data->end_y);

     pixel_val = horiz_scrollbar_value;
     data->min_time_disp = pixel_to_val(&pixel_val, data->min_x,
					data->max_x,
					&data->origin_x, &data->end_x);

     pixel_val = horiz_scrollbar_value + horiz_slider_size;
     data->max_time_disp = pixel_to_val(&pixel_val, data->min_x,
					data->max_x,
					&data->origin_x, &data->end_x);


/*
 * Copy appropriate part of pixmap into the window,
 *  depending on which drawing area we are currently copying.
 */
     if(w == data->px_label_da_widget)         /* px_label */
       {
	XCopyArea(XtDisplay(w), data->px_label_pix,
		  XtWindow(w),  data->px_label_gc,
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[0])     /* px_y_axis */
       {
	XCopyArea(XtDisplay(w), data->pix[0], XtWindow(w), data->gc[0],
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[1])     /* px_x_axis */
       {
	XCopyArea(XtDisplay(w), data->pix[1], XtWindow(w), data->gc[1],
		  horiz_scrollbar_value, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->ro_label_da_widget)         /* ro_label */
       {
	XCopyArea(XtDisplay(w), data->ro_label_pix,
		  XtWindow(w),  data->ro_label_gc,
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[2])     /* ro_y_axis */
       {
	XCopyArea(XtDisplay(w), data->pix[2], XtWindow(w), data->gc[2],
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[3])     /* ro_x_axis */
       {
	XCopyArea(XtDisplay(w), data->pix[3], XtWindow(w), data->gc[3],
		  horiz_scrollbar_value, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->discharge_label_da_widget)         /* discharge_label */
       {
	XCopyArea(XtDisplay(w), data->discharge_label_pix,
		  XtWindow(w),  data->discharge_label_gc,
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[4])     /* discharge_axis */
       {
	XCopyArea(XtDisplay(w), data->pix[4], XtWindow(w), data->gc[4],
		  0, vertical_scrollbar_value,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[5])     /* hydrograph */
       {
	XCopyArea(XtDisplay(w), data->pix[5], XtWindow(w), data->gc[5],
		  horiz_scrollbar_value, vertical_scrollbar_value,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[6])     /* stage_axis */
       {
	XCopyArea(XtDisplay(w), data->pix[6], XtWindow(w), data->gc[6],
		  0, vertical_scrollbar_value,
		  width, height,
		  0, 0);
       }
     else if(w == data->stage_label_da_widget)         /* stage_label */
       {
	XCopyArea(XtDisplay(w), data->stage_label_pix,
		  XtWindow(w),  data->stage_label_gc,
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[7])     /* x_axis */
       {
	XCopyArea(XtDisplay(w), data->pix[7], XtWindow(w), data->gc[7],
		  horiz_scrollbar_value, 0,
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
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/copy_one_tulplot_drawing_area.c,v $";
 static char rcs_id2[] = "$Id: copy_one_tulplot_drawing_area.c,v 1.1 1995/09/08 14:57:07 page Exp $";}
/*  ===================================================  */

}
