/* File: undo_change_ts.c
 *
 * Sets hydrograph area to insensitive and removes event handlers
 * for rubberbanding.
 *
 * Reactivates the event handler for crosshairs.
 *
 * Resets scrollbars, sliders and hydrograph to sensitive and
 * resets ts_change_flag.
 *
 */


#include "plot.h"
#include "ifp_struct.h"
extern void draw_info_lines(Widget, combined_struct *);
extern void tschng_mod_made(combined_struct *);

void undo_change_ts(w, data, call_data)
   Widget                       w;       /* widget data structure */
   combined_struct              *data;   /* tables and plot data structure pointer */
   XmAnyCallbackStruct          *call_data;
{
   Arg       wargs[10];
   XGCValues line_gcv;
   int       i, n;                       /* counters */
   Pixel     foreground, background;
   Window        pixmap_root;
   int           pixmap_x, pixmap_y;
   unsigned int  pixmap_width, pixmap_height;
   unsigned int  pixmap_border_width, pixmap_depth;

  /* printf("in undo_change_ts\n"); */

   if(call_data->event->xbutton.window != XtWindow(w))
   {
           return;
   }
   /* Set hydrograph area to insensitive and remove event handlers
      for rubberbanding.
   */
   XtSetSensitive(data->plot->drawing_area_widget[5], FALSE);

   XtRemoveEventHandler(data->plot->drawing_area_widget[5],
		     ButtonPressMask, FALSE, start_rubber_band, data);
   XtRemoveEventHandler(data->plot->drawing_area_widget[5],
		     ButtonMotionMask, FALSE, track_rubber_band, data);
   XtRemoveEventHandler(data->plot->drawing_area_widget[5],
		     ButtonReleaseMask, FALSE, end_rubber_band, data);

   /*
    * Reactivate the event handler for crosshairs.
    */
   if(data->plot->num_rr_oper > 0)
     {
      XtAddEventHandler(data->plot->drawing_area_widget[1],
			ButtonPressMask, FALSE, crosshairs_timer, data);
      XtAddEventHandler(data->plot->drawing_area_widget[3],
			ButtonPressMask, FALSE, crosshairs_timer, data);
     }
   XtAddEventHandler(data->plot->drawing_area_widget[5],
		     ButtonPressMask, FALSE, crosshairs_timer, data);

   for(i=0; i<data->plot->end[data->plot->plot_index[data->plot->ts_index]]; i++)
      data->plot->y[data->plot->ts_index][i] =
      data->plot->orig_ts_array[data->plot->plot_index[data->plot->ts_index]][i];

   line_gcv.foreground = get_pixel_by_name(w,
			       data->plot->ts_color[data->plot->ts_index]);
   XChangeGC(XtDisplay(data->plot->drawing_area_widget[5]),
	     data->plot->line_gc[data->plot->ts_index], GCForeground, &line_gcv);
   draw_graph_line(data->plot->drawing_area_widget[5], data, data->plot->ts_index);

   for(i = 0; i < *data->plot->num_plotted_ts; i++)
   {
      XtSetSensitive(data->plot->legend[i], TRUE);
      n=0;
      XtSetArg(wargs[n], XmNbackground,
	       get_pixel_by_name(data->plot->legend[i], "gray85")); n++;
      XtSetValues(data->plot->legend[i], wargs, n);
   }

   n=0;
   XtSetArg(wargs[n], XmNbackground, &background); n++;
   XtGetValues(data->plot->drawing_area_widget[5], wargs, n);
   foreground = line_gcv.foreground;
   line_gcv.foreground = background;
   XChangeGC(XtDisplay(data->plot->drawing_area_widget[5]),
	     data->plot->line_gc[data->plot->ts_index], GCForeground, &line_gcv);
   XGetGeometry(XtDisplay(w), data->plot->pix[5],
		&pixmap_root, &pixmap_x, &pixmap_y,
		&pixmap_width, &pixmap_height,
		&pixmap_border_width, &pixmap_depth);

   XFillRectangle(XtDisplay(data->plot->drawing_area_widget[5]), data->plot->pix[5],
		  data->plot->line_gc[data->plot->ts_index], 0, 0, pixmap_width,
		  pixmap_height);

   line_gcv.foreground = foreground;
   XChangeGC(XtDisplay(data->plot->drawing_area_widget[5]),
	     data->plot->line_gc[data->plot->ts_index], GCForeground, &line_gcv);

   draw_axes(data->plot->drawing_area_widget[7], data);
   draw_lines(data->plot->drawing_area_widget[5], data, NULL);
   draw_info_lines(data->plot->drawing_area_widget[5], data);

   XClearArea(XtDisplay(data->plot->drawing_area_widget[5]),
			XtWindow(data->plot->drawing_area_widget[5]),
			0, 0, 1, 1, TRUE);

   /*  Reset scrollbars, sliders and hydrograph to sensitive
       and reset ts_change_flag.
   */
   XtSetSensitive(data->plot->vertical_scale_widget, TRUE);
   XtSetSensitive(data->plot->horiz_scale_widget, TRUE);
   XtSetSensitive(data->plot->vertical_scrollbar_widget, TRUE);
   XtSetSensitive(data->plot->horiz_scrollbar_widget, TRUE);

   XtSetSensitive(data->plot->undo_widget, FALSE);
   XtSetSensitive(data->plot->drawing_area_widget[5], TRUE);

   data->plot->ts_change_flag = START;

   if(data->tables->menu_index > -1)
      for(i=0; i<data->plot->end[data->plot->plot_index[data->plot->ts_index]]; i++)
	 data->tables->changed[data->tables->menu_index][i] = 0;
   update_table(data);

   /* reset flag for tschng_mod_made */
   tschng_mod_made(data);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/PlotTulsa/RCS/undo_change_ts.c,v $";
 static char rcs_id2[] = "$Id: undo_change_ts.c,v 1.4 2007/05/16 16:43:41 aivo Exp $";}
/*  ===================================================  */

}
