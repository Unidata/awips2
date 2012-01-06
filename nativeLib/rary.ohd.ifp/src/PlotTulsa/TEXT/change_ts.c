#include "plot.h"
#include "ifp_struct.h"

/* File: change_ts.c
 *
 * Allows for the selection and modification of time series plots.
 * Sets scrollbars and sliders insensitive during the change process.
 *
 * Removes the crosshairs event handler when rubberbanding.
 *
 * When necessary, calls routine to change table values to match
 * new plotted values.
 *
 */
extern void find_menu_index( combined_struct *);
extern void draw_info_lines(Widget, combined_struct *);
extern void tschng_mod_made(combined_struct *);

void change_ts(w, data, call_data)
   Widget                       w;            /* widget data structure  */
   combined_struct              *data;        /* tables and plot data structure pointer  */
   XmAnyCallbackStruct          *call_data;   /* call back window data structure pointer */
{
   Arg       wargs[10];          /* window resource data structure array */
   XGCValues line_gcv;           /* graphics context data structures     */
   XmString  cmpd_label_str;     /* compound label string                */
   int       i, n;               /* counters                             */
   int       ts_index = -1;      /* time series index                    */
   char      *label_str;         /* label string pointer                 */
   Pixel     foreground;         /* window foreground color              */
   Pixel     background;         /* window background color              */
   Window    pixmap_root;        /* root window structure                */
   int       pixmap_x;           /* window x position                    */
   int       pixmap_y;           /* window y position                    */
   unsigned int pixmap_width;    /* window width                         */
   unsigned int pixmap_height;   /* window height                        */
   unsigned int pixmap_border_width;  /* window border width */
   unsigned int pixmap_depth;         /* window depth        */

  /* printf("in change_ts\n"); */

/* Set scrollbars and sliders insensitive during change_ts operation.
   Reset later.
*/
   XtSetSensitive(data->plot->vertical_scale_widget, FALSE);
   XtSetSensitive(data->plot->horiz_scale_widget, FALSE);
   XtSetSensitive(data->plot->vertical_scrollbar_widget, FALSE);
   XtSetSensitive(data->plot->horiz_scrollbar_widget, FALSE);

/* Set undo_widget to sensitive to allow user to undo time series now.
   Reset later.
*/
   XtSetSensitive(data->plot->undo_widget, TRUE);

   /* get the time series name and index: */
   n=0;
   XtSetArg(wargs[n], XmNlabelString, &cmpd_label_str); n++;
   XtGetValues(w, wargs, n);

   label_str = xs_get_string_from_xmstring(cmpd_label_str);
   for(i = 0; i < *data->plot->num_plotted_ts; i++)
   {
      if(strcmp(label_str, data->plot->ts_name[i]) == 0)
	 ts_index = i;
      else
      {
	 XtSetSensitive(data->plot->legend[i], FALSE);
	 n=0;
	 XtSetArg(wargs[n], XmNbackground,
		  get_pixel_by_name(data->plot->legend[i], "white")); n++;
	 XtSetValues(data->plot->legend[i], wargs, n);
      }
   }
   data->plot->ts_index = ts_index;
   
   /* add check to see if table has been created - added to fix when
      ifp_num_columns apps_default set to 0   dp - 29 Oct. 96
   */
   if(data->tables->table_created == TRUE)
      find_menu_index(data);
   else
      data->tables->menu_index = -1;

   if(ts_index < 0)
   {
      for(i = 0; i < *data->plot->num_plotted_ts; i++)
	 XtSetSensitive(data->plot->legend[i], TRUE);
      return;
   }

   if(ts_index > *data->plot->num_plotted_ts) return;

   ts_index = data->plot->ts_index;

   if(data->plot->ts_change_flag == START)
   {
      data->plot->ts_change_flag = DONE;
      /*printf("---------->change- data->lpt.x=%d data->rpt.x=%d data->cpt.x=%d\n",
             data->plot->lpt.x,data->plot->rpt.x,data->plot->cpt.x); 
      */  
      data->plot->lpt.x = 0;
      data->plot->rpt.x = 0;
      data->plot->cpt.x = 0;

      /* prepare for rubberbanding */
      XtAddEventHandler(data->plot->drawing_area_widget[5],
			ButtonPressMask, FALSE, start_rubber_band, data);
      XtAddEventHandler(data->plot->drawing_area_widget[5],
			ButtonMotionMask, FALSE, track_rubber_band, data);
      XtAddEventHandler(data->plot->drawing_area_widget[5],
			ButtonReleaseMask, FALSE, end_rubber_band, data);

      /*
       * Remove the crosshairs event handler.  This will be reactivated
       *  when rubberbanding is done.
       */
      if(data->plot->num_rr_oper > 0)
	{
	 XtRemoveEventHandler(data->plot->drawing_area_widget[1],
			   ButtonPressMask, FALSE, crosshairs_timer, data);
	 XtRemoveEventHandler(data->plot->drawing_area_widget[3],
			   ButtonPressMask, FALSE, crosshairs_timer, data);
	}
      XtRemoveEventHandler(data->plot->drawing_area_widget[5],
			ButtonPressMask, FALSE, crosshairs_timer, data);

      /* redraw the line with a different color: */
      line_gcv.foreground = get_pixel_by_name(w, SELECTED_LINE_COLOR);
      XChangeGC(XtDisplay(w), data->plot->line_gc[ts_index],
		GCForeground, &line_gcv);
      draw_graph_line(data->plot->drawing_area_widget[5], data, ts_index);
      XClearArea(XtDisplay(data->plot->drawing_area_widget[5]),
			   XtWindow(data->plot->drawing_area_widget[5]),
			   0, 0, 1, 1, TRUE);
   }
   else
   {
      data->plot->start_end_sw = START;
      data->plot->ts_change_flag = START;
      data->plot->ipt = 1000;
      line_gcv.foreground = get_pixel_by_name(w, data->plot->ts_color[ts_index]);
      XChangeGC(XtDisplay(data->plot->drawing_area_widget[5]),
		data->plot->line_gc[ts_index], GCForeground, &line_gcv);
      draw_graph_line(data->plot->drawing_area_widget[5], data, ts_index);
      data->plot->start_end_sw = START;
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
		data->plot->line_gc[ts_index], GCForeground, &line_gcv);
      XGetGeometry(XtDisplay(w), data->plot->pix[5],
			    &pixmap_root, &pixmap_x, &pixmap_y,
			    &pixmap_width, &pixmap_height,
			    &pixmap_border_width, &pixmap_depth);

      XFillRectangle(XtDisplay(data->plot->drawing_area_widget[5]),
		     data->plot->pix[5], data->plot->line_gc[ts_index],
		     0, 0, pixmap_width, pixmap_height);

      line_gcv.foreground = foreground;
      XChangeGC(XtDisplay(data->plot->drawing_area_widget[5]),
		data->plot->line_gc[ts_index], GCForeground, &line_gcv);

      draw_axes(data->plot->drawing_area_widget[7], data);
      draw_lines(data->plot->drawing_area_widget[5], data, NULL);
      draw_info_lines(data->plot->drawing_area_widget[5], data);

      XClearArea(XtDisplay(data->plot->drawing_area_widget[5]),
			   XtWindow(data->plot->drawing_area_widget[5]),
			   0, 0, 1, 1, TRUE);

      /*
	 Remove rubberbanding event handlers.
      */
      XtRemoveEventHandler(data->plot->drawing_area_widget[5],
			   ButtonPressMask, FALSE, start_rubber_band, data);
      XtRemoveEventHandler(data->plot->drawing_area_widget[5],
			   ButtonMotionMask, FALSE, track_rubber_band,
			   data);
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
      /*
      Reset scrollbars, sliders, and hydrograph to sensitive.
      Reset undo_widget to insensitive.
      */
      XtSetSensitive(data->plot->vertical_scale_widget, TRUE);
      XtSetSensitive(data->plot->horiz_scale_widget, TRUE);
      XtSetSensitive(data->plot->vertical_scrollbar_widget, TRUE);
      XtSetSensitive(data->plot->horiz_scrollbar_widget, TRUE);
      XtSetSensitive(data->plot->drawing_area_widget[5], TRUE);

      XtSetSensitive(data->plot->undo_widget, FALSE);

      /* call routine to change table values (if necessary)
	 to match new plotted values
      */
      update_table(data);
      tschng_mod_made(data);
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/ifp/src/PlotTulsa/RCS/change_ts.c,v $";
 static char rcs_id2[] = "$Id: change_ts.c,v 1.6 2007/05/16 16:45:23 aivo Exp $";}
/*  ===================================================  */

}
