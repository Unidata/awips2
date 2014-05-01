/* File: mp_undo_change_ts.c
 *
 * Sets hydrograph area to insensitive and removes event handlers
 *  for rubberbanding.
 *
 * Reactivates the event handler for crosshairs.
 *
 * Resets scrollbars, sliders and hydrograph to sensitive and
 *  resets ts_change_flag.
 *
 */

#include "mods_plot.h"

void mp_undo_change_ts(w, data, call_data)
   Widget                       w;              /* Widget data structure        */
   mods_plot_struct             *data;          /* Mods plot data structure pointer     */
   XmDrawingAreaCallbackStruct  *call_data;     /* Xmscale call back structure pointer  */
{
   Arg       wargs[10];                         /* Window resource data structure array */
   XGCValues gcv;                               /* Graphics context data structures     */
   int       i, n;                              /* Counters                             */
   Pixel     foreground;                        /* Window foreground color              */
   Pixel     background;                        /* Window background color              */
   Window       pixmap_root;                    /* Root window structure                */
   int          pixmap_x;                       /* Window x position                    */
   int          pixmap_y;                       /* Window y position                    */
   unsigned int pixmap_width;                   /* Window width                         */
   unsigned int pixmap_height;                  /* Window height                        */
   unsigned int pixmap_border_width;            /* Window border width                  */
   unsigned int pixmap_depth;                   /* Window  depth                        */

  /* printf("in undo_change_ts\n"); */

   data->ts_change_flag = UNDO;

   /* Set hydrograph area to insensitive and remove event handler
      for undo_change_ts.
   */
   XtSetSensitive(data->drawing_area_widget[2], FALSE);
   XtRemoveEventHandler(data->undo_widget,
			ButtonPressMask, FALSE, mp_undo_change_ts, data);
   XtRemoveEventHandler(data->drawing_area_widget[2],
		     ButtonPressMask, FALSE, mp_start_rubber_band, data);
   XtRemoveEventHandler(data->drawing_area_widget[2],
		     ButtonMotionMask, FALSE, mp_track_rubber_band, data);
   XtRemoveEventHandler(data->drawing_area_widget[2],
		     ButtonReleaseMask, FALSE, mp_end_rubber_band, data);
   /*
    * Reactivate the event handler for crosshairs.
    */
   XtAddEventHandler(data->drawing_area_widget[2],
		     ButtonPressMask, FALSE, mp_crosshairs_timer, data);

   for(i=0; i<data->num_pts-1; i++)
      data->ts_array[data->ts_index][i] =
      data->orig_ts_array[data->ts_index][i];

   if(data->mod_type_sw == RRICHNG)
      mp_draw_rectangles(data->drawing_area_widget[2], data, PX,
			 data->ts_index);
   else  /* UH or ROCHNG */
      mp_draw_rectangles(data->drawing_area_widget[2], data, RO,
			 data->ts_index);

   for(i = 0; i < data->num_ts_sel; i++)
   {
      XtSetSensitive(data->legend[i], TRUE);
      n=0;
      XtSetArg(wargs[n], XmNbackground,
	       get_pixel_by_name(data->legend[i], "gray85")); n++;
      XtSetValues(data->legend[i], wargs, n);
   }

   n=0;
   XtSetArg(wargs[n], XmNbackground, &background); n++;
   XtGetValues(data->drawing_area_widget[2], wargs, n);
   foreground = gcv.foreground;
   gcv.foreground = background;
   XChangeGC(XtDisplay(data->drawing_area_widget[2]),
	     data->gc[2], GCForeground, &gcv);
   XGetGeometry(XtDisplay(w), data->pix[2],
		&pixmap_root, &pixmap_x, &pixmap_y,
		&pixmap_width, &pixmap_height,
		&pixmap_border_width, &pixmap_depth);

   XFillRectangle(XtDisplay(data->drawing_area_widget[2]), data->pix[2],
		  data->gc[2], 0, 0, pixmap_width,
		  pixmap_height);

   gcv.foreground = foreground;
   XChangeGC(XtDisplay(data->drawing_area_widget[2]),
	     data->gc[2], GCForeground, &gcv);

   for(i = 0; i < data->num_ts_plotted; i++)
      if(data->mod_type_sw == RRICHNG)
	 mp_draw_rectangles(data->drawing_area_widget[2], data, PX, i);
      else  /* UH or ROCHNG */
	 mp_draw_rectangles(data->drawing_area_widget[2], data, RO, i);

   mp_draw_end_obs_line(data->drawing_area_widget[2], data);

   XClearArea(XtDisplay(data->drawing_area_widget[2]),
			XtWindow(data->drawing_area_widget[2]),
			0, 0, 1, 1, TRUE);

   XtSetSensitive(data->undo_widget, TRUE);
   XtSetSensitive(data->drawing_area_widget[2], TRUE);
   XtSetSensitive(data->done_widget, TRUE);

   data->ts_change_flag = START;
  /*
   if(data->tables->menu_index > -1)
      for(i=0; i<data->end[data->ts_index]; i++)
	 data->tables->changed[data->tables->menu_index][i] = 0;
   update_table(data);
  */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_undo_change_ts.c,v $";
 static char rcs_id2[] = "$Id: mp_undo_change_ts.c,v 1.1 1995/09/08 14:59:10 page Exp $";}
/*  ===================================================  */

}
