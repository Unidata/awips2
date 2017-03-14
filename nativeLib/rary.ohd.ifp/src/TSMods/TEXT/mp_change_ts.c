#include "mods_plot.h"

/* File: mp_change_ts.c
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

char *   xs_get_string_from_xmstring();

void mp_change_ts(w, data, event)
   Widget                       w;      /* widget data structure  */
   mods_plot_struct             *data;  /* mods plot data structure pointer  */
   XEvent                       *event;
{
   Arg       wargs[10];               /* window resource data structure array */
   XGCValues gcv;                     /* graphics context data structures     */
   XmString  cmpd_label_str;          /* compound label string                */
   int       i, n;                    /* counters                             */
   int       ts_index = -1;           /* time series index                    */
   char      *label_str;              /* label string pointer                 */
   Pixel     foreground;              /* window foreground color              */
   Pixel     background;              /* window background color              */
   Window        pixmap_root;         /* root window structure                */
   int           pixmap_x;            /* window x position                    */
   int           pixmap_y;            /* window y position                    */
   unsigned int  pixmap_width;        /* window width                         */
   unsigned int  pixmap_height;       /* window height                        */
   unsigned int  pixmap_border_width; /* window border width                  */
   unsigned int  pixmap_depth;        /* window depth                         */


   /* Set the "Done" button insensitive while in change mode. */
   XtSetSensitive(data->done_widget, FALSE);

   /* get the time series name and index: */
   n=0;
   XtSetArg(wargs[n], XmNlabelString, &cmpd_label_str); n++;
   XtGetValues(w, wargs, n);

   label_str = xs_get_string_from_xmstring(cmpd_label_str);
   for(i = 0; i < data->num_ts_sel; i++)
   {
      if(strcmp(label_str, data->op_name[i]) == 0)
	 ts_index = i;
      else
      {
	 XtSetSensitive(data->legend[i], FALSE);
	 n=0;
	 XtSetArg(wargs[n], XmNbackground,
		  get_pixel_by_name(data->legend[i], "white")); n++;
	 XtSetValues(data->legend[i], wargs, n);
      }
   }
   data->ts_index = ts_index;
   /*find_menu_index(data);*/

   if(ts_index < 0)
   {
      for(i = 0; i < data->num_ts_sel; i++)
	 XtSetSensitive(data->legend[i], TRUE);
      return;
   }

   if(ts_index > data->num_ts_sel) return;

   ts_index = data->ts_index;

   if(data->ts_change_flag == START)
   {
      data->ts_change_flag = DONE;
      
      data->lpt.x = 0;
      data->rpt.x = 0;
      data->cpt.x = 0;
      XtAddEventHandler(data->undo_widget,
			ButtonPressMask, FALSE, mp_undo_change_ts, data);
      
      /* prepare for rubberbanding */
      XtAddEventHandler(data->drawing_area_widget[2],
			ButtonPressMask, FALSE, mp_start_rubber_band, data);
                            
      XtAddEventHandler(data->drawing_area_widget[2],
			ButtonMotionMask, FALSE, mp_track_rubber_band, data);
      XtAddEventHandler(data->drawing_area_widget[2],
			ButtonReleaseMask, FALSE, mp_end_rubber_band, data);

      /*
       * Remove the crosshairs event handler.  This will be reactivated
       *  when rubberbanding is done.
       */
      XtRemoveEventHandler(data->drawing_area_widget[2],
			ButtonPressMask, FALSE, mp_crosshairs_timer, data);

      if(data->mod_type_sw == RRICHNG)
	 mp_draw_rectangles(data->drawing_area_widget[2], data, PX, ts_index);
      else  /* UH or ROCHNG */
	 mp_draw_rectangles(data->drawing_area_widget[2], data, RO, ts_index);

      XClearArea(XtDisplay(data->drawing_area_widget[2]),
			   XtWindow(data->drawing_area_widget[2]),
			   0, 0, 1, 1, TRUE);
   }
   else
   {
      data->start_end_sw = START;
      data->ts_change_flag = START;
      data->ipt = 1000;

      if(data->mod_type_sw == RRICHNG)
	 mp_draw_rectangles(data->drawing_area_widget[2], data, PX, ts_index);
      else  /* UH or ROCHNG */
	 mp_draw_rectangles(data->drawing_area_widget[2], data, RO, ts_index);

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

      XFillRectangle(XtDisplay(data->drawing_area_widget[2]),
		     data->pix[2], data->gc[2],
		     0, 0, pixmap_width, pixmap_height);

      gcv.foreground = foreground;
      XChangeGC(XtDisplay(data->drawing_area_widget[2]),
		data->gc[2], GCForeground, &gcv);
      for(i=0; i<data->num_ts_plotted; i++)
	 if(data->mod_type_sw == RRICHNG)
	    mp_draw_rectangles(data->drawing_area_widget[2], data, PX, i);
	 else  /* UH or ROCHNG */
	   {
	    if(data->mod_type_sw == UH)
	      {
	      /*
	      * Rescale unit hydrograph ordinates so they plot
	      *  with meaningful units.  The MUHGCH subroutine
	      *  would do this anyway, this is just to make the
	      *  plot look right.
	      * gfs, 11/14/91
	      */
	       int   j;
	       float orig_uhg_area, new_uhg_area;

	    
               orig_uhg_area = 0.0;
	       new_uhg_area = 0.0;
              
	       for(j=0; j<data->num_pts; j++)
		  {
		   orig_uhg_area=  data->orig_ts_array[0][j];
		   new_uhg_area = data->ts_array[0][j];
                   
		  }
	       for(j=0; j<data->num_pts; j++){
		   data->ts_array[0][j] =
		     data->ts_array[0][j] * (orig_uhg_area / new_uhg_area);
                    
               }     
	      }   /* end  if(data->mod_type_sw == UH) */
	    mp_draw_rectangles(data->drawing_area_widget[2], data, RO, i);
	   }      /* end  else  UH or ROCHNG */

      mp_draw_end_obs_line(data->drawing_area_widget[2], data);

      XClearArea(XtDisplay(data->drawing_area_widget[2]),
			   XtWindow(data->drawing_area_widget[2]),
			   0, 0, 1, 1, TRUE);

      XtRemoveEventHandler(data->drawing_area_widget[2],
			   ButtonPressMask, FALSE, mp_start_rubber_band, data);
      XtRemoveEventHandler(data->drawing_area_widget[2],
			   ButtonMotionMask, FALSE, mp_track_rubber_band,
			   data);
      XtRemoveEventHandler(data->drawing_area_widget[2],
			   ButtonReleaseMask, FALSE, mp_end_rubber_band, data);

      /*
       * Reactivate the event handler for crosshairs.
       */
      XtAddEventHandler(data->drawing_area_widget[2],
			ButtonPressMask, FALSE, mp_crosshairs_timer, data);

      XtSetSensitive(data->undo_widget, TRUE);
      XtSetSensitive(data->drawing_area_widget[2], TRUE);
      XtSetSensitive(data->done_widget, TRUE);


   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_change_ts.c,v $";
 static char rcs_id2[] = "$Id: mp_change_ts.c,v 1.2 2002/10/10 16:34:00 dws Exp $";}
/*  ===================================================  */

}
