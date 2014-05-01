/* File: rc_draw_info_lines.c
 *
 * Gets the width and height of the drawing area, as well as
 * the foreground and background colors to make graphics context
 * for lines to draw into pixmap.
 *
 * Creates graphics context for the flood_flow, flood_stg, and
 * max discharge of record lines (if rating curve exists).
 *
 */

#include "rating_curve.h"

void rc_draw_info_lines(w, data)
   Widget                        w;      /* widget data structure */
   rc_struct                     *data;  /* rating curve data structure pointer */
{
   int           n;                      /* array index */
   Arg           wargs[10];              /* window resource data structure array */
   Dimension     width, height;          /* widget dimensions */
   Dimension     pix_width, pix_height;  /* drawing area dimensions */
   GC            info_gc;                /* graphics context data structure */
   Pixel         foreground, background; /* foreground, backround colors */
   XGCValues     info_gcv;               /* graphics context data structure */
   float         val;                    /* end of obsevations value   */
   float         discharge;              /* discharge rate  */
   float         stage_metric;           /* metric river depth */
   int           x1, y1, x2, y2;         /* plot points */

  /* printf("in rc_draw_info_lines\n"); */

/*
 * Get the width and height of the drawing area.
 * Also, get foreground and background colors to make
 *  graphics context for lines to draw into pixmap.
 */
   n=0;
   XtSetArg(wargs[n], XmNwidth, &width); n++;
   XtSetArg(wargs[n], XmNheight, &height); n++;
   XtSetArg(wargs[n], XmNforeground, &foreground); n++;
   XtSetArg(wargs[n], XmNbackground, &background); n++;
   XtGetValues(w, wargs, n);

   pix_width = data->width_scale * width;
   pix_height = data->height_scale * height;

/* Create graphics context for the flood_flow, flood_stg, and
   max discharge of record lines (if rating curve exists).
*/
   info_gcv.foreground = get_pixel_by_name(w, "blue");
   info_gcv.line_style = LineOnOffDash;
   info_gc = XCreateGC(XtDisplay(w), data->pix[4],
		       GCForeground | GCLineStyle, &info_gcv);

   if(data->RC == TRUE)
   {
      /* draw flood lines */
      info_gcv.foreground = get_pixel_by_name(w, "red");
      XChangeGC(XtDisplay(data->drawing_area_widget[4]),
		info_gc, GCForeground, &info_gcv);

      if (data->flood_flow > 0.0)
      {
	 discharge = data->flood_flow;
	 y1  = 0;
	 y2  = pix_height;
	 val = discharge;
	 x1  = x2 = val_to_pixel(&val, &data->min_q,
				 &data->q_axis_max,
				 &data->origin_x, &data->end_x);

	 if(x1 > 0.0 && x1 <= pix_width)
	    XDrawLine(XtDisplay(w), data->pix[4], info_gc, x1, y1, x2, y2);
      }

      if(data->flood_stg > 0.0)
      {
	 discharge = data->flood_stg;
	 x1 = 0;
	 x2 = pix_width;
	 val = discharge;
	 y1 = y2 = val_to_pixel(&val, &data->min_stg,
				&data->stg_axis_max,
				&data->origin_y, &data->end_y);

	 if(y1 > 0.0 && y1 <= pix_height)
	    XDrawLine(XtDisplay(w), data->pix[4], info_gc, x1, y1, x2, y2);
      }

      /*  Draw max of record lines */
      info_gcv.foreground = get_pixel_by_name(w, "medium slate blue");
      XChangeGC(XtDisplay(data->drawing_area_widget[4]),
		info_gc, GCForeground, &info_gcv);

      if(data->max_record_q > 0.0)
      {
	 discharge = data->max_record_q;
	 y1 = 0;
	 y2 = pix_height;
	 val = discharge;
	 x1 = x2 = val_to_pixel(&val, &data->min_q,
				&data->q_axis_max,
				&data->origin_x, &data->end_x);

	 if(x1 > 0.0 && x1 <= pix_width)
	    XDrawLine(XtDisplay(w), data->pix[4], info_gc, x1, y1, x2, y2);
      }

      if(data->max_record_stg > 0.0)
      {
	 discharge = data->max_record_stg;
	 x1 = 0;
	 x2 = pix_width;
	 val = discharge;
	 y1 = y2 = val_to_pixel(&val, &data->min_stg,
				&data->stg_axis_max,
				&data->origin_y, &data->end_y);

	 if(y1 > 0.0 && y1 <= pix_height)
	    XDrawLine(XtDisplay(w), data->pix[4], info_gc, x1, y1, x2, y2);
      }

   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rc_draw_info_lines.c,v $";
 static char rcs_id2[] = "$Id: rc_draw_info_lines.c,v 1.1 1995/09/08 14:57:47 page Exp $";}
/*  ===================================================  */

}
