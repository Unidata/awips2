/* File: mp_draw_rectangles.c   */

#include "mods_plot.h"

void mp_draw_rectangles(w, data, px_ro_flag, index)
  Widget           w;                   /* Widget data structure                */
  mods_plot_struct *data;               /* Mods plot data structure pointer     */
  int              px_ro_flag;          /* Height calculation flag              */
  int              index;
  {
     float         val, *x_val;         /* Intermediate x-axis values   */
     int           i, j;                /* Counters                     */
     int           x_offset;            /* X position from the label border     */
     int           num_intervals;       /* Number of intervals                  */
     int           num_bars;
     XRectangle   *rectangles;          /* Rectangle structure pointer  */
     XGCValues    gcv;                  /* Graphics context data structure      */
     static char  *colors[] = {"turquoise", "plum", "aquamarine",
			      "cyan", "magenta", "spring green"};
     int          num_colors;           /* Number of colors in the colors array */
     int          color_num;            /* number of color in colors array      */
/*
     char         *colors2[] = {"sky blue", "thistle", "lime green",
			       "medium turquoise", "orchid", "green"};
*/
    /* printf("in draw_rectangles\n"); */

     /* set the number of time intervals */
     num_intervals = data->num_pts - 1;

     x_offset = ((data->end_x - data->origin_x) /
		(float)(num_intervals) * 0.25) + (index*data->bar_width);
     rectangles = (XRectangle *)malloc((num_intervals) *
				       sizeof(XRectangle));

     x_val = (float *)malloc((data->num_pts) * sizeof(float));
     for(i=0; i<data->num_pts; i++)
	x_val[i] = (float)i;

     for(j=0; j<data->end_obs; j++)
     {
	val = x_val[j];
	rectangles[j].x = val_to_pixel(&val, &x_val[0],
				       &x_val[data->num_pts-1],
				       &data->origin_x,
				       &data->end_x);
	rectangles[j].x += x_offset;

	if(px_ro_flag == PX)
	{
	   val = data->ts_array[index][j];
	   rectangles[j].height = val_to_pixel(&val, &data->min_y,
					       &data->y_axis_max,
					       &data->origin_y,
					       &data->end_y);

	   rectangles[j].y = data->origin_y;
	}
	else  /* px_ro_flag == RO */
	{
	   val = data->ts_array[index][j];
	   rectangles[j].y = val_to_pixel(&val, &data->min_y,
					  &data->y_axis_max,
					  &data->origin_y,
					  &data->end_y);

	   rectangles[j].height = data->origin_y - rectangles[j].y;
	}
	rectangles[j].width = data->bar_width;
     }

     /* draw rectangles in the proper colors. */
     num_colors = (XtNumber(colors))/2;
     color_num = (index < num_colors) ? index : index%num_colors;
     if(data->ts_change_flag == DONE)
	gcv.foreground = get_pixel_by_name(w, RB_LINE_COLOR);
     else
	gcv.foreground = get_pixel_by_name(w, colors[color_num]); 
     XChangeGC(XtDisplay(data->drawing_area_widget[2]),
	       data->gc[2], GCForeground, &gcv);
     XFillRectangles(XtDisplay(w), data->pix[2],
		     data->gc[2],
		     rectangles, data->end_obs);

     /* fill rectangle structures for data past the end of observed
	time then plot with different colors */
     for(j=j; j<num_intervals; j++)
     {
	val = x_val[j];
	rectangles[j].x = val_to_pixel(&val, &x_val[0],
				       &x_val[data->num_pts-1],
				       &data->origin_x,
				       &data->end_x);
	rectangles[j].x += x_offset;

	if(px_ro_flag == PX)
	{
	   val = data->ts_array[index][j];
	   rectangles[j].height = val_to_pixel(&val, &data->min_y,
					       &data->y_axis_max,
					       &data->origin_y,
					       &data->end_y);

	   rectangles[j].y = data->origin_y;
	}
	else   /* px_ro_flag == RO */
	{
	   val = data->ts_array[index][j];
	   rectangles[j].y = val_to_pixel(&val, &data->min_y,
					  &data->y_axis_max,
					  &data->origin_y,
					  &data->end_y);

	   rectangles[j].height = data->origin_y - rectangles[j].y;
	}

	rectangles[j].width = data->bar_width;
     }

     /* draw rectangles in the proper colors. */
     num_bars = num_intervals - data->end_obs;
     if(data->ts_change_flag == DONE)
	gcv.foreground = get_pixel_by_name(w, RB_LINE_COLOR);
     else
	gcv.foreground = get_pixel_by_name(w, colors[color_num+3]);
     XChangeGC(XtDisplay(data->drawing_area_widget[2]),
	       data->gc[2], GCForeground, &gcv);
     XFillRectangles(XtDisplay(w), data->pix[2],
		     data->gc[2],
		     &rectangles[data->end_obs],
		     num_bars);

     free(rectangles);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_draw_rectangles.c,v $";
 static char rcs_id2[] = "$Id: mp_draw_rectangles.c,v 1.1 1995/09/08 14:59:04 page Exp $";}
/*  ===================================================  */

  }  /* end of mp_draw_rectangles */
