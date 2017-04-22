/* File: draw_rectangles.c
 *
 * Rectangle drawing function
 *
 */

#include "plot.h"
#include "ifp_struct.h"

void draw_rectangles(w, data, px_ro_flag, bar_width)
  Widget          w;             /* widget data structure */
  combined_struct *data;         /* tables and plot data structure pointer */
  int             px_ro_flag;    /* height calculation flag */
  short           bar_width;     /* rectangle width */
  {
     float        val, *x_val;   /* intermediate x axis values */
     int          i, j;          /* counters */
     int          x_offset;
     XRectangle   *rectangles;   /* rectangle structure pointer */
     XGCValues    gcv;           /* graphics context data structure */
     char         *colors[] = {"turquoise", "plum", "aquamarine",
			      "cyan", "magenta", "spring green"};
     char         *colors2[] = {"sky blue", "thistle", "lime green",
			       "medium turquoise", "orchid", "green"};
     int          num_colors;    /* number of colors in the color arrays */	       		       
     int          color_num;     /* used to decide which color to use */
     int          num_rrm_plotted;  /* number of rainfall/runoff models plotted */
     
     /* Calculate number of rainfall/runoff models for which data can be plotted
         in the alloted space.  
     */
     if( (data->plot->end_x - data->plot->origin_x) * 0.75 >=
         data->plot->num_rr_pts * data->plot->num_rr_oper * bar_width )
        num_rrm_plotted = data->plot->num_rr_oper;
     else
     {
        num_rrm_plotted = ((data->plot->end_x - data->plot->origin_x) * 0.75)/
                          ( data->plot->num_rr_pts * bar_width);
        printf("Warning: Not enough space to plot all of the px or ro time series.\n");
        printf("         Plotting first %d time series.  Expand window or expand\n", num_rrm_plotted);
        printf("         horizontal scale to see more time series.\n");
      
     }                                     
/*                          
     printf("???????????? -minus 1 %d\n",data->plot->end_x);
     printf("???????????? -minus 2 %d\n",data->plot->origin_x);
     printf("???????????? one = %d\n",(data->plot->end_x - data->plot->origin_x) * 0.75);
     printf("???????????? two = %d\n",(data->plot->num_rr_pts * bar_width));
     printf("???????????? three = %d\n",num_rrm_plotted);
*/     
    /* Calculate the offset at the start of each time frame */
     x_offset = (data->plot->end_x - data->plot->origin_x) /
		(float)data->plot->num_rr_pts * 0.25;
		
     rectangles = (XRectangle *)malloc(data->plot->num_rr_pts *
				       sizeof(XRectangle));

     x_val = (float *)malloc((data->plot->num_rr_pts+1) * sizeof(float));
     for(i=0; i<data->plot->num_rr_pts+1; i++)
	x_val[i] = (float)i;

     for(i=0; i<num_rrm_plotted; i++)
     {
	for(j=0; j<data->plot->end_obs_px_ro; j++)
	{
	   val = x_val[j];
	   rectangles[j].x = val_to_pixel(&val, &x_val[0],
					  &x_val[data->plot->num_rr_pts],
					  &data->plot->origin_x,
					  &data->plot->end_x);
	   rectangles[j].x += x_offset;

	   if(px_ro_flag == PX)
	   {
	      val = data->plot->px[i][j];
	      /* to find height of rectangle for PX graph the origin_y must
		 be subtracted to correct for the placement of the graph
		 down from the top of the drawing area
	      */
	      rectangles[j].height = val_to_pixel(&val, &data->plot->px_min,
					      &data->plot->px_ro_y_axis_max,
						  &data->plot->origin_y,
						  &data->plot->end_y) -
				     data->plot->origin_y;

	      rectangles[j].y = data->plot->origin_y;
	   }
	   else  /* px_ro_flag == RO */
	   {
	      val = data->plot->ro[i][j];
	      rectangles[j].y = val_to_pixel(&val, &data->plot->ro_min,
					     &data->plot->px_ro_y_axis_max,
					     &data->plot->origin_y,
					     &data->plot->end_y);

	      rectangles[j].height = data->plot->origin_y - rectangles[j].y;
	   }
	   rectangles[j].width = bar_width;
	}

	/* decide if it's a px or ro time series then draw rectangles
	   in the proper colors. */
        num_colors = (XtNumber(colors))/2;
	color_num = (i < num_colors) ? i : i%num_colors ; 
	if(px_ro_flag == PX)
	{
	   gcv.foreground = get_pixel_by_name(w, colors[color_num]);
	   XChangeGC(XtDisplay(data->plot->drawing_area_widget[1]),
		     data->plot->gc[1], GCForeground, &gcv);
	   XFillRectangles(XtDisplay(w), data->plot->pix[1],
			   data->plot->gc[1],
			   rectangles, data->plot->end_obs_px_ro);
	}
	else
	{
	   gcv.foreground = get_pixel_by_name(w, colors2[color_num]);
	   XChangeGC(XtDisplay(data->plot->drawing_area_widget[3]),
		     data->plot->gc[3], GCForeground, &gcv);
	   XFillRectangles(XtDisplay(w), data->plot->pix[3],
			   data->plot->gc[3],
			   rectangles, data->plot->end_obs_px_ro);
	}

	/* fill rectangle structures for data past the end of observed
	   time then plot with different colors */
	for(j=j; j<data->plot->num_rr_pts; j++)
	{
	   val = x_val[j];
	   rectangles[j].x = val_to_pixel(&val, &x_val[0],
					  &x_val[data->plot->num_rr_pts],
					  &data->plot->origin_x,
					  &data->plot->end_x);
	   rectangles[j].x += x_offset;

	   if(px_ro_flag == PX)
	   {
	      val = data->plot->px[i][j];
	      rectangles[j].height = val_to_pixel(&val, &data->plot->px_min,
					      &data->plot->px_ro_y_axis_max,
						  &data->plot->origin_y,
						  &data->plot->end_y) -
				     data->plot->origin_y;

	      rectangles[j].y = data->plot->origin_y;
	   }
	   else  /* px_ro_flag == RO */
	   {
	      val = data->plot->ro[i][j];
	      rectangles[j].y = val_to_pixel(&val, &data->plot->ro_min,
					     &data->plot->px_ro_y_axis_max,
					     &data->plot->origin_y,
					     &data->plot->end_y);

	      rectangles[j].height = data->plot->origin_y - rectangles[j].y;
	   }

	   rectangles[j].width = bar_width;
	}

	/* decide if it's a px or ro time series then draw rectangles
	   in the proper colors. */
	if(px_ro_flag == PX)
	{
	   gcv.foreground = get_pixel_by_name(w, colors[color_num+3]);
	   XChangeGC(XtDisplay(data->plot->drawing_area_widget[1]),
		     data->plot->gc[1], GCForeground, &gcv);
	   XFillRectangles(XtDisplay(w), data->plot->pix[1],
			   data->plot->gc[1],
			   &rectangles[data->plot->end_obs_px_ro],
			   (data->plot->num_rr_pts - data->plot->end_obs_px_ro));
	}
	else
	{
	   gcv.foreground = get_pixel_by_name(w, colors2[color_num+3]);
	   XChangeGC(XtDisplay(data->plot->drawing_area_widget[3]),
		     data->plot->gc[3], GCForeground, &gcv);
	   XFillRectangles(XtDisplay(w), data->plot->pix[3],
			   data->plot->gc[3],
			   &rectangles[data->plot->end_obs_px_ro],
			   (data->plot->num_rr_pts - data->plot->end_obs_px_ro));
	}

	x_offset += bar_width;
     }  /* end of num_rr_oper for loop */

     free(rectangles);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/draw_rectangles.c,v $";
 static char rcs_id2[] = "$Id: draw_rectangles.c,v 1.2 1996/12/10 20:11:55 dws Exp $";}
/*  ===================================================  */

  }  /* end of draw_rectangles */
