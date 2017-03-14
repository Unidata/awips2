/*  File: draw_axes.c
 *
 *  Draws and labels hydrograph x axis
 *
 */

#include "plot.h"
#include "ifp_struct.h"

void draw_axes(w, data)
   Widget                        w;      /* widget data structure */
   combined_struct               *data;  /* tables and plot data structure pointer */
 {
   int           i;                      /* counter */
   Pixel         foreground;             /* foreground color */
   XSegment      *ticks;                 /* graph ticks structure pointer */
   float         val;                    /* x axis time values  */
   XFontStruct   *label_font;            /* font label structure pointer */
   Font          font_id;                /* font resource id */
   int           length;                 /* length of day hour string used in the graph */
   int           x_offset;               /* position from label border */
   unsigned int  line_width;             /* value used to define the x positions in
					    drawing the x axis */
   int           num_skip;               /* value used in calculating the no. of points
					    to skip before writing a label */
   int           default_num_skip;       /* default number to skip */
   int           text_width;
   int           num_labels;             /* number of labels used in the xy-plot */

  /* printf("in draw_axes\n"); */

   ticks = (XSegment *)malloc(*data->plot->num_pts * sizeof(XSegment));

/* Draw x axis */
   line_width = 4;
   XSetLineAttributes(XtDisplay(w), data->plot->gc[7],
		      line_width, 0, 0, 0);
   XDrawLine(XtDisplay(w), data->plot->pix[7], data->plot->gc[7],
	     data->plot->origin_x, line_width/2,
	     data->plot->end_x, line_width/2);

   line_width = 1;
   XSetLineAttributes(XtDisplay(w), data->plot->gc[7],
		      line_width, 0, 0, 0);

/* Create and draw tick marks */

   for(i=0; i<*data->plot->num_pts; i++)
   {
      val = data->plot->x[i];
      ticks[i].x1 = (int)(data->plot->origin_x +
			 (data->plot->end_x - data->plot->origin_x) *
			 (val - *data->plot->min_x) /
			 (*data->plot->max_x - *data->plot->min_x));
      ticks[i].x2 = ticks[i].x1;
      ticks[i].y1 = line_width/2;
      ticks[i].y2 = line_width/2 + 6;
   }
   XDrawSegments(XtDisplay(w), data->plot->pix[7], data->plot->gc[7],
		 ticks, *data->plot->num_pts);
   /* Draw tick mark at left end of x axis. */
   XDrawLine(XtDisplay(w), data->plot->pix[7], data->plot->gc[7],
	     data->plot->end_x-1, line_width/2,
	     data->plot->end_x-1, line_width/2+6);

/*
 * Make sure line width is set to 1 after all this.
 */
   line_width = 1;
   XSetLineAttributes(XtDisplay(w), data->plot->gc[7],
		      line_width, 0, 0, 0);

/*  Label x axis */
   /*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
   
   /*--AV  --*/
   font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
   XSetFont(XtDisplay(w), data->plot->gc[7], font_id);
   label_font = XQueryFont(XtDisplay(w), font_id);

   /* Determine number of points to skip before writing next label
    * so labels do not overwrite each other
    * Added code to figure out default_num_skip based on the plot_delta_t
    * D. Page - 13 Jan. 1998
    */
   if(data->plot->plot_delta_t == 6 || data->plot->plot_delta_t == 12)
      default_num_skip = 2;
   else if(data->plot->plot_delta_t == 1 || data->plot->plot_delta_t == 2) 
      default_num_skip = 6;
   else if(data->plot->plot_delta_t == 24)
      default_num_skip = 1;
   else                          /* 4 or 8 hour plot? */
      default_num_skip = 3;
   
   num_skip = default_num_skip;
      
   text_width = (int)XTextWidth(label_font, "00.00", 5);
   num_labels = (*data->plot->num_pts-1) / num_skip + 1;
   while((text_width*num_labels) > (data->plot->end_x-num_labels))
   {
      num_skip += default_num_skip;      
      if(num_skip == 18 || num_skip == 30 || num_skip == 36)
            num_skip += default_num_skip;
            
      num_labels = (*data->plot->num_pts-1) / num_skip + 1;
   }

   /* write labels */
   for(i=0; i<*data->plot->num_pts; i+=num_skip)
   {
      if(i == *data->plot->num_pts-1)
	 XDrawLine(XtDisplay(w), data->plot->pix[7], data->plot->gc[7],
		   ticks[i].x1-1, ticks[i].y1,
		   ticks[i].x2-1, ticks[i].y2+8);
      else
	 XDrawLine(XtDisplay(w), data->plot->pix[7], data->plot->gc[7],
		   ticks[i].x1, ticks[i].y1,
		   ticks[i].x2, ticks[i].y2+8);

      length = strlen(data->plot->day_hrs[i]);
      x_offset = (int)(XTextWidth(label_font, data->plot->day_hrs[i], length)/2);
      if(ticks[i].x1+(x_offset) > data->plot->end_x)
	 XDrawString(XtDisplay(w), data->plot->pix[7], data->plot->gc[7],
		     ticks[i].x1-(x_offset*2), ticks[i].y2+20,
		     data->plot->day_hrs[i], length);
      else if(ticks[i].x1-x_offset < data->plot->origin_x)
	 if(ticks[i+num_skip].x1-x_offset > ticks[i].x1+x_offset*2+1)
	    XDrawString(XtDisplay(w), data->plot->pix[7], data->plot->gc[7],
			ticks[i].x1, ticks[i].y2+20,
			data->plot->day_hrs[i], length);
	 else
	    continue;
      else
	 XDrawString(XtDisplay(w), data->plot->pix[7], data->plot->gc[7],
		     ticks[i].x1-x_offset, ticks[i].y2+20,
		     data->plot->day_hrs[i], length);
   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/draw_axes.c,v $";
 static char rcs_id2[] = "$Id: draw_axes.c,v 1.5 2006/03/28 20:43:41 aivo Exp $";}
/*  ===================================================  */

}
