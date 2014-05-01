/* File: rc_draw_x_axis.c
 *
 * Draws the x axis, creates and draws tick marks and also,
 * determines the number of points to skip before writing
 * the next label.
 *
 */

#include "rating_curve.h"

void rc_draw_x_axis(w, data)
   Widget                        w;      /* widget data structure */
   rc_struct                     *data;  /* rating curve data structure pointer */
{
   int           i;                      /* counter */
   Pixel         foreground;             /* foreground color */
   XSegment      *ticks;                 /* graph ticks structure pointer */
   int           num_ticks=11;           /* initial number of ticks */
   XFontStruct   *label_font;            /* font label structure pointer */
   Font          font_id;                /* font resource id */
   int           length;                 /* length of x label string */
   int           x_offset;               /* x axis labels center positions */
   unsigned int  line_width;             /* value used to define the x positions in
					    drawing the x axis */
   int           num_skip;               /* number of points to skip before writing next label */
   int           text_width;             /* x axis label text width */
   int           num_labels;             /* number of labels used in the xy plot */
   char          x_label[14];            /* x axis labels */
   float         range;                  /* range of discharge rate values */
   char          control_str[7];         /* sets up the number of decimal points in the
					    y axis labels */
   /*printf("in rc_draw_x_axis\n");*/

   ticks = (XSegment *)malloc(num_ticks * sizeof(XSegment));

/* Draw x axis */
   line_width = 4;
   XSetLineAttributes(XtDisplay(w), data->gc[2],
		      line_width, 0, 0, 0);
   XDrawLine(XtDisplay(w), data->pix[2], data->gc[2],
	     data->origin_x, line_width/2,
	     data->end_x, line_width/2);

   line_width = 1;
   XSetLineAttributes(XtDisplay(w), data->gc[2],
		      line_width, 0, 0, 0);

/* Create and draw tick marks */

   for(i=0; i<num_ticks; i++)
   {
      ticks[i].x1 = data->origin_x +
		    (i*(data->end_x - data->origin_x)
		    / (num_ticks-1));
      ticks[i].x2 = ticks[i].x1;
      ticks[i].y1 = line_width/2;
      ticks[i].y2 = line_width/2 + 6;
   }
   XDrawSegments(XtDisplay(w), data->pix[2], data->gc[2],
		 ticks, num_ticks);
/*  Doesn't seem to be needed as in draw_axes.c */
   /* Draw tick mark at left end of x axis.
   if((data->rc_num-1) % 4 == 0)
      XDrawLine(XtDisplay(w), data->pix[2], data->gc[2],
		data->end_x-1, line_width/2,
		data->end_x-1, line_width/2+14);
   else
      XDrawLine(XtDisplay(w), data->pix[2], data->gc[2],
		data->end_x-1, line_width/2,
		data->end_x-1, line_width/2+6);
*/

/*
 * Make sure line width is set to 1 after all this.
 */
   line_width = 1;
   XSetLineAttributes(XtDisplay(w), data->gc[2],
		      line_width, 0, 0, 0);

/*  Label x axis */
/*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
  /*--*-misc-fixed-medium-r-normal--13-120-75-75-c-80-* is Rom11 --AV--*/
  font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
   
   XSetFont(XtDisplay(w), data->gc[2], font_id);
   label_font = XQueryFont(XtDisplay(w), font_id);

   /* Determine number of points to skip before writing next label
      so labels do not overwrite each other
   */
   num_skip = 1;
   sprintf(x_label, "%d", (int)data->q_axis_max);
   length = strlen(x_label);
   text_width = (int)XTextWidth(label_font, x_label, length);
   num_labels = num_ticks / num_skip + 1;
   while((text_width*num_labels) > (data->end_x-num_labels) &&
	 data->end_x > 100.0)
   {
      num_skip += 1;
      num_labels = num_ticks / num_skip + 1;
   }

   /* decide how many decimal points to print based on the range of values */
   range = data->q_axis_max - data->min_q;
   memset(control_str, '\0', 7);
   if(range > 10.0)
      strncpy(control_str, "%.0f", 6);
   else if(range > 1.0 && range <= 10.0)
      strncpy(control_str, "%.1f", 6);
   else if(range > 0.0 && range <= 1.0)
      strncpy(control_str, "%.2f", 6);


   for(i=0; i<num_ticks; i+=num_skip)
   {
      if(i == (num_ticks-1))
	 XDrawLine(XtDisplay(w), data->pix[2], data->gc[2],
		   ticks[i].x1-1, ticks[i].y1,
		   ticks[i].x2-1, ticks[i].y2+8);
      else
	 XDrawLine(XtDisplay(w), data->pix[2], data->gc[2],
		   ticks[i].x1, ticks[i].y1,
		   ticks[i].x2, ticks[i].y2+8);

      sprintf(x_label, control_str,
	      data->min_q + i * data->rc_q_increment);
      length = strlen(x_label);
      x_offset = (int)(XTextWidth(label_font, x_label, length)/2);

     /*  Check if the string is at the start or end of the axis then
	 draw at the appropriate place.
     */
      if(ticks[i].x1+(x_offset*2) > data->end_x)
	 XDrawString(XtDisplay(w), data->pix[2], data->gc[2],
		     ticks[i].x1-(x_offset*2), ticks[i].y2+20,
		     x_label, length);
      else if(ticks[i].x1-x_offset < data->origin_x)
	 if(ticks[i+num_skip].x1-x_offset > ticks[i].x1+x_offset*2+1)
	    XDrawString(XtDisplay(w), data->pix[2], data->gc[2],
			ticks[i].x1, ticks[i].y2+20,
			x_label, length);
	 else
	    continue;
      else
	 XDrawString(XtDisplay(w), data->pix[2], data->gc[2],
		     ticks[i].x1-x_offset, ticks[i].y2+20,
		     x_label, length);
   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rc_draw_x_axis.c,v $";
 static char rcs_id2[] = "$Id: rc_draw_x_axis.c,v 1.3 2006/03/28 20:43:59 aivo Exp $";}
/*  ===================================================  */

}
