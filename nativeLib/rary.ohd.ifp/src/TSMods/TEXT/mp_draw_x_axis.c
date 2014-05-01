/* File: mp_draw_x_axis.c
 *
 * Draws the x axis, creates and draws tick marks and also,
 * determines the number of points to skip before writing
 * the next label.
 *
 */

#include "mods_plot.h"

void mp_draw_x_axis(w, data)
   Widget               w;      /* Widget data structure */
   mods_plot_struct     *data;  /* Mods plot data structure pointer     */
{
   int          i;              /* Counter              */
   XSegment     *ticks;         /* Graph ticks structure pointer        */
   float        val;            /* Variable for holding counter value   */
   XFontStruct  *label_font;    /* Font label structure pointer */
   Font         font_id;        /* Font resource id     */
   int          length;         /* Length of x label string     */
   int          x_offset;       /* X axis labels center positions       */
   unsigned int line_width;     /* Value used to define the x positions in
				   drawing the x axis   */
   int          num_skip;       /* Number of points to skip before writing next label  */
   int          text_width;     /* X axis label text width      */
   int          num_labels;     /* Number of labels used in the xy plot        */

   /*printf("in mp_draw_x_axis\n");*/

   ticks = (XSegment *)malloc(data->num_pts * sizeof(XSegment));

/* Draw x axis */
   line_width = 4;
   XSetLineAttributes(XtDisplay(w), data->gc[3],
		      line_width, 0, 0, 0);
   XDrawLine(XtDisplay(w), data->pix[3], data->gc[3],
	     data->origin_x, line_width/2,
	     data->end_x, line_width/2);

   line_width = 1;
   XSetLineAttributes(XtDisplay(w), data->gc[3],
		      line_width, 0, 0, 0);

/* Create and draw tick marks */

   for(i=0; i<data->num_pts; i++)
   {
      val = (float)i;
      ticks[i].x1 = (int)(data->origin_x +
			 (data->end_x - data->origin_x) *
			 (val - data->min_x) /
			 (data->max_x - data->min_x));
      ticks[i].x2 = ticks[i].x1;
      ticks[i].y1 = line_width/2;
      ticks[i].y2 = line_width/2 + 6;
   }
   XDrawSegments(XtDisplay(w), data->pix[3], data->gc[3],
		 ticks, data->num_pts);
/*  Doesn't seem to be needed as in draw_axes.c */
   /* Draw tick mark at left end of x axis.
   if((data->num_pts-1) % 4 == 0)
      XDrawLine(XtDisplay(w), data->pix[3], data->gc[3],
		data->end_x-1, line_width/2,
		data->end_x-1, line_width/2+14);
   else
      XDrawLine(XtDisplay(w), data->pix[3], data->gc[3],
		data->end_x-1, line_width/2,
		data->end_x-1, line_width/2+6);
*/

/*
 * Make sure line width is set to 1 after all this.
 */
   line_width = 1;
   XSetLineAttributes(XtDisplay(w), data->gc[3],
		      line_width, 0, 0, 0);

/*  Label x axis */
   /*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
   /*--AV--*/
   font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
   XSetFont(XtDisplay(w), data->gc[3], font_id);
   label_font = XQueryFont(XtDisplay(w), font_id);

   /* Determine number of points to skip before writing next label
      so labels do not overwrite each other
   */
   num_skip = 4;
   text_width = (int)XTextWidth(label_font, "00.00", 5);
   num_labels = (data->num_pts-1) / num_skip + 1;
   while((text_width*num_labels) > (data->end_x-num_labels) &&
	 data->end_x > 100.0)
   {
      num_skip += 4;
      num_labels = (data->num_pts-1) / num_skip + 1;
   }

   for(i=0; i<data->num_pts; i+=num_skip)
   {
      if(i == data->num_pts-1)
	 XDrawLine(XtDisplay(w), data->pix[3], data->gc[3],
		   ticks[i].x1, ticks[i].y1,
		   ticks[i].x2, ticks[i].y2+8);
      else
	 XDrawLine(XtDisplay(w), data->pix[3], data->gc[3],
		   ticks[i].x1, ticks[i].y1,
		   ticks[i].x2, ticks[i].y2+8);

      length = strlen(data->day_hr[i]);
      x_offset = (int)(XTextWidth(label_font, data->day_hr[i], length)/2);

      /* Check if the string is at the start or end of the axis then
	 draw at the appropriate place.
      */
      if(ticks[i].x1+(x_offset*2) > .9*data->form_width)
	 XDrawString(XtDisplay(w), data->pix[3], data->gc[3],
		     ticks[i].x1-(x_offset*2), ticks[i].y2+20,
		     data->day_hr[i], length);
      else if(ticks[i].x1-x_offset < data->origin_x)
	 if(ticks[i+num_skip].x1-x_offset > ticks[i].x1+x_offset*2+1)
	    XDrawString(XtDisplay(w), data->pix[3], data->gc[3],
			ticks[i].x1, ticks[i].y2+20,
			data->day_hr[i], length);
	 else
	    continue;
      else
	 XDrawString(XtDisplay(w), data->pix[3], data->gc[3],
		     ticks[i].x1-x_offset, ticks[i].y2+20,
		     data->day_hr[i], length);
   }


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_draw_x_axis.c,v $";
 static char rcs_id2[] = "$Id: mp_draw_x_axis.c,v 1.3 2006/04/07 14:34:48 aivo Exp $";}
/*  ===================================================  */

}
