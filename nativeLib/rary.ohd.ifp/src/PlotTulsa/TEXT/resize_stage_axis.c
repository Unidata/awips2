/* File: resize_stage_axis.c
 *
 * Resizes events for Tulsa plot drawing areas.
 *
 * Creates a new image to proper scale and stores it in
 * the pixmap.
 *
 * Gets the width and height of the drawing area.
 *
 * Gets the foreground and background colors to make
 * the graphics context to draw into the pixmap.
 *
 * Sets the origin and end pixel values for the y axis.
 *
 */





#include "plot.h"
#include "ifp_struct.h"
#include "libXifp.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "mods_info.h"
#include "libXs.h"
#include "ifp_globals.h"


float         stage_to_q();     /* converts stage data to dischange rate data */
float         q_to_stage();     /* converts dischange rate data to stage data */

void resize_stage_axis(w, data, call_data)

  Widget                        w;            /* widget data structure */
  combined_struct               *data;        /* tables and plot data structure pointer */
  XmDrawingAreaCallbackStruct   *call_data;
{
  int   i, n;                                 /* counters */
  Arg   wargs[10];                            /* window resource data structure array */
  Dimension     width, height;                /* width and height of the drawing area */
  Dimension     pix_height;                   /* graph height */
  Pixel         foreground, background;       /* foreground, background colors */
  unsigned int  line_width;
  XSegment      ticks[11];                    /* tick marks on the y axis */
  int           mask;                         /* mask to declare which fields are valid */
  XGCValues     gcv;                          /* graphics context data structure */
  int           num_ticks=11;                 /* number of y axis tick marks */
  int           x_offset;                     /* x position from the label border */
  int           y_offset;                     /* y position from the label border */
  int           direction;                    /* direction text is drawn */
  int           ascent;                       /* position above the baseline */
  int           descent;                      /* position below the baseline */
  char          stg_label[14];                /* stage label character array */
  XFontStruct   *label_font;                  /* label font resource structure */
  Font          font_id;                      /* font resource id */
  XCharStruct   char_info;                    /* width, left bearing, right bearing string
						 information structure */
  int           length;                       /* length of stage label */
  float         discharge;                    /* river discharge rate */
  float         stage;                        /* river stage data */

  /* printf("in resize_stage_axis\n"); */

/*
 * Resize events for Tulsa plot drawing areas.
 * Create new image to proper scale and store in pixmap.
 *
 * Get the width and height of the drawing area.
 * Also, get foreground and background colors to make
 *  graphics context to draw into pixmap.
 */
  n=0;
  XtSetArg(wargs[n], XmNwidth, &width); n++;
  XtSetArg(wargs[n], XmNheight, &height); n++;
  XtSetArg(wargs[n], XmNforeground, &foreground); n++;
  XtSetArg(wargs[n], XmNbackground, &background); n++;
  XtGetValues(w, wargs, n);

  pix_height = data->plot->height_scale * height;
/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->plot->pix[6])
     XFreePixmap(XtDisplay(w), data->plot->pix[6]);

  data->plot->pix[6] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, pix_height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->plot->pix[6] == 0)
    {
     printf("No space to create pixmap for stage axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->plot->gc[6] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->plot->gc[6] = XCreateGC(XtDisplay(w), data->plot->pix[6], mask, &gcv);
    }
/*
 * Set background color of pixmap and put
 *   circle (elipse) into pixmap just to see if this works.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->plot->gc[6], background);
  XFillRectangle(XtDisplay(w), data->plot->pix[6], data->plot->gc[6],
		  0, 0, width, pix_height);

  XSetForeground(XtDisplay(w), data->plot->gc[6], foreground);
  line_width = 4;
  XSetLineAttributes(XtDisplay(w), data->plot->gc[6],
		     line_width, 0, 0, 0);
  XDrawLine(XtDisplay(w), data->plot->pix[6], data->plot->gc[6],
	    line_width/2, 0,
	    line_width/2, pix_height);

  line_width = 1;
  XSetLineAttributes(XtDisplay(w), data->plot->gc[6],
		     line_width, 0, 0, 0);

  if (data->plot->h_q_plot_indicator != 2)
  {
  for(i=0; i<num_ticks; i++)
     {
      ticks[i].x1 = 0;
      ticks[i].x2 = 10;
      ticks[i].y1 = ticks[i].y2 = pix_height -
				   ((pix_height * i) / (num_ticks-1));
				   /*(((9 * pix_height * i) / 10) / 10);*/
     }
  XDrawSegments(XtDisplay(w), data->plot->pix[6], data->plot->gc[6],
		ticks, num_ticks);
  /* Draw tick mark at bottom of stage axis. */
  XDrawLine(XtDisplay(w), data->plot->pix[6], data->plot->gc[6],
	    line_width/2, pix_height-1,
	    line_width/2+10, pix_height-1);
  }

/*
 * Make sure line width is set to 1 after all this.
 */
  line_width = 1;
  XSetLineAttributes(XtDisplay(w), data->plot->gc[6],
		     line_width, 0, 0, 0);

/*  Label y axis:  set the font and get info on string size in pixels
		   determine adjustments to placement of labels
		   determine the maximum value for stage axis
		   create and draw labels
		   only draw stage axis if there's either no rc shift mod in 
		     effect (h_q_plot_indicator = 0) or if there is, it's a 
		     stage plot (h_q_plot_indicator = 2)
*/

  if((data->plot->rc_data->RC == TRUE) && (data->plot->h_q_plot_indicator != 1))
  {
     /*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
  /*--*-misc-fixed-medium-r-normal--13-120-75-75-c-80-* is Rom11 --AV--*/
  font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
     XSetFont(XtDisplay(w), data->plot->gc[6], font_id);
     label_font = XQueryFont(XtDisplay(w), font_id);

     sprintf(stg_label, "%.2f",
	     ((*data->plot->discharge_axis_max - *data->plot->min_y) * 0/
	     (num_ticks - 1)));
     length = strlen(stg_label);
     x_offset = (int)(XTextWidth(label_font, stg_label, length));
     XTextExtents(label_font, stg_label, length, &direction,
		  &ascent, &descent, &char_info);
     y_offset = (int)((ascent+descent)/4);

     for(i=0; i<num_ticks; i++)
     {
	if(NWSRFS_general_units == 0)
	{
	   discharge = (*data->plot->discharge_axis_max - *data->plot->min_y) * i/
		       (num_ticks - 1);
	   /* convert from English to metric */
	   discharge = (discharge - data->plot->rc_data->q_add_constant) /
		       data->plot->rc_data->q_mult_conver_factor;

	   stage = q_to_stage(discharge);

	   /* convert from metric to English*/
	   stage = stage * data->plot->rc_data->stg_mult_conver_factor +
		   data->plot->rc_data->stg_add_constant;
	}
	else
	{
	   discharge = (*data->plot->discharge_axis_max - *data->plot->min_y) * i/
		       (num_ticks - 1);
	   stage = q_to_stage(discharge);
	}

	sprintf(stg_label, "%.2f", stage);

	/* Label placement: if lettering will go off bottom or top of drawing
	   area, adjust placement, otherwise center it on the tick mark.
	*/
	length = strlen(stg_label);
	if(ticks[i].y1+(y_offset*4) > pix_height)
	   XDrawString(XtDisplay(w), data->plot->pix[6], data->plot->gc[6],
		       ticks[i].x2+2, ticks[i].y1-y_offset,
		       stg_label, length);
	else if(ticks[i].y1-(y_offset*4) < data->plot->end_y)
	   XDrawString(XtDisplay(w), data->plot->pix[6], data->plot->gc[6],
		       ticks[i].x2+2, ticks[i].y1+y_offset*4,
		       stg_label, length);
	else
	   XDrawString(XtDisplay(w), data->plot->pix[6], data->plot->gc[6],
		       ticks[i].x2+2, ticks[i].y1+y_offset,
		       stg_label, length);
     }
  }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/resize_stage_axis.c,v $";
 static char rcs_id2[] = "$Id: resize_stage_axis.c,v 1.5 2006/03/28 20:44:24 aivo Exp $";}
/*  ===================================================  */

}
