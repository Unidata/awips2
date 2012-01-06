/* File: mp_draw_end_obs_line.c */

#include "mods_plot.h"

void mp_draw_end_obs_line(w, data)
   Widget                        w;     /* Widget data structure        */
   mods_plot_struct             *data;  /* Data structure pointer       */
{
   int   n;                     /* Counter                              */
   Arg   wargs[10];             /* Window resource data structure array */
   Dimension    width;          /* Window width         */
   Dimension    height;         /* Window height        */
   GC           info_gc;        /* Graphics context data structures     */
   Pixel        foreground;     /* Window foreground color              */
   Pixel        background;     /* Window background color              */
   XGCValues    info_gcv;       /* Graphics context data structures     */
   float        val;
   int          x1;             /* Horizontal pixel location            */
   int          y1;             /* Vertical pixel location              */
   int          x2;             /* Horizontal pixel location            */
   int          y2;             /* Vertical pixel location              */

  /* printf("in draw_info_lines\n"); */

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

/* Create graphics context for end_obs line.  Can be changed for
   flood and alert lines if needed (if rating curve exists).
*/
   info_gcv.foreground = get_pixel_by_name(w, "blue");
   info_gcv.line_style = LineOnOffDash;
   info_gc = XCreateGC(XtDisplay(w), data->pix[2],
		       GCForeground | GCLineStyle, &info_gcv);

   val = data->end_obs;
   x1 = x2 = val_to_pixel(&val, &data->min_x, &data->max_x,
			  &data->origin_x, &data->end_x);
   y1 = height;
   y2 = 0;

   if(x1 > 0.0 && x1 <= width)
      XDrawLine(XtDisplay(w), data->pix[2], info_gc, x1, y1, x2, y2);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_draw_end_obs_line.c,v $";
 static char rcs_id2[] = "$Id: mp_draw_end_obs_line.c,v 1.1 1995/09/08 14:59:03 page Exp $";}
/*  ===================================================  */

}
