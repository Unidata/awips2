/* File: resize_mp_x_axis_label.c
 *
 * Handles resize events for mods plot drawing areas.
 *
 * Creates a new image to proper scale and stores it in
 * the pixmap.
 *
 * Gets the width and height of the drawing area.
 *
 * Gets the foreground and background colors to make
 * the graphics context to draw into the pixmap.
 *
 * Clears the window and generates an Expose event for the
 * entire window.
 *
 */

#include "mods_plot.h"

void resize_mp_x_axis_label(w, data, call_data)

   Widget                        w;             /* widget data structure */
   mods_plot_struct              *data;         /* Mods plot structure data pointer */
   XmDrawingAreaCallbackStruct   *call_data;    /* XmDrawingArea call back structure pointer    */
{
   Arg          wargs[10];      /* Window resource data structure array */
   Dimension    width;          /* Window width         */
   Dimension    height;         /* Window height        */
   Pixel        foreground;     /* Window foreground color              */
   Pixel        background;     /* Window foreground color              */
   XGCValues    gcv;            /* graphics context data structures     */
   int          n;              /* Counter              */
   int          mask;           /* Foreground/backround graphic contexts mask   */
   XFontStruct  *label_font;    /* Font label structure pointer         */
   Font         font_id;        /* Font resource id     */
   char         axis_label[20]; /* axis label character array */
   int          x_loc;          /* x location of label string */
   int          length;         /* length of text in the label */
   int          x_offset;       /* x position from the label border */

   /*printf("in resize_mp_x_axis_label\n");*/
/*
 * Resize events for mods plot drawing areas.
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

/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->pix[4])
     XFreePixmap(XtDisplay(w), data->pix[4]);

  data->pix[4] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->pix[4] == 0)
    {
     printf("No space to create pixmap for discharge axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->gc[4] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->gc[4] = XCreateGC(XtDisplay(w), data->pix[4], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->gc[4], background);
  XFillRectangle(XtDisplay(w), data->pix[4], data->gc[4],
		  0, 0, width, height);

  XSetForeground(XtDisplay(w), data->gc[4], foreground);

  /* Set the appropriate axis label for the y axis */
  memset(axis_label, '\0', 20);
  if(data->mod_type_sw == UH || data->mod_type_sw == UHD)
     strcpy(axis_label, "TIME  (hours)");
  else if(data->mod_type_sw == ROCHNG || data->mod_type_sw == RRICHNG)
     strcpy(axis_label, "TIME  (day.hour)");
  else
     strcpy(axis_label, "unknown type");
/*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
  /*--*-misc-fixed-medium-r-normal--13-120-75-75-c-80-* is Rom11 --AV--*/
  font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
  XSetFont(XtDisplay(w), data->gc[4], font_id);
  /* Set x location of where to start drawing the label */
  label_font = XQueryFont(XtDisplay(w), font_id);
  length = strlen(axis_label);
  x_offset = (int)(XTextWidth(label_font, axis_label, length));
  x_loc = width/2 - x_offset/2;
  XDrawString(XtDisplay(w), data->pix[4], data->gc[4],
	      x_loc, height/2, axis_label, length);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/rz_mp_x_axis_label.c,v $";
 static char rcs_id2[] = "$Id: rz_mp_x_axis_label.c,v 1.3 2004/08/05 17:56:12 wkwock Exp $";}
/*  ===================================================  */

}
