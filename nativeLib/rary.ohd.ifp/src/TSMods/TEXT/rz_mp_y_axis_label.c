/* File: resize_mp_y_axis_label.c
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

void resize_mp_y_axis_label(w, data, call_data)

   Widget                        w;             /* widget data structure */
   mods_plot_struct              *data;         /* Mods plot data structure pointer     */
   XmDrawingAreaCallbackStruct   *call_data;    /* XmDrawingArea call back structure pointer    */
{
   Arg          wargs[10];              /* window resource data structure array */
   Dimension    width;                  /* Window width         */
   Dimension    height;                 /* Window height        */
   Pixel        foreground;             /* window foreground color              */
   Pixel        background;             /* window background color              */
   XGCValues    gcv;                    /* graphics context data structures     */
   int          n;                      /* Counter              */
   int          mask;                   /* Foreground/backround graphic contexts mask   */
   Font         font_id;                /* Font resource id     */
   char         axis_label[25];         /* axis label character array           */

   /*printf("in resize_mp_y_axis_label\n");*/

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
  if(data->pix[0])
     XFreePixmap(XtDisplay(w), data->pix[0]);

  data->pix[0] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->pix[0] == 0)
    {
     printf("No space to create pixmap for discharge axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->gc[0] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->gc[0] = XCreateGC(XtDisplay(w), data->pix[0], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->gc[0], background);
  XFillRectangle(XtDisplay(w), data->pix[0], data->gc[0],
		  0, 0, width, height);

  XSetForeground(XtDisplay(w), data->gc[0], foreground);

/* Set the appropriate axis label for the y axis */
  memset(data->mp_tracker_label, '\0', 4);
  memset(axis_label, '\0', 20);
  if(data->mod_type_sw == UH || data->mod_type_sw == UHD)
  {
     strcpy(axis_label, "ORDINATES ");
     strcpy(data->mp_tracker_label, "UHG");

     if(data->unit_sw == METRIC)
	strcat(axis_label, "cms/mm");
     else /* data->unit_sw == ENGLISH */
	strcat(axis_label, "1000 cfs/inch");
  }
  else /* ROCHNG or RRICHNG */
  {
     if(data->mod_type_sw == ROCHNG)
     {
	strcpy(axis_label, "RUNOFF ");
	strcpy(data->mp_tracker_label, "RO");
     }
     else if(data->mod_type_sw == RRICHNG && data->px_type_flag == RAIM)
     {
	strcpy(axis_label, "RAIN + MELT ");
	strcpy(data->mp_tracker_label, "RRI");
     }
     else if(data->mod_type_sw == RRICHNG && data->px_type_flag == MAP)
     {
	strcpy(axis_label, "RAINFALL ");
	strcpy(data->mp_tracker_label, "RRI");
     }

     if(data->unit_sw == METRIC)
	strcat(axis_label, "mm");
     else /* data->unit_sw == ENGLISH */
	strcat(axis_label, "inches");
  }

  /*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
  /*--*-misc-fixed-medium-r-normal--13-120-75-75-c-80-* is Rom11 --AV--*/
  font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
  XSetFont(XtDisplay(w), data->gc[0], font_id);
  mp_label_axis(w, axis_label, data->pix[0], data->gc[0],
		10, height);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/rz_mp_y_axis_label.c,v $";
 static char rcs_id2[] = "$Id: rz_mp_y_axis_label.c,v 1.3 2004/08/05 17:56:33 wkwock Exp $";}
/*  ===================================================  */

}
