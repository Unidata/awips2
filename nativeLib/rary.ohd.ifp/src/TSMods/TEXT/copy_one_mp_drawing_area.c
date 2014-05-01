#include "mods_plot.h"

/* File: copy_one_mp_drawing_area.c
 *
 * This function handles expose events for any drawing area within
 *  the graphical mods plot display.
 *
 * When the event window matches window of current drawing area
 *  and event count eq 0 (i.e., at end of series of exposes),
 *  copy from the pixmap to the window.
 *
 * Figure out which drawing area's pixmap to copy by matching
 *  the widget w with the drawing_area_widgets in the mods_plot_struct.
 *
 */

void copy_one_mp_drawing_area(w, data, call_data)

  Widget                        w;		/* widget data structure	*/
  mods_plot_struct              *data;          /* mods plot data structure pointer     */
  XmDrawingAreaCallbackStruct   *call_data;     /* XmDrawingArea call back structure pointer            */
{
  XExposeEvent  *event = (XExposeEvent *) call_data->event; /* expose event pointer     */
  int           n;                                          /* window resource array index      */
  Arg           wargs[5];                                   /* window resource data structure array     */
  Dimension     width, height;                              /* window width & height    */

  /* printf("in copy_one_mp_drawing_area\n"); */

  if(event->window == XtWindow(w) && event->count == 0)
    {
/*
 * Last event in series.
 * Get width and height of widget's window.
*/
     n=0;
     XtSetArg(wargs[n], XmNwidth, &width); n++;
     XtSetArg(wargs[n], XmNheight, &height); n++;
     XtGetValues(w, wargs, n);

/*
 * Copy appropriate part of pixmap into the window,
 *  depending on which drawing area we are currently copying.
 */
     if(w == data->drawing_area_widget[0])      /* y_axis_label */
       {
	XCopyArea(XtDisplay(w), data->pix[0],
		  XtWindow(w),  data->gc[0],
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[1])     /* y_axis */
       {
	XCopyArea(XtDisplay(w), data->pix[1], XtWindow(w), data->gc[1],
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[2])     /* graph */
       {
	XCopyArea(XtDisplay(w), data->pix[2], XtWindow(w), data->gc[2],
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[3])     /* x_axis */
       {
	XCopyArea(XtDisplay(w), data->pix[3], XtWindow(w), data->gc[3],
		  0, 0,
		  width, height,
		  0, 0);
       }
     else if(w == data->drawing_area_widget[4])     /* x_axis_label */
       {
	XCopyArea(XtDisplay(w), data->pix[4], XtWindow(w), data->gc[4],
		  0, 0,
		  width, height,
		  0, 0);
       }
     else
       {
	printf("Exposed widget does not match TulPlot drawing area");
	printf(" -- BIG PROBLEM\n");
	exit(1);
       }
    }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/copy_one_mp_drawing_area.c,v $";
 static char rcs_id2[] = "$Id: copy_one_mp_drawing_area.c,v 1.1 1995/09/08 14:58:49 page Exp $";}
/*  ===================================================  */

}
