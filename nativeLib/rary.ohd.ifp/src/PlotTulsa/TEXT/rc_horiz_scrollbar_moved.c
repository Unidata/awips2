/* File: rc_horiz_scrollbar_moved.c
 *
 * Moves the scrollbar to the top or bottom of the scroll area in
 * response to a toTop or toBottom scrollbar event.
 *
 * Clears the window and generates an Expose event for each of the
 * plot windows. The expose callback will cause the right part of
 * the pixmaps to be copied into each of the windows.
 *
 */

#include "rating_curve.h"
#include "ifp_struct.h"

void rc_horiz_scrollbar_moved(w, data, call_data)

  Widget                        w;          /* widget data structure */
  rc_struct                     *data;      /* rating curve structure pointer */
  XmScrollBarCallbackStruct     *call_data; /* call back scrollbar structure pointer */
{
   int  n;            /* array index */
   Arg  wargs[5];     /* window resource data structure array */
/*
 * If a toTop or toBottom scrollbar event, move the scrollbar
 * to the top or bottom of the scroll area
 */
   if(call_data->reason == 7 || call_data->reason == 8)
     {
      n = 0;
      XtSetArg(wargs[n], XmNvalue, call_data->value); n++;
      XtSetValues(w, wargs, n);
     }
/*
 * Clear the window and generate an Expose event for
 *   each of the plot windows.
 * The expose callback will cause the right part of
 *   the pixmaps to be copied into each of the windows.
 */
/*
 *       x_axis
 */
  if(XtIsRealized(data->drawing_area_widget[2]))
      XClearArea(XtDisplay(data->drawing_area_widget[2]),
		   XtWindow(data->drawing_area_widget[2]),
		   0, 0, 0, 0, TRUE);
/*
 *       x_axis_label
 */
  if(XtIsRealized(data->drawing_area_widget[3]))
      XClearArea(XtDisplay(data->drawing_area_widget[3]),
		   XtWindow(data->drawing_area_widget[3]),
		   0, 0, 0, 0, TRUE);
/*
 *       graph
 */
  if(XtIsRealized(data->drawing_area_widget[4]))
      XClearArea(XtDisplay(data->drawing_area_widget[4]),
		   XtWindow(data->drawing_area_widget[4]),
		   0, 0, 0, 0, TRUE);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rc_horiz_scrollbar_moved.c,v $";
 static char rcs_id2[] = "$Id: rc_horiz_scrollbar_moved.c,v 1.1 1995/09/08 14:57:49 page Exp $";}
/*  ===================================================  */

}

