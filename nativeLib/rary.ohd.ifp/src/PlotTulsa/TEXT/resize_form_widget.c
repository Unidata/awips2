/* File: resize_form_widget.c
 *
 * Gets the width and height of the drawing area and
 * sets width and height of form widget.
 *
 */

#include "plot.h"
#include "ifp_struct.h"

void resize_form_widget(w, form, call_data)

  Widget                        w;           /* widget data structures */
  Widget                        form;
  XmDrawingAreaCallbackStruct   *call_data;
{
  int   n;                       /* counter */
  Arg   wargs[10];               /* window resource data structure array */
  Dimension     width, height;   /* drawing area dimensions */

/* printf("in resize_form_widget\n"); */

/*
 * Get the width and height of the drawing area
 */
  n=0;
  XtSetArg(wargs[n], XmNwidth, &width); n++;
  XtSetArg(wargs[n], XmNheight, &height); n++;
  XtGetValues(w, wargs, n);
/*
 * Set width and height of form widget
 */
  n=0;
  XtSetArg(wargs[n], XmNwidth, width); n++;
  XtSetArg(wargs[n], XmNheight, height); n++;
  XtSetValues(form, wargs, n);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/resize_form_widget.c,v $";
 static char rcs_id2[] = "$Id: resize_form_widget.c,v 1.1 1995/09/08 14:58:00 page Exp $";}
/*  ===================================================  */

}
