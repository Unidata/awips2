/* File: resize_mp_form_widget.c
 *
 * Gets the width and height of the drawing area and
 * sets width and height of rating curve widget.
 *
 */

#include "mods_plot.h"

void resize_mp_form_widget(w, data, call_data)

  Widget                        w;              /* Widget data structure        */
  mods_plot_struct              *data;          /* Mods plot data structure pointer     */
  XmDrawingAreaCallbackStruct   *call_data;     /* XmDrawingArea call back structure pointer    */
{
  int   n;                              /* Counter      */
  Arg   wargs[10];                      /* window resource data structure array */
  Dimension     width;                  /* window width */
  Dimension     height;                 /* window height        */

 /*printf("in resize_mp_form_widget\n");*/

/*
 * Get the width and height of the drawing area
 */
  n=0;
  XtSetArg(wargs[n], XmNwidth, &width); n++;
  XtSetArg(wargs[n], XmNheight, &height); n++;
  XtGetValues(w, wargs, n);

/* Set width and height of form widget in structure. */
  data->form_width = width;
  data->form_height = height;

/*
 * Set width and height of form widget
 */
  n=0;
  XtSetArg(wargs[n], XmNwidth, width); n++;
  XtSetArg(wargs[n], XmNheight, height); n++;
  XtSetValues(data->form, wargs, n);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/resize_mp_form_widget.c,v $";
 static char rcs_id2[] = "$Id: resize_mp_form_widget.c,v 1.1 1995/09/08 14:59:12 page Exp $";}
/*  ===================================================  */

}

