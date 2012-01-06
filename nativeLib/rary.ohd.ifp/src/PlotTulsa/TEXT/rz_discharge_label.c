/* File: resize_discharge_label.c
 *
 * Resizes events for Tulsa plot drawing areas.
 * Creates new image to proper scale and store in pixmap.
 *
 * Gets the width and height of the drawing area.
 * Gets the foreground and background colors to make
 * the graphics context to draw into pixmap.
 *
 * Clears the window and generate an Expose event for the
 * entire window.
 *
 * Frees the old pixmap and creates one the size of the window.
 *
 */

#include "plot.h"
#include "ifp_struct.h"
#include <X11/Intrinsic.h>
#include "libXifp.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "mods_info.h"
#include "libXs.h"
#include "ifp_globals.h"
#include "c_call_f/fdcode.h"

void resize_discharge_label(w, data, call_data)

  Widget                        w;            /* widget data structure */
  plot_cb_struct                *data;        /* call back graph data structure pointer */
  XmDrawingAreaCallbackStruct   *call_data;
{
  int   i, n;                                 /* counters */
  Arg   wargs[10];                            /* window resource data structure array */
  Dimension     width, height;                /* width and height of the drawing area */
  Pixel         foreground, background;       /* foreground, background colors        */
  int           mask;                         /* mask to declare which fields are valid */
  XGCValues     gcv;                          /* graphics context data structure */
  char          axis_label[20];               /* graph axis labels */
  Font          font_id;                      /* font resource id */
  char          type_string[4];               /* data type code */
  char          std_units[4];                 /* Code for the standard forecast component */
  char          dimen[4];                     /* Data type dimension */
  char          time_scale[4];                /* Time scale for the data type */
  int           missing_allowed;              /* Indicates if missing data are allowed for
						 this data type in the forecast component. */
  int           nv_dt;                        /* number of values per time interval for this data type. */
  int           nadd;                         /* Number of pieces of additional information */
  int           err_flag;                     /* Number of pieces of additional information */

 /*  printf("in resize_discharge_label\n"); */

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
/*
 * Clear the window and generate an Expose event for the
 *   entire window.
 */
  if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
/*
 * Free the old pixmap and create one the size of the window.
 */
  if(data->discharge_label_pix)
     XFreePixmap(XtDisplay(w), data->discharge_label_pix);

  data->discharge_label_pix = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->discharge_label_pix == 0)
    {
     printf("No space to create pixmap for discharge_label drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->discharge_label_gc == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->discharge_label_gc = XCreateGC(XtDisplay(w), data->discharge_label_pix, mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->discharge_label_gc, background);
  XFillRectangle(XtDisplay(w), data->discharge_label_pix, data->discharge_label_gc,
		  0, 0, width, height);

  XSetForeground(XtDisplay(w), data->discharge_label_gc, foreground);

  memset(data->tracker_label, '\0', 5);
  memset(axis_label, '\0', 20);
  strncpy(type_string, data->disch_label_data_type, 4);
  FDCODE(type_string, std_units, dimen, &missing_allowed, &nv_dt,
	 time_scale, &nadd, &err_flag);
  if(strncmp(dimen, "L3/T", 4) == 0)
  {
     strcpy(data->tracker_label, "Q");
     if(data->h_q_plot_indicator != 2)
        if(NWSRFS_general_units == 0)
	   strncpy(axis_label, "DISCHARGE cfs", 13);
        else
	   strncpy(axis_label, "DISCHARGE cms", 13);
     else  /* only label this if h_q_plot_indicator=2 */
        strncpy(axis_label, "RC SHIFT", 8);
  }
  else if(strncmp(dimen, "L   ", 4) == 0)
  {
     if( strncmp(std_units, "FT", 2) == 0 || 
	 strncmp(std_units, "M ", 2) == 0 )
     {
        strcpy(data->tracker_label, "Elev");
        if(NWSRFS_general_units == 0)
	   strncpy(axis_label, "ELEVATION ft", 12);
        else
	   strncpy(axis_label, "ELEVATION m", 11);
     }
     else if( strncmp(std_units, "IN", 2) == 0 ||
              strncmp(std_units, "MM", 2) == 0 )
     {
        strcpy(data->tracker_label, "Depth");
        if(NWSRFS_general_units == 0)
           strncpy(axis_label, "Depth in", 8);
        else
           strncpy(axis_label, "Depth mm", 8);
     }
  }
  else if(strncmp(dimen, "L3  ", 4) == 0)
  {
     strcpy(data->tracker_label, "Vol");
     if(NWSRFS_general_units == 0)
	strncpy(axis_label, "VOLUME CFSD", 11);
     else
	strncpy(axis_label, "VOLUME M3", 9);
  }
  else if(strncmp(dimen, "TEMP", 4) == 0)
  {
     strcpy(data->tracker_label, "Deg");
     if(NWSRFS_general_units == 0)
	strncpy(axis_label, "Degrees Fahrenheit", 18);
     else
	strncpy(axis_label, "Degrees Celsius", 15);
  }
  else if(strncmp(dimen, "DLES", 4) == 0)
  {
     strcpy(data->tracker_label, "%");
     strncpy(axis_label, "Percentage", 10);
  }
  else printf("Error: invalid dimensions from FDCODE\n");

  /*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
  /*--*-misc-fixed-medium-r-normal--13-120-75-75-c-80-* is Rom11 --AV--*/
  font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
  XSetFont(XtDisplay(w), data->discharge_label_gc, font_id);
  label_axis(w, axis_label, data->discharge_label_pix,
	     data->discharge_label_gc,
	     10, height);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rz_discharge_label.c,v $";
 static char rcs_id2[] = "$Id: rz_discharge_label.c,v 1.5 2006/02/23 17:01:12 hsu Exp $";}
/*  ===================================================  */

}
