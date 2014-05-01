/* File: resize_rc_x_axis_label.c
 *
 * Resizes events for mods plot drawing areas.
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

#include "rating_curve.h"
#include "libXifp.h"
#include "mod_struct.h"
#define  ChooseTStypestruct
#include "mods_info.h"
#include "libXs.h"
#include "ifp_globals.h"

void resize_rc_x_axis_label(w, data, call_data)

   Widget                        w;         /* widget data structure */
   rc_struct                     *data;     /* rating curve data structure pointer */
   XmDrawingAreaCallbackStruct   *call_data;
{
   Arg           wargs[10];                 /* window resource data structure array */
   Dimension     width, height;             /* width and height of drawing area */
   Pixel         foreground, background;    /* foreground/backround colors */
   XGCValues     gcv;                       /* graphics context data structure */
   int           n;                         /* counter */
   int           mask;                      /* mask to declare which fields are valid */
   XFontStruct   *label_font;               /* font label structure pointer */
   Font          font_id;                   /* font resource id */
   char          axis_label[20];            /* axis label character array */
   int           x_loc;                     /* x location of label string */
   int           length;                    /* length of text in the label */
   int           x_offset;                  /* x position from the label border */
   int           direction;                 /* direction text is drawn */
   int           ascent;                    /* position above the baseline */
   int           descent;                   /* position below the baseline */
   XCharStruct   char_info;                 /* width, left bearing, right bearing string
					       information structure */

   /*printf("in resize_rc_x_axis_label\n");*/
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
  if(data->pix[3])
     XFreePixmap(XtDisplay(w), data->pix[3]);

  data->pix[3] = XCreatePixmap(XtDisplay(w),
			     DefaultRootWindow(XtDisplay(w)),
			     width, height,
			     DefaultDepthOfScreen(XtScreen(w))
			    );
  if(data->pix[3] == 0)
    {
     printf("No space to create pixmap for discharge axis drawing area\n");
     exit(1);
    }
/*
 * Create graphics context if needed.
 */
  if(data->gc[3] == NULL)
    {
     mask = GCForeground | GCBackground;
     gcv.foreground = foreground;
     gcv.background = background;
     data->gc[3] = XCreateGC(XtDisplay(w), data->pix[3], mask, &gcv);
    }
/*
 * Set background color of pixmap.
 * Must set foreground of gc to background color before
 *   calling XFillRectangle to set background color of pixmap.
 */
  XSetForeground(XtDisplay(w), data->gc[3], background);
  XFillRectangle(XtDisplay(w), data->pix[3], data->gc[3],
		  0, 0, width, height);

  XSetForeground(XtDisplay(w), data->gc[3], foreground);

  /* Set the appropriate axis label for the y axis */
  memset(axis_label, '\0', 20);
  if(NWSRFS_general_units == 0)
     strcpy(axis_label, "DISCHARGE  cfs");
  else
     strcpy(axis_label, "DISCHARGE  cms");

  /*font_id = XLoadFont(XtDisplay(w), "Rom11");*/
  /*--*-misc-fixed-medium-r-normal--13-120-75-75-c-80-* is Rom11 --AV--*/
  font_id = XLoadFont(XtDisplay(w), "*-misc-fixed-medium-r-normal--13-120-75-75-c-80-*");
  XSetFont(XtDisplay(w), data->gc[3], font_id);
  /* Set x location of where to start drawing the label */
  label_font = XQueryFont(XtDisplay(w), font_id);
  length = strlen(axis_label);
  x_offset = (int)(XTextWidth(label_font, axis_label, length));
  x_loc = width/2 - x_offset/2;
  XTextExtents(label_font, axis_label, length, &direction,
	       &ascent, &descent, &char_info);

  XDrawString(XtDisplay(w), data->pix[3], data->gc[3],
	      x_loc, (height-descent), axis_label, length);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rz_rc_x_axis_label.c,v $";
 static char rcs_id2[] = "$Id: rz_rc_x_axis_label.c,v 1.2 2002/02/11 19:42:34 dws Exp $";}
/*  ===================================================  */

}
