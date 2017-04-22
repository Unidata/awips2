#include "stage3_interface.h"
#include "stage3.h"
#include "drawa.h"
#include "stage3_globals.h"


/***************************************************************************/
/*  FUNCTION NAME:   get_reverse_color                                     */
/*       FUNCTION:   determine color to display text in row column widget  */
/***************************************************************************

Function type:
   integer

Called by function:
   display_gage

Functions called:
   none

Local variables:
   w - Widget structure; Drawing Area
   bg - integer; background color of widget
   color - XColor structure; color structure of background
   fgcolor - XColor structure; created foreground color

******************************************** BEGIN get_reverse_color *******/

int get_reverse_color(w, bg)
   Widget       w;
   int          bg;
{
   Display     *dpy = XtDisplay(w);
   int          scr = DefaultScreen(dpy);
   Colormap     cmap = DefaultColormap(dpy, scr);
   XColor       color, fgcolor;

 /*--------------------------------------------------------------*/
 /*     determine background color in Colormap (rgb) structure   */
 /*--------------------------------------------------------------*/
  color.pixel = bg;
  XQueryColor(dpy, cmap, &color);

 /*--------------------------------------------------------------*/
 /*     if bg has less than 50% green use full green             */
 /*     if bg has more than 50% green use almost none            */
 /*--------------------------------------------------------------*/
 if (color.green <= 32767)
    fgcolor.green = 65000;
 else
    fgcolor.green = 1000;

 /*--------------------------------------------------------------*/
 /*     if bg has less than 50% blue use full blue               */
 /*     if bg has more than 50% blue use almost none             */
 /*--------------------------------------------------------------*/
 if (color.blue <= 32767)
    fgcolor.blue = 65000;
 else
    fgcolor.blue = 1000;

 /*--------------------------------------------------------------*/
 /*     red level should remain same                             */
 /*--------------------------------------------------------------*/
 fgcolor.red = color.red;

 /*--------------------------------------------------------------*/
 /*     determine fgcolor in XColor coordinate structure         */
 /*--------------------------------------------------------------*/
 XAllocColor(dpy, cmap, &fgcolor);
 return (fgcolor.pixel);
}

/********************************************* END get_reverse_color *******/

