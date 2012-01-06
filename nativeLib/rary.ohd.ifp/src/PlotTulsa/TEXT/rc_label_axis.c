/* File: rc_label_axis.c
 *
 * Draws the rating curve axis labels.
 *
 */

#include "rating_curve.h"

void rc_label_axis(w, axis_label, pix, gc, xloc, height)
   Widget        w;              /* widget data structure */
   char          *axis_label;    /* axis label character pointer */
   Pixmap        pix;            /* graphic window resource */
   GC            gc;             /* graphics context data structure */
   int           xloc;           /* x label origin position */
   int           height;         /* label height */
{
   char          char_str[2];    /* label character string */
   int           i;              /* counter */
   int           label_length;
   int           y;              /* initial y label plot position */

   label_length = strlen(axis_label);

   y = height/2 - ((label_length/2+1)*15);

   for(i=0; i<label_length; i++)
   {
      memset(char_str, '\0', 2);
      memset(char_str, axis_label[i], 1);
      XDrawString(XtDisplay(w), pix, gc,
		  xloc, y+(i*15), char_str, 1);
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/rc_label_axis.c,v $";
 static char rcs_id2[] = "$Id: rc_label_axis.c,v 1.1 1995/09/08 14:57:51 page Exp $";}
/*  ===================================================  */

}
