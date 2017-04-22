/* File: label_axis.c
 *
 * Creates the axis label for the xy plot.
 *
 */

#include "plot.h"
#include "ifp_struct.h"

void label_axis(w, axis_label, pix, gc, xloc, height)
   Widget        w;
   char          *axis_label;         /* axis label character pointer */
   Pixmap        pix;
   GC            gc;                  /* graphic context structure */
   int           xloc;                /* x starting position of string */
   int           height;              /* label position height */
{
   char          char_str[2];         /* displayed string */
   int           i;                   /* counter */
   int           label_length;        /* label length value */
   int           y;                   /* y position value */

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
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/label_axis.c,v $";
 static char rcs_id2[] = "$Id: label_axis.c,v 1.1 1995/09/08 14:57:36 page Exp $";}
/*  ===================================================  */

}
