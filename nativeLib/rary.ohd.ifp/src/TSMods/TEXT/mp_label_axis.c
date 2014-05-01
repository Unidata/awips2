/* File: mp_label_axis.c
 *
 * Draws the rating curve axis labels.
 *
 */

#include "mods_plot.h"

void mp_label_axis(w, axis_label, pix, gc, xloc, height)
   Widget       w;              /* Widget data structure */
   char         *axis_label;    /* Axis label character pointer */
   Pixmap       pix;            /* Graphic window resource */
   GC           gc;             /* Graphics context data structure */
   int          xloc;           /* X label origin position */
   int          height;         /* Label height */
{
   char         char_str[2];    /* Label character string */
   int          i;              /* Counter */
   int          label_length;
   int          y;              /* Initial y label plot position */

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
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/TSMods/RCS/mp_label_axis.c,v $";
 static char rcs_id2[] = "$Id: mp_label_axis.c,v 1.1 1995/09/08 14:59:07 page Exp $";}
/*  ===================================================  */

}
