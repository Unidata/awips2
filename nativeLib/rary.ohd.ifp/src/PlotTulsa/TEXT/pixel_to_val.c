/* File: pixel_to_val.c
 *
 * Converts a pixel value to a value referenced to the xy graph axis.
 *
 */

float  pixel_to_val(pix_val, min, max, origin, end)
   float        *min, *max;   /* minimum ,maximum x or y axis value */
   int          *pix_val;     /* pixel value */
   int          *origin;      /* pixel value of the axis origin */
   int          *end;         /* pixel value of the end of the x or y axis */
       {
      float     val;

	 val = *min + (*pix_val - *origin)*(*max - *min)/
	       (*end - *origin);
      return val;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/pixel_to_val.c,v $";
 static char rcs_id2[] = "$Id: pixel_to_val.c,v 1.1 1995/09/08 14:57:41 page Exp $";}
/*  ===================================================  */

      }
