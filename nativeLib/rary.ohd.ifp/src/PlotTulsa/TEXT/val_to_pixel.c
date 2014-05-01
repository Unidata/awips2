/* File: val_to_pixel.c
 *
 * Returns the pixel value for a given data value.
 * If the values is missing (-999) then sets it to
 * a value outside the area.  dp - 18 Nov. 1997
 *
 */

int val_to_pixel(val, min, max, origin, end)
   float        *val;   /* data value pointer */
   float        *min, *max;  /* minimum and  maximum data values pointers */
   int          *origin, *end; /* origin and end pixel value pointers */
   {
      int       pix_val;  /* pixel value */
      if((int)*val == -999)
         pix_val = *origin + 10;
      else
         pix_val = (int)(*origin + (*end - *origin)*(*val - *min)/
		     (*max - *min));
      return pix_val;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/val_to_pixel.c,v $";
 static char rcs_id2[] = "$Id: val_to_pixel.c,v 1.2 1997/12/31 19:21:26 page Exp $";}
/*  ===================================================  */

   }

