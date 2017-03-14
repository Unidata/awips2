/* File: linear_int.c
 *
 * Linear interpolation given arrays xa and ya, number of values, n,
 * and the x coordinate.
 *
 */

/*test*/
float linear_int(xa, ya, n, x)
   int          n;
   float        xa[], ya[], x;
   {
      int       k;     /* array index */
      int       lo;    /* lower array member index value */
      int       hi;    /* higher array member index value */
      float     b;     /* calculated multiplication */
      float     y;     /* returned y interpolated value */

      lo = 0;
      hi = n;
      while(hi - lo > 1)
	   {
	   k = (hi +lo)/2;
	   if(xa[k] > x)
	      hi = k;
	   else
	      lo = k;
	   }
      b = (ya[hi] - ya[lo])/(xa[hi] - xa[lo]);
      y = (ya[lo] + b*(x - xa[lo]));
      return y;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/linear_int.c,v $";
 static char rcs_id2[] = "$Id: linear_int.c,v 1.1 1995/09/08 14:57:38 page Exp $";}
/*  ===================================================  */

      }
