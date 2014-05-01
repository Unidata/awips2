/* File: linear_int.c
 *
 *  Linear interpolation given arrays xa and ya, number of values, n,
 *  and the x coordinate.
 *
 */

float linear_int(xa, ya, n, x)
   int          n;    /* number of values*/
   float        xa[]; /* x array */
   float        ya[]; /* y array */
   float        x;    /* x coordinate value */
   {
      int       k;  /* average array index value */
      int       lo; /* lowest array index value */
      int       hi; /* highest array index value */
      float     b;  /* interpolation multiplication factor */
      float     y;  /* interpolated returned value */

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
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/linear_int.c,v $";
 static char rcs_id2[] = "$Id: linear_int.c,v 1.1 1995/09/08 15:00:51 page Exp $";}
/*  ===================================================  */

      }
