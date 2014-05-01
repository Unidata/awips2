/* File:log_int.c
 *
 *  Log interpolation given arrays xa and ya, number of values, n,
 *  and the x coordinate.
 */

#include "math.h"
float log_int(xa, ya, n, x)
   int          n;
   float        xa[], ya[], x;
   {
      int       k;    /* calculated array index value */
      int       lo;   /* lower array index value */
      int       hi;   /* higher array index value */
      float     b;    /* calculated multiplication factor */
      float     y;    /* returned interpolated y value */
      double     logyalo, logyahi; /* array log values */
      double     logxalo, logxahi;

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

      logyahi = log(ya[hi]);
      logyalo = log(ya[lo]);
      logxahi = log(xa[hi]);
      logxalo = log(xa[lo]);

      b = (logyahi - logyalo)/(logxahi - logxalo);
      y = logyalo + b*(log(x) - logxalo);
      y = exp(y);
      return y;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/log_int.c,v $";
 static char rcs_id2[] = "$Id: log_int.c,v 1.1 1995/09/08 14:57:39 page Exp $";}
/*  ===================================================  */

      }
