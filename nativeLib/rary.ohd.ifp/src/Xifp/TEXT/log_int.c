/* File: log_int.c
 *
 *  Log interpolation given arrays xa and ya, number of values, n,
 *  and the x coordinate.
 *
 */

#include "math.h"
float log_int(xa, ya, n, x)
   int          n;
   float        xa[]; /* x array */
   float        ya[]; /* y array */
   float        x;    /* x coordinate value */

   {
      int       k;  /* average array index value */
      int       lo; /* lowest array index value */
      int       hi; /* highest array index value */
      float     b;  /* interpolation multiplication factor */
      float     y;  /* interpolated returned value */
      double     logyalo;  /* low y array log value */
      double     logyahi;  /* high y array log value */
      double     logxalo;  /* low x array log value */
      double     logxahi;  /* high x array log value */

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
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/log_int.c,v $";
 static char rcs_id2[] = "$Id: log_int.c,v 1.1 1995/09/08 15:00:53 page Exp $";}
/*  ===================================================  */

      }
