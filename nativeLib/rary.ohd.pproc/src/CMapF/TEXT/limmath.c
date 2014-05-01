#include <math.h>
#include "limmath.h"

double xpabova(double a, double b) {
double c = a * b , csq;
/* returns (exp (a * b) - 1. ) / a, or its limit as a -> 0. */
  if ( ( csq = .25 * c*c ) > .001) {
    return (exp( c ) - 1.) / a;
  } else {
/* exp(.5*a*b) * sinh(.5*a*b)*b/(.5*a*b) */
    return exp(.5 * c) * b * (1. + csq / 6. *
			     (1. + csq / 20. *
			     (1. + csq / 42. )));
  }
}

double lnabova(double a, double b) {
double c = a * b , t;
/* returns ln (1. + a * b) / a, or its limit as a -> 0. */
  if ( fabs(c) > .01) {
    return log( 1. + c ) / a;
  } else {
/* using (1. + t) / (1. - t) = 1. + c when t = c / (2. + c) */
    t = c/(2. + c); t *= t;
    return b / (2. + c) * (2.    + t *
			  (2./3. + t *
			  (2./5. + t *
			  (2./7. ))));
  }
}

double snabova(double a, double b) {
double c = a * b , csq ;
/* returns sin(a * b) / a, or its limit as a -> 0. */
  if ( (csq = c*c ) > .001) {
	 return (sin( c ) ) / a;
  } else {
	 return b * ( 1. - csq / 6. *
			 ( 1. - csq / 20. *
			 ( 1. - csq / 42. )));
  }
}

double csabova(double a, double b) {
/* returns (1. - cos(a * b) ) / a / a, or its limit as a -> 0 */
double term = snabova( .5 * a, b);
  return .5 * term * term;
}

double atnabova(double a, double b, double c) {
/* returns atan2(a*b,1-a*c)/a, or its limit as a -> 0 */
double xi = a*b, eta = a*c, vsq = xi*xi + eta*eta, t;
  if ( vsq - 2. * eta + 1. <= 0.) return 0.;
  if (fabs(xi) > .01 * (1. - eta) ) {
	 return atan2(xi, 1. - eta) / a ;
  } else {
	 t = xi/(1. - eta); t *= t;
	 return b / (1. - eta) * (1.    - t *
				 (1./3. - t *
				 (1./5. - t *
				 (1./7. ))));
  }
}

