#include <stdio.h>
#include <math.h>
#include <float.h>

/*
 * logabova.c  - source file for conformal mapping function utility.
 * Written 12/21/94 by
 * Dr. Albion Taylor
 * NOAA / OAR / ARL                  Phone: (301) 713-0295 ext 132
 * Rm. 3151, 1315 East-West Highway  Fax:   (301) 713-0119
 * Silver Spring, MD 20910           E-mail: ADTaylor@arlrisc.ssmc.noaa.gov
 */


double log1pabovera(double a,double b){
/* returns  log(1. + a*b)/a, or its limit as a -> 0.*/
#define FSM .01
double arg1,temp;
  arg1 = a * b;
  if (arg1 <= -1.) return log ( DBL_MIN) ;
  if ( (arg1<0 ? -arg1 : arg1) < FSM) {
    temp = arg1/(2. + arg1);temp *= temp;
    return 2. * b / (2. + arg1) * ( 1.    + temp *
				  ( 1./3. + temp *
				  ( 1./5. + temp *
				  ( 1./7. + temp *
				  ( 1./9.))))) ;
  } else {
    return log (1. + arg1) / a;
  }
#undef FSM

}

double cperiodic(double value,double begin,double end){
double first,last;
first = begin<end?begin:end;
last = begin<end?end:begin;
value = fmod(value-first,last-first);
return value<0 ? value + last : value + first;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/emapfc/RCS/logabova.c,v $";
 static char rcs_id2[] = "$Id: logabova.c,v 1.1 2004/09/16 15:00:43 dsa Exp $";}
/*  ===================================================  */

}
