/*
 * limmath.h
 *
 *  Created on: Aug 26, 2011
 *      Author: snaples
 */

#ifndef LIMMATH_H_
#define LIMMATH_H_

#include <math.h>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef RADPDEG
#define RADPDEG (M_PI/180.)
#define DEGPRAD (180./M_PI)
#endif /*RADPDEG*/
#ifdef __cplusplus
extern "C" {
#endif
double xpabova(double a, double b) ;
double lnabova(double a, double b) ;
double snabova(double a, double b) ;
double csabova(double a, double b) ;
double atnabova(double a, double b, double c) ;
double eqvlat(double lat1,double lat2) ;
#ifdef __cplusplus
}
#endif
#endif /*_LIMMATH_*/

