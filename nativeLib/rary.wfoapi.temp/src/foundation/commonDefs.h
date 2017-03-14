/* ---------------------------------------------------------------------------
** This software is in the public domain, furnished "as is", without technical
** support, and with no warranty, express or implied, as to its usefulness for
** any purpose.
**
** commonDefs.h
** Common global definitions
**
** Author: Biere, Leserman, Mathewson, etc.
** -------------------------------------------------------------------------*/
#ifndef _commonDefs_h
#define _commonDefs_h

#ifdef IDENT_H
static const char* const commonDefs_h_Id =
  "$Id: .commonDefs.h__temp22079,v 1.4 2005/01/14 18:27:04 fluke Exp $";
#endif

/*
 * common typedefs
 */

typedef unsigned char byte;

/*
 * angle constants
 */
#ifndef PI
static const double PI = 3.141592653589793238462643383279502884197169399375106;
#endif
#ifndef DEG_TO_RAD
static const double DEG_TO_RAD = 0.017453292519943295769236907684886127134428;
#endif
#ifndef RAD_TO_DEG
static const double RAD_TO_DEG = 57.29577951308232087679815481410517033240547;
#endif

/*
 * geographic constants
 */
static const float R_EARTH = 6370.0; /* in km */

/*
 * some handy C++ functions
 */

#ifndef FORCED_INLINE
  #define FORCE_INLINE
#else
  #ifdef __GNUC__
    #if __GNUC__ > 2
      #define FORCE_INLINE __attribute__((always_inline))
    #else
      #define FORCE_INLINE
    #endif
  #else
    #define FORCE_INLINE
  #endif
#endif

#ifdef __cplusplus

#include <algorithm>
using std::swap;
using std::max;
using std::min;

#endif

#endif
