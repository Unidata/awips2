/*****************************************************************************
 * type.h
 *
 * DESCRIPTION
 *    This file contains some simple common types used by this project.
 *
 * HISTORY
 *   12/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef TYPE_H
#define TYPE_H

#ifndef SINT4_TYPE
 #define SINT4_TYPE
 #ifdef _64Bit
  typedef signed int sInt4;
  typedef unsigned int uInt4;
 #else
  typedef signed long int sInt4;
  typedef unsigned long int uInt4;
 #endif
 typedef float Float4;
 typedef unsigned char uChar;
 typedef signed char sChar;
 typedef unsigned short int uShort;
 typedef signed short int sShort;
#endif

/* #define LATLON_DECIMALS 6 */
typedef struct {
   double lt, lg;
} LatLon;

typedef struct {
   uChar f_valid;
   double X, Y;
} Point;

#endif
