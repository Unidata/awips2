/*****************************************************************************
 * grib2api.h
 *
 * DESCRIPTION
 *    This file contains the header information needed to call the grib2
 * decoder library.
 *
 * HISTORY
 *   12/2003 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef GRIB2API_H
#define GRIB2API_H

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

void unpk_grib2 (sInt4 *kfildo, Float4 *ain, sInt4 *iain, sInt4 *nd2x3,
                 sInt4 *idat, sInt4 *nidat, Float4 *rdat, sInt4 *nrdat,
                 sInt4 *is0, sInt4 *ns0, sInt4 *is1, sInt4 *ns1, sInt4 *is2,
                 sInt4 *ns2, sInt4 *is3, sInt4 *ns3, sInt4 *is4, sInt4 *ns4,
                 sInt4 *is5, sInt4 *ns5, sInt4 *is6, sInt4 *ns6, sInt4 *is7,
                 sInt4 *ns7, sInt4 *ib, sInt4 *ibitmap, sInt4 *ipack,
                 sInt4 *nd5, Float4 *xmissp, Float4 *xmisss, sInt4 *inew,
                 sInt4 *iclean, sInt4 *l3264b, sInt4 *iendpk, sInt4 *jer,
                 sInt4 *ndjer, sInt4 *kjer);

void pk_grib2 (sInt4 * kfildo, Float4 * ain, sInt4 * iain, sInt4 * nx,
               sInt4 * ny, sInt4 * idat, sInt4 * nidat, Float4 * rdat,
               sInt4 * nrdat, sInt4 * is0, sInt4 * ns0, sInt4 * is1,
               sInt4 * ns1, sInt4 * is3, sInt4 * ns3, sInt4 * is4,
               sInt4 * ns4, sInt4 * is5, sInt4 * ns5, sInt4 * is6,
               sInt4 * ns6, sInt4 * is7, sInt4 * ns7, sInt4 * ib,
               sInt4 * ibitmap, sInt4 * ipack, sInt4 * nd5, sInt4 * missp,
               Float4 * xmissp, sInt4 * misss, Float4 * xmisss, sInt4 * inew,
               sInt4 * minpk, sInt4 * iclean, sInt4 * l3264b, sInt4 * jer,
               sInt4 * ndjer, sInt4 * kjer);

#endif
