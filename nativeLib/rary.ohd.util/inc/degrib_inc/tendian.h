/*****************************************************************************
 * tendian.h
 *
 * DESCRIPTION
 *    This file contains all the utility functions that the Driver uses to
 * solve endian'ness related issues.
 *
 * HISTORY
 *    9/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef TENDIAN_H
#define TENDIAN_H

#include <stdio.h>

/*
 * MadeOnIntel    ==> LittleEndian
 * NotMadeOnIntel ==> BigEndian
 */
#undef BIG_ENDIAN
#undef LITTLE_ENDIAN

#if defined(_LINUX_)
  #define LITTLE_ENDIAN
#elif defined(_UNIX_)
  #define BIG_ENDIAN
#elif defined(_WINDOWS_)
  #define LITTLE_ENDIAN
#else
  #define LITTLE_ENDIAN
#endif

/* The following #defines are used to make the code easier to read. */
#ifdef BIG_ENDIAN
  #define FREAD_BIG fread
  #define FREAD_LIT revfread
  #define FWRITE_BIG fwrite
  #define FWRITE_LIT revfwrite
  #define MEMCPY_BIG memcpy
  #define MEMCPY_LIT revmemcpy
#else
  #define FREAD_BIG revfread
  #define FREAD_LIT fread
  #define FWRITE_BIG revfwrite
  #define FWRITE_LIT fwrite
  #define MEMCPY_BIG revmemcpy
  #define MEMCPY_LIT memcpy
#endif

void memswp (void *Data, const size_t elem_size, const size_t num_elem);

void *revmemcpy (void *Dst, void *Src, const size_t len);
void *revmemcpyRay (void *Dst, void *Src, const size_t elem_size,
                    const size_t num_elem);

size_t revfread (void *Dst, size_t elem_size, size_t num_elem, FILE *fp);
size_t revfwrite (void *Src, size_t elem_size, size_t num_elem, FILE *fp);

int memBitRead (void *Dst, size_t dstLen, void *Src, size_t numBits,
                unsigned char *BufLoc, int *numUsed);
int memBitWrite (void *Src, size_t srcLen, void *Dst, size_t numBits,
                 unsigned char *bufLoc, int *numUsed);
#endif
