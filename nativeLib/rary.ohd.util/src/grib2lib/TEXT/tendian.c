/*****************************************************************************
 * tendian.c
 *
 * DESCRIPTION
 *    This file contains all the utility functions that the Driver uses to
 * solve endian'ness related issues.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL / RSIS): Created.
 *  12/2002 Rici Yu, Fangyu Chi, Mark Armstrong, & Tim Boyer
 *          (RY,FC,MA,&TB): Code Review 2.
 *
 * NOTES
 *****************************************************************************
 */
#include <stdio.h>
#include <string.h>
#include "grib2lib_inc/tendian.h"

/*****************************************************************************
 * memswp() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To swap memory in the Data array based on the knownledge that there are
 * "num_elem" elements, each of size "elem_size".
 *
 * ARGUMENTS
 *      Data = A pointer to the data to be swapped. (Input/Output)
 * elem_size = The size of an individual element. (Input)
 *  num_elem = The number of elements to swap. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *
 * NOTES
 * 1) A similar routine was provided with the GRIB2 library.  It was called:
 * "unpk_swap".  Since it had the restriction that it only dealt with long
 * ints, I felt that I needed more flexibility.  In addition this procedure
 * may be more efficient.  I did an operation count for swapping an array
 * that consisted of 1 long int.  "unpk_swap" = 46 operations, "memswp" = 33.
 * 2) Could try this with exclusive or?
 *****************************************************************************
 */
void memswp (void *Data, const size_t elem_size, const size_t num_elem)
{
   size_t j;            /* Element count */
   char *data;          /* Allows us to treat Data as an array of char. */
   char temp;           /* A temporary holder of a byte when swapping. */
   char *ptr, *ptr2;    /* Pointers to the two bytes to swap. */

   if (elem_size == 1) {
      return;
   }
   data = (char *) Data;
   for (j = 0; j < elem_size * num_elem; j += elem_size) {
      ptr = data + j;
      ptr2 = ptr + elem_size - 1;
      while (ptr2 > ptr) {
         temp = *ptr;
         *(ptr++) = *ptr2;
         *(ptr2--) = temp;
      }
   }
}

/*****************************************************************************
 * revmemcpy() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To copy memory similar to memcpy, but in a reverse manner.  In order to
 * have the same arguments as memcpy, this can not handle arrays... For
 * arrays use revmemcpyRay().  Returns the same thing that memcpy does.
 *
 * ARGUMENTS
 * Dst = The destination for the data. (Output)
 * Src = The source of the data. (Input)
 * len = The length of Src in bytes. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void *
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *
 * NOTES
 * 1) This came about as I was trying to improve on the use of memcpy.  I
 *    figured that revmemcpy would be faster than memcpy followed by memswp.
 * 2) Assumes that Dst is allocated to a size of Src.
 * 3) Problems with MEMCPY if len != sizeof (dst)... Is it left or right
 *    justified?
 *****************************************************************************
 */
void *revmemcpy (void *Dst, void *Src, const size_t len)
{
   size_t j;            /* Byte count */
   char *src = (char *) Src; /* Allows us to treat Src as an array of char. */
   char *dst = (char *) Dst; /* Allows us to treat Dst as an array of char. */

   src = src + len - 1;
   for (j = 0; j < len; ++j) {
      *(dst++) = *(src--);
   }
   return Dst;
}

/*****************************************************************************
 * revmemcpyRay() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To copy memory similar to memcpy, but in a reverse manner.  This handles
 * the case when we need to reverse memcpy an array of data.
 *
 * ARGUMENTS
 *       Dst = The destination for the data. (Output)
 *       Src = The source of the data. (Input)
 * elem_size = The size of a single element. (Input)
 *  num_elem = The number of elements in Src. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void *
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *
 * NOTES
 * 1) Assumes that Dst is allocated to a size of Src.
 *****************************************************************************
 */
void *revmemcpyRay (void *Dst, void *Src, const size_t elem_size,
                    const size_t num_elem)
{
   size_t i;            /* Element count. */
   size_t j;            /* Byte count. */
   char *src = (char *) Src; /* Allows us to treat Src as an array of char. */
   char *dst = (char *) Dst; /* Allows us to treat Dst as an array of char. */

   if (elem_size == 1) {
      return memcpy (Dst, Src, num_elem);
   }
   src -= (elem_size + 1);
   for (i = 0; i < num_elem; ++i) {
      src += 2 * elem_size;
      for (j = 0; j < elem_size; ++j) {
         *(dst++) = *(src--);
      }
   }
   return Dst;
}

/*****************************************************************************
 * revfread() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To do an "fread", but in a reverse manner.
 *
 * ARGUMENTS
 *       Dst = The destination for the data. (Output)
 * elem_size = The size of a single element. (Input)
 *  num_elem = The number of elements in Src. (Input)
 *        fp = The file to read from. (Input)
 *
 * FILES/DATABASES:
 *   It is assumed that file is already opened and in the correct place.
 *
 * RETURNS: size_t
 *   Number of elements read.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *
 * NOTES
 *   Decided to read it in and then swap.  The thought here being that it is
 * faster than a bunch of fgetc.  This is the exact opposite method as
 * revfwrite.
 *****************************************************************************
 */
size_t revfread (void *Dst, size_t elem_size, size_t num_elem, FILE * fp)
{
   size_t ans;          /* The answer from fread. */
   size_t j;            /* Byte count. */
   char *dst;           /* Allows us to treat Dst as an array of char. */
   char temp;           /* A temporary holder of a byte when swapping. */
   char *ptr, *ptr2;    /* Pointers to the two bytes to swap. */

   ans = fread (Dst, elem_size, num_elem, fp);
   if (elem_size == 1) {
      return ans;
   }
   if (ans == num_elem) {
      dst = (char *) Dst;
      for (j = 0; j < elem_size * num_elem; j += elem_size) {
         ptr = dst + j;
         ptr2 = ptr + elem_size - 1;
         while (ptr2 > ptr) {
            temp = *ptr;
            *(ptr++) = *ptr2;
            *(ptr2--) = temp;
         }
      }
   }
   return ans;
}

/*****************************************************************************
 * revfwrite() -- Review 12/2002
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To do an "fwrite", but in a reverse manner.
 *
 * ARGUMENTS
 *       Src = The source of the data. (Input)
 * elem_size = The size of a single element. (Input)
 *  num_elem = The number of elements in Src. (Input)
 *        fp = The file to write to. (Output)
 *
 * FILES/DATABASES:
 *   It is assumed that file is already opened and in the correct place.
 *
 * RETURNS:
 *   Returns number of elements written, or EOF on error.
 *
 * HISTORY
 *   9/2002 Arthur Taylor (MDL/RSIS): Created.
 *  11/2002 Arthur Taylor (MDL/RSIS): Updated.
 *  12/2002 (RY,FC,MA,&TB): Code Review.
 *
 * NOTES
 *   Decided to write using a bunch of fput, since this is buffered.  The
 * thought here, is that it is faster than swapping memory and then writing.
 * This is the exact opposite method as revfread.
 *****************************************************************************
 */
size_t revfwrite (void *Src, size_t elem_size, size_t num_elem, FILE * fp)
{
   char *ptr;           /* Current byte to put to file. */
   size_t i;            /* Byte count */
   size_t j;            /* Element count */
   char *src;           /* Allows us to treat Src as an array of char. */

   if (elem_size == 1) {
      return fwrite (Src, elem_size, num_elem, fp);
   } else {
      src = (char *) Src;
      ptr = src - elem_size - 1;
      for (j = 0; j < num_elem; ++j) {
         ptr += 2 * elem_size;
         for (i = 0; i < elem_size; ++i) {
            if (fputc ((int) *(ptr--), fp) == EOF) {
               return 0;
            }
         }
      }
      return num_elem;
   }
}

/*****************************************************************************
 * memBitRead() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To read bits from an unsigned char buffer array of memory.
 *   Assumes BufLoc is valid before first call.  Typically this means do
 * a "bufLoc = 8;" before the first call.
 *
 * ARGUMENTS
 *       Dst = Where to put the results. (Output)
 *    dstLen = Length in bytes of Dst. (Input)
 *       Src = The data to read the bits from. (Input)
 *   numBits = How many bits to read. (Input)
 *    BufLoc = Which bit to start reading from in Src.
 *             Starts at 8 goes to 1. (Input/Output)
 *   numUsed = How many bytes from Src were used while reading (Output)
 *
 * FILES/DATABASES: None
 *
 * RETURNS:
 *   Returns 1 on error, 0 if ok.
 *
 * HISTORY
 *   4/2003 Arthur Taylor (MDL/RSIS): Created
 *   5/2004 AAT: Bug in call to MEMCPY_BIG when numBytes != dstLen.
 *          On big endian machines we need to right justify the number.
 *
 * NOTES
 * 1) Assumes binary bit stream is "big endian". Resulting in no byte
 *    boundaries ie 00100110101101 => 001001 | 10101101
 *****************************************************************************
 */
int memBitRead (void *Dst, size_t dstLen, void *Src, size_t numBits,
                unsigned char *bufLoc, int *numUsed)
{
   unsigned char *src = (unsigned char *) Src; /* Allows us to treat Src as
                                                * an array of char. */
   unsigned char *dst = (unsigned char *) Dst; /* Allows us to treat Dst as
                                                * an array of char. */
   size_t numBytes;     /* How many bytes are needed in dst. */
   unsigned char dstLoc; /* Where we are writing to in dst. */
   unsigned char *ptr;  /* Current byte we are writing to in dst. */
   static unsigned char BitMask[] = {
      0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff
   };

   if (numBits == 0) {
      memset (Dst, 0, dstLen);
      (*numUsed) = 0;
      return 0;
   }
   numBytes = ((numBits - 1) / 8) + 1;
   if (dstLen < numBytes) {
      return 1;
   }
   memset (Dst, 0, dstLen);
   dstLoc = ((numBits - 1) % 8) + 1;
   if ((*bufLoc == 8) && (dstLoc == 8)) {
#ifdef LITTLE_ENDIAN
      MEMCPY_BIG (Dst, Src, numBytes);
#else
      /* If numBytes != dstLen, then we need to right justify the ans */
      MEMCPY_BIG (dst + (dstLen - numBytes), Src, numBytes);
#endif
      (*numUsed) = numBytes;
      return 0;
   }
#ifdef LITTLE_ENDIAN
   ptr = dst + (numBytes - 1);
#else
   ptr = dst + (dstLen - numBytes);
#endif

   *numUsed = 0;
   /* Deal with most significant byte in dst. */
   if (*bufLoc >= dstLoc) {
#ifdef LITTLE_ENDIAN
      (*ptr--) |= ((*src & BitMask[*bufLoc]) >> (*bufLoc - dstLoc));
#else
      (*ptr++) |= ((*src & BitMask[*bufLoc]) >> (*bufLoc - dstLoc));
#endif
      (*bufLoc) -= dstLoc;
   } else {
      if (*bufLoc != 0) {
         *ptr |= ((*src & BitMask[*bufLoc]) << (dstLoc - *bufLoc));
         /* Assert: dstLoc should now be dstLoc - InitBufLoc */
         dstLoc = dstLoc - *bufLoc;
         /* Assert: bufLoc should now be 0 */
      }
      src++;
      (*numUsed)++;
      /* Assert: bufLoc should now be 8 */
      /* Assert: We want to >> by bufLoc - dstLoc = 8 - dstLoc */
#ifdef LITTLE_ENDIAN
      *(ptr--) |= (*src >> (8 - dstLoc));
#else
      *(ptr++) |= (*src >> (8 - dstLoc));
#endif
      (*bufLoc) = 8 - dstLoc;
   }
   /* Assert: dstLoc should now be 8, but we don't use again in procedure. */

   /* We have now reached the state which we want after each iteration of
    * the loop.  That is initDstLoc == 8, initBufLoc = bufLoc < dstLoc. */
#ifdef LITTLE_ENDIAN
   while (ptr >= dst) {
#else
   while (ptr < dst + dstLen) {
#endif
      if (*bufLoc != 0) {
         *ptr |= ((*src & BitMask[*bufLoc]) << (8 - *bufLoc));
         /* Assert: dstLoc should now be initDstLoc (8) - initBufLoc */
         /* Assert: bufLoc should now be 0 */
      }
      src++;
      (*numUsed)++;
      /* Assert: bufLoc should now be 8 */
      /* Assert: dstLoc should now be initDstLoc (8) - initBufLoc */
      /* Assert: We want to >> by bufLoc - dstLoc = (8 - (8 - initbufLoc)). */
#ifdef LITTLE_ENDIAN
      *(ptr--) |= (*src >> *bufLoc);
#else
      *(ptr++) |= (*src >> *bufLoc);
#endif
   }
   if (*bufLoc == 0) {
      (*numUsed)++;
      *bufLoc = 8;
   }
   return 0;
}

/*****************************************************************************
 * memBitWrite() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To write bits from a data structure to an unsigned char buffer array of
 * memory.
 *   Assumes that the part of Dst we don't write to have been correctly
 * initialized.  Typically this means do a "memset (dst, 0, sizeof (dst));"
 * before the first call.
 *   Also assumes BufLoc is valid before first call.  Typically this means do
 * a "bufLoc = 8;" before the first call.
 *
 * ARGUMENTS
 *       Src = The data to read from. (Input)
 *    srcLen = Length in bytes of Src. (Input)
 *       Dst = The char buffer to write the bits to. (Output)
 *   numBits = How many bits to write. (Input)
 *    BufLoc = Which bit in Dst to start writing to.
 *             Starts at 8 goes to 1. (Input/Output)
 *   numUsed = How many bytes were written to Dst. (Output)
 *
 * FILES/DATABASES: None
 *
 * RETURNS:
 *   Returns 1 on error, 0 if ok.
 *
 * HISTORY
 *   4/2003 Arthur Taylor (MDL/RSIS): Created
 *
 * NOTES
 * 1) Assumes binary bit stream should be "big endian". Resulting in no byte
 *    boundaries ie 00100110101101 => 001001 | 1010110
 * 2) Assumes that Dst is already zero'ed out.
 *****************************************************************************
 */
int memBitWrite (void *Src, size_t srcLen, void *Dst, size_t numBits,
                 unsigned char *bufLoc, int *numUsed)
{
   unsigned char *src = (unsigned char *) Src; /* Allows us to treat Src as
                                                * an array of char. */
   unsigned char *dst = (unsigned char *) Dst; /* Allows us to treat Dst as
                                                * an array of char. */
   size_t numBytes;     /* How many bytes are needed from src. */
   unsigned char srcLoc; /* Which bit we are reading from in src. */
   unsigned char *ptr;  /* Current byte we are reading from in src. */
   static unsigned char BitMask[] = {
      0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff
   };

   if (numBits == 0) {
      return 0;
   }
   numBytes = ((numBits - 1) / 8) + 1;
   if (srcLen < numBytes) {
      return 1;
   }
   srcLoc = ((numBits - 1) % 8) + 1;

   if ((*bufLoc == 8) && (srcLoc == 8)) {
      MEMCPY_BIG (Dst, Src, numBytes);
      (*numUsed) = numBytes;
      return 0;
   }
#ifdef LITTLE_ENDIAN
   ptr = src + (numBytes - 1);
#else
   ptr = src + (srcLen - numBytes);
#endif

   *numUsed = 0;
   /* Deal with most significant byte in src. */
   if (*bufLoc >= srcLoc) {
#ifdef LITTLE_ENDIAN
      (*dst) |= ((*(ptr--) & BitMask[srcLoc]) << (*bufLoc - srcLoc));
#else
      (*dst) |= ((*(ptr++) & BitMask[srcLoc]) << (*bufLoc - srcLoc));
#endif
      (*bufLoc) -= srcLoc;
   } else {
      if (*bufLoc != 0) {
         (*dst) |= ((*ptr & BitMask[srcLoc]) >> (srcLoc - *bufLoc));
         /* Assert: srcLoc should now be srcLoc - InitBufLoc */
         srcLoc = srcLoc - *bufLoc;
         /* Assert: bufLoc should now be 0 */
      }
      dst++;
      (*dst) = 0;
      (*numUsed)++;
      /* Assert: bufLoc should now be 8 */
      /* Assert: We want to >> by bufLoc - srcLoc = 8 - srcLoc */
#ifdef LITTLE_ENDIAN
      (*dst) |= (*(ptr--) << (8 - srcLoc));
#else
      (*dst) |= (*(ptr++) << (8 - srcLoc));
#endif
      (*bufLoc) = 8 - srcLoc;
   }
   /* Assert: dstLoc should now be 8, but we don't use again in procedure. */

   /* We have now reached the state which we want after each iteration of
    * the loop.  That is initSrcLoc == 8, initBufLoc = bufLoc < srcLoc. */
#ifdef LITTLE_ENDIAN
   while (ptr >= src) {
#else
   while (ptr < src + srcLen) {
#endif
      if (*bufLoc == 0) {
         dst++;
         (*numUsed)++;
#ifdef LITTLE_ENDIAN
         (*dst) = *(ptr--);
#else
         (*dst) = *(ptr++);
#endif
      } else {
         (*dst) |= ((*ptr) >> (8 - *bufLoc));
         dst++;
         (*numUsed)++;
         (*dst) = 0;
#ifdef LITTLE_ENDIAN
         (*dst) |= (*(ptr--) << *bufLoc);
#else
         (*dst) |= (*(ptr++) << *bufLoc);
#endif
      }
   }
   if (*bufLoc == 0) {
      dst++;
      (*numUsed)++;
      (*bufLoc) = 8;
      (*dst) = 0;
   }
   return 0;
}

/*****************************************************************************
 * main() --
 *
 * Arthur Taylor / MDL
 *
 * PURPOSE
 *   To test the memBitRead, and memBitWrite routines, to make sure that they
 * function correctly on some sample data..
 *
 * ARGUMENTS
 * argc = The number of arguments on the command line. (Input)
 * argv = The arguments on the command line. (Input)
 *
 * FILES/DATABASES: None
 *
 * RETURNS: void
 *
 * HISTORY
 *    4/2003 Arthur Taylor (MDL/RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifdef DEBUG_ENDIAN
int main (int argc, char **argv)
{
   unsigned char buff[5], buff2[5];
   unsigned char bufLoc = 8;
   unsigned char *ptr, *ptr2;
   int numUsed;

   buff[0] = 0x8f;
   buff[1] = 0x8f;
   buff[2] = 0x8f;
   buff[3] = 0x8f;
   buff[4] = 0x8f;

   bufLoc = 7;
   memBitRead (buff2, sizeof (buff2), buff, 39, &bufLoc, &numUsed);
   printf ("%d %d %d %d %d ", buff2[0], buff2[1], buff2[2], buff2[3],
           buff2[4]);
   printf ("-------should be----- ");
   printf ("143 143 143 143 15\n");

   memset (buff, 0, sizeof (buff));
   bufLoc = 8;
   ptr = buff;
   ptr2 = buff2;
   memBitWrite (ptr2, sizeof (buff2), ptr, 9, &bufLoc, &numUsed);
   ptr += numUsed;
   ptr2++;
   memBitWrite (ptr2, sizeof (buff2), ptr, 7, &bufLoc, &numUsed);
   ptr += numUsed;
   ptr2++;
   memBitWrite (ptr2, sizeof (buff2), ptr, 7, &bufLoc, &numUsed);
   ptr += numUsed;
   ptr2++;
   memBitWrite (ptr2, sizeof (buff2), ptr, 9, &bufLoc, &numUsed);
   ptr += numUsed;
   ptr2++;
   memBitWrite (ptr2, sizeof (buff2), ptr, 8, &bufLoc, &numUsed);
   ptr += numUsed;
   printf ("%d %d %d %d %d ", buff[0], buff[1], buff[2], buff[3], buff[4]);
   printf ("-------should be----- ");
   printf ("199 143 31 143 15\n");
   return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/grib2lib/RCS/tendian.c,v $";
 static char rcs_id2[] = "$Id: tendian.c,v 1.1 2004/09/16 17:30:40 dsa Exp $";}
/*  ===================================================  */

}
#endif
