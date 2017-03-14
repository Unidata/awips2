// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// HashUtil.C
// Implementation of the general-purpose hash function provider class.
//
// Author: Tom Cargill
// ---------------------------------------------------------------------------
#include "HashUtil.H"
#include "TextString.H"
#include <stdlib.h>
#include "AbsTime.H"

#ifdef IDENT_C
static const char* const HashUtil_C_Id =
"$Id$";
#endif

// -- fileScope --------------------------------------------------------------
// HashUtil::hash()
//
// Return a quasirandom number as a deterministic function of
// the specified text string key.
//
// -- implementation ---------------------------------------------------------
// A 16-bit version of the hash described in P.K. Pearson,
// Fast Hashing of Variable-Length Text Strings, CACM 33:6.
// The 16-bit hash is built from two 8-bit hashes: one of the
// the even bytes, and one of the odd. See also Cargill,
// Pearson's String Hash, C++ Report, Sept. 94.  
// ---------------------------------------------------------------------------
unsigned HashUtil::hash(const TextString &key)
    {
    static unsigned char table[256] = { 0, 0 };
    if( table[0] == table[1] ) // fill and permute table on first call
        {
        for( int j = 0; j < 256; ++j )
            table[j] = j;
        for( int k = 0; k < 256; ++k )
            {
            int r = rand()&0xFF;
            unsigned char xchg = table[k];
            table[k] = table[r];
            table[r] = xchg;
            }
        }
    const unsigned char *k = (const unsigned char *) key.stringPtr();
    // avoid TextString::op[] because it is too slow
    unsigned even = 0, odd = 0, c;
    while( (c = *k++) != 0 )     // pick up even char
        {
        even = table[ even ^ c ];
        if( (c = *k++) == 0 )    // pick up odd char
            break;
        odd = table[ odd ^ c ];
        }
    return (even<<8) | odd;
    }

// -- fileScope --------------------------------------------------------------
// HashUtil::isPrime()
//
// Return true iff the given integer n is prime.
//
// ---------------------------------------------------------------------------
bool HashUtil::isPrime(int n)
    {
    if( n>2 && n%2 == 0 )
        return false;
    for( int div = 3; div*div <= n; div += 2 )
        if( n%div == 0 )
            return false;
    return true;
    }

// -- fileScope --------------------------------------------------------------
// HashUtil::hash()
//
// Return a quasirandom number as a deterministic function of
// the specified AbsTime.
//
unsigned HashUtil::hash(const AbsTime& t)
    {
    return (unsigned) t.unixTime();
    }
