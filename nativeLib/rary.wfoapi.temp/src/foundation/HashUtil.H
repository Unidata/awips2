// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// HashUtil.H
// General-purpose hash function provider.
//
// Author: Tom Cargill
// ---------------------------------------------------------------------------
#ifndef _HashUtil_H
#define _HashUtil_H

#ifdef IDENT_H
static const char* const HashUtil_H_Id =
"$Id$";
#endif

#include "commonDefs.h"

class TextString;
class AbsTime;

class IPC_Target;

struct HashUtil

    {
    // Implemented in HashUtil.C    
    static unsigned hash(const TextString&);

    // Implemented in D-2D/threadIPC/IPC_Target.C    
    static unsigned hash(const IPC_Target&);    

    static unsigned hash(const void *key) { return (unsigned)key; }
    static unsigned hash(const int key)            { return (unsigned)key; }
    static unsigned hash(const unsigned key)       { return key; }
    static unsigned hash(const long key)           { return (unsigned)key; }
    static unsigned hash(const unsigned long key)  { return (unsigned)key; }
    static unsigned hash(const short key)          { return (unsigned)key; }
    static unsigned hash(const unsigned short key) { return (unsigned)key; }
    static unsigned hash(const char key)           { return (unsigned)key; }
    static unsigned hash(const unsigned char key)  { return (unsigned)key; }
    static unsigned hash(const AbsTime& key);

    // ... add more overloadings, e.g. hash(long), as needed
        
    // In a future implementation, the above hash functions could be made
    // free functions, to allow the user to use more types as keys.
    // The hash functions currently in HashUtil would be free, overloaded,
    // and in a separate file, named something like DictHash.[HC]
        
    static bool isPrime(int n);
    };

#endif
