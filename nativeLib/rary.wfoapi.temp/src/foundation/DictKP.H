// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// DictKP.H
// Dict<K,V> replacement to reduce code bloat when K and/or V is a pointer
//
// Author: Tom Cargill
// ---------------------------------------------------------------------------
#ifndef _DictKP_H
#define _DictKP_H

#ifdef IDENT_TEMPLATE_H
static const char* const DictKP_H_Id =
  "$Id: .DictKP.H__temp27950,v 1.2 2003/05/06 23:11:38 fluke Exp $";
#endif

#include "Dict.H"

// -- module -----------------------------------------------------------------
//
//  A DictKP is a highly optimized Dictionary where the value
//  is a pointer.  Use of this class for values that are pointers prevents
//  code bloat. Other dictionary classes available include Dict, DictPP, 
//  and DictPV.
//
//  The Dict class should be used when neither the key or value are pointers,
//  the DictPP class should be used when both key and value are pointers,
//  and the DictPV class should be used when the key is a pointer.
//
//  DictKP is a set of <K,V> pairs, such that each K is unique.
//     K is a TextString, an integral type or a pointer.
//     V is any pointer.
//     (K stands for "key", V for "value".)
//
//     If d is a Dict<K,V>, then:
//
//     1. d.map(k) returns true if there is any <k,v> in d.
//     2. If d.map(k), then d.map(k,value)
//          sets value=v; returns true
//        otherwise
//          value is unchanged; returns false.
//     3. After d.remove(k), d.map(k) is false
//     4. Effectively, d.add(k,v) first performs d.remove(k)
//          and then adds <k,v> to the set.
//     5. d.size() returns the size of the set of pairs.
//     6. The elements of the set are exactly:
//          <d.key(0),d.value(0)> .. <d.key(sz-1),d.value(sz-1)>
//          where sz = d.size();
//     7. After d.orderByK(), indexing is ordered by < on K,
//         i.e., d.key(i) < d.key(i+1).
//
// -- implementation ---------------------------------------------------------
//
// Implemented completely in-line.  Implemented in terms of Dict<K, void*>
// to prevent code bloat.  Refer to Dict.C for more details.
//
// ---------------------------------------------------------------------------

// K is the key, V is the pointer
template <class K, class V>
class DictKP 
    {   
    public:
        // use generated: dtor, default ctor, copy ctor, op=

        // look up the key/value
        bool map(const K &key) const { return impl.map(key); }
        bool map(const K &key, V &val) const
            { return impl.map(key, (void*&) val); }

        // change contents of dictionary
        void remove(const K &key) { impl.remove(key); }
        void add(const K &key, const V &val) { impl.add(key, (void*&)val); }

        // current dictionary size
        int size() const { return impl.size(); }

        // routines for indexing access to dictionary
        void orderByK() const { impl.orderByK(); }
        const K &key(int i) const { return impl.key(i); }
        const V &value(int i) const { return (const V&) impl.value(i); }

        // logical operations
        bool operator==(const DictKP<K,V> &rhs) const { return impl == rhs.impl; }
        bool operator!=(const DictKP<K,V> &rhs) const { return !operator==(rhs); }

        float testsPerSearch() const { return impl.testsPerSearch(); }

    private:

        Dict<K, void*> impl;  // implemented as void* to prevent code bloat

    };

#endif



