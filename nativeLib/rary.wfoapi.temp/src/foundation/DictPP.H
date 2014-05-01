// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// DictPP.H
// Dict<K,V> replacement to reduce code bloat when both K and V are pointers
//
// Author: Tom Cargill
// ---------------------------------------------------------------------------
#ifndef _DictPP_H
#define _DictPP_H

#ifdef IDENT_TEMPLATE_H
static const char* const DictPP_H_Id =
  "$Id: .DictPP.H__temp27950,v 1.2 2003/05/06 23:11:40 fluke Exp $";
#endif

#include "Dict.H"

// -- module -----------------------------------------------------------------
//
//  A DictPP is a highly optimized Dictionary where both the
//  key and value are pointers.  They can be pointers to different types.
//  Use of this class for keys and values that are both pointers prevents
//  code bloat. Other dictionary classes available include Dict, DictPV, 
//  and DictKP.
//
//  The Dict class should be used when neither the key or value are pointers,
//  the DictPV class should be used when the key is a pointer, and the
//  DictKP class should be used when the value is a pointer..
//
//  DictPP is a set of <K,V> pairs, such that each K is unique.
//     K is any pointer.
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
// Implemented completely in-line.  Implemented in terms of Dict<void*, void*>
// to prevent code bloat. Refer to Dict.C for more details.
//
// ---------------------------------------------------------------------------

// both key and value are pointers
template <class K, class V>
class DictPP 
    {   
    public:
        // use generated: dtor, default ctor, copy ctor, op=

        // look up the key/value
        bool map(const K &key) const { return impl.map((void*&)key); }
        bool map(const K &key, V &val) const
            { return impl.map((void*&)key, (void*&) val); }

        // change contents of dictionary
        void remove(const K &key) { impl.remove((void*&)key); }
        void add(const K &key, const V &val)
            { impl.add((void*&)key, (void*&)val); }

        // current dictionary size
        int size() const { return impl.size(); }

        //routines for indexing access to dictionary
        void orderByK() const { impl.orderByK(); }
        const K &key(int i) const { return (const K&)impl.key(i); }
        const V &value(int i) const { return (const V&) impl.value(i); }

        // comparision operators
        bool operator==(const DictPP<K,V> &rhs) const { return impl == rhs.impl; }
        bool operator!=(const DictPP<K,V> &rhs) const { return !operator==(rhs); }

        float testsPerSearch() const { return impl.testsPerSearch(); }

    private:

        Dict<void*, void*> impl; // implement as void*,void* to prevent bloat

    };

#endif




