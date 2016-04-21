// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// Set.H
// General-purpose Set class template
//
// Author: Brian Woods
// ---------------------------------------------------------------------------
#ifndef _SetP_H
#define _SetP_H

#ifdef IDENT_TEMPLATE_H
static const char* const SetP_H_Id =
"$Id$";
#endif

#include "commonDefs.h"
#include "Set.H"

template <class K>
class SetP 
    {
    public:
        // Using default d'tor, copy c'tor, op=
        SetP(const K &key)           { impl.add( (void* &) key ); }
        SetP()                       {}
        
        void emptySet(void)          { impl.emptySet(); }
        bool map(const K &key) const { return impl.map( (void* &) key ); }
        void remove(const K &key)    { impl.remove( (void* &) key ); }
        void add(const K &key)       { impl.add( (void* &) key ); } 

        int size(void) const             { return impl.size(); }
        void orderByK(void) const        { impl.orderByK(); }
        const K &key(int i) const        { return (const K&)impl.key( i ); }
        const K &operator[](int i) const { return (const K&)impl.key( i ); }

        bool operator==(const SetP<K> &rhs)  { return impl == rhs.impl; }
        bool operator!=(const SetP<K> &rhs)  { return !operator==( rhs ); }

        // Worded functions to avoid any possible ambiguities
        bool isRhsSubset(const SetP<K> &rhs) const
            { return impl.isRhsSubset( rhs.impl ) ; }         
        bool isRhsProperSubset(const SetP<K> &rhs) const
            { return impl.isRhsProperSubset( rhs.impl ); }
        void setMinus(const SetP<K> &rhs) 
            { impl.setMinus( rhs.impl ); }
        void setUnion(const SetP<K> &rhs) 
            { impl.setUnion( rhs.impl ); }
        void setIntersection(const SetP<K> &rhs) 
            { impl.setIntersection( rhs.impl ); }
        bool mutuallyInclusive(const SetP<K>& rhs) const
            { return impl == rhs.impl; }
        bool mutuallyExclusive(const SetP<K>& rhs) const
            { return impl.mutuallyExclusive( rhs.impl ); }

        float testsPerSearch(void) const { return impl.testsPerSearch(); }

    private:
        Set<void *> impl;       // Implement as void* to prevent coad bloat
    };



#endif
