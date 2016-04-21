// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// DictOfSet.H
// General-purpose dictionary of set class template
//
// Author: Brian Woods
// ---------------------------------------------------------------------------
#ifndef _DictOfSet_H
#define _DictOfSet_H

#ifdef IDENT_TEMPLATE_H
static const char* const DictOfSet_H_Id =
"$Id: .DictOfSet.H__temp27950,v 1.3 2003/05/06 23:11:39 fluke Exp $";
#endif

#include "commonDefs.h"
#include "Set.H"
#include "Dict.H"

// -- module ------------------------------------------------------------------
//
// Implements a dictionary of sets using the DictKP and Set classes.
//
//-----------------------------------------------------------------------------

template <class K, class V>
class DictOfSet
    {
    public:
        // c'tor's and d'tor
        DictOfSet() {}
        ~DictOfSet();
        DictOfSet(const DictOfSet &);
        DictOfSet<K,V>& operator=(const DictOfSet<K,V> &rhs);

        //operations upon the sets
        bool map(const K &key) const { return impl.map( key ); }
        bool map(const K &key, Set<V> &set) const; 
        
        void add(const K &key, const Set<V> &set);
        void remove(const K &key);
        int size(void) const            { return impl.size(); }
        void orderByK(void) const       { impl.orderByK(); }
        const K &key(int i) const       { return impl.key( i ); }
        const Set<V> &value(int i) const;
       

        // Operations on the set's elements
        bool map(const K &key, const V &val) const
            { Set<V> *newSet; return ( pointerMap( key, newSet ) ?
                  newSet->map( val ) : false ); }
        
        void remove(const K &key, const V &val);
        void add(const K &key, const V &val);
        int size(const K& key)
            { Set<V> *newSet;
            return ( pointerMap( key, newSet ) ? newSet->size() : 0 ); }
        void orderByK(const K &key) const
            { Set<V> *newSet;
            if ( pointerMap( key, newSet ) ) newSet->orderByK(); }
        const V &value(const K &key, int i) const;

        // Set operations
        void setUnion(const K &key, const Set<V> &rhs)
            { Set<V> *newSet;
            if ( pointerMap( key, newSet ) ) newSet->setUnion( rhs );}
        
        void setIntersection(const K &key, const Set<V> &rhs)
            { Set<V> *newSet;
            if ( pointerMap( key, newSet ) ) newSet->setIntersection( rhs ); }
        
        void setMinus(const K &key, const Set<V> &rhs)
            { Set<V> *newSet;
            if ( pointerMap( key, newSet ) ) newSet->setMinus( rhs );}
        
        bool isRhsSubset(const K &key, const Set<V> &rhs)
            { Set<V> *newSet; return ( pointerMap( key, newSet ) ?
                  newSet->isRhsSubset( rhs ) : false ); }
        
        bool isRhsProperSubset(const K &key, const Set<V> &rhs)
            { Set<V> *newSet; return ( pointerMap( key, newSet ) ?
                  newSet->isRhsProperSubset( rhs ) :
                  false ); }
        
        bool mutuallyInclusive(const K &key, const Set<V> &rhs)
            { Set<V> *newSet; return ( pointerMap( key, newSet ) ?
                  newSet->mutuallyInclusive( rhs ) :
                  false ); }
        
        bool mutuallyExclusive(const K &key, const Set<V> &rhs)
            { Set<V> *newSet; return ( pointerMap( key, newSet ) ?
                  newSet->mutuallyExclusive( rhs ) :
                  false ); }

        // Comparrison operators on the dictionaries
        bool operator==(const DictOfSet<K,V> &rhs) const;
        bool operator!=(const DictOfSet<K,V> &rhs) const
            { return !operator==( rhs ); }

        float testsPerSearch(void) const // key==key's per map/add/remove
            { return impl.testsPerSearch(); }

    private:
        bool pointerMap(const K &key, Set<V>* &set) const
            { return impl.map(key, (void* &) set ); }
        
        Dict<K, void*> impl;
    };

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSet<K,V>::DictOfSet(const DictOfSet&)
//
// Copy constructor
//
// ---------------------------------------------------------------------------
template <class K, class V>
DictOfSet<K,V>::DictOfSet(const DictOfSet<K,V> &rhs)
    {
    for ( int i = 0; i < rhs.size(); i++ )
        impl.add( rhs.key( i ), new Set<V>( rhs.value( i ) ) );
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSet<K,V>::operator==(const DictOfSet& rhs)
//
// Equality operator
//
// -- implementation ---------------------------------------------------------
// Because the order of both sets and dictionaries is unimportant and this
// dictionary solves collision by chaining the equality operator needs to
// map elements from this to rhs.  If successfull, the function will then
// test the sets for equality.  Of course, all of this wouldn't be
// necessary if sets weren't implemented as void * ...
// ---------------------------------------------------------------------------
template <class K, class V>
bool DictOfSet<K,V>::operator==(const DictOfSet<K,V> &rhs) const
    {
    // If they're of different size return false
    if ( impl.size() != rhs.impl.size() )
        return false;

    Set<V> *set;
    for ( int i = 0; i < impl.size(); i++ )
        {
        // Map values in this to rhs
        if ( !rhs.pointerMap( impl.key( i ), set ) )
            return false;

        // Test the sets for equality
        if ( *set != *(Set<V> *)impl.value( i ) )
            return false;
        }

    return true;
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSet<K,V>::~DictOfSet()
//
// Destructor
//
// ---------------------------------------------------------------------------
template <class K, class V>
DictOfSet<K,V>::~DictOfSet()
    {
    Set<V> *newSet;
    
    for ( int i = 0; i < impl.size(); i++ )
        {
        pointerMap( impl.key( i ), newSet );
        delete newSet;
        }
    }

// -- public -----------------------------------------------------------------
// template <class K,class V>
// DictOfSet<K,V>::operator=(const DictOfSet<K,V> &rhs)
//
// copy c'tor
//
// ---------------------------------------------------------------------------
template <class K, class V>
DictOfSet<K,V>& DictOfSet<K,V>::operator=(const DictOfSet<K,V> &rhs)
    {
    // Delete the memory for each set.  Make sure we use a pointer to
    // a Set<V> and not a void* pointer, so that the Set destructor
    // will run.  This I learned from the great notification server
    // memory leak fiasco from 6/98, Murray
    Set<V> *newSet;
    int i;
    for (i = 0; i < impl.size(); i++ )
        {
        pointerMap( impl.key( i ), newSet );
        delete newSet;
        }

    // Clean out all the entries by reinitializing with an empty dict.
    impl = Dict<K, void*>();

    // Copy over the new set's information
    for ( i = 0; i < rhs.size(); i++ )
        impl.add( rhs.key( i ), (void*)new Set<V>( rhs.value( i ) ) );

    return *this;
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSet<K,V>::remove(const K &key)
//
// removes a set from the dictionary and delete's the set
//
// ---------------------------------------------------------------------------
template <class K, class V>
void DictOfSet<K,V>::remove(const K &key)
    {
    Set<V> *newSet;
    if ( pointerMap( key, newSet ) )
        {
        delete newSet;
        impl.remove( key );
        }
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// Set<K>::remove(const K &key, const V &val)
//
// Removes an element from a set and removes the set if that element
// was the last
//
// ---------------------------------------------------------------------------
template <class K, class V>
void DictOfSet<K,V>::remove(const K &key, const V &val)
    {
    Set<V> *newSet;
    
    if ( pointerMap( key, newSet ) )
        {
        newSet->remove( val );
        if ( newSet->size() == 0 )
            {
            delete newSet;
            impl.remove( key );
            }
        }
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSet<K,V>::add(const K &key, const Set<V> &set)
//
// iff there is a set at the key then delete the set and add the new set
//
// ---------------------------------------------------------------------------
template <class K, class V>
void DictOfSet<K,V>::add(const K &key, const Set<V> &set)
    {
    Set<V> *newSet;
    
    if ( pointerMap( key, newSet ) )
         *newSet = set;

    else
        impl.add( key, (void*)new Set<V>( set ) );
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSet<K,V>::add(const K &key, const V &val)
//
// iff there is a set at the key then add the val to the set, otherwise
// create a set at the key and add the element
//
// ---------------------------------------------------------------------------
template <class K, class V>
void DictOfSet<K,V>::add(const K &key, const V &val)
    {
    Set<V> *newSet;
    if ( pointerMap( key, newSet ) )
         newSet->add( val );

    else
        impl.add( key, (void*)new Set<V>( val ) );
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSet<K,V>::map(const K &key, Set<V> &set)
//
// iff there is a set at the key then add place a copy of the set in set.
// otherwise, do nothing and return false.
//
// ---------------------------------------------------------------------------
template <class K, class V>
bool DictOfSet<K,V>::map(const K &key, Set<V> &set) const 
    {
    Set<V> *newSet;
    if ( !impl.map( key, (void* &)newSet ) )
        return false;

    set = *newSet;
    return true;
    }


// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSet<K,V>::value(int i)
//
// iff there is a set at the value then return the set
// otherwise, return an empty set available from the set class.
//
// ---------------------------------------------------------------------------
template <class K, class V>
const Set<V> &DictOfSet<K,V>::value(int i) const
    {
    static Set<V> dummy;        // Only one per instantiation
    
    if ( i < impl.size() )
        return *( (Set<V> *)impl.value( i ) );

    return dummy;
    }


// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSet<K,V>::value(const K &key, int i)
//
// iff there is a set at the key then return the value held there
// otherwise, return an empty V value available from the set class.
//
// ---------------------------------------------------------------------------
template <class K, class V>
const V &DictOfSet<K,V>::value(const K &key, int i) const
    {
    static V dummy;             // Only one per instantiation
    
    Set<V> *newSet;

    if ( pointerMap( key, newSet ) )
        return newSet->key( i );

    return dummy;
    }

#endif
