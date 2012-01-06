// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// DictOfSetKP.H
// General-purpose dictionary of set class template
//
// Author: Brian Woods
// ---------------------------------------------------------------------------
#ifndef _DictOfSetKP_H
#define _DictOfSetKP_H

#ifdef IDENT_TEMPLATE_H
static const char* const DictOfSetKP_H_Id =
"$Id: .DictOfSetKP.H__temp27950,v 1.3 2003/05/06 23:11:39 fluke Exp $";
#endif

#include "commonDefs.h"
#include "SetP.H"
#include "Set.H"
#include "Dict.H"
#include "DictOfSet.H"

// -- module ------------------------------------------------------------------
//
// Implements a dictionary of sets using the DictKP and Set classes.
//
//-----------------------------------------------------------------------------

template <class K, class V>
class DictOfSetKP
    {
    public:
        // c'tor's and d'tor
        DictOfSetKP() {}
        ~DictOfSetKP();
        DictOfSetKP(const DictOfSetKP &);
        DictOfSetKP<K,V>& operator=(const DictOfSetKP<K,V> &rhs);

        //operations upon the sets
        bool map(const K &key) const { return impl.map( key ); }
        bool map(const K &key, SetP<V> &set) const; 
        
        void add(const K &key, const SetP<V> &set);
        void remove(const K &key);
        int size(void) const            { return impl.size(); }
        void orderByK(void) const       { impl.orderByK(); }
        const K &key(int i) const       { return impl.key( i ); }
        const SetP<V> &value(int i) const;
       

        // Operations on the set's elements
        bool map(const K &key, const V &val) const
            { SetP<V> *newSet; return ( pointerMap( key, newSet ) ?
                  newSet->map( val ) : false ); }
        
        void remove(const K &key, const V &val);
        void add(const K &key, const V &val);
        int size(const K& key)
            { SetP<V> *newSet;
            return ( pointerMap( key, newSet ) ? newSet->size() : 0 ); }
        void orderByK(const K &key) const
            { SetP<V> *newSet;
            if ( pointerMap( key, newSet ) ) newSet->orderByK(); }
        const V &value(const K &key, int i) const;

        // Set operations
        void setUnion(const K &key, const SetP<V> &rhs)
            { SetP<V> *newSet;
            if ( pointerMap( key, newSet ) ) newSet->setUnion( rhs );}
        
        void setIntersection(const K &key, const SetP<V> &rhs)
            { SetP<V> *newSet;
            if ( pointerMap( key, newSet ) ) newSet->setIntersection( rhs ); }
        
        void setMinus(const K &key, const SetP<V> &rhs)
            { SetP<V> *newSet;
            if ( pointerMap( key, newSet ) ) newSet->setMinus( rhs );}
        
        bool isRhsSubset(const K &key, const SetP<V> &rhs)
            { SetP<V> *newSet; return ( pointerMap( key, newSet ) ?
                  newSet->isRhsSubset( rhs ) : false ); }
        
        bool isRhsProperSubset(const K &key, const SetP<V> &rhs)
            { SetP<V> *newSet; return ( pointerMap( key, newSet ) ?
                  newSet->isRhsProperSubset( rhs ) :
                  false ); }
        
        bool mutuallyInclusive(const K &key, const SetP<V> &rhs)
            { SetP<V> *newSet; return ( pointerMap( key, newSet ) ?
                  newSet->mutuallyInclusive( rhs ) :
                  false ); }
        
        bool mutuallyExclusive(const K &key, const SetP<V> &rhs)
            { SetP<V> *newSet; return ( pointerMap( key, newSet ) ?
                  newSet->mutuallyExclusive( rhs ) :
                  false ); }

        // Comparrison operators on the dictionaries
        bool operator==(const DictOfSetKP<K,V> &rhs) const;
        bool operator!=(const DictOfSetKP<K,V> &rhs) const
            { return !operator==( rhs ); }

        float testsPerSearch(void) const // key==key's per map/add/remove
            { return impl.testsPerSearch(); }

    private:
        bool pointerMap(const K &key, SetP<V>* &set) const
            { return impl.map(key, (void* &) set ); }

        // Return dummy variables needed for special cases
        Set<V> &setDummy(void);
        K      &keyDummy(void);
        
        Dict<K, void*> impl;
    };

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSetKP<K,V>::DictOfSetKP(const DictOfSetKP&)
//
// Copy constructor
//
// ---------------------------------------------------------------------------
template <class K, class V>
DictOfSetKP<K,V>::DictOfSetKP(const DictOfSetKP<K,V> &rhs)
    {
    for ( int i = 0; i < rhs.size(); i++ )
        impl.add( rhs.key( i ), new SetP<V>( rhs.value( i ) ) );
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSetKP<K,V>::operator==(const DictOfSetKP& rhs)
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
bool DictOfSetKP<K,V>::operator==(const DictOfSetKP<K,V> &rhs) const
    {
    // If they're of different size return false
    if ( impl.size() != rhs.impl.size() )
        return false;

    SetP<V> *set;
    for ( int i = 0; i < impl.size(); i++ )
        {
        // Map values in this to rhs
        if ( !rhs.pointerMap( impl.key( i ), set ) )
            return false;

        // Test the sets for equality
        if ( *set != *(SetP<V> *)impl.value( i ) )
            return false;
        }

    return true;
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSetKP<K,V>::~DictOfSetKP()
//
// Destructor
//
// ---------------------------------------------------------------------------
template <class K, class V>
DictOfSetKP<K,V>::~DictOfSetKP()
    {
    SetP<V> *newSet;
    
    for ( int i = 0; i < impl.size(); i++ )
        {
        pointerMap( impl.key( i ), newSet );
        delete newSet;
        }
    }

// -- public -----------------------------------------------------------------
// template <class K,class V>
// DictOfSetKP<K,V>::operator=(const DictOfSetKP<K,V> &rhs)
//
// copy c'tor
//
// ---------------------------------------------------------------------------
template <class K, class V>
DictOfSetKP<K,V>& DictOfSetKP<K,V>::operator=(const DictOfSetKP<K,V> &rhs)
    {
    int i;
    
    // Empty the current set.
    for (i = 0; i < impl.size(); i++ )
        {
        SetP<V> *set = ( SetP<V> *)impl.value(i);
        delete set;
        impl.remove( key( i ) );
        }

    // Copy over the new set's information
    for ( i = 0; i < rhs.size(); i++ )
        impl.add( rhs.key( i ), (void*)new SetP<V>( rhs.value( i ) ) );

    return *this;
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSetKP<K,V>::remove(const K &key)
//
// removes a set from the dictionary and delete's the set
//
// ---------------------------------------------------------------------------
template <class K, class V>
void DictOfSetKP<K,V>::remove(const K &key)
    {
    SetP<V> *newSet;
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
void DictOfSetKP<K,V>::remove(const K &key, const V &val)
    {
    SetP<V> *newSet;
    
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
// DictOfSetKP<K,V>::add(const K &key, const SetP<V> &set)
//
// iff there is a set at the key then delete the set and add the new set
//
// ---------------------------------------------------------------------------
template <class K, class V>
void DictOfSetKP<K,V>::add(const K &key, const SetP<V> &set)
    {
    SetP<V> *newSet;
    
    if ( pointerMap( key, newSet ) )
         *newSet = set;

    else
        impl.add( key, (void*)new SetP<V>( set ) );
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSetKP<K,V>::add(const K &key, const V &val)
//
// iff there is a set at the key then add the val to the set, otherwise
// create a set at the key and add the element
//
// ---------------------------------------------------------------------------
template <class K, class V>
void DictOfSetKP<K,V>::add(const K &key, const V &val)
    {
    SetP<V> *newSet;
    if ( pointerMap( key, newSet ) )
         newSet->add( val );

    else
        impl.add( key, (void*)new SetP<V>( val ) );
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSetKP<K,V>::map(const K &key, SetP<V> &set)
//
// iff there is a set at the key then add place a copy of the set in set.
// otherwise, do nothing and return false.
//
// ---------------------------------------------------------------------------
template <class K, class V>
bool DictOfSetKP<K,V>::map(const K &key, SetP<V> &set) const 
    {
    SetP<V> *newSet;
    if ( !impl.map( key, (void* &)newSet ) )
        return false;

    set = *newSet;
    return true;
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSetKP<K,V>::value(int i)
//
// iff there is a set at the value then return the set
// otherwise, return an empty set available from the set class.
//
// ---------------------------------------------------------------------------
template <class K, class V>
const SetP<V> &DictOfSetKP<K,V>::value(int i) const
    {
    static SetP<V> dummy;        // Only one per instantiation
    
    if ( i < impl.size() )
        return *( (SetP<V> *)impl.value( i ) );

    return dummy;
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// DictOfSetKP<K,V>::value(const K &key, int i)
//
// iff there is a set at the key then return the value held there
// otherwise, return an empty V value available from the set class.
//
// ---------------------------------------------------------------------------
template <class K, class V>
const V &DictOfSetKP<K,V>::value(const K &key, int i) const
    {
    static V dummy;             // Only one per instantiation
    
    SetP<V> *newSet;

    if ( pointerMap( key, newSet ) )
        return newSet->key( i );

    return dummy;
    }

#endif
