// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// Set.H
// General-purpose Set class template
//
// Author: Tom Cargill
// ---------------------------------------------------------------------------
#ifndef _Set_H
#define _Set_H

#ifdef IDENT_TEMPLATE_H
static const char* const Set_H_Id =
"$Id: .Set.H__temp27950,v 1.3 2003/05/06 23:11:50 fluke Exp $";
#endif

// -- module -----------------------------------------------------------------
//
//  A Set (Set) is a set of <K>, such that each K is unique.
//     K is a TextString, an integral type or a pointer.
//     (K stands for "key")
//
//     If d is a Set<K>, then:
//
//     1. d.map(k) returns true iff there is any <k> in d.
//     3. After d.remove(k), d.map(k) is false
//     4. d.size() returns the size of the set.
//     5. The elements of the set are exactly:
//          <d.key(0)> .. <d.key(sz-1)>
//          where sz = d.size();
//     6. After d.orderByK(), indexing is ordered by < on K,
//         i.e., d.key(i) < d.key(i+1).
//
// This is one of a family of two Set template classes, each
// with identical syntax and semantics (except for the substituted classes).
// Which class to use depends on the following considerations.
//
// Set<K> is the most general class, and may be used where any of the other
// classes can be.  It may lead to unnecessarily large object files
// ("code bloat"), however, and so one of the following classes should be
// used where possible.
//
// SetP<K> should be used when K is a pointer.
//
// -- implementation ---------------------------------------------------------
// This class is based on Tom Cargill's Dict class.  The main difference
// is that this class does not store values, only keys.
// ---------------------------------------------------------------------------

#include "commonDefs.h"
#include "Set.H"
#include "HashUtil.H"
#include "TextString.H"
#include "LogStream.H"

#include <stdlib.h>

template <class K>
class Set 
    {
    public:
        ~Set();    // not a base class
        Set() { create(); }
        Set(const Set&);
        Set(const K &key) { create(); add( key ); }
        void emptySet(void) { destroy(); create(); }

        bool map(const K &key) const { return doMap(key, 0); }
        void remove(const K &key);
        void add(const K &key);

        int size(void) const { return _entries; }
        void orderByK(void) const;
        const K &key(int i) const;
        const K &operator[](int i) const
            { return key( i ); }

        Set& operator=(const Set&);   
        bool operator==(const Set&) const;    // Test for mutual inclusivity
        bool operator!=(const Set &rhs) const // Not mutually inclusive...
            { return !operator==(rhs); }

        // Worded functions to avoid any possible ambiguities
        bool isRhsSubset(const Set &rhs) const;         // rhs  <= this
        bool isRhsProperSubset(const Set &rhs) const;   // rhs  <  this
        void setMinus(const Set &rhs);                  // this -= rhs
        void setUnion(const Set &rhs);                  // this |= rhs
        void setIntersection(const Set &rhs);           // this &= rhs
        bool mutuallyInclusive(const Set& rhs) const // this {...} == rhs {...}
            { return operator==( rhs ); }
        bool mutuallyExclusive(const Set& rhs) const;

        float testsPerSearch() const; // key==key's per map/add/remove

        
        
        // representation: hashing with chained collision resolution

        struct SetNode {    // forward declare eventually?
                K key;
                SetNode *link;
                SetNode(const K &k, SetNode *l)
                    : key(k), link(l) {}
            };
    
    private: 
        SetNode **_hashTbl;
        unsigned hash(const K&) const;
        int _tableSz;             // size of array hashTbl points to
        int _entries;             // Sets size
        bool _ordered;            // client has requested sorting (< K)
        bool _vectorSorted;       // vector has been sorted.
        
        // context passed from doMap() to add() and remove()
        struct SearchContext {    // forward declare eventually
                SetNode *current;     // node where key found
                SetNode *previous;    // previous => previous->link == current
                int bucket;            // hash used in search 
            };
    
        bool doMap(const K&, SearchContext *) const; // do the work
        
        void resize(int newTableSz);
        void adjustEntries(int delta);  // always +1 or -1
    
        // indexing: aux vector is built when needed, removed when invalid   
        SetNode **_vector;               // should be mutable
        void buildVector(void);
        void sortVector(int lo, int hi);
        bool validIndex(int) const;

        // utilities:
        void create(void);        // arbitrary memory to empty state
        void destroy(void);       // remove object from memory

        Set *_muthis;    // simulate mutable; muthis == this
    
        // instrumentation:
        long _searches, _tests;      // should be mutable
    };

// -- private ----------------------------------------------------------------
// template <class K>
// Set<K>::hash()
//
// Return a quasirandom number as a deterministic function of
// the specified text string key.
//
// -- implementation ---------------------------------------------------------
// Defining the preprocess symbol SET_TEST_FIXED_HASH (to an integral
// value) will cause the fuction to always return that value, for testing
// or debugging purposes.  The Set will still perform correctly, but
// will be very inefficient, and put a strain on the hash collision code.
// ---------------------------------------------------------------------------
template <class K>
inline unsigned Set<K>::hash(const K &key) const
    {
#ifndef SET_TEST_FIXED_HASH        // for test/debug
    return HashUtil::hash(key);
#else
    return SET_TEST_FIXED_HASH;     // e.g. 0
#endif
    }

// -- private ----------------------------------------------------------------
// template <class K>
// Set<K>::create()
//
// Create an empty Set.
//
// -- implementation ---------------------------------------------------------
// The preprocessor symbol SET_TEST_INITIAL_SIZE may be used to change the
// initial size of INITIAL_SIZE, the initial size of the hash bucket array,
// for testing or debugging purposes.  The value of INITIAL_SIZE may also
// be changed in place, for production code, if desired.
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::create(void)
    {
    _entries = 0;
    _vector = 0;
    _ordered = _vectorSorted = false;
    _searches = _tests = 0;
    _muthis = this;               // simulate mutable
    _tableSz = 0;
    _hashTbl = 0;
#ifndef SET_TEST_INITIAL_SIZE  // for test/debug
    const int INITIAL_SIZE = 50;    // tunable parameter
#else
    const int INITIAL_SIZE = SET_TEST_INITIAL_SIZE;
#endif
    resize(INITIAL_SIZE);
    }

// -- private ----------------------------------------------------------------
// template <class K>
// Set<K>::destroy()
//
// Destroy a Set, delete all allocated memory, and take it to the
// undefined state.
//
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::destroy() // take to undefined state
    {
    SetNode *next;
    for( int h = _tableSz-1; h >= 0; --h )
        for( SetNode *n = _hashTbl[h]; n; n = next )
            {
            next = n->link;
            delete n;
            }
    delete [] _vector;    // may be null
    delete [] _hashTbl;
    }

// -- public -----------------------------------------------------------------
// template <class K>
// Set::~Set()
//
// Destructor.
//
// ---------------------------------------------------------------------------
template <class K>
Set<K>::~Set(void)
    {
    destroy();
    }

// -- private ----------------------------------------------------------------
// template <class K>
// Set<K>::validIndex()
//
// Build the vector of all Set entries, if needed, and return true
// iff the specified index is within the limits of the vector.
//
// ---------------------------------------------------------------------------
template <class K>
inline bool Set<K>::validIndex(int i) const
    {
    bool valid = (i>=0 && i<_entries);
    if (!valid)
        logBug << "invalid index in Dict<K,V>::validIndex()" << std::endl;
    else
        {
        if (!_vector)
            _muthis->buildVector();

        // If this is not index 0, and we have not sorted our vector
        // yet, but this is an ordered dict, initiate the quick sort.
        if (i && !_vectorSorted && _ordered)
            {
            _muthis-> sortVector (0, _entries-1);
            _muthis-> _vectorSorted = true;
            }
        }
    
    return valid;        // and stumble on
    }

// -- public -----------------------------------------------------------------
// template <class K>
// Set<D,V>::key()
//
// Return a reference to an indexed key.
//
// ---------------------------------------------------------------------------
template <class K>
const K &Set<K>::key(int i) const
    {
    static K dummy;   // one per instantiation!
    if ( validIndex(i) )
        return _vector[i]->key;

    else
        return dummy;
    }

// -- private ----------------------------------------------------------------
// template <class K>
// Set<D>::adjustEntries()
//
// Adjust the internal Set structure to add or remove a member.
//
// -- implementation ---------------------------------------------------------
// GROWBY is the factor by which the table expands when it is automatically
// enlarged to accommodate more entries.  It must be greater than one, for
// the table to get bigger, and in the original implementation, is two.
// Larger values of GROWBY will result in faster growing of the table, and
// more wasted space in the array of buckets.
//
// The preprocessor symbol SET_TEST_GROWBY may be used to specify a
// different value of GROWBY, while testing or debugging.
//
// The fullness of the bucket table is also affected by the comparison
// near the end of the function.  The average length of the chains may
// be adjusted by adding a factor before _tableSz.  For example, changing
// it to if (_entries >= 3 * _tableSz) will allow chains to get up to an
// average length of 3, before adjustment.
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::adjustEntries(int delta) // delta is +1 or -1
    {
    _entries += delta;
    if( _vector )
        {        // invalidate vector
        delete [] _vector;
        _vectorSorted = false;
        _vector = 0;
        }
#ifndef SET_TEST_GROWBY     // for test/debug
    const int GROWBY = 2;     // tunable parameter: >=1
#else
    const int GROWBY =  SET_TEST_GROWBY;
#endif
    if( _entries >= _tableSz )
        resize(_tableSz*GROWBY);
    }

// -- private ----------------------------------------------------------------
// template <class K>
// Set<D,V>::resize()
//
// Resize the physical Set representation to accommodate up to
// newTableSz entries.
//
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::resize(int newTableSz)  // called from adjustEntries and create
    {
    newTableSz = newTableSz/2*2 + 3;          // force odd
    while( !HashUtil::isPrime(newTableSz) )   // force prime
        newTableSz += 2;
    SetNode **newTable = new SetNode*[newTableSz];   // assume
    int h;
    for(h = newTableSz-1; h >= 0; --h )
        newTable[h] = 0;
    for( h = _tableSz-1; h >= 0; --h )
        {
        SetNode *n = _hashTbl[h];
        while( n )
            {
            SetNode *xfer = n;
            n = n->link;
            int newH = hash(xfer->key) % newTableSz;
            xfer->link = newTable[newH];
            newTable[newH] = xfer;
            }
        }
    delete [] _hashTbl;
    _hashTbl = newTable;
    _tableSz = newTableSz;
    }

// -- private ----------------------------------------------------------------
// template <class K>
// Set<D,V>::buildVector()
//
// Create a vector of pointers to each Set entry, to impose
// an ordering on the Set.
//
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::buildVector(void)
    {
    _vector = new SetNode*[_entries];    // assume new success
    int minKeyIndex = 0;
    for( int h = _tableSz-1, i = 0; h >= 0; --h )
        for( SetNode *n = _hashTbl[h]; n; n = n->link )
            {
            _vector[i] = n;
            // If we are ordering this vector, find the index of the
            // smallest key.
            if (_ordered && _vector[i]-> key < _vector[minKeyIndex]-> key)
                minKeyIndex = i;
            i++;
            }

    // Make sure that the first element in the vector, is the
    // smallest, only if this vector is ordered.
    if (_ordered)
        {
        SetNode *xchg = _vector[minKeyIndex];
        _vector[minKeyIndex] = _vector[0];
        _vector[0] = xchg;
        }
    }

// -- public -----------------------------------------------------------------
// template <class K>
// Set<D,V>::orderByK()
//
// Set an internal mode to ensure that the vector is ordered by key,
// rather than the default unpresetable ordering.
//
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::orderByK() const
    {
    _muthis->_ordered = true;
    if( _vector )
        {            // invalidate vector
        delete [] _vector;
        _muthis->_vectorSorted = false;
        _muthis->_vector = 0;
        }
    }

// -- private ----------------------------------------------------------------
// template <class K>
// Set<D,V>::sortVector()
//
// Sort the Set ordering by key.
//
// -- implementation ---------------------------------------------------------
// The quicksort algorithm, used herein, is fast for most cases, but can have
// bad worst-case behavior when the data is already sorted in the wrong
// direction.
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::sortVector(int lo, int hi) // quicksort
    {
    // use only < on K
    
    while( lo < hi )
        {
        SetNode * const e = _vector[lo];
        int up = lo, dn = hi;
        for(; up <= dn; )
            {
            while( _vector[up]->key < e->key ) ++up;
            while( e->key < _vector[dn]->key ) --dn;
            if( up <= dn )
                {
                SetNode *xchg = _vector[up];
                _vector[up++] = _vector[dn];
                _vector[dn--] = xchg;
                }
            }
        sortVector(lo, dn);
        lo = up;
        }
    }

// -- private ----------------------------------------------------------------
// template <class K>
// Set<V>::doMap()
//
// General purpose map routine.  If there is an entry in the Set
// with a matching key, return true; otherwise, return false.
// If any event, return context information in cxt that indicates where
// an entry with the specified key either is, or should go, within
// the Set.
//
// ---------------------------------------------------------------------------
template <class K>
bool Set<K>::doMap(const K &key, SearchContext *cxt) const
    {
    SearchContext scratch;
    if( !cxt )
        cxt = &scratch;
    cxt->bucket = hash(key) % _tableSz;
    cxt->current = _hashTbl[cxt->bucket];
    cxt->previous = 0;
    _muthis->_searches += 1;        // stats
    while( cxt->current )
        {
        _muthis->_tests += 1;        // stats
        if( cxt->current->key == key )
            return true;

        cxt->previous = cxt->current;
        cxt->current = cxt->current->link;
        }
    return false;
    }

// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::add()
//
// Add an entry to the Set.  If the key is already in the Set,
// do nothing...
//
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::add(const K &key)
    {
    SearchContext sc;
    if( doMap(key, &sc) )
        return;                 // If key already present then do nothing.

    // Key is missing: Add <key> node to head of chain
    _hashTbl[sc.bucket] = new SetNode(key, _hashTbl[sc.bucket]);
    // assume new success
    adjustEntries(+1);
    }

// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::remove()
//
// Remove any entry with the given key from the Set.
//
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::remove(const K &key)
    {
    SearchContext sc;
    if( !doMap(key, &sc) )  // Key absent: Do nothing
        return;
    // Key found: Remove node from hashTbl
    if( sc.previous )
        sc.previous->link = sc.current->link;   // general case
    else
        _hashTbl[sc.bucket] = sc.current->link;    // first in chain
    delete sc.current;
    adjustEntries(-1);
    }

// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::operator==()
//
// Return true iff two Set have the same set of entries.
//
// ---------------------------------------------------------------------------
template <class K>
bool Set<K>::operator==(const Set<K> &rhs) const // naive
    {
    if( _entries != rhs._entries )
        return false;

    for( int i = 0; i < _entries; ++i )
        if( !rhs.map( key( i ) ) )
            return false;
    return true;
    }

// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::operator=()
//
// Assignment operator.  Make this Set have the same entries as another.
//
// ---------------------------------------------------------------------------
template <class K>
Set<K>& Set<K>::operator=(const Set<K> &rhs)  // naive
    {
    if( this == &rhs )
        return *this;
    destroy();
    create();
    for( int i = 0; i < rhs._entries; ++i )
        add(rhs.key(i));
    _ordered = rhs._ordered;
    return *this;
    }

// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::Set()
//
// Copy constructor.
//
// ---------------------------------------------------------------------------
template <class K>
Set<K>::Set(const Set<K> &master)  // naive
    {
    create();
    for( int i = 0; i < master._entries; ++i )
        add(master.key(i));
    _ordered = master._ordered;
    }

// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::testsPerSearch()
//
// Instrumentation routine for testing or debugging.
// Return the average number of tests done when searching for a key.
//
// ---------------------------------------------------------------------------
template <class K>
float Set<K>::testsPerSearch(void) const
    {
    return _searches==0 ? 0.0 : (float)_tests/_searches;
    }

// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::isRhsSubset()
//
// Is every element in rhs also in this?
//
// ---------------------------------------------------------------------------
template <class K>
bool Set<K>::isRhsSubset(const Set<K> &rhs) const 
    {
    if( this == &rhs )
        return true;

    if ( rhs._entries > _entries )
        return false;
    
    for( int i = 0; i < rhs._entries; ++i )
        if ( !map( rhs.key( i ) ) )
            return false;

    return true;
    }


// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::isRhsProperSubset()
//
// is rhs smaller than this and is every element in rhs an element of this.
//
// ---------------------------------------------------------------------------
template <class K>
bool Set<K>::isRhsProperSubset(const Set<K> &rhs) const
    {
    if( ( this == &rhs ) || ( rhs._entries >= _entries ) )
        return false;
    
    for( int i = 0; i < rhs._entries; ++i )
        if ( !map( rhs.key( i ) ) )
            return false;

    return true;
    }


// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::setMinus()
//
// Subtract all elements common to both this and rhs from this.
//
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::setMinus(const Set<K> &rhs)
    {
    // If we;re subtrstin from ourselves then make an empty set.
    if( this == &rhs )
        {
        destroy();
        create();
        return;
        }
    
    for( int i = 0; i < rhs._entries; ++i )
        if ( map( rhs.key( i ) ) )
             remove( rhs.key( i ) );
    }


// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::setUnion()
//
// Adds all elements from rhs that are not already in this to this.
//
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::setUnion(const Set<K> &rhs)
    {
    // No unique elements to this and rhs here
    if( *this == rhs )
        return;
    
    for( int i = 0; i < rhs._entries; ++i )
        add( rhs.key( i ) );
    }


// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::setIntersection()
//
// leaves this containing the intersection of this and rhs
//
// ---------------------------------------------------------------------------
template <class K>
void Set<K>::setIntersection(const Set<K> &rhs)
    {
    // Intersection of self and self == self
    if( this == &rhs )
        return;
    
    for( int i = 0; i < _entries; ++i )
        if ( !rhs.map( key( i ) ) )
            remove( key( i-- ) );
    }   


// -- public -----------------------------------------------------------------
// template <class K>
// Set<K>::mutuallyExclusive()
//
// returns false iff rhs[x] == this[x], otherwise returns true.
//
// ---------------------------------------------------------------------------
template <class K>
bool Set<K>::mutuallyExclusive(const Set<K> &rhs) const
    {
    // A set and itself aren't mutually exclusive
    if( this == &rhs )
        return false;
    
    for( int i = 0; i < _entries; ++i )
        if ( rhs.map( key( i ) ) )
            return false;

    return true;
    }   


#endif
