// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// Dict.H
// General-purpose dictionary class template
//
// Author: Tom Cargill
// ---------------------------------------------------------------------------

// -- module -----------------------------------------------------------------
//
//  A Dictionary (Dict) is a set of <K,V> pairs, such that each K is unique.
//     K is a TextString, an integral type or a pointer.
//     V is any type with value semantics.
//     (K stands for "key", V for "value".)
//
//     If d is a Dict<K,V>, then:
//
//     1. d.map(k) returns true iff there is any <k,v> in d.
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
// This is one of a family of four dictionary template classes, each
// with identical syntax and semantics (except for the substituted classes).
// Which class to use depends on the following considerations.
//
// Dict<K,V> is the most general class, and may be used where any of the other
// classes can be.  It may lead to unnecessarily large object files
// ("code bloat"), however, and so one of the following classes should be
// used where possible.
//
// DictPP<K,V> should be used when both K and V are pointers; otherwise
// DictKP<K,V> should be used when V is a pointer, and
// DictPV<K,V> should be used when K is a pointer.
//
// -- implementation -- ------------------------------------------------------
// The following preprocessor symbols are intended for use in debugging
// or testing:
// DICT_TEST_FIXED_HASH, when set (must be to an integral value), will always
// return that value from the hash() function.
// DICT_TEST_INITIAL_SIZE, when set, may be used to change the initial size of
// the hash bucket array.
// DICT_TEST_GROWBY, when set, will set the factor by which the hash bucket
// array size increases when it needs to grow.
// ---------------------------------------------------------------------------

#ifndef _Dict_H
#define _Dict_H

#ifdef IDENT_TEMPLATE_H
static const char* const Dict_H_Id =
"$Id: .Dict.H__temp27950,v 1.3 2003/05/06 23:11:38 fluke Exp $";
#endif

#include "commonDefs.h"
#include "HashUtil.H"
#include "TextString.H"
#include "LogStream.H"

#include <stdlib.h>

template <class K, class V>
class Dict
    {
    public:
        ~Dict();    // not a base class
        Dict() { create(); }
        Dict(const Dict&);

        bool map(const K &key) const { return doMap(key, 0, 0); }
        bool map(const K &key, V &val) const { return doMap(key, 0, &val); }
        void remove(const K &key);
        void add(const K &key, const V &val);

        int size() const { return _entries; }
        void orderByK() const;
        const K &key(int i) const;
        const V &value(int i) const
            {
            V dummy;
            return validIndex(i) ? _vector[i]->value : dummy;
            }

        Dict& operator=(const Dict&);
        bool operator==(const Dict&) const;
        bool operator!=(const Dict &rhs) const{ return !operator==(rhs); }

        float testsPerSearch() const; // key==key's per map/add/remove

    private:
        // representation: hashing with chained collision resolution

        struct DictNode {    // forward declare eventually?
                K key;
                V value;
                DictNode *link;
                DictNode(const K &k, const V &v, DictNode *l)
                    : key(k), value(v), link(l) {}
            };

        DictNode **_hashTbl;
        unsigned hash(const K&) const;
        int _tableSz;             // size of array hashTbl points to
        int _entries;             // dictionary size
        bool _ordered;            // vector is ordered by < on K

        // context passed from doMap() to add() and remove()
        struct SearchContext {    // forward declare eventually
                DictNode *current;     // node where key found
                DictNode *previous;    // previous => previous->link == current
                int bucket;            // hash used in search
            };

        bool doMap(const K&, SearchContext *, V * = 0) const; // do the work

        void resize(int newTableSz);
        void adjustEntries(int delta);  // always +1 or -1

        // indexing: aux vector is built when needed, removed when invalid
        DictNode **_vector;               // should be mutable
        void buildVector();
        void sortVector(int lo, int hi);
        bool validIndex(int) const;

        // utilities:
        void create();        // arbitrary memory to empty state
        void destroy();       // remove object from memory

        Dict *_muthis;    // simulate mutable; muthis == this

        // instrumentation:
        long _searches, _tests;      // should be mutable
    };

// -- private ----------------------------------------------------------------
// template <class K, class V>
// Dict<K,V>::hash()
//
// Return a quasirandom number as a deterministic function of
// the specified text string key.
//
// -- implementation ---------------------------------------------------------
// Defining the preprocess symbol DICT_TEST_FIXED_HASH (to an integral
// value) will cause the fuction to always return that value, for testing
// or debugging purposes.  The dictionary will still perform correctly, but
// will be very inefficient, and put a strain on the hash collision code.
// ---------------------------------------------------------------------------
template <class K, class V>
inline unsigned Dict<K,V>::hash(const K &key) const
    {
#ifndef DICT_TEST_FIXED_HASH        // for test/debug
    return HashUtil::hash(key);
#else
    return DICT_TEST_FIXED_HASH;     // e.g. 0
#endif
    }

// -- private ----------------------------------------------------------------
// template <class K, class V>
// Dict<K,V>::create()
//
// Create an empty dictionary.
//
// -- implementation ---------------------------------------------------------
// The preprocessor symbol DICT_TEST_INITIAL_SIZE may be used to change the
// initial size of INITIAL_SIZE, the initial size of the hash bucket array,
// for testing or debugging purposes.  The value of INITIAL_SIZE may also
// be changed in place, for production code, if desired.
// ---------------------------------------------------------------------------
template <class K, class V>
void Dict<K,V>::create()
    {
    _entries = 0;
    _vector = 0;
    _ordered = false;
    _searches = _tests = 0;
    _muthis = this;               // simulate mutable
    _tableSz = 0;
    _hashTbl = 0;
#ifndef DICT_TEST_INITIAL_SIZE  // for test/debug
    const int INITIAL_SIZE = 50;    // tunable parameter
#else
    const int INITIAL_SIZE = DICT_TEST_INITIAL_SIZE;
#endif
    resize(INITIAL_SIZE);
    }

// -- private ----------------------------------------------------------------
// template <class K, class V>
// Dict<K,V>::destroy()
//
// Destroy a dictionary, delete all allocated memory, and take it to the
// undefined state.
//
// ---------------------------------------------------------------------------
template <class K, class V>
void Dict<K,V>::destroy() // take to undefined state
    {
    DictNode *next;
    for( int h = _tableSz-1; h >= 0; --h )
        for( DictNode *n = _hashTbl[h]; n; n = next )
            {
            next = n->link;
            delete n;
            }
    delete [] _vector;    // may be null
    delete [] _hashTbl;
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// Dict::~Dict()
//
// Destructor.
//
// ---------------------------------------------------------------------------
template <class K, class V>
Dict<K,V>::~Dict()
    {
    destroy();
    }

// -- private ----------------------------------------------------------------
// template <class K, class V>
// Dict<K,V>::validIndex()
//
// Build the vector of all dictionary entries, if needed, and return true
// iff the specified index is within the limits of the vector.
//
// ---------------------------------------------------------------------------
template <class K, class V>
inline bool Dict<K,V>::validIndex(int i) const
    {
    if( !_vector )
        _muthis->buildVector();
    bool valid = (i>=0 && i<_entries);
    if (!valid)
        logBug << "invalid index in Dict<K,V>::validIndex()" << std::endl;
    return valid;        // and stumble on
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// Dict<D,V>::key()
//
// Number the dictionary pairs, and return a reference to an indexed key.
//
// ---------------------------------------------------------------------------
template <class K, class V>
const K &Dict<K,V>::key(int i) const
    {
    static K dummy;   // one per instantiation!
    return validIndex(i) ? _vector[i]->key : dummy;
    }


// This is commented out because it does not work with the HP compiler +
// TDL C++ + informix header files :(
// -- public -----------------------------------------------------------------
// template <class K, class V>
// Dict<D,V>::value()
//
// Number the dictionary pairs, and return a reference to an indexed value.
//
// ---------------------------------------------------------------------------
//template <class K, class V>
//inline const V &Dict<K,V>::value(int i) const
//    {
//    static V dummy;
//    return validIndex(i) ? _vector[i]->value : _dummy;
//    }

// -- private ----------------------------------------------------------------
// template <class K, class V>
// Dict<D,V>::adjustEntries()
//
// Adjust the internal dictionary structure to add or remove a member.
//
// -- implementation ---------------------------------------------------------
// GROWBY is the factor by which the table expands when it is automatically
// enlarged to accommodate more entries.  It must be greater than one, for
// the table to get bigger, and in the original implementation, is two.
// Larger values of GROWBY will result in faster growing of the table, and
// more wasted space in the array of buckets.
//
// The preprocessor symbol DICT_TEST_GROWBY may be used to specify a
// different value of GROWBY, while testing or debugging.
//
// The fullness of the bucket table is also affected by the comparison
// near the end of the function.  The average length of the chains may
// be adjusted by adding a factor before _tableSz.  For example, changing
// it to if (_entries >= 3 * _tableSz) will allow chains to get up to an
// average length of 3, before adjustment.
// ---------------------------------------------------------------------------
template <class K, class V>
void Dict<K,V>::adjustEntries(int delta) // delta is +1 or -1
    {
    _entries += delta;
    if( _vector )
        {        // invalidate vector
        delete [] _vector;
        _vector = 0;
        }
#ifndef DICT_TEST_GROWBY     // for test/debug
    const int GROWBY = 2;     // tunable parameter: >=1
#else
    const int GROWBY =  DICT_TEST_GROWBY;
#endif
    const int AVG_CHAIN_LENGTH = 4;  // tunable parameter: >=1
    if( _entries >= AVG_CHAIN_LENGTH * _tableSz )
        resize(_tableSz*GROWBY);
    }

// -- private ----------------------------------------------------------------
// template <class K, class V>
// Dict<D,V>::resize()
//
// Resize the physical dictionary representation to accommodate up to
// newTableSz entries.
//
// ---------------------------------------------------------------------------
template <class K, class V>
void Dict<K,V>::resize(int newTableSz)  // called from adjustEntries and create
    {
    newTableSz = newTableSz/2*2 + 3;          // force odd
    while( !HashUtil::isPrime(newTableSz) )   // force prime
        newTableSz += 2;
    DictNode **newTable = new DictNode*[newTableSz];   // assume
    int h;
    for( h = newTableSz-1; h >= 0; --h )
        newTable[h] = 0;
    for( h = _tableSz-1; h >= 0; --h )
        {
        DictNode *n = _hashTbl[h];
        while( n )
            {
            DictNode *xfer = n;
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
// template <class K, class V>
// Dict<D,V>::buildVector()
//
// Create a vector of pointers to each dictionary entry, to impose
// an ordering on the Dictionary.
//
// ---------------------------------------------------------------------------
template <class K, class V>
void Dict<K,V>::buildVector()
    {
    _vector = new DictNode*[_entries];    // assume new success
    for( int h = _tableSz-1, i = 0; h >= 0; --h )
        for( DictNode *n = _hashTbl[h]; n; n = n->link )
            _vector[i++] = n;
    if( _ordered )
        sortVector(0, _entries-1);
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// Dict<D,V>::orderByK()
//
// Set an internal mode to ensure that the vector is ordered by key,
// rather than the default unpredictable ordering.
//
// ---------------------------------------------------------------------------
template <class K, class V>
void Dict<K,V>::orderByK() const
    {
    _muthis->_ordered = true;
    if( _vector )
        {            // invalidate vector
        delete [] _vector;
        _muthis->_vector = 0;
        }
    }

// -- private ----------------------------------------------------------------
// template <class K, class V>
// Dict<D,V>::sortVector()
//
// Sort the dictionary ordering by key.
//
// -- implementation ---------------------------------------------------------
// The quicksort algorithm, used herein, is fast for most cases, but can have
// bad worst-case behavior when the data is already sorted in the wrong
// direction.
// ---------------------------------------------------------------------------
template <class K, class V>
void Dict<K,V>::sortVector(int lo, int hi) // quicksort
    {
    // use only < on K

    while( lo < hi )
        {
        DictNode * const e = _vector[lo];
        int dn, up;
        for( up = lo, dn = hi; up <= dn; )
            {
            while( _vector[up]->key < e->key ) ++up;
            while( e->key < _vector[dn]->key ) --dn;
            if( up <= dn )
                {
                DictNode *xchg = _vector[up];
                _vector[up++] = _vector[dn];
                _vector[dn--] = xchg;
                }
            }
        sortVector(lo, dn);
        lo = up;
        }
    }

// -- private ----------------------------------------------------------------
// template <class K, class V>
// Dict<D,V>::doMap()
//
// General purpose map routine.  If there is an entry in the dictionary
// with a matching key, return true, and set value to point to the
// corresponding value; otherwise, return false, and don't change value.
// If any event, return context information in cxt that indicates where
// an entry with the specified key either is, or should go, within
// the dictionary.
//
// ---------------------------------------------------------------------------
template <class K, class V>
bool Dict<K,V>::doMap(const K &key, SearchContext *cxt, V *value) const
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
            {
            if( value )
                *value = cxt->current->value;
            return true;
            }
        cxt->previous = cxt->current;
        cxt->current = cxt->current->link;
        }
    return false;
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// Dict<K,V>::add()
//
// Add an entry to the dictionary.  If the key is already in the dictionary,
// replace the old entry with this one.
//
// ---------------------------------------------------------------------------
template <class K, class V>
void Dict<K,V>::add(const K &key, const V &value)
    {
    SearchContext sc;
    if( doMap(key, &sc) )
        {    // key already present -- update value
        sc.current->value = value;
        return;
        }
    // Key is missing: Add <key,value> node to head of chain
    _hashTbl[sc.bucket] = new DictNode(key, value, _hashTbl[sc.bucket]);
    // assume new success
    adjustEntries(+1);
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// Dict<K,V>::remove()
//
// Remove any entry with the given key from the dictionary.
//
// ---------------------------------------------------------------------------
template <class K, class V>
void Dict<K,V>::remove(const K &key)
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
// template <class K, class V>
// Dict<K,V>::operator==()
//
// Return true iff two dictionaries have the same set of entries.
//
// ---------------------------------------------------------------------------
template <class K, class V>
bool Dict<K,V>::operator==(const Dict<K,V> &rhs) const // naive
    {
    if( _entries != rhs._entries )
        return false;
    V v;
    for( int i = 0; i < _entries; ++i )
        if( !rhs.map(key(i), v) || v != value(i) )
            return false;
    return true;
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// Dict<K,V>::operator=()
//
// Assignment operator.  Make this dictionary have the same entries as another.
//
// ---------------------------------------------------------------------------
template <class K, class V>
Dict<K,V>& Dict<K,V>::operator=(const Dict<K,V> &rhs)  // naive
    {
    if( this == &rhs )
        return *this;
    destroy();
    create();
    for( int i = 0; i < rhs._entries; ++i )
        add(rhs.key(i), rhs.value(i));
    _ordered = rhs._ordered;
    return *this;
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// Dict<K,V>::Dict()
//
// Copy constructor.
//
// ---------------------------------------------------------------------------
template <class K, class V>
Dict<K,V>::Dict(const Dict<K,V> &master)  // naive
    {
    create();
    for( int i = 0; i < master._entries; ++i )
        add(master.key(i), master.value(i));
    _ordered = master._ordered;
    }

// -- public -----------------------------------------------------------------
// template <class K, class V>
// Dict<K,V>::testsPerSearch()
//
// Instrumentation routine for testing or debugging.
// Return the average number of tests done when searching for a key.
//
// ---------------------------------------------------------------------------
template <class K, class V>
float Dict<K,V>::testsPerSearch() const
    {
    return _searches==0 ? 0.0 : (float)_tests/_searches;
    }

#endif
