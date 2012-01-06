// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// SeqOf.H
// General-purpose sequence class template.
//
// Author: Tom Cargill
// ---------------------------------------------------------------------------
#ifndef _SeqOf_H
#define _SeqOf_H

#ifdef IDENT_TEMPLATE_H
static const char* const SeqOf_H_Id =
"$Id: .SeqOf.H__temp27950,v 1.4 2003/05/06 23:11:49 fluke Exp $";
#endif

#include "RCObject.H"
#include "RCPtr.H"
#include "commonDefs.h"
#include "LogStream.H"

template <class T>
struct SeqValue: public RCObject
    {
    public:
        SeqValue(void);
        SeqValue(const T *data, unsigned int length);
        SeqValue(const SeqValue &rhs);
        virtual ~SeqValue(void);


        void setLength(int length);

        int _logSz;
        int _physSz;
        T *_rep;

    private:
        void init(void);
        void init(const T *data, unsigned int length);
    };

template <class T>
SeqValue<T>::SeqValue(void)
    {
    init();
    }

template <class T>
SeqValue<T>::SeqValue(const T *data, unsigned int length)
    {
    init(data, length);
    }

template <class T>
SeqValue<T>::~SeqValue(void)
    {
    delete[] _rep;
    }

template <class T>
SeqValue<T>::SeqValue(const SeqValue<T> &rhs)
    {
    init(rhs._rep, rhs._logSz);
    }

template <class T>
void SeqValue<T>::init(const T *data, unsigned int length)
    {
    _physSz = _logSz = length;
    _rep = new T[_physSz];
    for (int i = 0 ; i < _logSz ; i++)
        _rep[i] = data[i];
    }

template <class T>
void SeqValue<T>::init(void)
    {
    _physSz = _logSz = 0;
    _rep = 0;
    }

template <class T>
void SeqValue<T>::setLength(int newLogSz)
    {
    if( newLogSz < 0 )
        {
        logBug << "negative argument to SeqOf.setLength" << std::endl;
        newLogSz = 0;   // and stumble on
        }

    const int THRESHOLD = 10;    // must shrink/grow by at least this absolute,
    const float ALPHA = 0.10;    // and by at least this proportion

    if (newLogSz<_physSz-THRESHOLD && newLogSz<_physSz*(1-ALPHA) ) // shrink
        _physSz = newLogSz;
    else if (newLogSz > _physSz )  // grow
        _physSz = max(newLogSz,
          max((int)(_physSz*(1+ALPHA)), _physSz+THRESHOLD));
    else
        {
        _logSz = newLogSz;  // physical size unchanged
        return;
        }

    T *oldRep = _rep;
    _rep = new T[_physSz];

    // Must not copy more than the NEW physical size,
    // and not more than the OLD logical size.
    int i;
    for (i = min(_physSz,_logSz)-1; i >= 0; --i )
        if (oldRep)
            _rep[i] = oldRep[i];
        else
            _rep[i] = T();

    for (i = min(_physSz,_logSz) ; i < _physSz ; i++)
        _rep[i] = T();

    delete [] oldRep;
    _logSz = newLogSz;
    }




template <class T>
class SeqOf
    {
    public:
        SeqOf();
        SeqOf(const T &);                     // length is 1
        SeqOf(const T *vector, int length);   // length >= 0
        virtual ~SeqOf();

        bool operator==(const SeqOf &) const;
        bool operator!=(const SeqOf &s) const { return !operator==(s); }

        int length() const { return this->value->_logSz; }
        void setLength(int length);                  // valid length is >= 0
        SeqOf operator+(const SeqOf &rhs) const      // concatenation
            { return SeqOf<T>(this, &rhs); }         // use private ctor

        void append(const SeqOf &rhs);
        void append(const T &t);

        // Valid index/lo/hi parameters must be >=0 and <length(), with lo<=hi
        T &operator[](int index);
        const T &operator[](int index) const;
        SeqOf extract(int lo, int hi) const;     // yield [lo]..[hi] as SeqOf
        void remove(int lo, int hi);             // replace [lo]..[hi] by empty
        void replace(int lo, int hi, const SeqOf &s);// replace [lo]..[hi] by s
        void replace(int index, const SeqOf &s)        // replace [index] by s
            { replace(index, index, s); }
        void prepend(const SeqOf &s)                   // insert s before [0]
            { doReplace(-1, 0, s); }
        void insertBefore(int index, const SeqOf &s);// insert s before [index]
        void insertAfter(int index, const SeqOf &s); // insert s after [index]

        // If t is found in this SeqOf then this function returns its index.
        int find(const T &t, int startPos = 0) const;

        int max(void) const;
        int min(void) const;

        // --- Sorting related stuff ---
        enum SortOrder { LESSTHAN, EQUAL, GREATER };
        typedef SortOrder (*Compar)(const T &v1, const T &v2);

        // default comparison functions
        static SortOrder ascendCmp(const T &v1, const T &v2)
            {
            if (v1 > v2)
                return GREATER;

            return (v1 == v2) ? EQUAL : LESSTHAN;
            }

        static SortOrder decendCmp(const T &v1, const T &v2)
            {
            if (v1 > v2)
                return LESSTHAN;

            return (v1 == v2) ? EQUAL : GREATER;
            }

        // Sorts this sequence in ascending or decending order
        void sort(bool ascendingOrder = true)
            { sort(ascendingOrder ? ascendCmp : decendCmp); }
        // Sorts this sequence using the supplied comparison function
        // The two functions found above (ascendCmp() and decendCmp())
        // can be used for cmp.  They use the <, >, and == operators
        // beloning to the class T.
        void sort(Compar cmp);

        // finds the entries in this list that aren't in other list
        SeqOf notInList(const SeqOf<T>& otherList) const;

        // find entries that are common to two lists
        SeqOf in(const SeqOf<T>& otherList) const;

        std::ostream &printOn(std::ostream &os) const;

        // STL like methods
        template <class Iterator>
        void assign(Iterator beg, Iterator end);
        const T &front(void) const { return (*this)[0]; }
        T &front(void) { return (*this)[0]; }
        const T &back(void) const { return (*this)[this->value->_logSz-1]; }
        T &back(void) { return (*this)[this->value->_logSz-1]; }
        void swap(int index1, int index2);
        void reverse(void);
        void push_front(const T &t) { prepend(t); }
        void pop_front(void) { remove(0, 0); }
        void push_back(const T &t) { append(t); }
        void pop_back(void) { remove(length()-1, length()-1); }
        bool empty(void) const { return length() == 0; }


        // deprecated
        const T *dataPtr() const  // underlying representation: caveat emptor
            { return this->value->_rep; }
    private:
        RCPtr<SeqValue<T> > value;

        void doReplace(int, int, const SeqOf &);   // see definition
        SeqOf(const SeqOf *left, const SeqOf *right);  // concatenate for op+
        int checkIndex(int i) const;    // verify that 0 <= i < logSz
        void reportIndexError(int i) const;
    };

//
// Inline function definitions
//
template <class T>
inline int SeqOf<T>::checkIndex(int i) const    // verify that 0 <= i < logSz
    {
    if ( i < 0 || i >= this->value->_logSz )
        reportIndexError(i);
    return i<0 ? 0 : i>=this->value->_logSz ? 0 : i;  // and stumble on
    }


template <class T>
inline std::ostream &operator<<(std::ostream &os, const SeqOf<T> &seq)
    {
    return seq.printOn(os);
    }


template <class T>
inline T &SeqOf<T>::operator[](int index)
    {
    if (this->value->isShared())
        this->value = new SeqValue<T>(this->value->_rep,
          this->value->_logSz);
    this->value->markUnshareable();
    return this->value->_rep[checkIndex(index)];
    }

template <class T>
inline const T &SeqOf<T>::operator[](int index) const
    {
    return this->value->_rep[checkIndex(index)];
    }

// -- module -----------------------------------------------------------------
//
// SeqOf<T> is a sequence of T values. At any time, the logical state of
// a SeqOf<T> object, s, is the values: s[0], s[1], .. s[s.length()-1].
//
// 1. T must support the following uses:
//      T x, y;
//      x = y, x == y, x != y;
//      delete [] new T[1];
//      x < y
//   os << x;             // where: std::ostream &os
//
// 2. References returned from operator[] remain valid until a
//    non-const member function (other than operator[]) is called.
//
// 3. New elements created by setLength() are initialized by T::T().
//
// 4. The underlying representation of the sequence is an array. Elements
//    of the sequence may be indexed by integers from 0 to length()-1.
//    The size of the physical array allocated may exceed length().
//
// -- implementation ---------------------------------------------------------
//
// The implementation is via an array of objects.  As the size of the array
// changes, memory is allocated and released as necessary.  To reduce the
// number of memory allocations and deallocations required, the physical size
// of the memory required for the sequence is tracked and adjusted in a more
// optimized way, somewhat independently of the logical size of the sequence.
//
// ---------------------------------------------------------------------------

template <class T>
SeqOf<T>::SeqOf()
    {
    this->value = new SeqValue<T>();
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::SeqOf()
//
// Constructor to make sequence of length one from a single element.
//
// -- implementation ---------------------------------------------------------
// Call the initialize member function to initialize a representation
// array of length 1.  Copy the element specified by the input parameter
// to the first element in the array.
//
// ---------------------------------------------------------------------------
template <class T>
SeqOf<T>::SeqOf(const T &t)
    {
    this->value = new SeqValue<T>();
    this->value->setLength(1);
    this->value->_rep[0] = t;
    }

template <class T>
inline void SeqOf<T>::swap(int index1, int index2)
    {
    if (index1 == index2)
        return;
    T tmp((*this)[index1]);
    (*this)[index1] = (*this)[index2];
    (*this)[index2] = tmp;
    }

template <class T>
void SeqOf<T>::reverse(void)
    {
    int front = 0, back = length() - 1;
    while (front < back)
        {
        swap(front, back);
        front++;
        back--;
        }
    }

template <class T>
inline void SeqOf<T>::append(const SeqOf<T> &rhs)
    {
    doReplace(this->value->_logSz - 1, this->value->_logSz, rhs);
    }

template <class T>
inline void SeqOf<T>::append(const T &t)
    {
    if (this->value->isShared())
        this->value = new SeqValue<T>(this->value->_rep, this->value->_logSz);

    if (this->value->_logSz + 1 > this->value->_physSz)
        setLength(this->value->_logSz + 1); // setLength sets _logSz
    else
        this->value->_logSz++;              // we have to

    this->value->_rep[this->value->_logSz - 1] = t;
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::SeqOf()
//
// Constructor to make a sequence from an array.
//
// -- implementation ---------------------------------------------------------
// Call the initialize member function to initialize a representation array
// of the needed size.  Copy elements from the input array to the
// representation array.
// ---------------------------------------------------------------------------
template <class T>
SeqOf<T>::SeqOf(const T *vector, int sz)
    {
    this->value = new SeqValue<T>(vector, sz);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::~SeqOf()
//
// Class destructor.
//
// -- implementation ---------------------------------------------------------
// This class maintains an array allocated on the heap to hold the sequence
// representation.  If memory was allocated on the heap, it is returned by
// the destructor.  If the sequence is of length zero or one, the pointer
// to the array (_rep) will point to a member variable (_inSitu) instead of
// to an object on the heap.  In this case there is no dynamic memory to free.
// ---------------------------------------------------------------------------
template <class T>
SeqOf<T>::~SeqOf()
    {
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::replace()
//
// Replace a specified range of the sequence with a new range.
// Elements [lo]..[hi] (inclusive) of the sequence are replaced by the entire
// sequence provided as an argument.  In general, this will result in changing
// the length of the sequence, and elements above [hi] will have new indices.
//
// -- implementation ---------------------------------------------------------
// The implementation simply calls the private member function doReplace.
// The doReplace function is a bit different than replace(), because
// its indices are exclusive, rather than inclusive, so the implementation
// needs to adjust the indices before calling it.
// ---------------------------------------------------------------------------
template <class T>
inline void SeqOf<T>::replace(int lo, int hi, const SeqOf<T> &s)
    {
    if ( lo > hi )
        {
        logBug << "bad indices [" << lo << ',' << hi << "] in SeqOf.replace,"
          << " size=" << this->value->_logSz << std::endl;
        hi = lo;   // and stumble on
        }
    doReplace(checkIndex(lo)-1, checkIndex(hi)+1, s);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::find()
//
// Checks to see if the parameter t is in this SeqOf.  If it is found then
// this function returns its index.  If it is not found then this function
// returns -1.  The parameter startPos starts the search at the given index.
//
// ---------------------------------------------------------------------------
template <class T>
int SeqOf<T>::find(const T &t, int startPos) const
    {
    // If startPos is out of range then abort.
    if ((startPos < 0) || (startPos >= this->value->_logSz))
      return -1;

    // Try and find a match
    for (int i = startPos ; i < this->value->_logSz ; i++)
        if (this->value->_rep[i] == t)
            return i;

    // No match found
    return -1;
    }


// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::max()
//
// Returns the index of the greatest value in the SeqOf.  The index of the
// greatest value of an empty list is -1.
//
// ---------------------------------------------------------------------------
template <class T>
int SeqOf<T>::max(void) const
    {
    if (!length())
        return -1;
    T m = (*this)[0];
    int index = 0;
    for (int i = 1 ; i < length() ; i++)
        if ((*this)[i] > m)
            {
            m = (*this)[i];
            index = i;
            }
    return index;
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::min()
//
// Returns the index of the greatest value in the SeqOf.  The index of the
// greatest value of an empty list is -1.
//
// ---------------------------------------------------------------------------
template <class T>
int SeqOf<T>::min(void) const
    {
    if (!length())
        return -1;
    T m = (*this)[0];
    int index = 0;
    for (int i = 1 ; i < length() ; i++)
        if ((*this)[i] < m)
            {
            m = (*this)[i];
            index = i;
            }
    return index;
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::sort()
//
// Sorts the elements in this SeqOf<T>.  The supplied comparison function
// is used to compare each element.
//
// -- implementation ---------------------------------------------------------
// Sorts the elements using a shell sort.  This algorithm is based upon the
// shell sort presented in "Program Design with PASCAL" by Thomas L. Naps
// and Bhagat Singh.  A shell sort is of order O(n(log2(n)**2).  While this
// is not as fast as a quick sort it is not recursive and thus saves on
// stack space.
// ---------------------------------------------------------------------------
template <class T>
void SeqOf<T>::sort(Compar cmp)
    {
    if (this->value->isShared())
        this->value = new SeqValue<T>(this->value->_rep, this->value->_logSz);

    int k = this->value->_logSz / 2;

    while (k > 0)
        {
        for (int i = k ; i < this->value->_logSz ; i++)
            {
            int j = i - k;
            while (j >= 0)
                {
                if (cmp(this->value->_rep[j], this->value->_rep[j + k])
                  == GREATER)
                    {
                    T tmp = this->value->_rep[j];
                    this->value->_rep[j] = this->value->_rep[j + k];
                    this->value->_rep[j + k] = tmp;
                    j = j - k;
                    }
                else
                    j = -1;
                }
            }

        k = k / 2;
        }
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::remove()
//
// Remove a specified range of the sequence.
// Elements [lo]..[hi] (inclusive) of the sequence are deleted.
// In general, this will shorten the sequence, causing elements above [hi]
// to have new, lower, indices.
//
// -- implementation ---------------------------------------------------------
// The implementatin relies on the default constructor creating an empty
// sequence, and calls the more general replace function to replace the
// specified range with such an empty sequence.
// ---------------------------------------------------------------------------
template <class T>
inline void SeqOf<T>::remove(int lo, int hi)
    {
    replace(lo, hi, SeqOf<T>());    // checks done in replace()
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::insertBefore()
//
// Insert another sequence before the element at a given index.
// A sequence specified as a parameter is inserted into this sequence
// just before the element at index.  After the insertion, elements previously
// at or above [index] will have new indices.
//
// -- implementation ---------------------------------------------------------
// The private checkIndex function is called to ensure that the specified
// index is in range.
// The private doReplace function is called with appropriate (exclusive)
// indices, to replace the empty sequence between indices index-1 and index
// with the specified sequence to be inserted.
// ---------------------------------------------------------------------------
template <class T>
inline void SeqOf<T>::insertBefore(int index, const SeqOf<T> &s)
    {
    doReplace(index-1, checkIndex(index), s);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::insertAfter()
//
// Insert another sequence after the element at a given index.
// A sequence specified as a parameter is inserted into this sequence
// just after the element at index.  After the insertion, elements previously
// above [index] will have new indices.
//
// -- implementation ---------------------------------------------------------
// The private checkIndex function is called to ensure that the specified
// index is in range.
// The private doReplace function is called with appropriate (exclusive)
// indices, to replace the empty sequence between indices index and index+1
// with the specified sequence to be inserted.
// ---------------------------------------------------------------------------
template <class T>
inline void SeqOf<T>::insertAfter(int index, const SeqOf<T> &s)
    {
    doReplace(index, checkIndex(index)+1, s);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::setLength()
//
// Set the logical length of the sequence.
// If the new size is shorter than the old, elements beyond the new end
// of the sequence will be deleted.  If the new size is longer than the old,
// new elements will be created via their default constructor (or uninitalized
// if they are a built-in type).
//
// -- implementation ---------------------------------------------------------
// setLength() is intended to reduce reallocation thrashing, at the
// expense of some memory. Decreased logical size does not result in a
// reallocation unless the change in size is "large enough." When a new
// logical size exceeds the current physical size and therefore demands
// an allocation, it may be larger than needed, anticipating future
// growth.  Choose suitable <THRESHOLD,ALPHA> for suitable
// characteristics.
// ---------------------------------------------------------------------------
template <class T>
inline void SeqOf<T>::setLength(int newLogSz)
    {
    if (newLogSz == length())
        return;
    if (this->value->isShared())
        this->value = new SeqValue<T>(this->value->_rep, this->value->_logSz);

    this->value->setLength(newLogSz);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::extract() const
//
// Make a new sequence from copies of element from [lo]..[hi].
//
// -- implementation ---------------------------------------------------------
// After validating the input parameters, the constructor taking an array
// of the base type and a size is called, with an appropriate pointer into
// the representation array.  The offset and size for the constructor are
// first calculated from the input parameters.
// ---------------------------------------------------------------------------
template <class T>
inline SeqOf<T> SeqOf<T>::extract(int lo, int hi) const
    {
    if ( lo > hi )
        {
        logBug << "bad indices [" << lo << ',' << hi
          << "] in SeqOf.extract, size=" << this->value->_logSz << std::endl;
        hi = lo;      // and stumble on
        }
    lo = checkIndex(lo);
    return SeqOf<T>(this->value->_rep+lo, checkIndex(hi)-lo+1);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::operator==() const
//
// Return true if each element of each sequence has the same value.
//
// -- implementation ---------------------------------------------------------
// If the two sequences have different sizes, they can't be equal.
// Otherwise, if any pair of elements at the same index in the two sequences
// differ, the sequences aren't equal.
// ---------------------------------------------------------------------------
template <class T>
bool SeqOf<T>::operator==(const SeqOf<T> &rhs) const
    {
    if (&this->value == &rhs.value)
        return true;

    if (this->value->_logSz != rhs.value->_logSz )
        return false;

    for (int i = this->value->_logSz-1; i >= 0; --i )
        if (this->value->_rep[i] != rhs.value->_rep[i] )
            return false;
    return true;
    }

// -- public -----------------------------------------------------------------
// SeqOf<T>::printOn()
//
// Print the sequence to an ostream.
//
// ---------------------------------------------------------------------------
template <class T>
std::ostream &SeqOf<T>::printOn(std::ostream &os) const
    {
    os << "[";
    for (int i = 0; i < this->value->_logSz; ++i )
        {
        os << this->value->_rep[i];
        if (i != this->value->_logSz - 1)
            os << ", ";
        }
    return os << " ]";
    }

// -- private ----------------------------------------------------------------
// template <class T>
// SeqOf<T>::SeqOf()
//
// Constructor to make a new sequence by concatenating its two arguments.
//
// -- implementation ---------------------------------------------------------
// Sufficient storage for the concatenated sequence is allocated via
// the initialize member function.  Elements are copied in order from the
// two input sequences to the new representation array.
// ---------------------------------------------------------------------------
template <class T>
SeqOf<T>::SeqOf(const SeqOf<T> *lhs, const SeqOf<T> *rhs)
    {
    this->value = new SeqValue<T>(lhs->value->_rep, lhs->value->_logSz);
    setLength(lhs->value->_logSz + rhs->value->_logSz);
    for (int i = lhs->value->_logSz ;
      i < lhs->value->_logSz + rhs->value->_logSz ; i++)
        this->value->_rep[i] = rhs->value->_rep[i - lhs->value->_logSz];
    }

// -- private ----------------------------------------------------------------
// template <class T>
// SeqOf<T>::doReplace()
//
// doReplace(below, above, seq) replaces the subsequence
// [below+1]..[above-1] by seq, where below>=-1 and above<=logSz.
// The indexing scheme differs from the public functions, making
// it more expressive. E.g., prepend is doReplace(-1, 0, s). */
//
// -- implementation ---------------------------------------------------------
// Special cases to consider:
// Source and destinations sequences might be the same.
// Source might be longer than the range being replaced.
// Source might be shorter than the range being replaced.
// ---------------------------------------------------------------------------
template <class T>
void SeqOf<T>::doReplace(int below, int above, const SeqOf<T> &seq)
    {
    if (this->value->isShared())
        this->value = new SeqValue<T>(this->value->_rep, this->value->_logSz);

    // below and above are verified by each caller

    // avoid alias
    SeqOf<T> *source = (this==&seq) ? new SeqOf<T>(seq) : (SeqOf<T> *)&seq;
    const int destSz = above-below-1;   // destination size
    const int srcSz = source->value->_logSz;    // source size
    const int delta = srcSz-destSz;     // logical growth
    if (delta<0 )
        {
        // fill hole
        for( int to = above+delta; to-delta < this->value->_logSz; ++to )
            this->value->_rep[to] = this->value->_rep[to-delta];
        setLength(this->value->_logSz+delta);
        }
    else if (delta>0 )
        {
        setLength(this->value->_logSz+delta);
        // make hole
        for (int to = this->value->_logSz-1; to-delta >= above; --to )
            this->value->_rep[to] = this->value->_rep[to-delta];
        }
    for (int i = 0; i < srcSz; ++i )
        this->value->_rep[++below] = source->value->_rep[i];
    if (source != &seq )
        delete source;
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::notInList()
//
// Returns a list of entries that are in this list, but not in the other
// list.
//
// For example, if this list contains 1, 2, 3, 4.  And the otherList contains
// 1, 3, 5, 7, then (*this)notInList(otherList) would return 2, 4.  And
// otherList.notInList(*this) would return 5, 7.
// ---------------------------------------------------------------------------
template <class T>
SeqOf<T> SeqOf<T>::notInList(const SeqOf<T>& otherList) const
    {
    SeqOf<T> rval;  // to be returned

    // check each entry in this list for a match in otherList
    // if no match then add it to the return list.
    for (int i = 0 ; i < length() ; i++)
        if (otherList.find((*this)[i]) == -1)
            rval.append((*this)[i]);

    return rval;
    }

// -- public -----------------------------------------------------------------
// template <class T>
// SeqOf<T>::in()
//
// Returns a list of entries that are in this sequence and the given
// sequence.
// ---------------------------------------------------------------------------
template <class T>
SeqOf<T> SeqOf<T>::in(const SeqOf<T>& otherList) const
    {
    SeqOf<T> rval;  // to be returned

    // check each entry in this list
    // for a match in otherList
    // if a match then add it to the return list.
    for (int i = 0 ; i < length() ; i++)
        if (otherList.find((*this)[i]) != -1)
            rval.append((*this)[i]);

    return rval;
    }

// -- public -----------------------------------------------------------------
// template <class T>
// template <class Iterator>
// SeqOf<T>::assign()
//
// Assigns elements from beg to end into this SeqOf.
// This function acts the same as the assign() method
// of STL containers.  In particular, it assumes that
// this SeqOf already has a size sufficient to hold
// all the elements.  So, setLength should be called
// ahead of time to avoid logBugs from checkIndex.
// ---------------------------------------------------------------------------
template <class T>
template <class Iterator>
void SeqOf<T>::assign(Iterator beg, Iterator end)
    {
    if (this->value->isShared())
        this->value = new SeqValue<T>(this->value->_rep,
          this->value->_logSz);
    int i = 0;
    while (beg != end)
        this->value->_rep[checkIndex(i++)] = *(beg++);
    }

template <class T>
void SeqOf<T>::reportIndexError(int i) const
    {
    logBug << "index [" << i << "] out of range in SeqOf.checkIndex,"
      << " size=" << this->value->_logSz << std::endl;
    }

#endif
