// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// QueueImpl.H
// An implementation of a queue class.
//
// Author: David H. Leserman
// ---------------------------------------------------------------------------

// -- module -----------------------------------------------------------------
//
// The queue is a first-in first-out data structure.
//
// -- implementation ---------------------------------------------------------
//
// Implemented as a Dictionary with an unsigned long as key and type T
// as the value.  If type T is a pointer, then the specialized
// Dictionary for pointer values is used.  _head is the key for the
// first-in entry.  _tail is the key for the last-in entry.  _head
// chases _tail as items are deleted.  _tail chases _head as items are
// inserted.  The unsigned long key (including _head and _tail) will
// wrap from maximum value to zero, forming a virtual circular
// structure.
//
// ---------------------------------------------------------------------------

#ifndef _QueueImpl_H
#define _QueueImpl_H

#ifdef IDENT_H
static const char* const QueueImpl_H_Id =
  "$Id: .QueueImpl.H__temp27950,v 1.2 2003/05/06 23:11:47 fluke Exp $";
#endif

#include "commonDefs.h"
#include "Dict.H"
#include "LogStream.H"

template <class T, class DictType>
class QueueImpl // not a base class
    {
    public:
        // Uses default copy c'tor, destructor, & op=
        
        void insert (const T& item);
        T remove (void);
        const T & peek(void) const;

        const T &operator[](int i) const { return _repr.value(i); }
        unsigned long length (void) const { return _repr.size(); }
        bool isEmpty(void) const { return (_head==_tail); }

    public:
        QueueImpl() : _head(0), _tail(0) {}
        
        unsigned long _head;
        unsigned long _tail;
        DictType _repr;  
        bool gotItem(T& item) const;
    };


// -- public -----------------------------------------------------------------
// template <class T, class DictType>
// T QueueImpl<T, DictType>::remove (void)

// Return the item at the head of the queue, removing it from the queue.
// ---------------------------------------------------------------------------
template <class T, class DictType>
T QueueImpl<T, DictType>::remove (void) 
    {
    T item;
    if (gotItem(item))
        {
        _repr.remove(_head);
        _head += 1;
        }
    return item;
    }

// -- public -----------------------------------------------------------------
// template <class T, class DictType>
// void QueueImpl<T, DictType>::insert(const T& item)

// Add an item to the tail of the queue.
// ---------------------------------------------------------------------------
template <class T, class DictType>
void QueueImpl<T, DictType>::insert(const T& item) 
    {
    if ((_tail+1) == _head)
        {
        logBug << "Queue would overflow.  Item not added."
               << std::endl;
        }
    else 
        {
        _repr.add(_tail, item);
        _tail += 1;
        }
    }

// -- public -----------------------------------------------------------------
// template <class T, class DictType>
// const T &  QueueImpl<T, DictType>::peek(void) const

// Return the item at the head of the queue, but don't remove it.
// ---------------------------------------------------------------------------
template <class T, class DictType>
const T & QueueImpl<T, DictType>::peek(void) const
    {
    static T item;
    gotItem(item);
    return item;
    }

// -- private ----------------------------------------------------------------
// template <class T, class DictType>
// bool QueueImpl<T, DictType>::gotItem(T& item) const

// Returns true if an item was retrieved from the dictionary,
// otherwise returns false.
// ---------------------------------------------------------------------------
template <class T, class DictType>
bool QueueImpl<T, DictType>::gotItem(T& item) const
    {
    if (isEmpty())
        {
        logBug << "Attempt to remove from or peek at an empty queue."
               << std::endl;
        return false; // RETURN
        }
    else // not empty
        {
        bool success = _repr.map(_head,item);
        if (!success)
            {
            logBug << "Failure to extract item from representation."
                   << "_head = " << _head << std::endl;
            return false; // RETURN
            }
        }
    return true; // success
    }

#endif
