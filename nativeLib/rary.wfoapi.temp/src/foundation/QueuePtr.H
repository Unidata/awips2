// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// QueuePtr.H
// A queue class whose data is actually pointers to data.
//
// Author: David H. Leserman
// ---------------------------------------------------------------------------

// -- module -----------------------------------------------------------------
// The queue is a first-in first-out data structure that holds pointers to data.
// Important:  The template argument must be a pointer!!!

// -- implementation ---------------------------------------------------------
// Implemented completely inline with a QueueImpl object doing the
// real work.  The QueueImpl object is instantiated with a Dict class
// instantiated with an unsigned long as key and type T as a pointer to a value.
// ---------------------------------------------------------------------------

#ifndef _QueuePtr_H
#define _QueuePtr_H

#ifdef IDENT_TEMPLATE_H
static const char* const QueuePtr_H_Id =
  "$Id$";
#endif

#include "DictKP.H"
#include "QueueImpl.H"

template <class T>
class QueuePtr // not a base class
    {
    public:
        QueuePtr(void) {}
    
        // Uses default copy c'tor, destructor, & op=
        
        void insert (const T& item) {_repr.insert (item);}
        T remove (void) {return _repr.remove ();}
        const T & peek (void) const {return _repr.peek ();}

        const T &operator[](int i) const { return _repr[i]; }
        unsigned long length (void) const { return _repr.length (); }
        bool isEmpty (void) const { return _repr.isEmpty (); }
        
    private:
        QueueImpl <T, DictKP <unsigned long, T> > _repr;  // not a pointer
    };

//
// Inline function definitions
//

#ifdef COMPILER_INLINES_TEMPLATES_WELL

//
// The following inline definitions have been moved above to the class
// declaration due to a compiler limitation which prevents inline
// function definitions of template member functions from being
// implemented inline unless they are explicitly defined in the class
// definition.
//

// Note that the function definitions below are commented out by the #ifdef 
// a few lines above.
//

// -- public -----------------------------------------------------------------
// template <class T>
// QueuePtr<T>::QueuePtr(void)

// Null Constructor.
// ---------------------------------------------------------------------------
template <class T>
QueuePtr<T>::QueuePtr(void) const 
    {
    // Defined above in class declaration due to compiler limitation 
    // in defining inline member functions within class templates.
    }

// -- public -----------------------------------------------------------------
// template <class T>
// void QueuePtr<T>::insert (const T& item)

// Add an item to the tail of the queue.
// ---------------------------------------------------------------------------
template <class T>
void QueuePtr<T>::insert (const T& item)
    {
    // Defined above in class declaration due to compiler limitation 
    // in defining inline member functions within class templates.
    }

// -- public -----------------------------------------------------------------
// template <class T>
// T QueuePtr<T>::remove (void)

// Return the item at the head of the queue, removing it from the queue.
// ---------------------------------------------------------------------------
template <class T>
T QueuePtr<T>::remove (void)
    {
    // Defined above in class declaration due to compiler limitation 
    // in defining inline member functions within class templates.
    }

// -- public -----------------------------------------------------------------
// template <class T>
// const T &  QueuePtr<T>::peek (void) const

// Return the item at the head of the queue, but don't remove it.
// ---------------------------------------------------------------------------
template <class T>
const T & QueuePtr<T>::peek (void) const
    {
    // Defined above in class declaration due to compiler limitation 
    // in defining inline member functions within class templates.
    }

// -- public -----------------------------------------------------------------
// template <class T>
// unsigned long QueuePtr<T>::length(void)

// Returns the number of items that are in the queue.
// ---------------------------------------------------------------------------
template <class T>
unsigned long QueuePtr<T>::length(void) const 
    {
    // Defined above in class declaration due to compiler limitation 
    // in defining inline member functions within class templates.
    }

// -- public -----------------------------------------------------------------
// template <class T>
// bool QueuePtr<T>::isEmpty(void)

// Returns true if there are no items in the queue, otherwise returns
// false.
// ---------------------------------------------------------------------------
template <class T>
bool QueuePtr<T>::isEmpty(void) const 
    {
    // Defined above in class declaration due to compiler limitation 
    // in defining inline member functions within class templates.
    }

#endif

//
// The above #endif ends the block of "commented-out" function definitions,
// here for documentation purposes.  This block started with the
// #ifdef COMPILER_INLINES_TEMPLATES_WELL well back in the file.
//

#endif
