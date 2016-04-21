// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// RCPtr.H
// Template class for smart pointers-to-T objects.
// T must support the RCObject interface.
// ---------------------------------------------------------------------------
#ifndef RCPtr_H
#define RCPtr_H

#include "commonDefs.h"

template<class T>
class RCPtr
    {
    public:
        RCPtr(T* realPtr = 0);
        RCPtr(const RCPtr& rhs) : pointee(rhs.pointee) { init(); }
        ~RCPtr() FORCE_INLINE;
        RCPtr& operator=(const RCPtr& rhs) FORCE_INLINE;
        T* operator->() const;
        T& operator*() const;

    private:
        T *pointee;
        void init() FORCE_INLINE;
    };

// -- private ----------------------------------------------------------------
// RCPtr::init()
//
// Utility initialization method.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
template<class T>
inline void RCPtr<T>::init()
    {
    if (pointee == 0)
        return;

    if (pointee->isShareable() == false)
        pointee = new T(*pointee);

    pointee->addReference();
    }

template <class T>
inline RCPtr<T> &RCPtr<T>::operator=(const RCPtr& rhs)
    {
    if (pointee != rhs.pointee)
        {
        if (pointee)
            pointee->removeReference();

        pointee = rhs.pointee;
        init();
        }

    return *this;
    }

// -- public -----------------------------------------------------------------
// RCPtr::RCPtr()
//
// Constructor for an RCPtr.  Takes a pointer to the "real" data type.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
template<class T>
inline RCPtr<T>::RCPtr(T* realPtr) : pointee(realPtr)
    {
    init();
    }

// -- public -----------------------------------------------------------------
// RCPtr::~RCPtr()
//
// Destructor.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
template<class T>
inline RCPtr<T>::~RCPtr()
    {
    if (pointee)
        pointee->removeReference();
    }

// -- public -----------------------------------------------------------------
// RCPtr::operator->()
//
// Dereference operator for RCPtr.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
template<class T>
inline T* RCPtr<T>::operator->() const
    {
    return pointee;
    }

// -- public -----------------------------------------------------------------
// RCPtr::operator->()
//
// Dereference operator for RCPtr.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
template<class T>
inline T& RCPtr<T>::operator*() const
    {
    return *pointee;
    }

#endif
