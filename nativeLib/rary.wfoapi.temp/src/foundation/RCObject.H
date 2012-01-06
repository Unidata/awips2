// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// RCObject.H
// Base class for reference-counted objects
// ---------------------------------------------------------------------------
#ifndef RCObject_H
#define RCObject_H

#ifdef IDENT_TEMPLATE_H
static const char* const RCObject_H_Id =
"$Id: .RCObject.H__temp27950,v 1.2 2003/05/06 23:11:48 fluke Exp $";
#endif

#include "commonDefs.h"

class RCObject
    {
    public:
        void addReference();
        void removeReference();
        void markUnshareable();
        bool isShareable() const;
        bool isShared() const;

    protected:
        RCObject();
        RCObject(const RCObject& rhs);
        RCObject& operator=(const RCObject& rhs);
        virtual ~RCObject();

    private:
        int refCount;
        bool shareable;
    };


// -- protected --------------------------------------------------------------
// RCObject::RCObject()
//
// Constructor for an RCObject.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline RCObject::RCObject() : refCount(0), shareable(true)
    {
    }

// -- protected --------------------------------------------------------------
// RCObject::RCObject()
//
// Copy constructor for an RCObject.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline RCObject::RCObject(const RCObject&) : refCount(0), shareable(true)
    {
    }

// -- protected --------------------------------------------------------------
// RCObject::RCObject()
//
// Destructor for an RCObject.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline RCObject::~RCObject()
    {
    }

// -- public -----------------------------------------------------------------
// RCObject::removeReference()
//
// Decrements the objects reference count.  If the reference count falls to
// zero then kills this object by calling delete.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline void RCObject::removeReference()
    {
    if (--refCount == 0)
        delete this;
    }

// -- protected --------------------------------------------------------------
// RCObject::operator=()
//
// Assignment operator for a reference counted object.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline RCObject& RCObject::operator=(const RCObject&)
    {
    return *this;
    }

// -- public -----------------------------------------------------------------
// RCObject::addReference()
//
// Increments the reference count.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline void RCObject::addReference()
    {
    ++refCount;
    }

// -- public -----------------------------------------------------------------
// RCObject::markUnshareable()
//
// Sets this object so that it will not be shared.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline void RCObject::markUnshareable()
    {
    shareable = false;
    }

// -- public -----------------------------------------------------------------
// RCObject::isShareable()
//
// Returns true if this object can be shared.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool RCObject::isShareable() const
    {
    return shareable;
    }

// -- public -----------------------------------------------------------------
// RCObject::isShared()
//
// Returns true if this object is being shared.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool RCObject::isShared() const
    {
    return refCount > 1;
    }

#endif
