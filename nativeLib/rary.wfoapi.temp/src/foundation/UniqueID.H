// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// UniqueID.H
//
// Class that encapsulates an identifier that is guaranteed to be
// unique within a process, across process boundaries, and across host
// boundaries.  Objects of this class can serve as dictionary keys.
//
// Author:  Sean Kelly and Gerry Murray
//-----------------------------------------------------------------------------

#ifndef _UniqueID_H
#define _UniqueID_H

#ifdef IDENT_H
static const char* const UniqueID_H_Id =
"$Id: .UniqueID.H__temp27950,v 1.2 2003/05/06 23:11:54 fluke Exp $";
#endif

#include "commonDefs.h"

#include <iostream>
#include <unistd.h>       // for pid_t
#include <sys/socket.h>    // for gethostbyname system call.
#include <netinet/in.h>
#include <netdb.h>


class UniqueID
    {
        friend byte* serialize(const UniqueID& arg, byte* addr);
        friend long serialLength(const UniqueID& arg);
        friend byte* quantize(UniqueID* arg, byte* addr);
    public:
        // Object Management.
        UniqueID (void);
        // uses compiler-generated d'tor, copy c'tor, & op=

        // Dictionary key functions.
        // The following are needed so that this can be a Dict key
        bool operator== (const UniqueID &rhs) const;
        bool operator!= (const UniqueID &rhs) const;
        bool operator<  (const UniqueID &rhs) const;
        operator unsigned() const;

        // Primarily for ostream::operator<<
        unsigned long hostID() const { return _hostID; }
        unsigned long uniqueWithinProcessID() const
            { return _uniqueWithinProcessID; }
        pid_t processID() const { return _processID; }

    private:
        unsigned long _hostID, _uniqueWithinProcessID;
        pid_t  _processID;
    };

std::ostream& operator<<(std::ostream& o, UniqueID id);

//
// Inline function definitions
//

// -- public -----------------------------------------------------------------
// bool :UniqueID:operator==(const DepictTupleKey &rhs) const
//
// Returns true if all three id components from the specified Unique
// ID match the corresponding components from this object.
// Otherwise returns false.
//
// ---------------------------------------------------------------------------

inline bool UniqueID::operator==(const UniqueID &rhs) const
    {
    return (_hostID == rhs._hostID
            && _processID == rhs._processID
            && _uniqueWithinProcessID == rhs._uniqueWithinProcessID);
    }

// -- public -----------------------------------------------------------------
// bool UniqueID::operator!= (const UniqueID &rhs) const
//
// Returns true if one of the three id components from the specified
// Unique Identifier do not match the corresponding components from
// this object.  Otherwise returns false.
//
// ---------------------------------------------------------------------------

inline bool UniqueID::operator!= (const UniqueID &rhs) const
    {
    return !operator==(rhs);
    }

// -- public -----------------------------------------------------------------
// bool UniqueID::operator< (const UniqueID &rhs) const
//
// Returns true if the host ID member of this object is less than the
// hostID of the specified object.  If the hostIDs are equal, then the
// processID's are compared.  If the processIDs are equal, then the
// _unique within process ID's are compared.
//
// ---------------------------------------------------------------------------

inline bool UniqueID::operator< (const UniqueID &rhs) const
    {
    if (_hostID != rhs._hostID)
        return _hostID < rhs._hostID;
    else if (_processID != rhs._processID)
        return _processID < rhs._processID;
    else
        return _uniqueWithinProcessID < rhs._uniqueWithinProcessID;
    }

// -- public -----------------------------------------------------------------
// UniqueID::operator unsigned() const
//
// Converts this object to an unsigned int by exclusive or-ing the
// three ID components.  The Dictionary needs this conversion operator
// in order to do hashing.  The return value does not need to be unique.
//
// ---------------------------------------------------------------------------

inline UniqueID::operator unsigned() const
    {
    return _hostID ^ _uniqueWithinProcessID ^ _processID;
    }

#endif
