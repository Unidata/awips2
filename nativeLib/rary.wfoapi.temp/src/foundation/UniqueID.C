// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// UniqueID.C
//
// Implementation of a class that encapsulates an ID that is
// guaranteed to be unique within a process, across process
// boundaries, and across host boundaries.  Objects of this class can
// serve as dictionary keys.
//
// Author: Sean Kelly and Gerry Murray
// ---------------------------------------------------------------------------
#ifdef IDENT_C
static const char* const UniqueID_C_Id =
"$Id: .UniqueID.C__temp27950,v 1.2 2003/05/06 23:11:54 fluke Exp $";
#endif


#include "LogStream.H"
#include "UniqueID.H"
#include <sys/socket.h>    // for gethostbyname system call.
#include <netinet/in.h>
#include <netdb.h>

// -- public -----------------------------------------------------------------
// UniqueID::UniqueID ()
//
// Constructor that initializes the data members in such a way that no
// other object of this class, (even ones instantiated on different
// processes and different hosts) will have the same three id
// components.
//
// -- implementation ----------------------------------------------------------
//
// The host name is obtained only when the first object of this class
// is created.  Repeated use of the two system calls, "gethostname"
// and "gethostbyname" might be expensive, and the host should not
// change during the life of the process anyways.  The local id
// (_uniqueWithinProcessID) is the value of the counter of the number
// of executions of this constructor.
//
// ---------------------------------------------------------------------------

UniqueID::UniqueID (void)
    {
    // Cache the ID of the process that is running this object.
    _processID = getpid();

    // The host ID is the internet address of the machine that is
    // running this process.  The id is obtained from UNIX only once,
    // since this ID will not change during the life of the process.
    static unsigned long hostID = 0;
    if (!hostID)
        {
        char hostName[1024];
        if (gethostname (hostName, 1024))
            {
            logProblem << "Can not obtain the name of the host machine. "
                       << "IDs will not be unique across platforms."
                       << std::endl;
            hostID = 1;
            }
        else
            {
            hostent *hostInfo = gethostbyname(hostName);
            if (!hostInfo)
                {
                logProblem << "Can not obtain the name of the host machine. "
                           << "IDs will not be unique across platforms."
                           << std::endl;
                hostID = 1;
                }
            else
                {
                unsigned long *hostIDptr =
                          (unsigned long *) hostInfo->h_addr_list[0];
                hostID = *hostIDptr;
                }
            }
        }
    _hostID = hostID;

    // The id within the process is equal to the number of times that
    // this constructor has been executed during the life of the
    // process.
    static unsigned long uniqueWithinProcessID = 1;
    _uniqueWithinProcessID = uniqueWithinProcessID++;
    }

// -- global -----------------------------------------------------------------
// operator<<()
//
// Ostream operator for stream display of unique IDs.
// ---------------------------------------------------------------------------
std::ostream& operator<<(std::ostream& o, UniqueID id)
    {
    return o << "(0x" << std::hex << id.hostID() << ", "
             << std::dec << id.processID() << ", "
             << std::dec << id.uniqueWithinProcessID() << ')';
    }
