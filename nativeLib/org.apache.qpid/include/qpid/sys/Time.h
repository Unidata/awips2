#ifndef _sys_Time_h
#define _sys_Time_h

/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */

#include "qpid/sys/IntegerTypes.h"
/*
 * The platform defines its notion of time as a TimePrivate type. The
 * platform's implementation knows how to handle this type.
 */
#if defined (_WIN32)
#  include "windows/Time.h"
#else
#  include "posix/Time.h"
#endif

#include "qpid/CommonImportExport.h"

#include <limits>
#include <iosfwd>

namespace qpid {
namespace sys {

class Duration;

/**
 * @class AbsTime
 *
 * Class to represent an instant in time.
 *
 * The time resolution is in nanosecs, and this is held with 64 bits
 * giving a total time span from about 25 million years ago to 25 million
 * years hence. As an aside the internal time can sensibly be negative
 * meaning before the epoch (probably 1/1/1970 although this class doesn't
 * care).
 *
 * The AbsTime class is a value class and so you don't need to add any
 * accessors to its internal state. If you think you want to replace its value,
 * you need to construct a new AbsTime and assign it, viz:
 *
 *  AbsTime when = now();
 *  ...
 *  when = AbsTime(when, 2*TIME_SEC); // Advance timer 2 secs
 *
 * AbsTime is not intended to be used to represent calendar dates/times
 * but you can construct a Duration since the Unix Epoch, 1970-1-1-00:00,
 * so that you can convert to a date/time if needed:
 *
 *  int64_t nanosec_since_epoch = Duration(EPOCH, now());
 *
 * There are some sensible operations that are currently missing from
 * AbsTime, but nearly all that's needed can be done with a mixture of
 * AbsTimes and Durations.
 *
 * For example, convenience operators to add a Duration and AbsTime returning
 * an AbsTime would fit here (although you can already perform the operation
 * with one of the AbsTime constructors). However trying to add 2 AbsTimes
 * doesn't make sense.
 */
class AbsTime {
    friend class Duration;
    friend class Condition;

    TimePrivate timepoint;

public:

    inline AbsTime() : timepoint() {}
    QPID_COMMON_EXTERN AbsTime(const AbsTime& time0, const Duration& duration);
    // Default assignment operation fine
    // Default copy constructor fine

    QPID_COMMON_EXTERN static AbsTime now();
    QPID_COMMON_EXTERN static AbsTime FarFuture();
    QPID_COMMON_EXTERN static AbsTime Epoch();

    bool operator==(const AbsTime& t) const { return t.timepoint == timepoint; }

    friend bool operator<(const AbsTime& a, const AbsTime& b);
    friend bool operator>(const AbsTime& a, const AbsTime& b);
    QPID_COMMON_EXTERN friend std::ostream& operator << (std::ostream&, const AbsTime&);
};

QPID_COMMON_EXTERN std::ostream& operator << (std::ostream&, const AbsTime&);

/**
 * @class Duration
 * Class to represent the duration between instants of time.
 *
 * As AbsTime, this class also uses nanosecs for its time
 * resolution where possible. For the most part a duration can be dealt
 * with like a 64 bit integer, and indeed there is an implicit conversion which
 * makes this quite convenient.
 */
class Duration {
    static int64_t max() { return std::numeric_limits<int64_t>::max(); }
    int64_t nanosecs;

    friend class AbsTime;

public:
    QPID_COMMON_EXTERN inline Duration(int64_t time0 = 0);
    QPID_COMMON_EXTERN explicit Duration(const AbsTime& start, const AbsTime& finish);
    inline operator int64_t() const;
};

std::ostream& operator << (std::ostream&, const Duration&);

inline AbsTime now() { return AbsTime::now(); }

inline bool operator<(const AbsTime& a, const AbsTime& b)
{ return a.timepoint < b.timepoint; }
inline bool operator>(const AbsTime& a, const AbsTime& b)
{ return a.timepoint > b.timepoint; }

Duration::Duration(int64_t time0) :
    nanosecs(time0)
{}

Duration::operator int64_t() const
{ return nanosecs; }

/** Nanoseconds per second. */
const Duration TIME_SEC  = 1000*1000*1000;
/** Nanoseconds per millisecond */
const Duration TIME_MSEC = 1000*1000;
/** Nanoseconds per microseconds. */
const Duration TIME_USEC = 1000;
/** Nanoseconds per nanosecond. */
const Duration TIME_NSEC = 1;

/** Value to represent an infinite timeout */
const Duration TIME_INFINITE = std::numeric_limits<int64_t>::max();

/** Absolute time point for the Unix epoch: 1970-01-01T00:00:00 */
const AbsTime EPOCH = AbsTime::Epoch();

/** Time greater than any other time */
const AbsTime FAR_FUTURE = AbsTime::FarFuture();

/** Portable sleep for a number of seconds */
QPID_COMMON_EXTERN void sleep(int secs);

/** Portable sleep for a number of microseconds */
QPID_COMMON_EXTERN void usleep(uint64_t usecs);

/** Output formatted date/time for now*/
void outputFormattedNow(std::ostream&);

}}

#endif  /*!_sys_Time_h*/
