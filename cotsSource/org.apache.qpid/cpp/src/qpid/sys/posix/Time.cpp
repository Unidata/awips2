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

#include "qpid/sys/posix/PrivatePosix.h"

#include "qpid/sys/Time.h"
#include <ostream>
#include <time.h>
#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>

namespace {
int64_t max_abstime() { return std::numeric_limits<int64_t>::max(); }
}

namespace qpid {
namespace sys {

AbsTime::AbsTime(const AbsTime& t, const Duration& d) :
    timepoint(d == Duration::max() ? max_abstime() : t.timepoint+d.nanosecs)
{}

AbsTime AbsTime::FarFuture() {
    AbsTime ff; ff.timepoint = max_abstime(); return ff;
}

AbsTime AbsTime::now() {
    struct timespec ts;
    ::clock_gettime(CLOCK_REALTIME, &ts);
    AbsTime time_now;
    time_now.timepoint = toTime(ts).nanosecs;
    return time_now;
}

Duration::Duration(const AbsTime& time0) :
    nanosecs(time0.timepoint)
{}

Duration::Duration(const AbsTime& start, const AbsTime& finish) :
    nanosecs(finish.timepoint - start.timepoint)
{}

struct timespec& toTimespec(struct timespec& ts, const Duration& t) {
    ts.tv_sec  = t / TIME_SEC;
    ts.tv_nsec = t % TIME_SEC;
    return ts; 
}

struct timeval& toTimeval(struct timeval& tv, const Duration& t) {
    tv.tv_sec = t/TIME_SEC;
    tv.tv_usec = (t%TIME_SEC)/TIME_USEC;
    return tv;
}

Duration toTime(const struct timespec& ts) {
    return ts.tv_sec*TIME_SEC + ts.tv_nsec;
}

std::ostream& operator<<(std::ostream& o, const Duration& d) {
    return o << int64_t(d) << "ns";   
}

namespace {
inline std::ostream& outputFormattedTime(std::ostream& o, const ::time_t* time) {
    ::tm timeinfo;
    char time_string[100];
    ::strftime(time_string, 100,
               "%Y-%m-%d %H:%M:%S",
               localtime_r(time, &timeinfo));
    return o << time_string;
}
}

std::ostream& operator<<(std::ostream& o, const AbsTime& t) {
    ::time_t rawtime(t.timepoint/TIME_SEC);
    return outputFormattedTime(o, &rawtime);
}

void outputFormattedNow(std::ostream& o) {
    ::time_t rawtime;
    ::time(&rawtime);
    outputFormattedTime(o, &rawtime);
    o << " ";
}

void sleep(int secs) {
    ::sleep(secs);
}

void usleep(uint64_t usecs) {
    ::usleep(usecs);
}

}}
