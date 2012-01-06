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

#include "qpid/sys/Time.h"
#include <ostream>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread/thread_time.hpp>
#include <windows.h>

using namespace boost::posix_time;

namespace qpid {
namespace sys {

AbsTime::AbsTime(const AbsTime& t, const Duration& d) {
    if (d == Duration::max()) {
        timepoint = ptime(max_date_time);
    }
    else {
        time_duration td = microseconds(d.nanosecs / 1000);
        timepoint = t.timepoint + td;
    }
}

AbsTime AbsTime::FarFuture() {
    AbsTime ff;
    ptime maxd(max_date_time);
    ff.timepoint = maxd;
    return ff;
}

AbsTime AbsTime::now() {
    AbsTime time_now;
    time_now.timepoint = boost::get_system_time();
    return time_now;
}

Duration::Duration(const AbsTime& time0) : nanosecs(0) {
    time_period p(ptime(min_date_time), time0.timepoint);
    nanosecs = p.length().total_nanoseconds();
}

Duration::Duration(const AbsTime& start, const AbsTime& finish) {
    time_duration d = finish.timepoint - start.timepoint;
    nanosecs = d.total_nanoseconds();
}

std::ostream& operator<<(std::ostream& o, const Duration& d) {
    return o << int64_t(d) << "ns";   
}

std::ostream& operator<<(std::ostream& o, const AbsTime& t) {
    std::string time_string = to_simple_string(t.timepoint);
    return o << time_string;
}


void toPtime(ptime& pt, const AbsTime& t) {
    pt = t.getPrivate();
}

void sleep(int secs) {
    ::Sleep(secs * 1000);
}

void usleep(uint64_t usecs) {
    DWORD msecs = usecs / 1000;
    if (msecs == 0)
        msecs = 1;
    ::Sleep(msecs);
}

void outputFormattedNow(std::ostream& o) {
    ::time_t rawtime;
    ::tm timeinfo;
    char time_string[100];

    ::time( &rawtime );
    ::localtime_s(&timeinfo, &rawtime);
    ::strftime(time_string, 100,
               "%Y-%m-%d %H:%M:%S",
               &timeinfo);
    o << time_string << " ";
}
}}
