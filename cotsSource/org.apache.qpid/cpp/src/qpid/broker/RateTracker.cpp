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
#include "qpid/broker/RateTracker.h"

using qpid::sys::AbsTime;
using qpid::sys::Duration;
using qpid::sys::TIME_SEC;

namespace qpid {
namespace broker {

RateTracker::RateTracker() : currentCount(0), lastCount(0), lastTime(AbsTime::now()) {}

RateTracker& RateTracker::operator++()
{
    ++currentCount;
    return *this;
}

double RateTracker::sampleRatePerSecond()
{
    int32_t increment = currentCount - lastCount;    
    AbsTime now = AbsTime::now();
    Duration interval(lastTime, now);
    lastCount = currentCount;
    lastTime = now;
    //if sampling at higher frequency than supported, will just return the number of increments
    if (interval < TIME_SEC) return increment;
    else if (increment == 0) return 0;
    else return increment / (interval / TIME_SEC);
}

}} // namespace qpid::broker
