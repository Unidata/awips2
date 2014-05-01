#ifndef QPID_BROKER_RATETRACKER_H
#define QPID_BROKER_RATETRACKER_H

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

namespace qpid {
namespace broker {

/**
 * Simple rate tracker: represents some value that can be incremented,
 * then can periodcially sample the rate of increments.
 */
class RateTracker
{
  public:
    RateTracker();
    /**
     * Increments the count being tracked. Can be called concurrently
     * with other calls to this operator as well as with calls to
     * sampleRatePerSecond().
     */
    RateTracker& operator++();
    /**
     * Returns the rate of increments per second since last
     * called. Calls to this method should be serialised, but can be
     * called concurrently with the increment operator
     */
    double sampleRatePerSecond();
  private:
    volatile int32_t currentCount;
    int32_t lastCount;
    qpid::sys::AbsTime lastTime;
};
}} // namespace qpid::broker

#endif  /*!QPID_BROKER_RATETRACKER_H*/
