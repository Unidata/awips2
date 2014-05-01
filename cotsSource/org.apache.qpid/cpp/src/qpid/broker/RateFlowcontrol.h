#ifndef broker_RateFlowcontrol_h
#define broker_RateFlowcontrol_h

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
#include "qpid/sys/IntegerTypes.h"

#include <algorithm>

namespace qpid {
namespace broker {

// Class to keep track of issuing flow control to make sure that the peer doesn't exceed
// a given message rate
//
// Create the object with the target rate
// Then call sendCredit() whenever credit is issued to the peer
// Call receivedMessage() whenever a message is received, it returns the credit to issue.
// 
// sentCredit() be sensibly called with a 0 parameter to indicate
// that we sent credit but treat it as if the value was 0 (we may do this at the start of the connection
// to allow our peer to send messages)
//
// receivedMessage() can be called with 0 to indicate that we've not received a message, but
// tell me what credit I can send.
class RateFlowcontrol {
    uint32_t rate; // messages per second
    uint32_t maxCredit; // max credit issued to client (issued at start)
    uint32_t requestedCredit;
    qpid::sys::AbsTime creditSent;
   
public:
    RateFlowcontrol(uint32_t r) :
         rate(r),
         maxCredit(0),
         requestedCredit(0),
         creditSent(qpid::sys::FAR_FUTURE)
    {}

    uint32_t getRate() const {
        return rate;
    }
    void sentCredit(const qpid::sys::AbsTime& t, uint32_t credit);
    uint32_t receivedMessage(const qpid::sys::AbsTime& t, uint32_t  msgs);
    uint32_t availableCredit(const qpid::sys::AbsTime& t);
    bool flowStopped() const;
};

inline void RateFlowcontrol::sentCredit(const qpid::sys::AbsTime& t, uint32_t credit) {
    // If the client isn't currently requesting credit (ie it's not sent us anything yet) then
    // this credit goes to the max credit held by the client (it can't go to reduce credit
    // less than 0)
    int32_t nextRequestedCredit = requestedCredit - credit;
    if ( nextRequestedCredit<0 ) {
        requestedCredit = 0;
        maxCredit -= nextRequestedCredit;
    } else {
        requestedCredit = nextRequestedCredit;
    }
    creditSent = t;
}

inline uint32_t RateFlowcontrol::availableCredit(const qpid::sys::AbsTime& t) {
    qpid::sys::Duration d(creditSent, t);
    // Could be -ve before first sentCredit
    int64_t toSend = std::min(rate * d / qpid::sys::TIME_SEC, static_cast<int64_t>(requestedCredit));
    return toSend > 0 ? toSend : 0;
}

inline uint32_t RateFlowcontrol::receivedMessage(const qpid::sys::AbsTime& t, uint32_t  msgs) {
    requestedCredit +=msgs;
    // Don't send credit for every message, only send if more than 0.5s since last credit or
    // we've got less than .25 of the max left (heuristic)
    return requestedCredit*4 >= maxCredit*3 || qpid::sys::Duration(creditSent, t) >= 500*qpid::sys::TIME_MSEC
        ? availableCredit(t)
        : 0;
}

inline bool RateFlowcontrol::flowStopped() const {
    return requestedCredit >= maxCredit;
}

}}

#endif // broker_RateFlowcontrol_h
