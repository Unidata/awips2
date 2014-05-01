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
#include "qpid/cluster/ErrorCheck.h"
#include "qpid/cluster/EventFrame.h"
#include "qpid/cluster/ClusterMap.h"
#include "qpid/cluster/Cluster.h"
#include "qpid/framing/ClusterErrorCheckBody.h"
#include "qpid/framing/ClusterConfigChangeBody.h"
#include "qpid/log/Statement.h"

#include <algorithm>

namespace qpid {
namespace cluster {

using namespace std;
using namespace framing;
using namespace framing::cluster;

ErrorCheck::ErrorCheck(Cluster& c)
    : cluster(c), mcast(c.getMulticast()), frameSeq(0), type(ERROR_TYPE_NONE), connection(0)
{}

void ErrorCheck::error(
    Connection& c, ErrorType t, framing::SequenceNumber seq, const MemberSet& ms,
    const std::string& msg)
{
    // Detected a local error, inform cluster and set error state.
    assert(t != ERROR_TYPE_NONE); // Must be an error.
    assert(type == ERROR_TYPE_NONE); // Can't be called when already in an error state.
    type = t;
    unresolved = ms;
    frameSeq = seq;
    connection = &c;
    message = msg;
    QPID_LOG(debug, cluster<< (type == ERROR_TYPE_SESSION ? " channel" : " connection")
             << " error " << frameSeq << " on " << c
             << " must be resolved with: " << unresolved
             << ": " << message);
    mcast.mcastControl(
        ClusterErrorCheckBody(ProtocolVersion(), type, frameSeq), cluster.getId());
    // If there are already frames queued up by a previous error, review
    // them with respect to this new error.
    for (FrameQueue::iterator i = frames.begin(); i != frames.end(); i = review(i))
        ;
}

void ErrorCheck::delivered(const EventFrame& e) {
    frames.push_back(e);
    review(frames.end()-1);
}

// Review a frame in the queue with respect to the current error.
ErrorCheck::FrameQueue::iterator ErrorCheck::review(const FrameQueue::iterator& i) {
    FrameQueue::iterator next = i+1;
    if(!isUnresolved() || !i->frame.getBody() || !i->frame.getMethod())
        return next;            // Only interested in control frames while unresolved.
    const AMQMethodBody* method = i->frame.getMethod();
    if (method->isA<const ClusterErrorCheckBody>()) {
        const ClusterErrorCheckBody* errorCheck =
            static_cast<const ClusterErrorCheckBody*>(method);

        if (errorCheck->getFrameSeq() == frameSeq) { // Addresses current error
            next = frames.erase(i);    // Drop matching error check controls
            if (errorCheck->getType() < type) { // my error is worse than his
                QPID_LOG(critical, cluster
                         << " local error " << frameSeq << " did not occur on member "
                         << i->getMemberId()
                         << ": " << message);
                throw Exception(
                    QPID_MSG("local error did not occur on all cluster members " << ": " << message));
            }
            else {              // his error is worse/same as mine.
                QPID_LOG(debug, cluster << " error " << frameSeq
                         << " resolved with " << i->getMemberId());
                unresolved.erase(i->getMemberId());
                checkResolved();
            }
        }
        else if (errorCheck->getFrameSeq() < frameSeq && errorCheck->getType() != NONE
                 && i->connectionId.getMember() != cluster.getId())
        {
            // This error occured before the current error so we
            // have processed past it.
            next = frames.erase(i); // Drop the error check control
            respondNone(i->connectionId.getMember(), errorCheck->getType(),
                        errorCheck->getFrameSeq());
        }
        // if errorCheck->getFrameSeq() > frameSeq then leave it in the queue.
    }
    else if (method->isA<const ClusterConfigChangeBody>()) {
        const ClusterConfigChangeBody* configChange =
            static_cast<const ClusterConfigChangeBody*>(method);
        if (configChange) {
            MemberSet members(decodeMemberSet(configChange->getCurrent()));
            QPID_LOG(debug, cluster << " apply config change to error "
                     << frameSeq << ": " << members);
            MemberSet intersect;
            set_intersection(members.begin(), members.end(),
                             unresolved.begin(), unresolved.end(),
                             inserter(intersect, intersect.begin()));
            unresolved.swap(intersect);
            checkResolved();
        }
    }
    return next;
}

void ErrorCheck::checkResolved() {
    if (unresolved.empty()) {   // No more potentially conflicted members, we're clear.
        type = ERROR_TYPE_NONE;
        QPID_LOG(debug, cluster << " error " << frameSeq << " resolved.");
    }
    else 
        QPID_LOG(debug, cluster << " error " << frameSeq
                 << " must be resolved with " << unresolved);
}

EventFrame ErrorCheck::getNext() {
    assert(canProcess());
    EventFrame e(frames.front());
    frames.pop_front();
    return e;
}

void ErrorCheck::respondNone(const MemberId& from, uint8_t type, framing::SequenceNumber frameSeq) {
    // Don't respond to non-errors or to my own errors.
    if (type == ERROR_TYPE_NONE || from == cluster.getId())
        return;
    QPID_LOG(debug, cluster << " error " << frameSeq << " did not occur locally.");
    mcast.mcastControl(
        ClusterErrorCheckBody(ProtocolVersion(), ERROR_TYPE_NONE, frameSeq),
        cluster.getId()
    );
}

}} // namespace qpid::cluster
