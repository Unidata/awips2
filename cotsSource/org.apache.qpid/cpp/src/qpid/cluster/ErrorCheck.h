#ifndef QPID_CLUSTER_ERRORCHECK_H
#define QPID_CLUSTER_ERRORCHECK_H

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

#include "qpid/cluster/MemberSet.h"
#include "qpid/cluster/Multicaster.h"
#include "qpid/framing/enum.h"
#include "qpid/framing/SequenceNumber.h"
#include <boost/function.hpp>
#include <deque>
#include <set>

namespace qpid {
namespace cluster {

class EventFrame;
class Cluster;
class Multicaster;
class Connection;

/**
 * Error checking logic.
 * 
 * When an error occurs queue up frames until we can determine if all
 * nodes experienced the error. If not, we shut down.
 */
class ErrorCheck
{
  public:
    typedef framing::cluster::ErrorType ErrorType;
    typedef framing::SequenceNumber SequenceNumber;
    
    ErrorCheck(Cluster&);

    /** A local error has occured */
    void error(Connection&, ErrorType, SequenceNumber frameSeq, const MemberSet&,
               const std::string& msg);

    /** Called when a frame is delivered */
    void delivered(const EventFrame&);

    /**@pre canProcess **/
    EventFrame getNext();

    bool canProcess() const { return type == NONE && !frames.empty(); }

    bool isUnresolved() const { return type != NONE; }

    /** Respond to an error check saying we had no error. */
    void respondNone(const MemberId&, uint8_t type, SequenceNumber frameSeq);
    
  private:
    static const ErrorType NONE = framing::cluster::ERROR_TYPE_NONE;
    typedef std::deque<EventFrame>  FrameQueue;
    FrameQueue::iterator review(const FrameQueue::iterator&);
    void checkResolved();
    
    Cluster& cluster;
    Multicaster& mcast;
    FrameQueue frames;
    MemberSet unresolved;
    SequenceNumber frameSeq;
    ErrorType type;
    Connection* connection;
    std::string message;
};

}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_ERRORCHECK_H*/
