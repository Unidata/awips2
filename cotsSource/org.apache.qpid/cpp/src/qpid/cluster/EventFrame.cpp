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
#include "qpid/cluster/EventFrame.h"
#include "qpid/cluster/Connection.h"

namespace qpid {
namespace cluster {

EventFrame::EventFrame() {}

EventFrame::EventFrame(const EventHeader& e, const framing::AMQFrame& f, int rc)
    : connectionId(e.getConnectionId()), frame(f), readCredit(rc), type(e.getType())
{}

std::ostream& operator<<(std::ostream& o, const EventFrame& e) {
    if (e.frame.getBody()) o << e.frame;
    else o << "null-frame";
    o << " " << e.type << " " << e.connectionId;
    if (e.readCredit) o << " read-credit=" << e.readCredit;
    return o;
}

}} // namespace qpid::cluster
