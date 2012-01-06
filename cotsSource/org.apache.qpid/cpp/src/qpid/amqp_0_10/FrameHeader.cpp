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
#include "qpid/amqp_0_10/FrameHeader.h"
#include <ios>
#include <iomanip>
#include <ostream>

using namespace std;

namespace qpid {
namespace amqp_0_10 {

bool FrameHeader::operator==(const FrameHeader& x) const {
    return flags == x.flags &&
        type == x.type &&
        size == x.size &&
        track == x.track &&
        channel == x.channel;
}

std::ostream& operator<<(std::ostream& o, const FrameHeader& f) {
    std::ios::fmtflags saveFlags = o.flags();
    return o << "Frame["
             << "flags=" << std::hex << std::showbase << int(f.getFlags()) << std::setiosflags(saveFlags)
             << " type=" << f.getType()
             << " size=" << f.getSize()
             << " track=" << int(f.getTrack())
             << " channel=" << f.getChannel()
             << "]";
}

}} // namespace qpid::amqp_0_10
