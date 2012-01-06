#ifndef QPID_CLUSTER_DECODER_H
#define QPID_CLUSTER_DECODER_H

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

#include "qpid/cluster/types.h"
#include "qpid/framing/FrameDecoder.h"
#include "qpid/sys/Mutex.h"
#include <boost/function.hpp>
#include <map>

namespace qpid {
namespace cluster {

class EventFrame;
class EventHeader;

/**
 * A map of decoders for connections.
 */
class Decoder
{
  public:
    typedef boost::function<void(const EventFrame&)> FrameHandler;

    Decoder(FrameHandler fh) : callback(fh) {}
    void decode(const EventHeader& eh, const char* data);
    void erase(const ConnectionId&);
    framing::FrameDecoder& get(const ConnectionId& c);

  private:
    typedef std::map<ConnectionId, framing::FrameDecoder> Map;
    sys::Mutex lock;
    Map map;
    void process(const EventFrame&);
    FrameHandler callback;
};
}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_DECODER_H*/
