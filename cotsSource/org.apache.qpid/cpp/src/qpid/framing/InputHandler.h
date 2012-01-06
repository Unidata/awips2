#ifndef _InputHandler_
#define _InputHandler_
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

#include "qpid/framing/FrameHandler.h"
#include <boost/noncopyable.hpp>

namespace qpid {
namespace framing {

// TODO aconway 2007-08-29: Eliminate, replace with FrameHandler.
class InputHandler : public FrameHandler {
  public:
    virtual ~InputHandler() {}
    virtual void received(AMQFrame&) = 0;
    void handle(AMQFrame& f) { received(f); }
};

}}


#endif
