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
#ifndef _Execution_
#define _Execution_

#include "qpid/framing/SequenceNumber.h"
#include "qpid/client/Demux.h"

namespace qpid {
namespace client {

/**@internal
 * 
 * Provides access to more detailed aspects of the session
 * implementation.
 */
class Execution 
{
public:
    virtual ~Execution() {}
    /**
     * Provides access to the demultiplexing function within the
     * session implementation
     */
    virtual Demux& getDemux() = 0;
    /**
     * Wait until notification has been received of completion of the
     * outgoing command with the specified id.
     */
    void waitForCompletion(const framing::SequenceNumber& id);
};

}}

#endif
