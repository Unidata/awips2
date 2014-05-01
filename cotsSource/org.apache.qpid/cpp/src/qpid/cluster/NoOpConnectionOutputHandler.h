#ifndef QPID_CLUSTER_NOOPCONNECTIONOUTPUTHANDLER_H
#define QPID_CLUSTER_NOOPCONNECTIONOUTPUTHANDLER_H

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
#include <qpid/sys/ConnectionOutputHandler.h>

namespace qpid {

namespace framing { class AMQFrame; }

namespace cluster {

/**
 * Output handler shadow connections, simply discards frames.
 */
class NoOpConnectionOutputHandler : public sys::ConnectionOutputHandler
{
  public:
    virtual void send(framing::AMQFrame&) {}
    virtual void close() {}
    virtual void abort() {}
    virtual void activateOutput() {}
    virtual void giveReadCredit(int32_t) {}
};

}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_NOOPCONNECTIONOUTPUTHANDLER_H*/
