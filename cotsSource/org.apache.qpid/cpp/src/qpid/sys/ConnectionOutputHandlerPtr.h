#ifndef QPID_SYS_CONNECTIONOUTPUTHANDLERPTR_H
#define QPID_SYS_CONNECTIONOUTPUTHANDLERPTR_H

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

#include "qpid/sys/ConnectionOutputHandler.h"

namespace qpid {
namespace sys {

/**
 * A ConnectionOutputHandler that delegates to another
 * ConnectionOutputHandler.  Allows the "real" ConnectionOutputHandler
 * to be changed without updating all the pointers/references
 * using the ConnectionOutputHandlerPtr
 */
class ConnectionOutputHandlerPtr : public ConnectionOutputHandler
{
  public:
    ConnectionOutputHandlerPtr(ConnectionOutputHandler* p) : next(p) { assert(next); }
    void set(ConnectionOutputHandler* p) { next = p; assert(next); }
    ConnectionOutputHandler* get() { return next; }
    const ConnectionOutputHandler* get() const { return next; }

    void close() { next->close(); }
    size_t getBuffered() const { return next->getBuffered(); }
    void abort() { next->abort(); }
    void activateOutput() { next->activateOutput(); }
    void giveReadCredit(int32_t credit) { next->giveReadCredit(credit); }
    void send(framing::AMQFrame& f) { next->send(f); }

  private:
    ConnectionOutputHandler* next;
};
}} // namespace qpid::sys

#endif  /*!QPID_SYS_CONNECTIONOUTPUTHANDLERPTR_H*/
