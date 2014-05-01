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
#include "qpid/client/SessionBase_0_10.h"
#include "qpid/client/Connection.h"
#include "qpid/client/SessionImpl.h"
#include "qpid/client/Future.h"
#include "qpid/framing/all_method_bodies.h"

namespace qpid {
namespace client {

using namespace framing;

SessionBase_0_10::SessionBase_0_10() {}
SessionBase_0_10::~SessionBase_0_10() {}

void SessionBase_0_10::close() 
{ 
    if (impl) impl->close(); 
}

void SessionBase_0_10::flush()
{
    impl->sendFlush();
}

void SessionBase_0_10::sync()
{
    ExecutionSyncBody b;
    b.setSync(true);
    impl->send(b).wait(*impl);
}

void SessionBase_0_10::markCompleted(const framing::SequenceSet& ids, bool notifyPeer)
{
    impl->markCompleted(ids, notifyPeer);
}

void SessionBase_0_10::markCompleted(const framing::SequenceNumber& id, bool cumulative, bool notifyPeer)
{
    impl->markCompleted(id, cumulative, notifyPeer);
}

void SessionBase_0_10::sendCompletion()
{
    impl->sendCompletion();
}

uint16_t SessionBase_0_10::getChannel() const { return impl->getChannel(); }

void SessionBase_0_10::suspend() { impl->suspend(); }
void SessionBase_0_10::resume(Connection c) { impl->resume(c.impl); }
uint32_t SessionBase_0_10::timeout(uint32_t seconds) { return impl->setTimeout(seconds); }

SessionId SessionBase_0_10::getId() const { return impl->getId(); }

bool SessionBase_0_10::isValid() const { return impl; }

}} // namespace qpid::client
