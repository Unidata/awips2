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
#include <iostream>
#include "qpid/framing/AMQFrame.h"
#include "qpid/framing/AMQMethodBody.h"
#include "qpid/framing/ExecutionSyncBody.h"
#include "qpid/framing/Proxy.h"

#include "unit_test.h"

using namespace qpid::framing;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(ProxyTestSuite)


QPID_AUTO_TEST_CASE(testScopedSync)
{
    struct DummyHandler : FrameHandler
    {
        void handle(AMQFrame& f) {
            AMQMethodBody* m = f.getMethod();
            BOOST_CHECK(m);
            BOOST_CHECK(m->isA<ExecutionSyncBody>());
            BOOST_CHECK(m->isSync());
        }
    };
    DummyHandler f;
    Proxy p(f);
    Proxy::ScopedSync s(p);
    p.send(ExecutionSyncBody(p.getVersion()));
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
