/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */


/**@file Unit tests for the client::Message class. */

#include "unit_test.h"
#include "qpid/client/Message.h"

using namespace qpid::client;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(ClientMessageTestSuite)

QPID_AUTO_TEST_CASE(MessageCopyAssign) {
    // Verify that message has normal copy semantics.
    Message m("foo");
    BOOST_CHECK_EQUAL("foo", m.getData());
    Message c(m);
    BOOST_CHECK_EQUAL("foo", c.getData());
    Message a;
    BOOST_CHECK_EQUAL("", a.getData());
    a = m;
    BOOST_CHECK_EQUAL("foo", a.getData());
    a.setData("a");
    BOOST_CHECK_EQUAL("a", a.getData());
    c.setData("c");
    BOOST_CHECK_EQUAL("c", c.getData());
    BOOST_CHECK_EQUAL("foo", m.getData());
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
