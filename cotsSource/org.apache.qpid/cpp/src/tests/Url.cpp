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


#include "unit_test.h"
#include "test_tools.h"
#include "qpid/Url.h"
#include <boost/assign.hpp>

using namespace std;
using namespace qpid;
using namespace boost::assign;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(UrlTestSuite)

#define URL_CHECK_STR(STR) BOOST_CHECK_EQUAL(Url(STR).str(), STR)
#define URL_CHECK_INVALID(STR) BOOST_CHECK_THROW(Url(STR), Url::Invalid)

QPID_AUTO_TEST_CASE(TestParseTcp) {
    URL_CHECK_STR("amqp:tcp:host:42");
    URL_CHECK_STR("amqp:tcp:host-._~%ff%23:42"); // unreserved chars and pct encoded hex.

    // Check defaults
    BOOST_CHECK_EQUAL(Url("amqp:host:42").str(), "amqp:tcp:host:42");
    BOOST_CHECK_EQUAL(Url("amqp:tcp:host").str(), "amqp:tcp:host:5672");
    BOOST_CHECK_EQUAL(Url("amqp:tcp:").str(), "amqp:tcp:127.0.0.1:5672");
    BOOST_CHECK_EQUAL(Url("amqp:").str(), "amqp:tcp:127.0.0.1:5672");
    BOOST_CHECK_EQUAL(Url("amqp::42").str(), "amqp:tcp:127.0.0.1:42");

    URL_CHECK_INVALID("amqp::badHost!#$#");
    URL_CHECK_INVALID("amqp::host:badPort");
}

QPID_AUTO_TEST_CASE(TestParseExample) {
    URL_CHECK_STR("amqp:example:x");
    URL_CHECK_INVALID("amqp:example:badExample");
}

QPID_AUTO_TEST_CASE(TestParseMultiAddress) {
    URL_CHECK_STR("amqp:tcp:host:0,example:y,tcp:foo:0,example:1");
    URL_CHECK_STR("amqp:example:z,tcp:foo:0");
    URL_CHECK_INVALID("amqp:tcp:h:0,");
    URL_CHECK_INVALID(",amqp:tcp:h");
}


QPID_AUTO_TEST_CASE(TestInvalidAddress) {
    URL_CHECK_INVALID("xxxx");
    URL_CHECK_INVALID("");
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
