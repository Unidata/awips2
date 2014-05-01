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

#include "qpid/console/Package.h"
#include "qpid/console/ClassKey.h"
#include "unit_test.h"

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(ConsoleTestSuite)

using namespace qpid::framing;
using namespace qpid::console;

QPID_AUTO_TEST_CASE(testClassKey) {
    uint8_t hash[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
    ClassKey k("com.redhat.test", "class", hash);

    BOOST_CHECK_EQUAL(k.getPackageName(), "com.redhat.test");
    BOOST_CHECK_EQUAL(k.getClassName(), "class");
    BOOST_CHECK_EQUAL(k.getHashString(), "00010203-04050607-08090a0b-0c0d0e0f");
    BOOST_CHECK_EQUAL(k.str(), "com.redhat.test:class(00010203-04050607-08090a0b-0c0d0e0f)");
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
