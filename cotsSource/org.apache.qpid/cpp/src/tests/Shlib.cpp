/*
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

#include "test_tools.h"
#include "qpid/sys/Shlib.h"
#include "qpid/Exception.h"

#include "unit_test.h"

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(ShlibTestSuite)

using namespace qpid::sys;
typedef void (*CallMe)(int*);


QPID_AUTO_TEST_CASE(testShlib) {
    // The CMake-based build passes in the module suffix; if it's not there,
    // this is a Linux/UNIX libtool-based build.
#if defined (QPID_MODULE_PREFIX) && defined (QPID_MODULE_SUFFIX)
    Shlib sh("./" QPID_MODULE_PREFIX "shlibtest" QPID_MODULE_SUFFIX);
#else
    Shlib sh(".libs/libshlibtest.so");
#endif
    // Double cast to avoid ISO warning.
    CallMe callMe=sh.getSymbol<CallMe>("callMe");
    BOOST_REQUIRE(callMe != 0);
    int unloaded=0;
    callMe(&unloaded);
    sh.unload();
    BOOST_CHECK_EQUAL(42, unloaded);
    try {
        sh.getSymbol("callMe");
        BOOST_FAIL("Expected exception");
    }
    catch (const qpid::Exception&) {}
}

QPID_AUTO_TEST_CASE(testAutoShlib) {
    int unloaded = 0;
    {
#if defined (QPID_MODULE_PREFIX) && defined (QPID_MODULE_SUFFIX)
        AutoShlib sh("./" QPID_MODULE_PREFIX "shlibtest" QPID_MODULE_SUFFIX);
#else
        AutoShlib sh(".libs/libshlibtest.so");
#endif
        CallMe callMe=sh.getSymbol<CallMe>("callMe");
        BOOST_REQUIRE(callMe != 0);
        callMe(&unloaded);
    }
    BOOST_CHECK_EQUAL(42, unloaded);
}


QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
