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
#include "qpid/sys/AtomicValue.h"

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(AtomicValueTestSuite)

QPID_AUTO_TEST_CASE(test) {
    qpid::sys::AtomicValue<int> x(0);
    BOOST_CHECK_EQUAL(++x, 1);
    BOOST_CHECK_EQUAL(--x,0);
    BOOST_CHECK_EQUAL(x+=5,5);
    BOOST_CHECK_EQUAL(x-=10,-5);
    BOOST_CHECK_EQUAL(x.fetchAndAdd(7), -5);
    BOOST_CHECK_EQUAL(x.get(),2);
    BOOST_CHECK_EQUAL(x.fetchAndSub(3), 2);
    BOOST_CHECK_EQUAL(x.get(),-1);

    BOOST_CHECK_EQUAL(x.valueCompareAndSwap(-1,10), -1);
    BOOST_CHECK_EQUAL(x.get(), 10);
    BOOST_CHECK_EQUAL(x.valueCompareAndSwap(5, 6), 10);
    BOOST_CHECK_EQUAL(x.get(), 10);

    BOOST_CHECK(!x.boolCompareAndSwap(5, 6));
    BOOST_CHECK_EQUAL(x.get(), 10);
    BOOST_CHECK(x.boolCompareAndSwap(10, 6));
    BOOST_CHECK_EQUAL(x.get(), 6);
}


QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
