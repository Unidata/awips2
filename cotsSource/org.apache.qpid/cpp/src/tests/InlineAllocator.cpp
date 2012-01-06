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

#include "qpid/InlineAllocator.h"
#include "unit_test.h"

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(InlineAllocatorTestSuite)

using namespace qpid;
using namespace std;

QPID_AUTO_TEST_CASE(testAllocate) {
    InlineAllocator<std::allocator<char>, 2> alloc;

    char* p = alloc.allocate(1);
    BOOST_CHECK(p == (char*)&alloc);
    alloc.deallocate(p,1);

    p = alloc.allocate(2);
    BOOST_CHECK(p == (char*)&alloc);
    alloc.deallocate(p,2);

    p = alloc.allocate(3);
    BOOST_CHECK(p != (char*)&alloc);
    alloc.deallocate(p,3);
}

QPID_AUTO_TEST_CASE(testAllocateFull) {
    InlineAllocator<std::allocator<char>, 1> alloc;

    char* p = alloc.allocate(1);
    BOOST_CHECK(p == (char*)&alloc);

    char* q = alloc.allocate(1);
    BOOST_CHECK(q != (char*)&alloc);

    alloc.deallocate(p,1);
    p = alloc.allocate(1);
    BOOST_CHECK(p == (char*)&alloc);

    alloc.deallocate(p,1);
    alloc.deallocate(q,1);
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
