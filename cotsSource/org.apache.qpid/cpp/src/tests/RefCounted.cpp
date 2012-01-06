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

#include "qpid/RefCounted.h"
#include <boost/intrusive_ptr.hpp>

#include "unit_test.h"

QPID_AUTO_TEST_SUITE(RefCountedTestSuiteTestSuite)

using boost::intrusive_ptr;
using namespace std;
using namespace qpid;

namespace qpid {
namespace tests {

struct CountMe : public RefCounted {
    static int instances;
    CountMe() { ++instances; }
    ~CountMe() { --instances; }
};

int CountMe::instances=0;

QPID_AUTO_TEST_CASE(testRefCounted) {
    BOOST_CHECK_EQUAL(0, CountMe::instances);
    intrusive_ptr<CountMe> p(new CountMe());
    BOOST_CHECK_EQUAL(1, CountMe::instances);
    intrusive_ptr<CountMe> q(p);
    BOOST_CHECK_EQUAL(1, CountMe::instances);
    q=0;
    BOOST_CHECK_EQUAL(1, CountMe::instances);
    p=0;
    BOOST_CHECK_EQUAL(0, CountMe::instances);
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
