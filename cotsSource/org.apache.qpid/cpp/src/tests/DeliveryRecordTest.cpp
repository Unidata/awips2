
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
#include "qpid/broker/DeliveryRecord.h"
#include "unit_test.h"
#include <iostream>
#include <memory>
#include <boost/format.hpp>

using namespace qpid::broker;
using namespace qpid::sys;
using namespace qpid::framing;
using boost::dynamic_pointer_cast;
using std::list;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(DeliveryRecordTestSuite)

QPID_AUTO_TEST_CASE(testSort)
{
    list<SequenceNumber> ids;
    ids.push_back(SequenceNumber(6));
    ids.push_back(SequenceNumber(2));
    ids.push_back(SequenceNumber(4));
    ids.push_back(SequenceNumber(5));
    ids.push_back(SequenceNumber(1));
    ids.push_back(SequenceNumber(3));

    list<DeliveryRecord> records;
    for (list<SequenceNumber>::iterator i = ids.begin(); i != ids.end(); i++) {
        DeliveryRecord r(QueuedMessage(0), Queue::shared_ptr(), "tag", false, false, false);
        r.setId(*i);
        records.push_back(r);
    }
    records.sort();

    SequenceNumber expected(0);
    for (list<DeliveryRecord>::iterator i = records.begin(); i != records.end(); i++) {
        BOOST_CHECK(i->getId() == ++expected);
    }
}


QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
