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
#include "qpid/framing/Array.h"
#include "qpid/client/QueueOptions.h"

#include "unit_test.h"

using namespace qpid::client;


namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(QueueOptionsTestSuite)

QPID_AUTO_TEST_CASE(testSizePolicy)
{
    QueueOptions ft;

    ft.setSizePolicy(REJECT,1,2);

    BOOST_CHECK(QueueOptions::strREJECT == ft.getAsString(QueueOptions::strTypeKey));
    BOOST_CHECK(1 == ft.getAsInt(QueueOptions::strMaxSizeKey));
    BOOST_CHECK(2 == ft.getAsInt(QueueOptions::strMaxCountKey));

    ft.setSizePolicy(FLOW_TO_DISK,0,2);
    BOOST_CHECK(QueueOptions::strFLOW_TO_DISK == ft.getAsString(QueueOptions::strTypeKey));
    BOOST_CHECK(1 == ft.getAsInt(QueueOptions::strMaxSizeKey));
    BOOST_CHECK(2 == ft.getAsInt(QueueOptions::strMaxCountKey));

    ft.setSizePolicy(RING,1,0);
    BOOST_CHECK(QueueOptions::strRING == ft.getAsString(QueueOptions::strTypeKey));

    ft.setSizePolicy(RING_STRICT,1,0);
    BOOST_CHECK(QueueOptions::strRING_STRICT == ft.getAsString(QueueOptions::strTypeKey));

    ft.clearSizePolicy();
    BOOST_CHECK(!ft.isSet(QueueOptions::strTypeKey));
    BOOST_CHECK(!ft.isSet(QueueOptions::strMaxSizeKey));
    BOOST_CHECK(!ft.isSet(QueueOptions::strMaxCountKey));
}

QPID_AUTO_TEST_CASE(testFlags)
{
    QueueOptions ft;

    ft.setPersistLastNode();
    ft.setOrdering(LVQ);

    BOOST_CHECK(1 == ft.getAsInt(QueueOptions::strPersistLastNode));
    BOOST_CHECK(1 == ft.getAsInt(QueueOptions::strLastValueQueue));

    ft.clearPersistLastNode();
    ft.setOrdering(FIFO);

    BOOST_CHECK(!ft.isSet(QueueOptions::strPersistLastNode));
    BOOST_CHECK(!ft.isSet(QueueOptions::strLastValueQueue));

}

QPID_AUTO_TEST_CASE(testSetOrdering)
{
    //ensure setOrdering(FIFO) works even if not preceded by a call to
    //setOrdering(LVQ)
    QueueOptions ft;
    ft.setOrdering(FIFO);
    BOOST_CHECK(!ft.isSet(QueueOptions::strLastValueQueue));

}

QPID_AUTO_TEST_CASE(testClearPersistLastNode)
{
    //ensure clear works even if not preceded by the setting on the
    //option
    QueueOptions ft;
    ft.clearPersistLastNode();
    BOOST_CHECK(!ft.isSet(QueueOptions::strPersistLastNode));
}


QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
