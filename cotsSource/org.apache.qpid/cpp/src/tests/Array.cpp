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
#include <sstream>
#include "qpid/framing/Array.h"
#include "qpid/framing/FieldValue.h"

#include "unit_test.h"

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(ArrayTestSuite)

using namespace qpid::framing;

void populate(std::vector<std::string>& data, int count = 10)
{
    for (int i = 0; i < count; i++) {
        std::stringstream out;
        out << "item-" << i;
        data.push_back(out.str());
    }
}

QPID_AUTO_TEST_CASE(testEncodeDecode)
{
    std::vector<std::string> data;
    populate(data);

    Array a(data);

    char buff[200];
    Buffer wbuffer(buff, 200);
    a.encode(wbuffer);

    Array b;
    Buffer rbuffer(buff, 200);
    b.decode(rbuffer);
    BOOST_CHECK_EQUAL(a, b);

    std::vector<std::string> data2;
    b.collect(data2);
    //BOOST_CHECK_EQUAL(data, data2);
    BOOST_CHECK(data == data2);
}

QPID_AUTO_TEST_CASE(testArrayAssignment)
{
    std::vector<std::string> data;
    populate(data);
    Array b;
    {
        Array a(data);
        b = a;
        BOOST_CHECK_EQUAL(a, b);
    }
    std::vector<std::string> data2;
    b.collect(data2);
    //BOOST_CHECK_EQUAL(data, data2);
    BOOST_CHECK(data == data2);
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
