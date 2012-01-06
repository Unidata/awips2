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
#include "qpid/StringUtils.h"

#include "unit_test.h"

QPID_AUTO_TEST_SUITE(StringUtilsTestSuite)

using namespace qpid;
using std::string;

QPID_AUTO_TEST_CASE(testSplit_general)
{
    std::vector<std::string> results = split("a bbbb, car,d123,,,e", ", ");
    BOOST_CHECK_EQUAL(5u, results.size());
    BOOST_CHECK_EQUAL(string("a"), results[0]);
    BOOST_CHECK_EQUAL(string("bbbb"), results[1]);
    BOOST_CHECK_EQUAL(string("car"), results[2]);
    BOOST_CHECK_EQUAL(string("d123"), results[3]);
    BOOST_CHECK_EQUAL(string("e"), results[4]);
}

QPID_AUTO_TEST_CASE(testSplit_noDelims)
{
    std::vector<std::string> results = split("abc", ", ");
    BOOST_CHECK_EQUAL(1u, results.size());
    BOOST_CHECK_EQUAL(string("abc"), results[0]);
}

QPID_AUTO_TEST_CASE(testSplit_delimAtEnd)
{
    std::vector<std::string> results = split("abc def,,", ", ");
    BOOST_CHECK_EQUAL(2u, results.size());
    BOOST_CHECK_EQUAL(string("abc"), results[0]);
    BOOST_CHECK_EQUAL(string("def"), results[1]);
}

QPID_AUTO_TEST_CASE(testSplit_delimAtStart)
{
    std::vector<std::string> results = split(",,abc def", ", ");
    BOOST_CHECK_EQUAL(2u, results.size());
    BOOST_CHECK_EQUAL(string("abc"), results[0]);
    BOOST_CHECK_EQUAL(string("def"), results[1]);
}

QPID_AUTO_TEST_CASE(testSplit_onlyDelims)
{
    std::vector<std::string> results = split(",, ,  ", ", ");
    BOOST_CHECK_EQUAL(0u, results.size());
}

QPID_AUTO_TEST_CASE(testSplit_empty)
{
    std::vector<std::string> results = split("", ", ");
    BOOST_CHECK_EQUAL(0u, results.size());
}

QPID_AUTO_TEST_SUITE_END()
