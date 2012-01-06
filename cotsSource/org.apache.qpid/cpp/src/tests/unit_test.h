#ifndef QPIPD_TEST_UNIT_TEST_H_
#define QPIPD_TEST_UNIT_TEST_H_

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

// Workaround so we can build against boost 1.33 and boost 1.34.
// Remove when we no longer need to support 1.33.
// 
#include <boost/version.hpp>
#include <limits.h> // Must be inclued beofre boost/test headers.

// #include the correct header file.
// 
#if (BOOST_VERSION < 103400)
# include <boost/test/auto_unit_test.hpp>
#else
# include <boost/test/unit_test.hpp>
#endif  // BOOST_VERSION

// Workarounds for BOOST_AUTO_TEST_CASE|SUITE|SUITE_END
// 
#if (BOOST_VERSION < 103300)

# define QPID_AUTO_TEST_SUITE(name)
# define QPID_AUTO_TEST_CASE(name)  BOOST_AUTO_UNIT_TEST(name)
# define QPID_AUTO_TEST_SUITE_END()

#elif (BOOST_VERSION < 103400)
// Note the trailing ';'
# define QPID_AUTO_TEST_SUITE(name) BOOST_AUTO_TEST_SUITE(name);
# define QPID_AUTO_TEST_SUITE_END() BOOST_AUTO_TEST_SUITE_END();

#endif  // Workarounds for BOOST_AUTO_TEST_CASE|SUITE|SUITE_END

//
// Default definitions for latest version of boost.
//

#ifndef QPID_AUTO_TEST_SUITE
# define QPID_AUTO_TEST_SUITE(name) BOOST_AUTO_TEST_SUITE(name)
#endif

#ifndef QPID_AUTO_TEST_CASE
# define QPID_AUTO_TEST_CASE(name)  BOOST_AUTO_TEST_CASE(name)
#endif

#ifndef QPID_AUTO_TEST_SUITE_END
# define QPID_AUTO_TEST_SUITE_END() BOOST_AUTO_TEST_SUITE_END()
#endif

#endif  // !QPIPD_TEST_UNIT_TEST_H_
