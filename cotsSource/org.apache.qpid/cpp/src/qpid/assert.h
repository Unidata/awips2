#ifndef QPID_ASSERT_H
#define QPID_ASSERT_H

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

#include <boost/current_function.hpp>

/**
 * Abort if !expr in debug mode, throw an exception if NDEBUG is set. 
 */
#define QPID_ASSERT(expr) ((expr) ? static_cast<void>(0) : ::qpid::assert_fail(#expr, BOOST_CURRENT_FUNCTION, __FILE__, __LINE__))

namespace qpid {

void assert_fail(char const * expr, char const * function, char const * file, long line); 

} // namespace qpid

#endif  /*!QPID_ASSERT_H*/
