#ifndef QPID_ASSERT_CPP
#define QPID_ASSERT_CPP

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
#include <sstream>
#include <iostream>
#include "qpid/framing/reply_exceptions.h"
#include <stdlib.h>

namespace qpid {

void assert_fail(char const * expr, char const * function, char const * file, long line) {
    std::ostringstream msg;
    msg << "Internal error: " << expr << " in function " << function
        << "(" << file << ":" << line << ")";
#ifdef NDEBUG
    throw framing::InternalErrorException(msg.str());
#else
    std::cerr << msg.str() << std::endl;
    abort();
#endif
}

} // namespace qpid

#endif  /*!QPID_ASSERT_CPP*/
