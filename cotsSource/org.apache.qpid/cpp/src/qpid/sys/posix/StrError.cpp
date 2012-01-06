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

#include "qpid/sys/StrError.h"

#include <string.h>

namespace qpid {
namespace sys {

std::string strError(int err) {
    char buf[512] = "Unknown error";
#ifdef _GNU_SOURCE
    // GNU strerror_r returns the message
    return ::strerror_r(err, buf, sizeof(buf));
#else
    // POSIX strerror_r doesn't return the buffer
    ::strerror_r(err, buf, sizeof(buf));
    return std::string(buf);
#endif
}

}}
