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
#include <string>
#include <string.h>
#include <windows.h>

namespace qpid {
namespace sys {

std::string strError(int err) {
    const size_t bufsize = 512;
    char buf[bufsize];
    if (0 == FormatMessage (FORMAT_MESSAGE_MAX_WIDTH_MASK
                            | FORMAT_MESSAGE_FROM_SYSTEM,
                            0,
                            err,
                            0,        // Default language
                            buf,
                            bufsize,
                            0))
    {
        strerror_s (buf, bufsize, err);
    }
    return std::string(buf);
}

}}
