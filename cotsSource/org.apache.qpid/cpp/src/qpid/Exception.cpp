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

#include "qpid/log/Statement.h"
#include "qpid/Exception.h"
#include <typeinfo>
#include <assert.h>
#include <string.h>

namespace qpid {

Exception::Exception(const std::string& msg) throw() : message(msg) {
    QPID_LOG_IF(debug, !msg.empty(), "Exception constructed: " << message);
}

Exception::~Exception() throw() {}

std::string Exception::getPrefix() const { return ""; }

std::string Exception::getMessage() const { return message; }

const char* Exception::what() const throw() {
    // Construct the what string the first time it is needed.
    if (whatStr.empty()) {
        whatStr = getPrefix();
        if (!whatStr.empty()) whatStr +=  ": ";
        whatStr += message;
    }
    return whatStr.c_str();
}

ClosedException::ClosedException(const std::string& msg)
  : Exception(msg) {}

std::string ClosedException::getPrefix() const { return "Closed"; }

} // namespace qpid
