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

#include "qpid/SessionId.h"
#include <sstream>

namespace qpid {

SessionId::SessionId(const std::string& u, const std::string& n) : userId(u), name(n) {}

bool SessionId::operator<(const SessionId& id) const {
    return userId < id.userId || (userId == id.userId && name < id.name);
}

bool SessionId::operator==(const SessionId& id) const {
    return id.name == name  && id.userId == userId;
}

std::ostream& operator<<(std::ostream& o, const SessionId& id) {
    return o << id.getUserId() << "." << id.getName();
}

std::string SessionId::str() const {
    std::ostringstream o;
    o << *this;
    return o.str();
}

} // namespace qpid
