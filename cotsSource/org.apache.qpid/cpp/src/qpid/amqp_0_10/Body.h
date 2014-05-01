#ifndef QPID_AMQP_0_10_BODY_H
#define QPID_AMQP_0_10_BODY_H

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
#include <string>
#include <ostream>

namespace qpid {
namespace amqp_0_10 {

/** Holds data from a body frame. */
class Body {
  public:
    Body() {}
    Body(size_t size_) : str(size_, '\0') {}
    Body(const char* data_, size_t size_) : str(data_, size_) {}

    size_t size() const { return str.size(); };
    const char* data() const { return str.data(); }
    char* data() { return const_cast<char*>(str.data()); }

    template <class S> void serialize(S& s) { s.raw(data(), size()); }
    
  private:
    std::string str;

  friend std::ostream& operator<<(std::ostream&, const Body&);
};

inline std::ostream& operator<<(std::ostream& o, const Body& b) {
    return o << b.str.substr(0, 16) << "... (" << b.size() << ")";
}

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_BODY_H*/
