/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/Address.h"

#include <ostream>

using namespace std;

namespace qpid {

TcpAddress::TcpAddress(const std::string& h, uint16_t p): host(h), port(p) {}

struct AddressOstreamVisitor : public boost::static_visitor<ostream&> {
    ostream& out;
    AddressOstreamVisitor(ostream& o) : out(o) {}
    template <class T> ostream& operator()(const T& data) { return out << data; }
};

ostream& operator<<(ostream& os, const Address& addr) {
    AddressOstreamVisitor visitor(os);
    return boost::apply_visitor(visitor, addr.value);
}

bool operator==(const TcpAddress& x, const TcpAddress& y) {
    return y.host==x.host && y.port == x.port;
}

ostream& operator<<(ostream& os, const TcpAddress& a) {
    return os << "tcp:" << a.host << ":" << a.port;
}

ExampleAddress::ExampleAddress(const char c) : data(c) {}

bool operator==(const ExampleAddress& x, const ExampleAddress& y) {
    return x.data == y.data;
}

ostream& operator<<(ostream& os, const ExampleAddress& ex) {
    return os << "example:" << ex.data;
}

} // namespace qpid
