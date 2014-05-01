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
#include "qpid/amqp_0_10/Map.h"
#include "qpid/amqp_0_10/Struct32.h"
#include "qpid/amqp_0_10/Array.h"
#include <ostream>

namespace qpid {
namespace amqp_0_10 {

MapValue::MapValue() : code(codeFor(uint8_t(0))), blob(in_place<uint8_t>(0)) {}

MapValue::MapValue(const MapValue& x) : code(x.code), blob(x.blob) {}

bool  MapValue::operator==(const MapValue& x) const {
    return code == x.code;      // FIXME aconway 2008-04-01: incomplete
}

struct OstreamVisitor : public MapValue::Visitor<std::ostream&> {
    std::ostream& out;
    OstreamVisitor(std::ostream& o) : out(o) {}
    template <class T> std::ostream& operator()(const T& t) {
        return out << t;
    }
};

std::ostream& operator<<(std::ostream& o, const MapValue& m) {
    o << typeName(m.getCode()) << ":";
    const_cast<MapValue&>(m).apply_visitor(OstreamVisitor(o));
    return o;
}

std::ostream& operator<<(std::ostream& o, const Map::value_type& v) {
    return o << v.first << "=" << v.second;
}
std::ostream& operator<<(std::ostream& o, const Map& map) {
    o << "map[";
    std::ostream_iterator<Map::value_type> i(o, " ");
    std::copy(map.begin(), map.end(), i);
    return o << "]";
}

uint32_t Map::contentSize() const {
    // FIXME aconway 2008-04-03: preview to 0-10 mapping: +4 for count.
    return /*4 +*/ Codec::Size()(begin(), end()); 
}

}} // namespace qpid::amqp_0_10
