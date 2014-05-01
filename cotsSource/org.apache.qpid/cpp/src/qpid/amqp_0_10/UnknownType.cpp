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
#include "qpid/amqp_0_10/UnknownType.h"
#include <boost/range/iterator_range.hpp>
#include <ostream>

namespace qpid {
namespace amqp_0_10 {

UnknownType::Width UnknownType::WidthTable[16] = {
    { 1, 0 },
    { 2, 0 },
    { 4, 0 },
    { 8, 0 },
    { 16, 0 },
    { 32, 0 },
    { 64, 0 },
    { 128, 0 },
    { 0, 1 },
    { 0, 2 },    
    { 0, 4 },
    { -1, -1 },                   // Invalid
    { 5, 0 },
    { 9, 0 },
    { -1, -1 },                   // Invalid    
    { 0, 0 }
};

int UnknownType::fixed() const { return WidthTable[code>>4].fixed; }
int UnknownType::variable() const { return WidthTable[code>>4].variable; }
UnknownType::UnknownType(uint8_t c) : code(c) { data.resize(fixed()); }

std::ostream& operator<<(std::ostream& o, const UnknownType& u) {
    return o << boost::make_iterator_range(u.begin(), u.end()) << std::endl;
}

}} // namespace qpid::amqp_0_10

