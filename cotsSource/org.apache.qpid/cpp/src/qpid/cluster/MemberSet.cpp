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
#include "MemberSet.h"
#include <ostream>
#include <algorithm>

namespace qpid {
namespace cluster {

MemberSet decodeMemberSet(const std::string& s) {
    MemberSet set;
    for (std::string::const_iterator i = s.begin(); i < s.end(); i += 8) {
        assert(size_t(i-s.begin())+8 <= s.size());
        set.insert(MemberId(std::string(i, i+8)));
    }
    return set;
}

MemberSet intersection(const MemberSet& a, const MemberSet& b)
{
    MemberSet intersection;
    std::set_intersection(a.begin(), a.end(),
                          b.begin(), b.end(),
                          std::inserter(intersection, intersection.begin()));
    return intersection;

}

std::ostream& operator<<(std::ostream& o, const MemberSet& ms) {
    copy(ms.begin(), ms.end(), std::ostream_iterator<MemberId>(o, " "));
    return o;
}

}} // namespace qpid::cluster
