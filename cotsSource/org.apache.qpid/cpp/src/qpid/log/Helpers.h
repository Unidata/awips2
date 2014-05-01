#ifndef QPID_LOG_HELPERS_H
#define QPID_LOG_HELPERS_H

/*
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

#include <boost/range.hpp>

#include <ostream>

namespace qpid {
namespace log {

/** @file Helper classes for logging complex types */

/// @internal
template <class Range>
struct ListFormatter {
    typedef typename boost::range_const_iterator<Range>::type Iterator;
    boost::iterator_range<Iterator> range;
    const char* separator;

    ListFormatter(const Range& r, const char* s=", ") : range(r), separator(s) {}
};

/// @internal
template <class Range>
std::ostream& operator<<(std::ostream& out, const ListFormatter<Range>& sl) {
    typename ListFormatter<Range>::Iterator i = sl.range.begin();
    if (i != sl.range.end()) out << *(i++);
    while (i != sl.range.end()) out << sl.separator << *(i++);
    return out;
}

/** Return a formatting object with operator <<
 *  to stream range as a separated list.
 *@param range: a range - all standard containers are ranges,
 * as is a pair of iterators.
 *@param separator: printed between elements, default ", "
 */
template <class Range>
ListFormatter<Range> formatList(const Range& range, const char* separator=", ") {
    return ListFormatter<Range>(range, separator);
}

/** Return a formatting object with operator <<
 *  to stream the range defined by iterators [begin, end)
 *  as a separated list.
 *@param begin, end: Beginning and end of range.
 *@param separator: printed between elements, default ", "
 */
template <class U, class V>
ListFormatter<std::pair<U,V> > formatList(U begin, V end, const char* separator=", ") {
    return formatList(std::make_pair(begin,end), separator);
}


}} // namespace qpid::log



#endif  /*!QPID_LOG_HELPERS_H*/
