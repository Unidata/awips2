#ifndef QPID_FRAMING_ISINSEQUENCESET_H
#define QPID_FRAMING_ISINSEQUENCESET_H

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

#include "qpid/framing/SequenceSet.h"

namespace qpid {
namespace framing {
/**
 * Functor to test whether values are in a sequence set.  This is a
 * stateful functor that requires the values to be supplied in order
 * and takes advantage of that ordering to avoid multiple scans.
 */
class IsInSequenceSet
{
  public:
    IsInSequenceSet(const SequenceSet& s) : set(s), i(set.rangesBegin()) {}

    bool operator()(const SequenceNumber& n) {
        while (i != set.rangesEnd() && i->end() <= n) ++i;
        return i != set.rangesEnd() && i->begin() <= n;
    }

  private:
    const SequenceSet& set;
    SequenceSet::RangeIterator i;
};

}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_ISINSEQUENCESET_H*/
