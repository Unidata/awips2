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
#ifndef _AccumulatedAck_
#define _AccumulatedAck_

#include <algorithm>
#include <functional>
#include <list>
#include <ostream>
#include "qpid/framing/SequenceNumber.h"
#include "qpid/framing/SequenceNumberSet.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
    namespace framing {

        struct Range
        {
            SequenceNumber start;
            SequenceNumber end;

            Range(SequenceNumber s, SequenceNumber e);
            bool contains(SequenceNumber i) const;
            bool intersect(const Range& r) const;
            bool merge(const Range& r);
            bool mergeable(const SequenceNumber& r) const;
        };
        /**
         * Keeps an accumulated record of acknowledged messages (by delivery
         * tag).
         */
        class AccumulatedAck {
	public:
            /**
             * Everything up to this value has been acknowledged.
             */
            SequenceNumber mark;
            /**
             * List of individually acknowledged messages greater than the
             * 'mark'.
             */
            std::list<Range> ranges;

            QPID_COMMON_EXTERN explicit AccumulatedAck(SequenceNumber r = SequenceNumber());
            QPID_COMMON_EXTERN void update(SequenceNumber firstTag, SequenceNumber lastTag);
            QPID_COMMON_EXTERN void consolidate();
            QPID_COMMON_EXTERN void clear();
            QPID_COMMON_EXTERN bool covers(SequenceNumber tag) const;
            void collectRanges(SequenceNumberSet& set) const;
            QPID_COMMON_EXTERN void update(const SequenceNumber cumulative, const SequenceNumberSet& range);
            void operator()(SequenceNumber first, SequenceNumber last) { update(first, last); }
        };
        QPID_COMMON_EXTERN std::ostream& operator<<(std::ostream&, const Range&);
        QPID_COMMON_EXTERN std::ostream& operator<<(std::ostream&, const AccumulatedAck&);
    }
}


#endif
