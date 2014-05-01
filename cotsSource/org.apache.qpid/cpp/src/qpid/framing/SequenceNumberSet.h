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
#ifndef _framing_SequenceNumberSet_h
#define _framing_SequenceNumberSet_h

#include <ostream>
#include "qpid/framing/amqp_types.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/SequenceNumber.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/InlineVector.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {

class SequenceNumberSet : public InlineVector<SequenceNumber, 2>
{
    typedef InlineVector<SequenceNumber, 2> Base;
public:
    typedef Base::const_iterator const_iterator;
    typedef Base::iterator iterator;

    void encode(Buffer& buffer) const;
    void decode(Buffer& buffer);
    uint32_t encodedSize() const;   
    QPID_COMMON_EXTERN SequenceNumberSet condense() const;
    QPID_COMMON_EXTERN void addRange(const SequenceNumber& start, const SequenceNumber& end);

    template <class T>
    void processRanges(T& t) const
    {
        if (size() % 2) { //must be even number        
            throw InvalidArgumentException("SequenceNumberSet contains odd number of elements");
        }
    
        for (SequenceNumberSet::const_iterator i = begin(); i != end(); i++) {
            SequenceNumber first = *(i);
            SequenceNumber last = *(++i);
            t(first, last);
        }
    }

    friend QPID_COMMON_EXTERN std::ostream& operator<<(std::ostream&, const SequenceNumberSet&);
};    


}} // namespace qpid::framing


#endif
