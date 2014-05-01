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
#include "qpid/framing/AccumulatedAck.h"

#include <assert.h>
#include <iostream>
#include <boost/bind.hpp>

using std::list;
using std::max;
using std::min;
using namespace qpid::framing;

AccumulatedAck::AccumulatedAck(SequenceNumber r) : mark(r) {}

void AccumulatedAck::update(SequenceNumber first, SequenceNumber last){
    assert(first <= last);
    if (last < mark) return;


    Range r(first, last);
    bool handled = false;
    bool markMerged = false;
    list<Range>::iterator merged = ranges.end();
    if (r.mergeable(mark)) {
        mark = r.end;
        markMerged = true;
        handled = true;
    } else {
        for (list<Range>::iterator i = ranges.begin(); i != ranges.end() && !handled; i++) {
            if (i->merge(r)) {
                merged = i;
                handled = true;
            } else if (r.start < i->start) {
                ranges.insert(i, r);
                handled = true;
            }
        }
    }
    if (!handled) {
        ranges.push_back(r);
    } else {
        while (!ranges.empty() && ranges.front().end <= mark) { 
            ranges.pop_front(); 
        }
        if (markMerged) {
            //new range is incorporated, but may be possible to consolidate
            merged = ranges.begin();
            while (merged != ranges.end() && merged->mergeable(mark)) {
                mark = merged->end;
                merged = ranges.erase(merged);
            }
        }
        if (merged != ranges.end()) {
            //consolidate ranges
            list<Range>::iterator i = merged;
            list<Range>::iterator j = i++;
            while (i != ranges.end() && j->merge(*i)) {
                j = i++;
            }
        }
    }
}

void AccumulatedAck::consolidate(){}

void AccumulatedAck::clear(){
    mark = SequenceNumber(0);//not sure that this is valid when wraparound is a possibility
    ranges.clear();
}

bool AccumulatedAck::covers(SequenceNumber tag) const{
    if (tag <= mark) return true;
    for (list<Range>::const_iterator i = ranges.begin(); i != ranges.end(); i++) {
        if (i->contains(tag)) return true;
    }
    return false;
}

void AccumulatedAck::collectRanges(SequenceNumberSet& set) const
{
    for (list<Range>::const_iterator i = ranges.begin(); i != ranges.end(); i++) {
        set.push_back(i->start);
        set.push_back(i->end);
    }
}

void AccumulatedAck::update(const SequenceNumber cumulative, const SequenceNumberSet& range)
{
    update(mark, cumulative);
    range.processRanges(*this);
}


bool Range::contains(SequenceNumber i) const 
{ 
    return i >= start && i <= end; 
}

bool Range::intersect(const Range& r) const 
{ 
    return r.contains(start) || r.contains(end) || contains(r.start) || contains(r.end); 
}

bool Range::merge(const Range& r) 
{ 
    if (intersect(r) || mergeable(r.end) || r.mergeable(end)) {
        start = min(start, r.start); 
        end = max(end, r.end); 
        return true;
    } else {
        return false;
    }
}

bool Range::mergeable(const SequenceNumber& s) const
{ 
    if (contains(s) || start - s == 1) {
        return true;
    } else {
        return false;
    }
}

Range::Range(SequenceNumber s, SequenceNumber e) : start(s), end(e) {}


namespace qpid{
namespace framing{
    std::ostream& operator<<(std::ostream& out, const Range& r)
    {
        out << "[" << r.start.getValue() << "-" << r.end.getValue() << "]";
        return out;
    }

    std::ostream& operator<<(std::ostream& out, const AccumulatedAck& a)
    { 
        out << "{mark: " << a.mark.getValue() << ", ranges: (";
        for (list<Range>::const_iterator i = a.ranges.begin(); i != a.ranges.end(); i++) {        
            if (i != a.ranges.begin()) out << ", ";
            out << *i;
        }
        out << ")]";
        return out;
    }
}}
