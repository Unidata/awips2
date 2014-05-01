#ifndef QPID_RANGESET_H
#define QPID_RANGESET_H

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

#include "qpid/InlineVector.h"
#include <boost/iterator/iterator_facade.hpp>
#include <boost/operators.hpp>
#include <boost/bind.hpp>
#include <algorithm>
#include <numeric>

namespace qpid {

/** A range of values, used in RangeSet.
 * Range(begin, end) includes begin but excludes end.
 * Range::makeClosed(first,last) includes both first and last.
 */
template <class T>
class Range {
  public:
    static Range makeClosed(const T& first, T last) { return Range(first, ++last); }

    Range() : begin_(), end_() {}
    explicit Range(const T& t) : begin_(t), end_(t) { ++end_; }
    Range(const T& b, const T& e) : begin_(b), end_(e) { assert(b <= e); }

    T begin() const { return begin_; }
    /** End of _open_ range, i.e. !contains(end()) */
    T end() const { return end_; }

    T first() const { assert(!empty()); return begin_; }
    /** Last in closed range, i.e. contains(end()) */
    T last() const { assert(!empty()); T ret=end_; return --ret; }

    void begin(const T& t) { begin_ = t; }
    void end(const T& t) { end_ = t; }
    size_t size() const { return end_ - begin_; }
    bool empty() const { return begin_ == end_; }

    bool contains(const T& x) const { return begin_ <= x && x < end_; }
    bool contains(const Range& r) const { return begin_ <= r.begin_ && r.end_ <= end_; }
    bool strictContains(const Range& r) const { return begin_ < r.begin_ && r.end_ < end_; }

    bool operator==(const Range& x) { return begin_ == x.begin_ && end_== x.end_; }

    bool operator<(const T& t) const { return end_ < t; }
    bool operator<(const Range<T>& r) const { return end_ < r.begin_; }

    /** touching ranges can be merged into a single range. */
    bool touching(const Range& r) const {
        return std::max(begin_, r.begin_) <= std::min(end_, r.end_);
    }

    /** @pre touching */
    void merge(const Range& r) {
        assert(touching(r));
        begin_ = std::min(begin_, r.begin_);
        end_ = std::max(end_, r.end_);
    }

    operator bool() const { return !empty(); }

    template <class S> void serialize(S& s) { s(begin_)(end_); }

  private:
    T begin_, end_;
};


/**
 * A set implemented as a list of [begin, end) ranges.
 * T must be LessThanComparable and Incrementable.
 * RangeSet only provides const iterators.
 */
template <class T>
class RangeSet
    : boost::additive1<RangeSet<T>,
                       boost::additive2<RangeSet<T>, Range<T>,
                                        boost::additive2<RangeSet<T>, T> > >
{
    typedef InlineVector<Range<T>, 3> Ranges; // TODO aconway 2008-04-21: what's the optimial inlined value?

  public:

    class iterator : public boost::iterator_facade<
        iterator,
        const T,
        boost::forward_traversal_tag>
    {
      public:
        iterator() : ranges(), iter(), value() {}

      private:
        typedef typename Ranges::const_iterator RangesIter;
        iterator(const Ranges& r, const RangesIter& i, const T& t)
            : ranges(&r), iter(i), value(t) {}

        void increment();
        bool equal(const iterator& i) const;
        const T& dereference() const { return value; }

        const Ranges* ranges;
        RangesIter iter;
        T value;

      friend class RangeSet<T>;
      friend class boost::iterator_core_access;
    };

    typedef iterator const_iterator;

    RangeSet() {}
    explicit RangeSet(const Range<T>& r) { *this += r; }
    RangeSet(const T& a, const T& b) { *this += Range<T>(a,b); }

    bool contiguous() const { return ranges.size() <= 1; }

    bool contains(const T& t) const;
    bool contains(const Range<T>&) const;

    /**@pre contiguous() */
    Range<T> toRange() const;

    bool operator==(const RangeSet<T>&) const;

    void addRange (const Range<T>&);
    void addSet (const RangeSet<T>&);

    RangeSet<T>& operator+=(const T& t) { return *this += Range<T>(t); }
    RangeSet<T>& operator+=(const Range<T>& r) { addRange(r); return *this; }
    RangeSet<T>& operator+=(const RangeSet<T>& s) { addSet(s); return *this; }

    void removeRange (const Range<T>&);
    void removeSet (const RangeSet<T>&);

    RangeSet<T>& operator-=(const T& t)  { return *this -= Range<T>(t); }
    RangeSet<T>& operator-=(const Range<T>& r) { removeRange(r); return *this; }
    RangeSet<T>& operator-=(const RangeSet<T>& s) { removeSet(s); return *this; }

    T front() const { return ranges.front().begin(); }
    T back() const { return ranges.back().end(); }

    // Iterate over elements in the set.
    iterator begin() const;
    iterator end() const;

    // Iterate over ranges in the set.
    typedef typename Ranges::const_iterator RangeIterator;
    RangeIterator rangesBegin() const { return ranges.begin(); }
    RangeIterator rangesEnd() const { return ranges.end(); }
    size_t rangesSize() const { return ranges.size(); }

    // The difference between the start and end of this range set
    uint32_t span() const;

    size_t size() const;
    bool empty() const { return ranges.empty(); }
    void clear() { ranges.clear(); }

    /** Return the largest contiguous range containing x.
     * Returns the empty range [x,x) if x is not in the set.
     */
    Range<T> rangeContaining(const T&) const;

    template <class S> void serialize(S& s) { s.split(*this); s(ranges.begin(), ranges.end()); }
    template <class S> void encode(S& s) const { s(uint16_t(ranges.size()*sizeof(Range<T>))); }
    template <class S> void decode(S& s) { uint16_t sz; s(sz); ranges.resize(sz/sizeof(Range<T>)); }

 private:
    static size_t accumulateSize(size_t s, const Range<T>& r) { return s+r.size(); }
    Ranges ranges;

  template <class U> friend std::ostream& operator<<(std::ostream& o, const RangeSet<U>& r);

  friend class iterator;
};

template <class T>
std::ostream& operator<<(std::ostream& o, const Range<T>& r) {
    return o << "[" << r.begin() << "," << r.end() << ")";
}

template <class T>
std::ostream& operator<<(std::ostream& o, const RangeSet<T>& rs) {
    std::ostream_iterator<Range<T> > i(o, " ");
    o << "{ ";
    std::copy(rs.ranges.begin(), rs.ranges.end(), i);
    return o << "}";
}

template <class T>
bool RangeSet<T>::contains(const T& t) const {
    typename Ranges::const_iterator i =
        std::lower_bound(ranges.begin(), ranges.end(), Range<T>(t));
    return i != ranges.end() && i->contains(t);
}

template <class T>
bool RangeSet<T>::contains(const Range<T>& r) const {
    typename Ranges::const_iterator i =
        std::lower_bound(ranges.begin(), ranges.end(), r);
    return i != ranges.end() && i->contains(r);
}

template <class T> void RangeSet<T>::addRange(const Range<T>& r) {
    if (r.empty()) return;
    typename Ranges::iterator i =
        std::lower_bound(ranges.begin(), ranges.end(), r);
    if (i == ranges.end() || !i->touching(r))
        ranges.insert(i, r);
    else {
        i->merge(r);
        typename Ranges::iterator j = i;
        if (++j != ranges.end() && i->touching(*j)) {
            i->merge(*j);
            ranges.erase(j);
        }
    }
}


template <class T> void RangeSet<T>::addSet(const RangeSet<T>& s) {
    typedef RangeSet<T>& (RangeSet<T>::*RangeSetRangeOp)(const Range<T>&);
    std::for_each(s.ranges.begin(), s.ranges.end(),
                  boost::bind((RangeSetRangeOp)&RangeSet<T>::operator+=, this, _1));
}

template <class T> void RangeSet<T>::removeRange(const Range<T>& r) {
    if (r.empty()) return;
    typename Ranges::iterator i,j;
    i = std::lower_bound(ranges.begin(), ranges.end(), r);
    if (i == ranges.end() || i->begin() >= r.end())
        return;                 // Outside of set
    if (*i == r)                // Erase i
        ranges.erase(i);
    else if (i->strictContains(r)) {  // Split i
        Range<T> i1(i->begin(), r.begin());
        Range<T> i2(r.end(), i->end());
        *i = i2;
        ranges.insert(i, i1);
    } else {
        if (i->begin() < r.begin()) { // Truncate i
            i->end(r.begin());
            ++i;
        }
        for (j = i; j != ranges.end() && r.contains(*j); ++j)
            ;                   // Ranges to erase.
        if (j != ranges.end() && j->end() > r.end())
            j->begin(r.end());   // Truncate j
        ranges.erase(i,j);
    }
}

template <class T> void RangeSet<T>::removeSet(const RangeSet<T>& r) {
    std::for_each(
        r.ranges.begin(), r.ranges.end(),
        boost::bind(&RangeSet<T>::removeRange, this, _1));
}

template <class T> Range<T> RangeSet<T>::toRange() const {
    assert(contiguous());
    return empty() ? Range<T>() :  ranges.front();
}

template <class T> void RangeSet<T>::iterator::increment() {
    assert(ranges && iter != ranges->end());
    if (!iter->contains(++value)) {
        ++iter;
        if (iter == ranges->end())
            *this=iterator();   // end() iterator
        else
            value=iter->begin();
    }
}

template <class T> bool RangeSet<T>::operator==(const RangeSet<T>& r) const {
    return ranges.size() == r.ranges.size() && std::equal(ranges.begin(), ranges.end(), r.ranges.begin());
}

template <class T> typename RangeSet<T>::iterator RangeSet<T>::begin() const {
    return empty() ? end() : iterator(ranges, ranges.begin(), front());
}

template <class T> typename RangeSet<T>::iterator RangeSet<T>::end() const {
    return iterator();
}

template <class T> bool RangeSet<T>::iterator::equal(const iterator& i) const {
    return ranges==i.ranges && (ranges==0 || value==i.value);
}

template <class T> Range<T> RangeSet<T>::rangeContaining(const T& t) const {
    typename Ranges::const_iterator i =
        std::lower_bound(ranges.begin(), ranges.end(), Range<T>(t));
    return (i != ranges.end() && i->contains(t)) ? *i : Range<T>(t,t);
}

template <class T> uint32_t RangeSet<T>::span() const {
    if (ranges.empty()) return 0;
    return ranges.back().last() - ranges.front().first();
}

template <class T> size_t RangeSet<T>::size() const {
    return std::accumulate(rangesBegin(), rangesEnd(), 0, &RangeSet<T>::accumulateSize);
}

} // namespace qpid


#endif  /*!QPID_RANGESET_H*/
