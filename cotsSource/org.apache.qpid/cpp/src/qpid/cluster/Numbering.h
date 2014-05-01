#ifndef QPID_CLUSTER_NUMBERING_H
#define QPID_CLUSTER_NUMBERING_H

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

#include <map>
#include <vector>

namespace qpid {
namespace cluster {

/**
 * A set of numbered T, with two way mapping number->T T->number
 * Used to construct numberings of objects by code sending and receiving updates.
 */
template <class T> class Numbering
{
  public:
    size_t size() const { return byNumber.size(); }

    size_t add(const T& t) {
        size_t n = (*this)[t];  // Already in the set?
        if (n == size()) {
            byObject[t] = n;
            byNumber.push_back(t);
        }
        return n;
    }

    void clear() { byObject.clear(); byNumber.clear(); }

    /**@return object at index n or T() if n > size() */
    T operator[](size_t n) const { return(n < size()) ? byNumber[n] : T(); }

    /**@return index of t or size() if t is not in the map */
    size_t operator[](const T& t) const {
        typename Map::const_iterator i = byObject.find(t);
        return (i != byObject.end()) ? i->second : size();
    }

    bool contains(const T& t) const { return (*this)[t] == size(); }

  private:
    typedef std::map<T, size_t> Map;
    Map byObject;
    std::vector<T> byNumber;
};

}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_NUMBERING_H*/
