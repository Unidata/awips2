#ifndef QPID_AMQP_0_10_SERIALIZABLESTRING_H
#define QPID_AMQP_0_10_SERIALIZABLESTRING_H

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
namespace qpid {
namespace amqp_0_10 {

/** Template for length-prefixed strings/arrays.
 * Unique parameter allows creation of distinct SerializableString
 * types with the smae T/SizeType
 */
template <class T, class SizeType, int Unique=0>
struct SerializableString : public std::basic_string<T> {
    SerializableString() {}
    template <class U> SerializableString(const U& u) : std::basic_string<T>(u) {}
    template <class I> SerializableString(const I& i, const I& j) : std::basic_string<T>(i,j) {}

    using std::basic_string<T>::operator=;

    template <class S> void serialize(S& s) { s.split(*this); }

    template <class S> void encode(S& s) const {
        s(SizeType(this->size()))(this->begin(), this->end());
    }

    template <class S> void decode(S& s) {
        SizeType newSize;
        s(newSize);
        this->resize(newSize);
        s(this->begin(), this->end());
    }
};

// TODO aconway 2008-02-29: separate ostream ops
template <class T, class SizeType>
std::ostream& operator<<(std::ostream& o, const SerializableString<T,SizeType>& s) {
    const std::basic_string<T> str(s);
    return o << str.c_str();    // TODO aconway 2008-02-29: why doesn't o<<str work?
}

}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_SERIALIZABLESTRING_H*/
