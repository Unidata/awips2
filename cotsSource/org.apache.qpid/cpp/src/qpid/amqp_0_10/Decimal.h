#ifndef TESTS_DECIMAL_H
#define TESTS_DECIMAL_H

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
#include <ostream>

namespace qpid {
namespace amqp_0_10 {

template <class E, class M> struct Decimal {
    E exponent;
    M mantissa;
    
    Decimal(E exp=0, M man=0) : exponent(exp), mantissa(man) {}

    bool operator==(const Decimal& d) const {
        return exponent == d.exponent && mantissa == d.mantissa;
    }

    // TODO aconway 2008-02-20: We could provide arithmetic operators
    // if anybody really cares about this type.

    template <class S> void serialize(S& s) { s(exponent)(mantissa); }
};

template<class E, class M>
inline std::ostream& operator<<(std::ostream& o, const Decimal<E,M>& d) {
    return o << "Decimal{" << d.mantissa << "/10^" << (int)d.exponent << "}";
}
}}

#endif  /*!TESTS_DECIMAL_H*/
