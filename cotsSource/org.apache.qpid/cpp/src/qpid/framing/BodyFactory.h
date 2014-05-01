#ifndef QPID_FRAMING_BODYFACTORY_H
#define QPID_FRAMING_BODYFACTORY_H

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

#include <boost/intrusive_ptr.hpp>

namespace qpid {
namespace framing {

/**
 * Indirect creation of body types to allow centralized changes to
 * memory management strategy.
 */
class BodyFactory {
  public:
    template <class BodyType> static boost::intrusive_ptr<BodyType> create() {
        return new BodyType;
    }

    template <class BodyType> static boost::intrusive_ptr<BodyType> copy(const BodyType& body) {
        return new BodyType(body);
    }
};

}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_BODYFACTORY_H*/
