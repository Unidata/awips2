#ifndef QPID_AMQP_0_10_UNIT_H
#define QPID_AMQP_0_10_UNIT_H

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

#include "qpid/amqp_0_10/ControlHolder.h"
#include "qpid/amqp_0_10/CommandHolder.h"
#include "qpid/amqp_0_10/Header.h"
#include "qpid/amqp_0_10/Body.h"
#include "qpid/amqp_0_10/FrameHeader.h"

#include <boost/variant.hpp>
#include <ostream>

namespace qpid {
namespace amqp_0_10 {

/**
 * A Unit contains a frame header and associated value.
 * For all types except BODY the frame header is for a complete segment.
 */
class Unit {
  public:
    explicit Unit(const FrameHeader& h=FrameHeader()) : header(h) { updateVariant(); }

    /**
     *@param flags: is ORed with the required flags for type T.
     */
    template <class T>
    explicit Unit(const T& t, uint8_t flags=0) : variant(t) { updateHeader(flags); }

    void setHeader(FrameHeader& h) { header = h; updateVariant(); }
    const FrameHeader& getHeader() const { return header; }

    template<class T> const T* get() const { return boost::get<T>(&variant); }
    template<class T> T* get() { return boost::get<T>(&variant); }
    template<class T> Unit& operator=(const T& t) { variant=t; return *this; }

    template <class V> typename V::result_type applyVisitor(V& v) const {
        variant.apply_visitor(v); 
    }
    
    template <class S> void serialize(S& s) { variant.apply_visitor(s); s.split(*this); }
    template <class S> void encode(S&) const {} 
    template <class S> void decode(S&) { updateHeader(header.getFlags()); }

  private:
    typedef boost::variant<ControlHolder, CommandHolder, Header, Body> Variant;

    void updateHeader(uint8_t flags);
    void updateVariant();
    
    Variant variant;
    FrameHeader header;

  friend std::ostream& operator<<(std::ostream& o, const Unit& u);
};

std::ostream& operator<<(std::ostream& o, const Unit& u);
    
}} // namespace qpid::amqp_0_10

#endif  /*!QPID_AMQP_0_10_UNIT_H*/
