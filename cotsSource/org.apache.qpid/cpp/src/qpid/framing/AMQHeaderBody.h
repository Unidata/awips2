#ifndef QPID_FRAMING_AMQHEADERBODY_H
#define QPID_FRAMING_AMQHEADERBODY_H

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
#include "qpid/framing/amqp_types.h"
#include "qpid/framing/AMQBody.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/DeliveryProperties.h"
#include "qpid/framing/MessageProperties.h"
#include "qpid/CommonImportExport.h"
#include <iostream>

#include <boost/optional.hpp>


namespace qpid {
namespace framing {

class AMQHeaderBody :  public AMQBody
{
    template <class T> struct OptProps { boost::optional<T> props; };
    template <class Base, class T>
    struct PropSet : public Base, public OptProps<T> {
        uint32_t encodedSize() const {
            const boost::optional<T>& p=this->OptProps<T>::props;
            return (p ? p->encodedSize() : 0) + Base::encodedSize();
        }
        void encode(Buffer& buffer) const {
            const boost::optional<T>& p=this->OptProps<T>::props;
            if (p) p->encode(buffer);
            Base::encode(buffer);
        }
        bool decode(Buffer& buffer, uint32_t size, uint16_t type) {
            boost::optional<T>& p=this->OptProps<T>::props;
            if (type == T::TYPE) {
                p=T();
                p->decodeStructBody(buffer, size);
                return true;
        }
            else
                return Base::decode(buffer, size, type);
        }        
        void print(std::ostream& out) const {
            const boost::optional<T>& p=this->OptProps<T>::props;
            if (p) out << *p;
            Base::print(out);
        }
    };

    struct Empty {
        uint32_t encodedSize() const { return 0; }
        void encode(Buffer&) const {};
        bool decode(Buffer&, uint32_t, uint16_t) const { return false; };
        void print(std::ostream&) const {}
    };

    // Could use boost::mpl::fold to construct a larger set.
    typedef  PropSet<PropSet<Empty, DeliveryProperties>, MessageProperties> Properties;

    Properties properties;
    
public:

    inline uint8_t type() const { return HEADER_BODY; }

    QPID_COMMON_EXTERN uint32_t encodedSize() const;
    QPID_COMMON_EXTERN void encode(Buffer& buffer) const;
    QPID_COMMON_EXTERN void decode(Buffer& buffer, uint32_t size);
    QPID_COMMON_EXTERN uint64_t getContentLength() const;
    QPID_COMMON_EXTERN void print(std::ostream& out) const;
    QPID_COMMON_EXTERN void accept(AMQBodyConstVisitor&) const;

    template <class T> T* get(bool create) {
        boost::optional<T>& p=properties.OptProps<T>::props;
        if (create && !p) p=T();
        return p.get_ptr();
    }

    template <class T> const T* get() const {
        return properties.OptProps<T>::props.get_ptr();
    }

    boost::intrusive_ptr<AMQBody> clone() const { return BodyFactory::copy(*this); }
};

}}



#endif  /*!QPID_FRAMING_AMQHEADERBODY_H*/
